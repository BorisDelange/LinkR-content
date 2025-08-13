# ==========================================
# server_code.R - Code Editor Server Logic
# ==========================================

# ======================================
# INITIALIZATION
# ======================================

# Initialize code storage variable
m$code_%widget_id% <- ""

# Fix ACE editor rendering issues on startup
# Delay ensures DOM is fully loaded before triggering resize
shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))

# ======================================
# CODE EDITOR KEYBOARD SHORTCUTS
# ======================================

# Handle comment/uncomment keyboard shortcut (Ctrl+Shift+C)
observe_event(input$code_%widget_id%_comment, {
    toggle_comments(
        input_id = "code_%widget_id%", 
        code = input$code_%widget_id%,
        selection = input$code_%widget_id%_comment$range, 
        session = session
    )
})

# Handle run all code keyboard shortcut (Ctrl+Shift+Enter)
observe_event(input$code_%widget_id%_run_all, {
    # Only allow code execution if user has console access
    if ("projects_widgets_console" %in% user_accesses) {
        m$code_%widget_id% <- input$code_%widget_id%
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

# Handle save keyboard shortcut (Ctrl+S)
observe_event(input$code_%widget_id%_save, {
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_output_settings_and_code_%widget_id%', Math.random());"))
})

# ======================================
# OUTPUT DISPLAY CONTROLLER
# ======================================

# Main code execution handler - triggered by display button or shortcuts
observe_event(input$display_output_%widget_id%, {
    
    # Determine current tab (default to output_settings if not set)
    current_tab <- if (length(input$current_tab_%widget_id%) == 0) {
        "output_settings"
    } else {
        input$current_tab_%widget_id%
    }
    
    # ====================
    # AUTO-GENERATE CODE FROM OUTPUT SETTINGS
    # ====================
    if (current_tab == "output_settings") {
        
        # Extract healthcare indicator settings from UI inputs
        indicator <- if (length(input$indicator_%widget_id%) > 0) {
            input$indicator_%widget_id%
        } else {
            "patient_count"  # Default indicator
        }
        
        # Extract indicator scope
        indicator_scope <- if (length(input$indicator_scope_%widget_id%) > 0) {
            input$indicator_scope_%widget_id%
        } else {
            "hospitalization"  # Default scope
        }
        
        # Extract selected hospital units
        hospital_units <- if (length(input$hospital_unit_%widget_id%) > 0) {
            input$hospital_unit_%widget_id%
        } else {
            NULL
        }
        
        # Generate R code based on current healthcare configuration
        generated_code <- generate_healthcare_code_%widget_id%(
            indicator = indicator,
            indicator_scope = indicator_scope,
            hospital_units = hospital_units,
            legend_1 = input$legend_1_%widget_id%,
            legend_2 = input$legend_2_%widget_id%
        )
        
        # Update ACE editor with generated code
        shinyAce::updateAceEditor(session, "code_%widget_id%", value = generated_code)
        
        # Store code and trigger execution
        m$code_%widget_id% <- generated_code
        shinyjs::delay(500, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());")))
    }
    # ====================
    # MANUAL CODE EXECUTION
    # ====================
    else if ("projects_widgets_console" %in% user_accesses) {
        # If on code tab, run whatever is currently in the editor
        m$code_%widget_id% <- input$code_%widget_id%
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

# ======================================
# HEALTHCARE CODE GENERATION FUNCTION
# ======================================

# Generate HTML code for healthcare indicators using OMOP data
generate_healthcare_code_%widget_id% <- function(indicator = "patient_count", indicator_scope = "hospitalization", hospital_units = c(), legend_1 = "", legend_2 = "") {
    
    code_lines <- c()
    
    # Define reusable CSS styles
    card_container_style <- "text-align: center; height: 100%; display: flex; align-items: center; flex-direction: column; justify-content: center;"
    icon_style <- "margin-bottom: 10px;"
    value_style <- "font-size: 36px; font-weight: bold; margin: 10px 0;"
    label_style <- "color: #666; font-size: 14px; text-transform: uppercase;"
    tooltip_style <- "visibility: hidden; opacity: 0; position: fixed; background-color: rgba(0,0,0,0.9); color: white; padding: 8px 12px; border-radius: 4px; font-size: 12px; z-index: 9999; transition: opacity 0.3s; min-width: 200px; max-width: 300px; text-align: left; box-shadow: 0 4px 12px rgba(0,0,0,0.4); line-height: 1.3; pointer-events: none;"
    
    # Define color schemes for different indicators
    patient_color <- "#2C699A"
    admission_color <- "#1e7e34"
    mortality_color <- "#dc3545"
    length_stay_color <- "#fd7e14"
    
    # Define tooltip variables for direct integration
    tooltip_class_name <- paste0("custom-tooltip-%widget_id%")
    tooltip_class_name_units <- paste0("custom-tooltip-units-%widget_id%")
    
    # JavaScript for dynamic positioning
    generate_tooltip_js <- function(tooltip_selector, parent_element = FALSE) {
        element_ref <- if(parent_element) "this.parentElement" else "this"
        paste0("
                    var tooltip = ", element_ref, ".querySelector('", tooltip_selector, "');
                    var rect = this.getBoundingClientRect();
                    var viewportWidth = window.innerWidth;
                    var viewportHeight = window.innerHeight;
                    
                    var left = rect.left + (rect.width / 2) - (200 / 2);
                    var top = rect.bottom + 8;
                    
                    if (left + 200 > viewportWidth - 10) {
                        left = viewportWidth - 210;
                    }
                    if (left < 10) {
                        left = 10;
                    }
                    
                    if (top + 100 > viewportHeight - 10) {
                        top = rect.top - 100 - 8;
                    }
                    
                    tooltip.style.left = left + 'px';
                    tooltip.style.top = top + 'px';
                    tooltip.style.visibility = 'visible';
                    tooltip.style.opacity = '1';
        ")
    }
    
    generate_tooltip_hide_js <- function(tooltip_selector, parent_element = FALSE) {
        element_ref <- if(parent_element) "this.parentElement" else "this"
        paste0(element_ref, ".querySelector('", tooltip_selector, "').style.visibility = 'hidden'; ", element_ref, ".querySelector('", tooltip_selector, "').style.opacity = '0';")
    }
    
    tooltip_js_show_value <- generate_tooltip_js(paste0(".", tooltip_class_name))
    tooltip_js_hide_value <- generate_tooltip_hide_js(paste0(".", tooltip_class_name))
    tooltip_js_show_units <- generate_tooltip_js(paste0(".", tooltip_class_name_units), TRUE)
    tooltip_js_hide_units <- generate_tooltip_hide_js(paste0(".", tooltip_class_name_units), TRUE)
    
    # Extract unit IDs and names
    if (length(hospital_units) > 0) {
        if (is.list(hospital_units) && length(hospital_units) > 0) {
            if (!is.null(names(hospital_units[[1]])) && "key" %in% names(hospital_units[[1]])) {
                unit_ids <- sapply(hospital_units, function(x) x$key)
                unit_names <- sapply(hospital_units, function(x) x$text)
            } else {
                unit_ids <- unlist(hospital_units)
                unit_names <- unlist(hospital_units)
            }
        } else {
            unit_ids <- hospital_units
            unit_names <- hospital_units
        }
        
        unit_ids <- as.character(unit_ids)
        unit_names <- as.character(unit_names)
    } else {
        unit_ids <- c()
        unit_names <- c()
    }
    
    # Get care site names for selected units when scope is hospital units
    if (indicator_scope == "hospital_units" && length(unit_ids) > 0) {
        code_lines <- c(code_lines,
            "# Get care site names for selected units",
            "care_sites <-",
            "    d$care_site %>%",
            paste0("    dplyr::filter(care_site_id %in% c(", paste(unit_ids, collapse = ", "), ")) %>%"),
            "    dplyr::distinct(care_site_id, care_site_name) %>%",
            "    dplyr::arrange(care_site_name) %>%",
            "    dplyr::collect()",
            "",
            "# Create unit names list for tooltip",
            "if (nrow(care_sites) > 0) {",
            "    unit_names_for_tooltip <- paste0(\"- \", care_sites$care_site_name, collapse = \"<br>\")",
            "} else {",
            "    unit_names_for_tooltip <- \"\"",
            "}",
            ""
        )
    } else if (indicator_scope == "hospital_units") {
        code_lines <- c(code_lines,
            "# Validation: Check if units are selected when scope is hospital units",
            'error_msg <- i18np$t("please_select_at_least_one_hospital_unit")',
            'stop(error_msg)',
            ""
        )
    }
    
    if (indicator == "patient_count") {
        if (indicator_scope == "hospitalization") {
            code_lines <- c(code_lines,
                "# Extract distinct patient count from all hospital visits",
                "patients <-",
                "    d$data_subset$visit_occurrence %>%",
                "    dplyr::distinct(person_id) %>%",
                "    dplyr::collect()",
                "",
                "value <- nrow(patients)",
                "",
                "# Generate HTML card result",
                "res <- div()",
                "",
                "if (value > 0) {",
                generate_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"hospitalized_patients\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", patient_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-hospital-user fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", patient_color, "; ", value_style, "\","),
                "            value",
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, "\","),
                "            do.call(div, legend_div_content)",
                "        )",
                "    )",
                "}",
                ""
            )
        } else {
            code_lines <- c(code_lines,
                "# Extract distinct patient count from selected hospital units",
                "patients <-",
                "    d$data_subset$visit_detail %>%",
                paste0("    dplyr::filter(care_site_id %in% c(", paste(unit_ids, collapse = ", "), ")) %>%"),
                "    dplyr::distinct(person_id) %>%",
                "    dplyr::collect()",
                "",
                "value <- nrow(patients)",
                "",
                "# Generate HTML card result with unit info",
                "res <- div()",
                "",
                "if (value > 0) {",
                generate_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"patients\")", "i18np$t(\"in_selected_units\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", patient_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-hospital-user fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", patient_color, "; ", value_style, "\","),
                "            value",
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, "\","),
                "            div(",
                "                do.call(div, legend_div_content),",
                paste0("                div(class = \"", tooltip_class_name, "\", div("),
                "                    paste0(i18np$t(\"hospital_units\"), \" :\"),",
                "                    div(",
                "                        HTML(unit_names_for_tooltip),",
                "                        style = \"margin-top: 10px;\"",
                "                    )",
                paste0("                ), style = \"", tooltip_style, "\"),"),
                "                style = \"position: relative; cursor: pointer;\",",
                paste0("                onmouseover = \"", tooltip_js_show_value, "        \","),
                paste0("                onmouseout = \"", tooltip_js_hide_value, "\""),
                "            )",
                "        )",
                "    )",
                "}",
                ""
            )
        }
        
    } else if (indicator == "admission_count") {
        if (indicator_scope == "hospitalization") {
            code_lines <- c(code_lines,
                "# Extract hospital admissions count",
                "admissions <-",
                "    d$data_subset$visit_occurrence %>%",
                "    dplyr::collect()",
                "",
                "value <- nrow(admissions)",
                "",
                "# Generate HTML card result",
                "res <- div()",
                "",
                "if (value > 0) {",
                generate_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"admission_count\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", admission_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-bed fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", admission_color, "; ", value_style, "\","),
                "            value",
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, "\","),
                "            do.call(div, legend_div_content)",
                "        )",
                "    )",
                "}",
                ""
            )
        } else {
            code_lines <- c(code_lines,
                "# Extract hospital unit stays count",
                "admissions <-",
                "    d$data_subset$visit_detail %>%",
                paste0("    dplyr::filter(care_site_id %in% c(", paste(unit_ids, collapse = ", "), ")) %>%"),
                "    dplyr::collect()",
                "",
                "value <- nrow(admissions)",
                "",
                "# Generate HTML card result with unit info",
                "res <- div()",
                "",
                "if (value > 0) {",
                generate_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"stays\")", "i18np$t(\"in_selected_units\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", admission_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-bed fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", admission_color, "; ", value_style, "\","),
                "            value",
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, "\","),
                "            div(",
                "                do.call(div, legend_div_content),",
                paste0("                div(class = \"", tooltip_class_name, "\", div("),
                "                    paste0(i18np$t(\"hospital_units\"), \" :\"),",
                "                    div(",
                "                        HTML(unit_names_for_tooltip),",
                "                        style = \"margin-top: 10px;\"",
                "                    )",
                paste0("                ), style = \"", tooltip_style, "\"),"),
                "                style = \"position: relative; cursor: pointer;\",",
                paste0("                onmouseover = \"", tooltip_js_show_value, "        \","),
                paste0("                onmouseout = \"", tooltip_js_hide_value, "\""),
                "            )",
                "        )",
                "    )",
                "}",
                ""
            )
        }
        
    } else if (indicator == "mortality") {
        if (indicator_scope == "hospitalization") {
            code_lines <- c(code_lines,
                "# Extract hospital mortality during hospitalization",
                "deaths <-",
                "    d$data_subset$death %>%",
                "    dplyr::inner_join(d$data_subset$visit_occurrence, by = \"person_id\") %>%",
                "    dplyr::filter(death_datetime >= visit_start_datetime,",
                "                  death_datetime <= pmax(visit_end_datetime, Sys.time(), na.rm = TRUE)) %>%",
                "    dplyr::distinct(person_id, death_datetime) %>%",
                "    dplyr::collect()",
                "",
                "total_patients <-",
                "    d$data_subset$visit_occurrence %>%",
                "    dplyr::distinct(person_id) %>%",
                "    dplyr::collect()",
                "",
                "death_count <- nrow(deaths)",
                "total_count <- nrow(total_patients)",
                "value <- if(total_count > 0) round((death_count / total_count) * 100, 1) else 0",
                "",
                "# Generate HTML card result",
                "res <- div()",
                "",
                "if (total_count > 0) {",
                generate_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"mortality\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", mortality_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-heart-broken fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", mortality_color, "; ", value_style, " position: relative; cursor: pointer;\","),
                "            paste0(value, \"%\"),",
                paste0("            onmouseover = \"", tooltip_js_show_value, "        \","),
                paste0("            onmouseout = \"", tooltip_js_hide_value, "\","),
                paste0("            div(class = \"", tooltip_class_name, "\", paste0(death_count, \" \", i18np$t(\"deaths_on_total_patients\"), \" \", total_count, \" \", tolower(i18np$t(\"patients\"))), style = \"", tooltip_style, "\")"),
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, "\","),
                "            do.call(div, legend_div_content)",
                "        )",
                "    )",
                "}",
                ""
            )
        } else {
            code_lines <- c(code_lines,
                "# Extract unit-specific mortality during hospitalization",
                "deaths <-",
                "    d$data_subset$death %>%",
                "    dplyr::inner_join(d$data_subset$visit_detail, by = \"person_id\") %>%",
                paste0("    dplyr::filter(care_site_id %in% c(", paste(unit_ids, collapse = ", "), "),"),
                "                  death_datetime >= visit_detail_start_datetime,",
                "                  death_datetime <= pmax(visit_detail_end_datetime, Sys.time(), na.rm = TRUE)) %>%",
                "    dplyr::distinct(person_id, death_datetime, visit_detail_id) %>%",
                "    dplyr::collect()",
                "",
                "total_patients <-",
                "    d$data_subset$visit_detail %>%",
                paste0("    dplyr::filter(care_site_id %in% c(", paste(unit_ids, collapse = ", "), ")) %>%"),
                "    dplyr::distinct(person_id) %>%",
                "    dplyr::collect()",
                "",
                "death_count <- nrow(deaths)",
                "total_count <- nrow(total_patients)",
                "value <- if(total_count > 0) round((death_count / total_count) * 100, 1) else 0",
                "",
                "# Generate HTML card result with unit info",
                "res <- div()",
                "",
                "if (total_count > 0) {",
                generate_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"deaths\")", "i18np$t(\"in_selected_units\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", mortality_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-heart-broken fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", mortality_color, "; ", value_style, " position: relative; cursor: pointer;\","),
                "            paste0(value, \"%\"),",
                paste0("            onmouseover = \"", tooltip_js_show_value, "        \","),
                paste0("            onmouseout = \"", tooltip_js_hide_value, "\","),
                paste0("            div(class = \"", tooltip_class_name, "\", paste0(death_count, \" \", i18np$t(\"deaths_on_total_patients\"), \" \", total_count, \" \", tolower(i18np$t(\"patients\"))), tags$br(), tags$br(), paste0(i18np$t(\"deaths_during_hospitalization\")), style = \"", tooltip_style, "\")"),
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, " position: relative;\","),
                "            div(",
                "                do.call(div, append(legend_div_content, list(style = \"cursor: pointer;\",",
                paste0("                onmouseover = \"", tooltip_js_show_units, "\","),
                paste0("                onmouseout = \"", tooltip_js_hide_units, "\"))),"),
                paste0("            div(class = \"", tooltip_class_name_units, "\", div("),
                "                paste0(i18np$t(\"hospital_units\"), \" :\"),",
                "                div(",
                "                    HTML(unit_names_for_tooltip),",
                "                    style = \"margin-top: 10px;\"",
                "                )",
                paste0("            ), style = \"", tooltip_style, "\")"),
                "        )",
                "    )",
                "}",
                ""
            )
        }
        
    } else if (indicator == "average_length_of_stay") {
        if (indicator_scope == "hospitalization") {
            code_lines <- c(code_lines,
                "# Extract hospital length of stay data",
                "stays <-",
                "    d$data_subset$visit_occurrence %>%",
                "    dplyr::filter(!is.na(visit_end_date)) %>%",
                "    dplyr::mutate(length_of_stay = as.numeric(difftime(visit_end_date, visit_start_date, units = \"days\"))) %>%",
                "    dplyr::collect()",
                "",
                "# Calculate average length of stay",
                "value <- if(nrow(stays) > 0) round(mean(stays$length_of_stay, na.rm = TRUE), 1) else 0",
                "",
                "# Generate HTML card result",
                "res <- div()",
                "",
                "if (value > 0) {",
                generate_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"average_length_of_stay\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", length_stay_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-calendar-day fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", length_stay_color, "; ", value_style, "\","),
                "            paste0(value, \" \"), tags$span(i18np$t(\"days\"), style = \"font-size: 18px;\")",
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, "\","),
                "            do.call(div, legend_div_content)",
                "        )",
                "    )",
                "}",
                ""
            )
        } else {
            code_lines <- c(code_lines,
                "# Extract unit length of stay data",
                "stays <-",
                "    d$data_subset$visit_detail %>%",
                paste0("    dplyr::filter(care_site_id %in% c(", paste(unit_ids, collapse = ", "), "),"),
                "                  !is.na(visit_detail_end_date)) %>%",
                "    dplyr::mutate(length_of_stay = as.numeric(difftime(visit_detail_end_date, visit_detail_start_date, units = \"days\"))) %>%",
                "    dplyr::collect()",
                "",
                "# Calculate average length of stay",
                "value <- if(nrow(stays) > 0) round(mean(stays$length_of_stay, na.rm = TRUE), 1) else 0",
                "",
                "# Generate HTML card result with unit info",
                "res <- div()",
                "",
                "if (value > 0) {",
                generate_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"average_length_of_stay\")", "i18np$t(\"in_selected_units\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", length_stay_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-calendar-day fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", length_stay_color, "; ", value_style, "\","),
                "            paste0(value, \" \"), tags$span(i18np$t(\"days\"), style = \"font-size: 18px;\")",
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, "\","),
                "            div(",
                "                do.call(div, legend_div_content),",
                paste0("                div(class = \"", tooltip_class_name, "\", div("),
                "                    paste0(i18np$t(\"hospital_units\"), \" :\"),",
                "                    div(",
                "                        HTML(unit_names_for_tooltip),",
                "                        style = \"margin-top: 10px;\"",
                "                    )",
                paste0("                ), style = \"", tooltip_style, "\"),"),
                "                style = \"position: relative; cursor: pointer;\",",
                paste0("                onmouseover = \"", tooltip_js_show_value, "        \","),
                paste0("                onmouseout = \"", tooltip_js_hide_value, "\""),
                "            )",
                "        )",
                "    )",
                "}",
                ""
            )
        }
    }
    
    code_lines <- c(code_lines, "res")
    
    generated_code <- paste(code_lines, collapse = "\n")
    
    return(generated_code)
}

# Helper function to generate legend content based on text fields
generate_legend_code_%widget_id% <- function(legend_1, legend_2, default_legend_1, default_legend_2 = NULL) {
    legend_lines <- c()
    
    legend_lines <- c(legend_lines,
        "    # Determine legend content based on text field values",
        paste0("    legend_1_text <- \"", legend_1, "\""),
        paste0("    legend_2_text <- \"", legend_2, "\""),
        "    ",
        "    # Build legend display",
        "    legend_div_content <- tagList()",
        "    if (nchar(legend_1_text) > 0) {",
        "        legend_div_content <- tagList(legend_div_content, legend_1_text)",
        "        if (nchar(legend_2_text) > 0) {",
        "            legend_div_content <- tagList(legend_div_content, tags$br(), legend_2_text)",
        "        }",
        "    } else if (nchar(legend_2_text) > 0) {",
        "        legend_div_content <- tagList(legend_div_content, legend_2_text)",
        "    }",
        "    ",
        "    # Use default text if both legends are empty",
        "    if (length(legend_div_content) == 0) {"
    )
    
    if (!is.null(default_legend_2)) {
        legend_lines <- c(legend_lines,
            paste0("        legend_div_content <- tagList(", default_legend_1, ", tags$br(), ", default_legend_2, ")"))
    } else {
        legend_lines <- c(legend_lines,
            paste0("        legend_div_content <- tagList(", default_legend_1, ")"))
    }
    
    legend_lines <- c(legend_lines,
        "    }",
        "    "
    )
    
    return(legend_lines)
}

# ======================================
# AUTO-EXECUTION TRIGGERS
# ======================================

# Auto-run code when data context changes
observe_event(m$selected_subset, {
    # Check if auto-run is enabled
    if (!isTRUE(input$auto_update_%widget_id%)) {
        return()
    }
    
    # Execute code when data context changes
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    print("======================================= subset (%widget_id%) ========================")
})

observe_event(m$subset_dates, {
    # Check if auto-run is enabled
    if (!isTRUE(input$auto_update_%widget_id%)) {
        return()
    }
    
    # Execute code when data context changes
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    print("======================================= dates (%widget_id%) ========================")
})

# ======================================
# CODE EXECUTION ENGINE
# ======================================

# Main code execution handler
observe_event(input$run_code_%widget_id%, {
    
    # Initialize result variable
    res <- NULL
    error_message <- NULL
    
    # Check if subset is selected when required
    subset_loaded <- TRUE
    if (is.null(m$subset_persons) || nrow(m$subset_persons) == 0) {
        subset_loaded <- FALSE
        error_message <- i18np$t("no_subset_selected")
    }
    
    # ====================
    # EXECUTE USER CODE
    # ====================
    if (subset_loaded){
        tryCatch({
            # Execute the R code in the current environment
            eval(parse(text = m$code_%widget_id%))
        }, error = function(e) {
            # Capture any execution errors
            error_message <<- paste("Error executing code:", e$message)
        })
    }
    
    # ====================
    # HANDLE EXECUTION RESULTS
    # ====================
    
    # Show error message if execution failed or no output was generated
    if (!is.null(error_message) || is.null(res)) {
        
        display_message <- if (!is.null(error_message)) {
            error_message
        } else {
            "No output generated. Please check your code and settings."
        }
        
        # Display error in console output
        output$console_output_%widget_id% <- renderText(display_message)
        shinyjs::hide("ui_output_div_%widget_id%")
        shinyjs::show("console_output_div_%widget_id%")
        
        # Clear UI output
        output$ui_output_%widget_id% <- renderUI(NULL)
    }
    
    # Display output if execution was successful
    if (is.null(error_message) && !is.null(res)) {
        # Render the healthcare indicator card in UI output
        output$ui_output_%widget_id% <- renderUI(res)
        shinyjs::hide("console_output_div_%widget_id%")
        shinyjs::show("ui_output_div_%widget_id%")
    }
    
    # ====================
    # AUTO-NAVIGATION
    # ====================
    # Optional: automatically switch to output tab after execution
    if (isFALSE(input$output_and_settings_side_by_side_%widget_id%)) shinyjs::click("output_button_%widget_id%")
})
