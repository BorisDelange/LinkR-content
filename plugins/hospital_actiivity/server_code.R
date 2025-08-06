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
            hospital_units = hospital_units
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
generate_healthcare_code_%widget_id% <- function(indicator = "patient_count", indicator_scope = "hospitalization", hospital_units = c()) {
    
    code_lines <- c()
    
    # Define tooltip variables for direct integration
    tooltip_class_name <- paste0("custom-tooltip-%widget_id%")
    tooltip_class_name_units <- paste0("custom-tooltip-units-%widget_id%")
    tooltip_style_value <- "visibility: hidden; opacity: 0; position: absolute; top: 100%; left: 0; background-color: rgba(0,0,0,0.9); color: white; padding: 8px 12px; border-radius: 4px; font-size: 12px; z-index: 1000; transition: opacity 0.3s; min-width: 200px; text-align: left; box-shadow: 0 2px 8px rgba(0,0,0,0.3); line-height: 1.3;"
    tooltip_js_show_value <- paste0("this.querySelector('.", tooltip_class_name, "').style.visibility = 'visible'; this.querySelector('.", tooltip_class_name, "').style.opacity = '1';")
    tooltip_js_hide_value <- paste0("this.querySelector('.", tooltip_class_name, "').style.visibility = 'hidden'; this.querySelector('.", tooltip_class_name, "').style.opacity = '0';")
    tooltip_js_show_units <- paste0("this.querySelector('.", tooltip_class_name_units, "').style.visibility = 'visible'; this.querySelector('.", tooltip_class_name_units, "').style.opacity = '1';")
    tooltip_js_hide_units <- paste0("this.querySelector('.", tooltip_class_name_units, "').style.visibility = 'hidden'; this.querySelector('.", tooltip_class_name_units, "').style.opacity = '0';")
    
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
        unit_ids_str <- paste(unit_ids, collapse = ", ")
        unit_names_str <- paste(paste0("- ", unit_names), collapse = "\\n")
    } else {
        unit_ids <- c()
        unit_names <- c()
        unit_ids_str <- ""
        unit_names_str <- ""
    }
    
    # Only generate care site code when scope is hospital units and units are selected
    if (indicator_scope == "hospital_units" && length(unit_ids) > 0) {
        code_lines <- c(code_lines,
            "# Get care site names for selected units",
            "care_sites <- get_query(\"",
            "    SELECT DISTINCT care_site_id, care_site_name",
            "    FROM care_site",
            paste0("    WHERE care_site_id IN (", unit_ids_str, ")"),
            "    ORDER BY care_site_name",
            "\")",
            "",
            "# Create unit names list for tooltip",
            "if (nrow(care_sites) > 0) {",
            "    unit_names_for_tooltip <- paste0(\"- \", care_sites$care_site_name, collapse = \"<br>\")",
            "} else {",
            "    unit_names_for_tooltip <- \"\"",
            "}",
            "",
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
                "patients <- get_query(\"",
                "    SELECT DISTINCT person_id",
                "    FROM visit_occurrence",
                "\")",
                "",
                "value <- nrow(patients)",
                "",
                "# Generate HTML card result",
                "res <- div()",
                "",
                "if (value > 0) {",
                "    res <- div(",
                "        style = \"",
                "            text-align: center;",
                "            height: 100%;",
                "            display: flex;",
                "            align-items: center;",
                "            flex-direction: column;",
                "            justify-content: center;",
                "        \",",
                "        div(",
                "            style = \"color: #2C699A; margin-bottom: 10px;\",",
                "            tags$i(class = \"fas fa-hospital-user fa-2x\")",
                "        ),",
                "        div(",
                "            style = \"color: #2C699A; font-size: 36px; font-weight: bold; margin: 10px 0;\",",
                "            value",
                "        ),",
                "        div(",
                "            style = \"color: #666; font-size: 14px; text-transform: uppercase;\",",
                "            div(",
                "                i18np$t(\"hospitalized_patients\")",
                "            )",
                "        )",
                "    )",
                "}",
                ""
            )
        } else {
            code_lines <- c(code_lines,
                "# Extract distinct patient count from selected hospital units",
                "patients <- get_query(\"",
                "    SELECT DISTINCT person_id",
                "    FROM visit_detail",
                paste0("    WHERE care_site_id IN (", unit_ids_str, ")"),
                "\")",
                "",
                "value <- nrow(patients)",
                "",
                "# Generate HTML card result with unit info",
                "res <- div()",
                "",
                "if (value > 0) {",
                "    res <- div(",
                "        style = \"",
                "            text-align: center;",
                "            height: 100%;",
                "            display: flex;",
                "            align-items: center;",
                "            flex-direction: column;",
                "            justify-content: center;",
                "        \",",
                "        div(",
                "            style = \"color: #2C699A; margin-bottom: 10px;\",",
                "            tags$i(class = \"fas fa-hospital-user fa-2x\")",
                "        ),",
                "        div(",
                "            style = \"color: #2C699A; font-size: 36px; font-weight: bold; margin: 10px 0;\",",
                "            value",
                "        ),",
                "        div(",
                "            style = \"color: #666; font-size: 14px; text-transform: uppercase;\",",
                "            div(",
                "                div(",
                "                    i18np$t(\"patients\"),",
                "                    tags$br(),",
                "                    i18np$t(\"in_selected_units\")",
                "                ),",
                paste0("                div(class = \"", tooltip_class_name, "\", div("),
                "                    paste0(i18np$t(\"hospital_units\"), \" :\"),",
                "                    div(",
                "                        HTML(unit_names_for_tooltip),",
                "                        style = \"margin-top: 10px;\"",
                "                    )",
                paste0("                ), style = \"", tooltip_style_value, "\"),"),
                "                style = \"position: relative; cursor: pointer;\",",
                paste0("                onmouseover = \"", tooltip_js_show_value, "\","),
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
                "admissions <- get_query(\"",
                "    SELECT visit_occurrence_id, person_id, visit_start_date, visit_end_date",
                "    FROM visit_occurrence",
                "\")",
                "",
                "value <- nrow(admissions)",
                "",
                "# Generate HTML card result",
                "res <- div()",
                "",
                "if (value > 0) {",
                "    res <- div(",
                "        style = \"",
                "            text-align: center;",
                "            height: 100%;",
                "            display: flex;",
                "            align-items: center;",
                "            flex-direction: column;",
                "            justify-content: center;",
                "        \",",
                "        div(",
                "            style = \"color: #1e7e34; margin-bottom: 10px;\",",
                "            tags$i(class = \"fas fa-bed fa-2x\")",
                "        ),",
                "        div(",
                "            style = \"color: #1e7e34; font-size: 36px; font-weight: bold; margin: 10px 0;\",",
                "            value",
                "        ),",
                "        div(",
                "            style = \"color: #666; font-size: 14px; text-transform: uppercase;\",",
                "            div(",
                "                i18np$t(\"admission_count\")",
                "            )",
                "        )",
                "    )",
                "}",
                ""
            )
        } else {
            code_lines <- c(code_lines,
                "# Extract hospital unit stays count",
                "admissions <- get_query(\"",
                "    SELECT visit_detail_id, person_id, visit_detail_start_date, visit_detail_end_date",
                "    FROM visit_detail",
                paste0("    WHERE care_site_id IN (", unit_ids_str, ")"),
                "\")",
                "",
                "value <- nrow(admissions)",
                "",
                "# Generate HTML card result with unit info",
                "res <- div()",
                "",
                "if (value > 0) {",
                "    res <- div(",
                "        style = \"",
                "            text-align: center;",
                "            height: 100%;",
                "            display: flex;",
                "            align-items: center;",
                "            flex-direction: column;",
                "            justify-content: center;",
                "        \",",
                "        div(",
                "            style = \"color: #1e7e34; margin-bottom: 10px;\",",
                "            tags$i(class = \"fas fa-bed fa-2x\")",
                "        ),",
                "        div(",
                "            style = \"color: #1e7e34; font-size: 36px; font-weight: bold; margin: 10px 0;\",",
                "            value",
                "        ),",
                "        div(",
                "            style = \"color: #666; font-size: 14px; text-transform: uppercase;\",",
                "            div(",
                "                div(",
                "                    i18np$t(\"stays\"),",
                "                    tags$br(),",
                "                    i18np$t(\"in_selected_units\")",
                "                ),",
                paste0("                div(class = \"", tooltip_class_name, "\", div("),
                "                    paste0(i18np$t(\"hospital_units\"), \" :\"),",
                "                    div(",
                "                        HTML(unit_names_for_tooltip),",
                "                        style = \"margin-top: 10px;\"",
                "                    )",
                paste0("                ), style = \"", tooltip_style_value, "\"),"),
                "                style = \"position: relative; cursor: pointer;\",",
                paste0("                onmouseover = \"", tooltip_js_show_value, "\","),
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
                "# Extract hospital mortality and patient data",
                "deaths <- get_query(\"",
                "    SELECT DISTINCT d.person_id, d.death_date",
                "    FROM death d",
                "    JOIN visit_occurrence v ON d.person_id = v.person_id",
                "\")",
                "",
                "total_patients <- get_query(\"",
                "    SELECT DISTINCT person_id",
                "    FROM visit_occurrence",
                "\")",
                "",
                "death_count <- nrow(deaths)",
                "total_count <- nrow(total_patients)",
                "value <- if(total_count > 0) round((death_count / total_count) * 100, 1) else 0",
                "",
                "# Generate HTML card result",
                "res <- div()",
                "",
                "if (total_count > 0) {",
                "    res <- div(",
                "        style = \"",
                "            text-align: center;",
                "            height: 100%;",
                "            display: flex;",
                "            align-items: center;",
                "            flex-direction: column;",
                "            justify-content: center;",
                "        \",",
                "        div(",
                "            style = \"color: #dc3545; margin-bottom: 10px;\",",
                "            tags$i(class = \"fas fa-heart-broken fa-2x\")",
                "        ),",
                "        div(",
                "            style = \"color: #dc3545; font-size: 36px; font-weight: bold; margin: 10px 0; position: relative; cursor: pointer;\",",
                "            paste0(value, \"%\"),",
                paste0("            onmouseover = \"", tooltip_js_show_value, "\","),
                paste0("            onmouseout = \"", tooltip_js_hide_value, "\","),
                paste0("            div(class = \"", tooltip_class_name, "\", paste0(death_count, \" décès sur \", total_count, \" patients\"), style = \"", tooltip_style_value, "\")"),
                "        ),",
                "        div(",
                "            style = \"color: #666; font-size: 14px; text-transform: uppercase;\",",
                "            i18np$t(\"mortality\")",
                "        )",
                "    )",
                "}",
                ""
            )
        } else {
            code_lines <- c(code_lines,
                "# Extract unit-specific mortality and patient data",
                "deaths <- get_query(\"",
                "    SELECT DISTINCT d.person_id, d.death_date",
                "    FROM death d",
                "    JOIN visit_detail vd ON d.person_id = vd.person_id",
                paste0("    WHERE vd.care_site_id IN (", unit_ids_str, ")"),
                "\")",
                "",
                "total_patients <- get_query(\"",
                "    SELECT DISTINCT person_id",
                "    FROM visit_detail",
                paste0("    WHERE care_site_id IN (", unit_ids_str, ")"),
                "\")",
                "",
                "death_count <- nrow(deaths)",
                "total_count <- nrow(total_patients)",
                "value <- if(total_count > 0) round((death_count / total_count) * 100, 1) else 0",
                "",
                "# Generate HTML card result with unit info",
                "res <- div()",
                "",
                "if (total_count > 0) {",
                "    res <- div(",
                "        style = \"",
                "            text-align: center;",
                "            height: 100%;",
                "            display: flex;",
                "            align-items: center;",
                "            flex-direction: column;",
                "            justify-content: center;",
                "        \",",
                "        div(",
                "            style = \"color: #dc3545; margin-bottom: 10px;\",",
                "            tags$i(class = \"fas fa-heart-broken fa-2x\")",
                "        ),",
                "        div(",
                "            style = \"color: #dc3545; font-size: 36px; font-weight: bold; margin: 10px 0; position: relative; cursor: pointer;\",",
                "            paste0(value, \"%\"),",
                paste0("            onmouseover = \"", tooltip_js_show_value, "\","),
                paste0("            onmouseout = \"", tooltip_js_hide_value, "\","),
                paste0("            div(class = \"", tooltip_class_name, "\", paste0(death_count, \" décès sur \", total_count, \" patients\"), style = \"", tooltip_style_value, "\")"),
                "        ),",
                "        div(",
                "            style = \"color: #666; font-size: 14px; text-transform: uppercase; position: relative;\",",
                "            div(",
                "                i18np$t(\"deaths\"),",
                "                tags$br(),",
                "                i18np$t(\"in_selected_units\"),",
                "                style = \"cursor: pointer;\",",
                paste0("                onmouseover = \"", tooltip_js_show_units, "\","),
                paste0("                onmouseout = \"", tooltip_js_hide_units, "\""),
                "            ),",
                paste0("            div(class = \"", tooltip_class_name_units, "\", div("),
                "                paste0(i18np$t(\"hospital_units\"), \" :\"),",
                "                div(",
                "                    HTML(unit_names_for_tooltip),",
                "                    style = \"margin-top: 10px;\"",
                "                )",
                paste0("            ), style = \"", tooltip_style_value, "\")"),
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
                "stays <- get_query(\"",
                "    SELECT visit_occurrence_id, person_id, visit_start_date, visit_end_date,",
                "           DATEDIFF('day', visit_start_date, visit_end_date) as length_of_stay",
                "    FROM visit_occurrence",
                "    WHERE visit_end_date IS NOT NULL",
                "\")",
                "",
                "# Calculate average length of stay",
                "value <- if(nrow(stays) > 0) round(mean(stays$length_of_stay, na.rm = TRUE), 1) else 0",
                "",
                "# Generate HTML card result",
                "res <- div()",
                "",
                "if (value > 0) {",
                "    res <- div(",
                "        style = \"",
                "            text-align: center;",
                "            height: 100%;",
                "            display: flex;",
                "            align-items: center;",
                "            flex-direction: column;",
                "            justify-content: center;",
                "        \",",
                "        div(",
                "            style = \"color: #fd7e14; margin-bottom: 10px;\",",
                "            tags$i(class = \"fas fa-calendar-day fa-2x\")",
                "        ),",
                "        div(",
                "            style = \"color: #fd7e14; font-size: 36px; font-weight: bold; margin: 10px 0;\",",
                "            paste0(value, \" \", tags$span(i18np$t(\"days\"), style = \"font-size: 18px;\"))",
                "        ),",
                "        div(",
                "            style = \"color: #666; font-size: 14px; text-transform: uppercase;\",",
                "            i18np$t(\"average_length_of_stay\")",
                "        )",
                "    )",
                "}",
                ""
            )
        } else {
            code_lines <- c(code_lines,
                "# Extract unit length of stay data",
                "stays <- get_query(\"",
                "    SELECT visit_detail_id, person_id, visit_detail_start_date, visit_detail_end_date,",
                "           DATEDIFF('day', visit_detail_start_date, visit_detail_end_date) as length_of_stay",
                "    FROM visit_detail",
                paste0("    WHERE care_site_id IN (", unit_ids_str, ")"),
                "    AND visit_detail_end_date IS NOT NULL",
                "\")",
                "",
                "# Calculate average length of stay",
                "value <- if(nrow(stays) > 0) round(mean(stays$length_of_stay, na.rm = TRUE), 1) else 0",
                "",
                "# Generate HTML card result with unit info",
                "res <- div()",
                "",
                "if (value > 0) {",
                "    res <- div(",
                "        style = \"",
                "            text-align: center;",
                "            height: 100%;",
                "            display: flex;",
                "            align-items: center;",
                "            flex-direction: column;",
                "            justify-content: center;",
                "        \",",
                "        div(",
                "            style = \"color: #fd7e14; margin-bottom: 10px;\",",
                "            tags$i(class = \"fas fa-calendar-day fa-2x\")",
                "        ),",
                "        div(",
                "            style = \"color: #fd7e14; font-size: 36px; font-weight: bold; margin: 10px 0;\",",
                "            paste0(value, \" \", tags$span(i18np$t(\"days\"), style = \"font-size: 18px;\"))",
                "        ),",
                "        div(",
                "            style = \"color: #666; font-size: 14px; text-transform: uppercase;\",",
                "            div(",
                "                div(",
                "                    i18np$t(\"average_length_of_stay\"),",
                "                    tags$br(),",
                "                    i18np$t(\"in_selected_units\")",
                "                ),",
                paste0("                div(class = \"", tooltip_class_name, "\", div("),
                "                    paste0(i18np$t(\"hospital_units\"), \" :\"),",
                "                    div(",
                "                        HTML(unit_names_for_tooltip),",
                "                        style = \"margin-top: 10px;\"",
                "                    )",
                paste0("                ), style = \"", tooltip_style_value, "\"),"),
                "                style = \"position: relative; cursor: pointer;\",",
                paste0("                onmouseover = \"", tooltip_js_show_value, "\","),
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

# ======================================
# AUTO-EXECUTION TRIGGERS
# ======================================

# Auto-run code when data context changes
observe_event(m$selected_person, {
    # Check if auto-run is enabled
    if (!isTRUE(input$auto_update_%widget_id%)) {
        return()
    }
    
    # Execute code when data context changes
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# ======================================
# CODE EXECUTION ENGINE
# ======================================

# Main code execution handler
observe_event(input$run_code_%widget_id%, {
    
    # Initialize result variable
    res <- NULL
    error_message <- NULL
    
    # ====================
    # EXECUTE USER CODE
    # ====================
    tryCatch({
        # Execute the R code in the current environment
        eval(parse(text = m$code_%widget_id%))
    }, error = function(e) {
        # Capture any execution errors
        error_message <<- paste("Error executing code:", e$message)
    })
    
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
