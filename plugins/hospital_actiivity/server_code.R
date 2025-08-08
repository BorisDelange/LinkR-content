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
    # Enhanced tooltip variables with portal-style positioning
    tooltip_class_name <- paste0("custom-tooltip-%widget_id%")
    tooltip_class_name_units <- paste0("custom-tooltip-units-%widget_id%")
    
    # Fixed positioning tooltip that escapes container bounds
    tooltip_style_value <- "visibility: hidden; opacity: 0; position: fixed; background-color: rgba(0,0,0,0.9); color: white; padding: 8px 12px; border-radius: 4px; font-size: 12px; z-index: 9999; transition: opacity 0.3s; min-width: 200px; max-width: 300px; text-align: left; box-shadow: 0 4px 12px rgba(0,0,0,0.4); line-height: 1.3; pointer-events: none;"
    
    # JavaScript for dynamic positioning
    tooltip_js_show_value <- paste0("
        var tooltip = this.querySelector('.", tooltip_class_name, "');
        var rect = this.getBoundingClientRect();
        var tooltipRect = tooltip.getBoundingClientRect();
        var viewportWidth = window.innerWidth;
        var viewportHeight = window.innerHeight;
        
        // Calculate optimal position
        var left = rect.left + (rect.width / 2) - (200 / 2);
        var top = rect.bottom + 8;
        
        // Adjust if tooltip would go off-screen horizontally
        if (left + 200 > viewportWidth - 10) {
            left = viewportWidth - 210;
        }
        if (left < 10) {
            left = 10;
        }
        
        // Adjust if tooltip would go off-screen vertically
        if (top + 100 > viewportHeight - 10) {
            top = rect.top - 100 - 8;
        }
        
        tooltip.style.left = left + 'px';
        tooltip.style.top = top + 'px';
        tooltip.style.visibility = 'visible';
        tooltip.style.opacity = '1';
    ")
    
    tooltip_js_hide_value <- paste0("this.querySelector('.", tooltip_class_name, "').style.visibility = 'hidden'; this.querySelector('.", tooltip_class_name, "').style.opacity = '0';")
    
    # Similar logic for units tooltip
    tooltip_js_show_units <- paste0("
        var tooltip = this.parentElement.querySelector('.", tooltip_class_name_units, "');
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
    
    tooltip_js_hide_units <- paste0("this.parentElement.querySelector('.", tooltip_class_name_units, "').style.visibility = 'hidden'; this.parentElement.querySelector('.", tooltip_class_name_units, "').style.opacity = '0';")
    
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
            "care_sites <- d$con %>% run_query(\"",
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
                "",
                #"visit_occurrence_ids <- paste(m$subset_persons$visit_occurrence_id, collapse = \", \")",
                #"",
                "patients <- d$con %>% run_query(paste0(\"",
                "    SELECT DISTINCT person_id",
                "    FROM visit_occurrence",
                #"    WHERE visit_occurrence_id IN (\", visit_occurrence_ids, \")",
                "    WHERE visit_start_date >= DATE('\", m$subset_dates[1], \"')",
                "    AND visit_start_date <= DATE('\", m$subset_dates[2], \"')",
                "\"))",
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
                "",
                #"visit_detail_ids <- paste(m$subset_persons$visit_detail_id, collapse = \", \")",
                #"",
                "patients <- d$con %>% run_query(paste0(\"",
                "    SELECT DISTINCT person_id",
                "    FROM visit_detail",
                paste0("    WHERE care_site_id IN (", unit_ids_str, ")"),
                #"    AND visit_detail_id IN (\", visit_detail_ids, \")",
                "    AND visit_detail_start_date >= DATE('\", m$subset_dates[1], \"')",
                "    AND visit_detail_start_date <= DATE('\", m$subset_dates[2], \"')",
                "\"))",
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
                "",
                #"visit_occurrence_ids <- paste(m$subset_persons$visit_occurrence_id, collapse = \", \")",
                #"",
                "admissions <- d$con %>% run_query(paste0(\"",
                "    SELECT visit_occurrence_id, person_id, visit_start_date, visit_end_date",
                "    FROM visit_occurrence",
                #"    WHERE visit_occurrence_id IN (\", visit_occurrence_ids, \")",
                "    WHERE visit_start_date >= DATE('\", m$subset_dates[1], \"')",
                "    AND visit_start_date <= DATE('\", m$subset_dates[2], \"')",
                "\"))",
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
                "",
                #"visit_detail_ids <- paste(m$subset_persons$visit_detail_id, collapse = \", \")",
                #"",
                "admissions <- d$con %>% run_query(paste0(\"",
                "    SELECT visit_detail_id, person_id, visit_detail_start_date, visit_detail_end_date",
                "    FROM visit_detail",
                paste0("    WHERE care_site_id IN (", unit_ids_str, ")"),
                #"    AND visit_detail_id IN (\", visit_detail_ids, \")",
                "    AND visit_detail_start_date >= DATE('\", m$subset_dates[1], \"')",
                "    AND visit_detail_start_date <= DATE('\", m$subset_dates[2], \"')",
                "\"))",
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
                "# Extract hospital mortality during hospitalization",
                "",
                #"visit_occurrence_ids <- paste(m$subset_persons$visit_occurrence_id, collapse = \", \")",
                #"",
                "# Common WHERE clause for both queries",
                "common_where <- paste0(\"",
                #"    WHERE v.person_id IN (\", person_ids, \")",
                "    WHERE v.visit_start_date >= DATE('\", m$subset_dates[1], \"')",
                "    AND v.visit_start_date <= DATE('\", m$subset_dates[2], \"')",
                "\")",
                "",
                "deaths <- d$con %>% run_query(paste0(\"",
                "    SELECT DISTINCT d.person_id, d.death_datetime",
                "    FROM death d",
                "    JOIN visit_occurrence v ON d.person_id = v.person_id",
                "    \", common_where, \"",
                "    AND d.death_datetime >= v.visit_start_datetime",
                "    AND d.death_datetime <= COALESCE(v.visit_end_datetime, CAST(NOW() AS DATE))",
                "\"))",
                "",
                "total_patients <- d$con %>% run_query(paste0(\"",
                "    SELECT DISTINCT v.person_id",
                "    FROM visit_occurrence v",
                "    \", common_where",
                "))",
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
                paste0("            div(class = \"", tooltip_class_name, "\", paste0(death_count, \" \", i18np$t(\"deaths_on_total_patients\"), \" \", total_count, \" \", tolower(i18np$t(\"patients\"))), style = \"", tooltip_style_value, "\")"),
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
                "# Extract unit-specific mortality during hospitalization",
                "",
                #"visit_detail_ids <- paste(m$subset_persons$visit_detail_id, collapse = \", \")",
                #"",
                "# Common WHERE clause for both queries",
                "common_where <- paste0(\"",
                #"    WHERE vd.visit_detail_id IN (\", visit_detail_ids, \")",
                "    WHERE vd.visit_detail_start_date >= DATE('\", m$subset_dates[1], \"')",
                "    AND vd.visit_detail_start_date <= DATE('\", m$subset_dates[2], \"')",
                "\")",
                "",
                "deaths <- d$con %>% run_query(paste0(\"",
                "    SELECT DISTINCT d.person_id, d.death_datetime, vd.visit_detail_id",
                "    FROM death d",
                "    JOIN visit_detail vd ON d.person_id = vd.person_id",
                "    \", common_where, \"",
                "    AND d.death_datetime >= vd.visit_detail_start_datetime",
                "    AND d.death_datetime <= COALESCE(vd.visit_detail_end_datetime, CAST(NOW() AS DATE))",
                "\"))",
                "",
                "total_patients <- d$con %>% run_query(paste0(\"",
                "    SELECT DISTINCT person_id",
                "    FROM visit_detail vd",
                "    \", common_where, \"",
                paste0("    AND care_site_id IN (", unit_ids_str, ")"),
                "\"))",
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
                paste0("            div(class = \"", tooltip_class_name, "\", paste0(death_count, \" \", i18np$t(\"deaths_on_total_patients\"), \" \", total_count, \" \", tolower(i18np$t(\"patients\"))), tags$br(), tags$br(), paste0(i18np$t(\"deaths_during_hospitalization\")), style = \"", tooltip_style_value, "\")"),
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
                "",
                #"visit_occurrence_ids <- paste(m$subset_persons$visit_occurrence_id, collapse = \", \")",
                #"",
                "stays <- d$con %>% run_query(paste0(\"",
                "    SELECT visit_occurrence_id, person_id, visit_start_date, visit_end_date,",
                "           DATEDIFF('day', visit_start_date, visit_end_date) as length_of_stay",
                "    FROM visit_occurrence",
                "    WHERE visit_end_date IS NOT NULL",
                #"    AND visit_occurrence_id IN (\", visit_occurrence_ids, \")",
                "    AND visit_start_date >= DATE('\", m$subset_dates[1], \"')",
                "    AND visit_start_date <= DATE('\", m$subset_dates[2], \"')",
                "\"))",
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
                "            paste0(value, \" \"), tags$span(i18np$t(\"days\"), style = \"font-size: 18px;\")",
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
                "",
                #"visit_detail_ids <- paste(m$subset_persons$visit_detail_id, collapse = \", \")",
                #"",
                "stays <- d$con %>% run_query(paste0(\"",
                "    SELECT visit_detail_id, person_id, visit_detail_start_date, visit_detail_end_date,",
                "           DATEDIFF('day', visit_detail_start_date, visit_detail_end_date) as length_of_stay",
                "    FROM visit_detail",
                paste0("    WHERE care_site_id IN (", unit_ids_str, ")"),
                "    AND visit_detail_end_date IS NOT NULL",
                #"    AND visit_detail_id IN (\", visit_detail_ids, \")",
                "    AND visit_detail_start_date >= DATE('\", m$subset_dates[1], \"')",
                "    AND visit_detail_start_date <= DATE('\", m$subset_dates[2], \"')",
                "\"))",
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
                "            paste0(value, \" \"), tags$span(i18np$t(\"days\"), style = \"font-size: 18px;\")",
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
