# ==========================================
# server_code.R - Code Editor Server Logic
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 HEALTHCARE DASHBOARD PLUGIN IMPLEMENTATION  🔧                         ██
# ██                                                                            ██
# ██  Healthcare dashboard plugin for OMOP data analysis.                       ██
# ██  Generates HTML cards displaying healthcare indicators.                     ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# HEALTHCARE DASHBOARD PLUGIN - CODE EDITOR SERVER FILE
# 
# This file handles the server-side logic for the healthcare dashboard plugin.
# It provides automatic HTML code generation from UI settings and manual code execution
# for healthcare indicators using OMOP Common Data Model.
# 
# CORE FUNCTIONALITY:
# - Automatic HTML code generation for healthcare indicators
# - Support for patient count, admission count, mortality, and length of stay metrics
# - Hospital unit filtering and scope selection
# - OMOP CDM database integration via get_query()
# - Responsive HTML card display with healthcare icons

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
    
    # ====================
    # VALIDATION: CHECK FOR UNIT SELECTION WHEN SCOPE IS HOSPITAL UNITS
    # ====================
    
    code_lines <- c(code_lines,
        "# Validation: Check if units are selected when scope is hospital units",
        'if ("', indicator_scope, '" == "hospital_units" && length(c(', paste0('"', hospital_units, '"', collapse = ", "), ')) == 0) {',
        '    error_msg <- if (m$language == "en") {',
        '        "Please select at least one hospital unit."',
        '    } else {',
        '        "Veuillez sélectionner au moins une unité hospitalière."',
        '    }',
        '    stop(error_msg)',
        '}',
        ""
    )
    
    # ====================
    # DATA EXTRACTION BASED ON INDICATOR
    # ====================
    
    if (indicator == "patient_count") {
        # Patient count indicator
        if (indicator_scope == "hospitalization") {
            code_lines <- c(code_lines,
                "# Extract distinct patient count from hospital visits",
                "patients <- get_query(\"",
                "    SELECT DISTINCT person_id",
                "    FROM visit_occurrence",
                "    WHERE visit_concept_id IN (9201, 9203)  -- Inpatient visits",
                "\")",
                ""
            )
        } else {
            # Hospital units scope
            unit_ids_str <- paste(hospital_units, collapse = ", ")
            code_lines <- c(code_lines,
                "# Extract distinct patient count from selected hospital units",
                paste0("patients <- get_query(\""),
                "    SELECT DISTINCT person_id",
                "    FROM visit_detail",
                paste0("    WHERE care_site_id IN (", unit_ids_str, ")"),
                "\")",
                ""
            )
        }
        
        # Set legend and icon
        code_lines <- c(code_lines,
            "# Set language-specific legend",
            'if (m$language == "en") legend <- "Patients" else if (m$language == "fr") legend <- "Patients"',
            'icon_class <- "fas fa-hospital-user fa-2x"',
            "value <- nrow(patients)",
            ""
        )
        
    } else if (indicator == "admission_count") {
        # Admission count indicator
        if (indicator_scope == "hospitalization") {
            code_lines <- c(code_lines,
                "# Extract hospital admissions count",
                "admissions <- get_query(\"",
                "    SELECT visit_occurrence_id, person_id, visit_start_date, visit_end_date",
                "    FROM visit_occurrence",
                "    WHERE visit_concept_id IN (9201, 9203)  -- Inpatient visits",
                "\")",
                ""
            )
        } else {
            # Hospital units scope - count unit stays
            unit_ids_str <- paste(hospital_units, collapse = ", ")
            code_lines <- c(code_lines,
                "# Extract hospital unit stays count",
                paste0("admissions <- get_query(\""),
                "    SELECT visit_detail_id, person_id, visit_detail_start_date, visit_detail_end_date",
                "    FROM visit_detail",
                paste0("    WHERE care_site_id IN (", unit_ids_str, ")"),
                "\")",
                ""
            )
        }
        
        # Set legend and icon
        legend_text <- if (indicator_scope == "hospitalization") {
            'if (m$language == "en") legend <- "Admissions" else if (m$language == "fr") legend <- "Admissions"'
        } else {
            'if (m$language == "en") legend <- "Unit Stays" else if (m$language == "fr") legend <- "Séjours"'
        }
        
        code_lines <- c(code_lines,
            "# Set language-specific legend",
            legend_text,
            'icon_class <- "fas fa-bed fa-2x"',
            "value <- nrow(admissions)",
            ""
        )
        
    } else if (indicator == "mortality") {
        # Mortality indicator
        if (indicator_scope == "hospitalization") {
            code_lines <- c(code_lines,
                "# Extract hospital mortality data",
                "deaths <- get_query(\"",
                "    SELECT DISTINCT d.person_id, d.death_date",
                "    FROM death d",
                "    JOIN visit_occurrence v ON d.person_id = v.person_id",
                "    WHERE v.visit_concept_id IN (9201, 9203)  -- Inpatient visits",
                "\")",
                ""
            )
        } else {
            # Hospital units scope
            unit_ids_str <- paste(hospital_units, collapse = ", ")
            code_lines <- c(code_lines,
                "# Extract unit-specific mortality data",
                paste0("deaths <- get_query(\""),
                "    SELECT DISTINCT d.person_id, d.death_date",
                "    FROM death d",
                "    JOIN visit_detail vd ON d.person_id = vd.person_id",
                paste0("    WHERE vd.care_site_id IN (", unit_ids_str, ")"),
                "\")",
                ""
            )
        }
        
        # Set legend and icon
        code_lines <- c(code_lines,
            "# Set language-specific legend",
            'if (m$language == "en") legend <- "Deaths" else if (m$language == "fr") legend <- "Décès"',
            'icon_class <- "fas fa-heart-broken fa-2x"',
            "value <- nrow(deaths)",
            ""
        )
        
    } else if (indicator == "average_length_of_stay") {
        # Average length of stay indicator
        if (indicator_scope == "hospitalization") {
            code_lines <- c(code_lines,
                "# Extract hospital length of stay data",
                "stays <- get_query(\"",
                "    SELECT visit_occurrence_id, person_id, visit_start_date, visit_end_date,",
                "           DATEDIFF(visit_end_date, visit_start_date) as length_of_stay",
                "    FROM visit_occurrence",
                "    WHERE visit_concept_id IN (9201, 9203)  -- Inpatient visits",
                "    AND visit_end_date IS NOT NULL",
                "\")",
                ""
            )
        } else {
            # Hospital units scope
            unit_ids_str <- paste(hospital_units, collapse = ", ")
            code_lines <- c(code_lines,
                "# Extract unit length of stay data",
                paste0("stays <- get_query(\""),
                "    SELECT visit_detail_id, person_id, visit_detail_start_date, visit_detail_end_date,",
                "           DATEDIFF(visit_detail_end_date, visit_detail_start_date) as length_of_stay",
                "    FROM visit_detail",
                paste0("    WHERE care_site_id IN (", unit_ids_str, ")"),
                "    AND visit_detail_end_date IS NOT NULL",
                "\")",
                ""
            )
        }
        
        # Set legend and icon
        code_lines <- c(code_lines,
            "# Calculate average length of stay",
            "avg_los <- if(nrow(stays) > 0) round(mean(stays$length_of_stay, na.rm = TRUE), 1) else 0",
            "",
            "# Set language-specific legend",
            'if (m$language == "en") legend <- "Avg. LOS (days)" else if (m$language == "fr") legend <- "DMS (jours)"',
            'icon_class <- "fas fa-calendar-day fa-2x"',
            "value <- avg_los",
            ""
        )
    }
    
    # ====================
    # HTML CARD GENERATION
    # ====================
    
    code_lines <- c(code_lines,
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
        "            tags$i(class = icon_class)",
        "        ),",
        "        div(",
        "            style = \"color: #2C699A; font-size: 36px; font-weight: bold; margin: 10px 0;\",",
        "            value",
        "        ),",
        "        div(",
        "            style = \"color: #666; font-size: 14px; text-transform: uppercase;\",",
        "            legend",
        "        )",
        "    )",
        "}",
        "",
        "res"
    )
    
    # Combine all code lines
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
