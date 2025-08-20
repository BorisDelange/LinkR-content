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
        
        # ====================
        # CODE GENERATION DISPATCHER
        # ====================
        # This dispatcher determines which code generation function to call
        # based on the selected healthcare indicator type
        
        # Determine which function to call based on the indicator category
        generated_code <- switch(indicator,
            # ====================
            # STANDARD INDICATORS
            # ====================
            # These indicators use the original admissions code generation function
            # They share similar data processing and visualization patterns
            "patient_count" = ,              # Number of patients
            "admission_count" = ,            # Number of admissions  
            "mortality_rate" = ,             # Mortality rate
            "average_length_of_stay" = ,     # Average length of stay
            "bed_occupancy_rate" = ,         # Bed occupancy rate
            "readmission_rate" = generate_admissions_ui_code_%widget_id%(
                indicator = indicator,
                indicator_scope = indicator_scope,
                hospital_units = hospital_units,
                legend_1 = input$legend_1_%widget_id%,
                legend_2 = input$legend_2_%widget_id%
            ),
            
            # ====================
            # TIMELINE INDICATORS  
            # ====================
            # Timeline indicators require different visualization parameters
            # They use time-series specific charts with custom axis labels
            "admission_timeline" = generate_admissions_timeline_code_%widget_id%(
                indicator = indicator,
                indicator_scope = indicator_scope,
                hospital_units = hospital_units,
                title = input$timeline_title_%widget_id%,        # Chart title
                x_label = input$timeline_x_label_%widget_id%,    # X-axis label (usually time)
                y_label = input$timeline_y_label_%widget_id%,    # Y-axis label (metric name)
                nb_bins = input$timeline_nb_bins_%widget_id%     # Number of histogram bins
            ),
            
            # ====================
            # DEMOGRAPHIC INDICATORS
            # ====================
            # Demographic indicators focus on patient characteristics
            # They may require different aggregation methods and visualizations
            "average_age" = generate_demographics_ui_code_%widget_id%(
                indicator = indicator,
                indicator_scope = indicator_scope,
                hospital_units = hospital_units,
                legend_1 = input$legend_1_%widget_id%,
                legend_2 = input$legend_2_%widget_id%
            ),
            "gender" = generate_demographics_ui_code_%widget_id%(
                indicator = indicator,
                indicator_scope = indicator_scope,
                hospital_units = hospital_units,
                title = input$timeline_title_%widget_id%,
                legend_1 = input$legend_1_%widget_id%,
                legend_2 = input$legend_2_%widget_id%
            ),
            
            # ====================
            # DEFAULT FALLBACK
            # ====================
            # If indicator is not recognized, default to standard admissions function
            # This ensures the application doesn't break with unexpected values
            generate_admissions_ui_code_%widget_id%(
                indicator = indicator,
                indicator_scope = indicator_scope,
                hospital_units = hospital_units,
                legend_1 = input$legend_1_%widget_id%,
                legend_2 = input$legend_2_%widget_id%
            )
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
    
    # Initialize result variables
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
            res <- eval(parse(text = m$code_%widget_id%))
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
            i18np$t("no_output_generated")
        }
        
        # Check if this is a user-friendly message that should be displayed with nice formatting
        ui_messages <- c(
            i18np$t("no_subset_selected"),
            i18np$t("no_output_generated"),
            i18np$t("please_select_at_least_one_hospital_unit")
        )
        
        # Check if the message contains any of the UI-friendly messages
        is_ui_message <- any(sapply(ui_messages, function(msg) grepl(msg, display_message, fixed = TRUE)))
        
        if (is_ui_message) {
            # Extract the actual message without "Error executing code:" prefix if present
            clean_message <- gsub("^Error executing code: ", "", display_message)
            # Display nice message in UI output
            output$ui_output_%widget_id% <- renderUI({
                div(
                    style = "display: flex; justify-content: center; align-items: center; height: 100%; text-align: center; padding: 10px;",
                    div(
                        style = "font-size: 14px; color: #6c757d;",
                        clean_message
                    )
                )
            })
            shinyjs::show("ui_output_div_%widget_id%")
            shinyjs::hide("plotly_output_div_%widget_id%")
            shinyjs::hide("ggplot_output_div_%widget_id%")
            shinyjs::hide("console_output_div_%widget_id%")
        } else {
            # Display other errors in console output
            output$console_output_%widget_id% <- renderText(display_message)
            shinyjs::hide("ui_output_div_%widget_id%")
            shinyjs::hide("plotly_output_div_%widget_id%")
            shinyjs::hide("ggplot_output_div_%widget_id%")
            shinyjs::show("console_output_div_%widget_id%")
            
            # Clear UI output
            output$ui_output_%widget_id% <- renderUI(NULL)
        }
        
        # Clear plot outputs
        output$plotly_output_%widget_id% <- plotly::renderPlotly(NULL)
        output$ggplot_output_%widget_id% <- renderPlot(NULL)
    }
    
    # ====================
    # DISPLAY OUTPUT BASED ON RESULT TYPE
    # ====================
    if (is.null(error_message) && !is.null(res)) {
        
        # Check if result is a plotly object
        if (inherits(res, "plotly")) {
            # ====================
            # RENDER PLOTLY OUTPUT
            # ====================
            # Display timeline/chart visualizations
            output$plotly_output_%widget_id% <- plotly::renderPlotly({
                res
            })
            
            # Hide other outputs and show plot
            shinyjs::hide("ui_output_div_%widget_id%")
            shinyjs::hide("ggplot_output_div_%widget_id%")
            shinyjs::hide("console_output_div_%widget_id%")
            shinyjs::show("plotly_output_div_%widget_id%")
            
            # Clear UI output
            output$ui_output_%widget_id% <- renderUI(NULL)
            
        } else if (inherits(res, "ggplot")) {
            # ====================
            # RENDER GGPLOT OUTPUT
            # ====================
            # Display ggplot visualizations (pie charts, etc.)
            output$ggplot_output_%widget_id% <- renderPlot({
                res
            })
            
            # Hide other outputs and show ggplot
            shinyjs::hide("ui_output_div_%widget_id%")
            shinyjs::hide("plotly_output_div_%widget_id%")
            shinyjs::hide("console_output_div_%widget_id%")
            shinyjs::show("ggplot_output_div_%widget_id%")
            
            # Clear UI output
            output$ui_output_%widget_id% <- renderUI(NULL)
            
        } else {
            # ====================
            # RENDER UI OUTPUT (CARDS)
            # ====================
            # Display healthcare indicator cards and other UI elements
            output$ui_output_%widget_id% <- renderUI(res)
            
            # Hide other outputs and show UI
            shinyjs::hide("plotly_output_div_%widget_id%")
            shinyjs::hide("ggplot_output_div_%widget_id%")
            shinyjs::hide("console_output_div_%widget_id%")
            shinyjs::show("ui_output_div_%widget_id%")
            
            # Clear plot outputs
            output$plotly_output_%widget_id% <- plotly::renderPlotly(NULL)
            output$ggplot_output_%widget_id% <- renderPlot(NULL)
        }
    }
    
    # ====================
    # AUTO-NAVIGATION
    # ====================
    # Optional: automatically switch to output tab after execution
    if (isFALSE(input$output_and_settings_side_by_side_%widget_id%)) shinyjs::click("output_button_%widget_id%")
})
