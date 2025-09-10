# ==========================================
# server_code.R - Hospital Stays Code Editor Server Logic
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 HOSPITAL STAYS PLUGIN - CUSTOMIZED IMPLEMENTATION  🔧                  ██
# ██                                                                            ██
# ██  This file generates a plotly timeline visualization of hospital stays.    ██
# ██  Shows patient visit details across different hospital units.              ██
# ██  Based on OMOP visit_detail and care_site tables.                          ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# HOSPITAL STAYS PLUGIN - CODE EDITOR SERVER FILE
# 
# This file handles the server-side logic for the hospital stays visualization.
# It provides a timeline view of patient visits across different care sites.
# 
# HOSPITAL STAYS SPECIFIC FEATURES:
# - Generates plotly timeline visualization of patient hospital stays
# - Shows visit details across different care sites/hospital units
# - Interactive hover information with stay duration and unit names
# - Automatic chronological ordering of hospital visits
# - Graceful UI messages when no patient selected or no data available
# 
# CORE FUNCTIONALITY:
# - Automatic R code generation for hospital stays timeline
# - Code editor with syntax highlighting and keyboard shortcuts
# - Elegant MessageBar UI for patient selection and data availability feedback
# - Auto-execution when patient selection changes
# - Integration with the widget's database storage system

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
        
        # For Hospital Stays plugin, we don't have UI parameters yet
        # Just generate the standard hospital stays visualization code
        generated_code <- generate_output_code_%widget_id%()
        
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
# CODE GENERATION FUNCTION
# ======================================

# Generate code for hospital stays visualization
# Since there are no UI parameters yet, this generates the standard plotly code
generate_output_code_%widget_id% <- function() {
    
    code_lines <- c(
        "# Query visit detail data for selected patient",
        "sql <- glue::glue_sql('SELECT * FROM visit_detail WHERE person_id = {m$selected_person}', .con = d$con)",
        "data <- DBI::dbGetQuery(d$con, sql)",
        "",
        "# Check if data exists",
        "if (nrow(data) == 0) {",
        "    stop(i18np$t('no_data_to_display'))",
        "}",
        "",
        "# Process and prepare data",
        "data <- data %>%",
        "    dplyr::select(visit_detail_start_datetime, visit_detail_end_datetime, care_site_id) %>%",
        "    dplyr::left_join(",
        "        d$care_site %>% dplyr::select(care_site_id, care_site_name) %>% dplyr::collect(),",
        "        by = 'care_site_id'",
        "    ) %>%",
        "    dplyr::arrange(visit_detail_start_datetime) %>%",
        "    dplyr::filter(!is.na(care_site_name)) %>%",
        "    dplyr::mutate(",
        "        service_order = as.numeric(forcats::fct_rev(forcats::fct_inorder(care_site_name)))",
        "    )",
        "",
        "# Prepare labels and format",
        "unique_levels <- data$service_order",
        "unique_labels <- data$care_site_name",
        "",
        "if (language == 'fr') datetime_format <- '%d-%m-%Y %H:%M' else datetime_format <- '%Y-%m-%d %H:%M'",
        "",
        "# Create plotly timeline visualization",
        "result <- plotly::plot_ly(data = data) %>%",
        "    plotly::add_segments(",
        "        x = ~visit_detail_start_datetime,",
        "        xend = ~visit_detail_end_datetime,",
        "        y = ~service_order,",
        "        yend = ~service_order,",
        "        line = list(color = 'steelblue', width = 10),",
        "        text = ~paste0(",
        "            i18np$t('hospital_unit'), ' : ', care_site_name, '<br>',",
        "            i18np$t('datetime_start'), ' : ', format(visit_detail_start_datetime, datetime_format), '<br>',",
        "            i18np$t('datetime_end'), ' : ', format(visit_detail_end_datetime, datetime_format)",
        "        ),",
        "        hoverinfo = 'text'",
        "    ) %>%",
        "    plotly::layout(",
        "        xaxis = list(",
        "            type = 'date',",
        "            tickmode = 'auto',",
        "            title = '',",
        "            nticks = 10,",
        "            tickfont = list(size = 10),",
        "            tickformat = datetime_format",
        "        ),",
        "        yaxis = list(",
        "            tickvals = unique_levels,",
        "            ticktext = unique_labels,",
        "            title = '',",
        "            automargin = TRUE",
        "        ),",
        "        hoverlabel = list(align = 'left')",
        "    ) %>%",
        "    plotly::config(displayModeBar = FALSE)"
    )
    
    # Combine all code lines
    generated_code <- paste(code_lines, collapse = "\n")
    
    return(generated_code)
}

# ======================================
# AUTO-EXECUTION TRIGGERS
# ======================================

# Auto-run code when patient selection changes
observe_event(m$selected_person, {
    # Check if auto-run is enabled
    if (!isTRUE(input$auto_update_%widget_id%)) {
        return()
    }
    
    # Execute code when patient changes
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# ======================================
# CODE EXECUTION ENGINE
# ======================================

# Main code execution handler
observe_event(input$run_code_%widget_id%, {
    
    # Initialize result variable
    result <- NULL
    error_message <- NULL
    
    # ====================
    # CHECK PATIENT SELECTION
    # ====================
    patient_selected <- TRUE
    if (is.null(m$selected_person) || is.na(m$selected_person)) {
        patient_selected <- FALSE
        error_message <- i18np$t("select_patient")
    }
    
    # ====================
    # HIDE ALL OUTPUTS INITIALLY
    # ====================
    # Hide all output containers before execution
    sapply(c("error_message_div_%widget_id%", "plot_div_%widget_id%", "table_div_%widget_id%", "datatable_div_%widget_id%", "dynamic_output_div_%widget_id%", "plotly_output_div_%widget_id%"), shinyjs::hide)
    
    # ====================
    # EXECUTE USER CODE
    # ====================
    if (patient_selected) {
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
    if (!is.null(error_message) || is.null(result)) {
        
        display_message <- if (!is.null(error_message)) {
            error_message
        } else {
            i18np$t("no_output_generated")
        }
        
        # Check if this is a user-friendly message that should be displayed with nice formatting
        ui_messages <- c(
            i18np$t("select_patient"),
            i18np$t("no_data_to_display"),
            i18np$t("no_output_generated")
        )
        
        # Check if the message contains any of the UI-friendly messages
        is_ui_message <- any(sapply(ui_messages, function(msg) grepl(msg, display_message, fixed = TRUE)))
        
        if (is_ui_message) {
            # Extract the actual message without "Error executing code:" prefix if present
            clean_message <- gsub("^Error executing code: ", "", display_message)
            # Display nice message in UI output
            output$dynamic_output_%widget_id% <- renderUI({
                div(
                    style = "display: flex; justify-content: center; align-items: center; height: 100%; text-align: center; padding: 10px;",
                    div(
                        style = "font-size: 14px; color: #6c757d;",
                        clean_message
                    )
                )
            })
            shinyjs::show("dynamic_output_div_%widget_id%")
        } else {
            # Display error message for actual errors
            output$error_message_%widget_id% <- renderUI(
                div(
                    shiny.fluent::MessageBar(
                        display_message, 
                        messageBarType = 5  # Error type
                    ), 
                    style = "display: inline-block;"
                )
            )
            shinyjs::show("error_message_div_%widget_id%")
        }
    }
    
    # Display output if execution was successful
    if (is.null(error_message) && !is.null(result)) {
        
        # Route output to appropriate renderer based on type
        # Hospital stays plugin primarily outputs plotly objects
        
        if ("plotly" %in% class(result) || "htmlwidget" %in% class(result)) {
            # Handle plotly objects (main output for hospital stays)
            output$plotly_output_%widget_id% <- plotly::renderPlotly(result)
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::show("plotly_output_div_%widget_id%")
            
        } else if ("ggplot" %in% class(result)) {
            # Handle ggplot objects
            output$plot_%widget_id% <- renderPlot(result)
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::show("plot_div_%widget_id%")
            
        } else if ("datatables" %in% class(result)) {
            # Handle DT datatable objects
            output$datatable_%widget_id% <- DT::renderDT(result)
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::show("datatable_div_%widget_id%")
            
        } else {
            # Handle other output types (text, HTML, etc.)
            output$dynamic_output_%widget_id% <- renderUI({
                if (is.character(result)) {
                    verbatimTextOutput(ns("text_display"))
                } else {
                    pre(capture.output(print(result)))
                }
            })
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::show("dynamic_output_div_%widget_id%")
        }
    }
    
    # ====================
    # AUTO-NAVIGATION
    # ====================
    # Optional: automatically switch to output tab after execution
    if (isFALSE(input$output_and_settings_side_by_side_%widget_id%)) shinyjs::click("output_button_%widget_id%")
    
    # ====================
    # SAVE AFTER DISPLAY IF REQUESTED
    # ====================
    # Check if save was requested after display (from Display + Save button)
    if (exists("save_after_display_%widget_id%") && save_after_display_%widget_id%()) {
        # Reset the flag
        save_after_display_%widget_id%(FALSE)
        
        # Trigger the save process
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_configuration_trigger_%widget_id%', Math.random());"))
        
        # Notify user
        show_message_bar("modif_saved", "success")
    }
})