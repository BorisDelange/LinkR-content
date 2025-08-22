# ==========================================
# server_code.R - Code Editor Server Logic
# ==========================================
# 
# Handles code editor functionality including:
# - Common initialization and keyboard shortcuts
# - Code execution controller
# - Auto-execution triggers based on data updates
# - Chart type specific logic imported from separate files
#
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
        
        # Get data source selection (default to person)
        data_source <- if (length(input$data_source_%widget_id%) > 0) {
            input$data_source_%widget_id%
        } else {
            "person"
        }
        
        # Get chart type (default to dygraphs)
        chart_type <- if (length(input$chart_type_%widget_id%) > 0) {
            input$chart_type_%widget_id%
        } else {
            "dygraphs"
        }
        
        # Get filtered concepts based on final chart type
        if (chart_type == "dygraphs") {
            allowed_domains <- c("Measurement", "Observation")
        } else if (chart_type == "plotly") {
            allowed_domains <- c("Measurement", "Observation", "Condition", "Procedure", "Drug")
        } else {
            allowed_domains <- c("Measurement", "Observation")
        }
        
        filtered_concepts <- selected_concepts %>% 
            dplyr::filter(domain_id %in% allowed_domains)
        
        # Generate R code for the current configuration based on chart type
        if (chart_type == "dygraphs") {
            generated_code <- generate_dygraphs_output_code_%widget_id%(
                data_source = data_source,
                concepts = filtered_concepts %>% 
                    dplyr::filter(concept_id %in% input$concepts_%widget_id%),
                synchronize_timelines = isTRUE(input$synchronize_timelines_%widget_id%)
            )
        } else {
            generated_code <- generate_plotly_output_code_%widget_id%(
                data_source = data_source,
                concepts = filtered_concepts %>% 
                    dplyr::filter(concept_id %in% input$concepts_%widget_id%),
                synchronize_timelines = isTRUE(input$synchronize_timelines_%widget_id%)
            )
        }
        
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
# COMMON HELPER FUNCTIONS
# ======================================

# Generate date range processing code block
generate_date_range_block_%widget_id% <- function(data_source, synchronize_timelines) {
    
    if (data_source == "person") {
        date_block <- paste0(
            "if (!is.na(m$selected_person)) {\n",
            "    sql <- glue::glue_sql('\n",
            "        SELECT \n",
            "            MIN(visit_start_datetime) AS min_visit_start_datetime, \n",
            "            MAX(visit_end_datetime) AS max_visit_end_datetime \n",
            "        FROM visit_occurrence \n",
            "        WHERE person_id = {m$selected_person} \n",
            "    ', .con = d$con)\n",
            "    \n",
            "    data_datetimes_range <- DBI::dbGetQuery(d$con, sql)\n",
            "}\n\n",
            "if (length(data_datetimes_range) > 0) {\n",
            "    data_datetimes_range <- c(data_datetimes_range$min_visit_start_datetime, \n",
            "                            data_datetimes_range$max_visit_end_datetime)\n",
            "    m$data_datetimes_range_%widget_id% <- data_datetimes_range\n",
            "}\n\n",
            "datetimes <- data_datetimes_range\n",
            "if (length(datetimes) > 0) m$datetimes_%widget_id% <- datetimes"
        )
    } else if (data_source == "visit_detail") {
        date_block <- paste0(
            "if (!is.na(m$selected_visit_detail)) {\n",
            "    sql <- glue::glue_sql('\n",
            "        SELECT \n",
            "            MIN(visit_detail_start_datetime) AS min_visit_start_datetime, \n",
            "            MAX(visit_detail_end_datetime) AS max_visit_end_datetime \n",
            "        FROM visit_detail \n",
            "        WHERE visit_detail_id = {m$selected_visit_detail} \n",
            "    ', .con = d$con)\n",
            "    \n",
            "    data_datetimes_range <- DBI::dbGetQuery(d$con, sql)\n",
            "}\n\n",
            "if (length(data_datetimes_range) > 0) {\n",
            "    data_datetimes_range <- c(data_datetimes_range$min_visit_start_datetime, \n",
            "                            data_datetimes_range$max_visit_end_datetime)\n",
            "    m$data_datetimes_range_%widget_id% <- data_datetimes_range\n",
            "}\n\n",
            "datetimes <- data_datetimes_range\n",
            "if (length(datetimes) > 0) m$datetimes_%widget_id% <- datetimes"
        )
    }
    
    # Add synchronization logic if needed
    if (synchronize_timelines) {
        sync_block <- paste0(
            "if (!is.null(m$debounced_datetimes_timeline_%tab_id%)) {\n",
            "    if (length(m$debounced_datetimes_timeline_%tab_id%()) > 0) {\n",
            "        datetimes <- m$debounced_datetimes_timeline_%tab_id%()\n",
            "    }\n",
            "}"
        )
        
        # Combine date and sync blocks
        combined_block <- paste(date_block, sync_block, sep = "\n\n")
        return(combined_block)
    }
    
    return(date_block)
}

# Build concepts table code block
generate_concepts_block_%widget_id% <- function(concepts) {
    
    if (nrow(concepts) == 0) {
        # Empty concepts table
        concepts_block <- paste0(
            "concepts <- tibble::tribble(\n",
            "    ~concept_id, ~concept_name, ~domain_id, ~vocabulary_id\n",
            ")"
        )
    } else {
        # Build concept rows
        concept_rows <- ""
        for (i in 1:nrow(concepts)) {
            row <- concepts[i, ]
            concept_rows <- paste0(concept_rows,
                "    ", row$concept_id, ", \"", row$concept_name, "\", \"", 
                row$domain_id, "\", \"", row$vocabulary_id, "\""
            )
            if (i < nrow(concepts)) {
                concept_rows <- paste0(concept_rows, ",\n")
            } else {
                concept_rows <- paste0(concept_rows, "\n")
            }
        }
        
        concepts_block <- paste0(
            "concepts <- tibble::tribble(\n",
            "    ~concept_id, ~concept_name, ~domain_id, ~vocabulary_id,\n",
            concept_rows,
            ")"
        )
    }
    
    return(concepts_block)
}

# ======================================
# AUTO-EXECUTION ON DATA UPDATES
# ======================================

# Auto-run code when patient selection changes
observe_event(m$selected_person, {
    # Check if auto-run is enabled and data source matches
    if (!isTRUE(input$automatically_update_output_%widget_id%) || 
        length(input$data_source_%widget_id%) == 0 || 
        input$data_source_%widget_id% != "person") {
        return()
    }
    
    # Reset timeline synchronization variables
    reset_timeline_variables_%widget_id%()
    
    # Execute code
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Auto-run code when visit detail selection changes
observe_event(m$selected_visit_detail, {
    # Check if auto-run is enabled and data source matches
    if (!isTRUE(input$automatically_update_output_%widget_id%) || 
        length(input$data_source_%widget_id%) == 0 || 
        input$data_source_%widget_id% != "visit_detail") {
        return()
    }
    
    # Reset timeline synchronization variables
    reset_timeline_variables_%widget_id%()
    
    # Execute code
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Helper function to reset timeline synchronization variables
reset_timeline_variables_%widget_id% <- function() {
    m$debounced_datetimes_timeline_%tab_id% <- reactiveVal()
    m$datetimes_timeline_%tab_id% <- reactiveVal()
    m$debounced_datetimes_timeline_%tab_id% <- reactiveVal()
    m$debounced_datetimes_timeline_%tab_id% <- reactive(m$datetimes_timeline_%tab_id%()) %>% debounce(500)
}

# ======================================
# IMPORT CHART TYPE SPECIFIC LOGIC
# ======================================

# Import dygraphs specific functions and handlers
%import_script('server_code_dygraphs.R')%

# Import plotly specific functions and handlers  
%import_script('server_code_plotly.R')%

# ======================================
# CODE EXECUTION ENGINE
# ======================================

# Main code execution handler
observe_event(input$run_code_%widget_id%, {
    
    fig <- character()
    
    # ====================
    # EXECUTE USER CODE
    # ====================
    eval(parse(text = m$code_%widget_id%))
    
    # ====================
    # HANDLE EXECUTION RESULTS
    # ====================
    
    # Show appropriate error message if no chart was generated
    if (length(fig) == 0) {
        # Determine specific error message based on context
        data_source <- if (length(input$data_source_%widget_id%) > 0) {
            input$data_source_%widget_id%
        } else {
            "person"
        }
        
        # Check if patient/visit is selected
        if (data_source == "person") {
            patient_selected <- !is.na(m$selected_person) && length(m$selected_person) > 0
            error_message <- if (!patient_selected) {
                i18np$t("no_patient_selected")
            } else {
                i18np$t("no_data_for_patient")
            }
        } else {
            visit_selected <- !is.na(m$selected_visit_detail) && length(m$selected_visit_detail) > 0
            error_message <- if (!visit_selected) {
                i18np$t("no_visit_selected")
            } else {
                i18np$t("no_data_for_patient")
            }
        }
        
        # Use same styling as Admissions and Demographics plugin
        output$error_message_%widget_id% <- renderUI({
            div(
                style = "display: flex; justify-content: center; align-items: center; height: 100%; text-align: center;",
                div(
                    style = "font-size: 14px; color: #6c757d;",
                    error_message
                )
            )
        })
        
        shinyjs::show("error_message_div_%widget_id%")
        shinyjs::hide("dygraph_div_%widget_id%")
        shinyjs::hide("plotly_div_%widget_id%")
    }
    
    # Display chart if generation was successful
    if (length(fig) > 0) {
        # Determine which output to use based on chart type
        chart_type <- if (length(input$chart_type_%widget_id%) > 0) {
            input$chart_type_%widget_id%
        } else {
            "dygraphs"
        }
        
        if (chart_type == "dygraphs") {
            output$dygraph_%widget_id% <- dygraphs::renderDygraph(fig)
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::hide("plotly_div_%widget_id%")
            shinyjs::show("dygraph_div_%widget_id%")
        } else {
            output$plotly_%widget_id% <- plotly::renderPlotly(fig)
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::hide("dygraph_div_%widget_id%")
            shinyjs::show("plotly_div_%widget_id%")
        }
    }
    
    # ====================
    # AUTO-NAVIGATION
    # ====================
    # If not in side-by-side mode, automatically switch to output tab
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

# ======================================
# TIMELINE SYNCHRONIZATION SYSTEM
# ======================================

# Initialize timeline variables if they don't exist
if (length(m$datetimes_timeline_%tab_id%) == 0) {
    # Main timeline reactive value
    m$datetimes_timeline_%tab_id% <- reactiveVal()
    
    # Debounced version to prevent excessive updates
    m$debounced_datetimes_timeline_%tab_id% <- reactive(m$datetimes_timeline_%tab_id%()) %>% debounce(500)
}

# Adjust chart padding when timeline synchronization is toggled
observe_event(input$synchronize_timelines_%widget_id%, {
    
    # Get current chart type to apply padding to correct container
    chart_type <- if (length(input$chart_type_%widget_id%) > 0) {
        input$chart_type_%widget_id%
    } else {
        "dygraphs"
    }
    
    if (input$synchronize_timelines_%widget_id%) {
        # Add left padding to align with synchronized timeline
        if (chart_type == "dygraphs") {
            shinyjs::runjs(sprintf(
                "document.getElementById('%s').style.paddingLeft = '80px'; var event = new Event('resize'); window.dispatchEvent(event);",
                ns("dygraph_div_%widget_id%")
            ))
        }
    } else {
        # Remove padding when synchronization is disabled
        if (chart_type == "dygraphs") {
            shinyjs::runjs(sprintf(
                "document.getElementById('%s').style.paddingLeft = '0px'; var event = new Event('resize'); window.dispatchEvent(event);",
                ns("dygraph_div_%widget_id%")
            ))
        }
    }
})

# Monitor dygraph date window changes and broadcast to other widgets
observe_event(input$dygraph_%widget_id%_date_window, {
    
    # Only process if timeline synchronization is enabled
    if (!input$synchronize_timelines_%widget_id%) return()
    
    # Convert date window from JavaScript format to R POSIXct
    datetime_values <- as.POSIXct(
        input$dygraph_%widget_id%_date_window, 
        format = "%Y-%m-%dT%H:%M:%OSZ", 
        tz = "UTC"
    )
    
    # Update the shared timeline reactive value
    m$datetimes_timeline_%tab_id%(datetime_values)
})

# Monitor plotly relayout events for timeline synchronization
observe_event(plotly::event_data("plotly_relayout", source = "plotly_%widget_id%"), {
    
    # Only process if timeline synchronization is enabled
    if (!input$synchronize_timelines_%widget_id%) return()
    
    relayout_data <- plotly::event_data("plotly_relayout", source = "plotly_%widget_id%")
    
    if (!is.null(relayout_data) && is.list(relayout_data)) {
        
        if ("xaxis.autorange" %in% names(relayout_data) && relayout_data[["xaxis.autorange"]]) {
            # Auto-range was selected, use full data range
            if (length(m$data_datetimes_range_%widget_id%) > 0) {
                m$datetimes_timeline_%tab_id%(m$data_datetimes_range_%widget_id%)
            }
        } else if ("xaxis.range[0]" %in% names(relayout_data) && "xaxis.range[1]" %in% names(relayout_data)) {
            # Manual range selection
            selected_min <- lubridate::force_tz(
                as.POSIXct(relayout_data[["xaxis.range[0]"]], origin = "1970-01-01"), 
                tzone = "UTC"
            )
            selected_max <- lubridate::force_tz(
                as.POSIXct(relayout_data[["xaxis.range[1]"]], origin = "1970-01-01"), 
                tzone = "UTC"
            )
            m$datetimes_timeline_%tab_id%(c(selected_min, selected_max))
        }
    }
})

# Listen for timeline changes from other synchronized widgets
observe_event(m$debounced_datetimes_timeline_%tab_id%(), {
    
    # Only process if synchronization is enabled and data is available
    if (!input$synchronize_timelines_%widget_id% || 
        length(m$debounced_datetimes_timeline_%tab_id%()) == 0 || 
        length(m$datetimes_%widget_id%) == 0) {
        return()
    }
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    
}, ignoreInit = TRUE)
