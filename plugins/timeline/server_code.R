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

# Initialize code blocks list
code <- list()

# Fix ACE editor rendering issues on startup
# Delay ensures DOM is fully loaded before triggering resize
shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))

# Auto-execute code when widget first loads to show initial chart
shinyjs::delay(1000, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-display_figure_%widget_id%', Math.random());")))

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
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_%widget_id%', Math.random());"))
})

# ======================================
# FIGURE DISPLAY CONTROLLER
# ======================================

# Main code execution handler - triggered by display button or shortcuts
observe_event(input$display_figure_%widget_id%, {
    
    # Determine current tab (default to figure_settings if not set)
    current_tab <- if (length(input$current_tab_%widget_id%) == 0) {
        "figure_settings"
    } else {
        input$current_tab_%widget_id%
    }
    
    # ====================
    # AUTO-GENERATE CODE FROM FIGURE SETTINGS
    # ====================
    if (current_tab == "figure_settings") {
        
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
            generated_code <- generate_dygraphs_figure_code(
                data_source = data_source,
                concepts = filtered_concepts %>% 
                    dplyr::filter(concept_id %in% input$concepts_%widget_id%),
                synchronize_timelines = isTRUE(input$synchronize_timelines_%widget_id%)
            )
        } else {
            generated_code <- generate_plotly_figure_code(
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
generate_date_range_block <- function(data_source, synchronize_timelines) {
    
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
generate_concepts_block <- function(concepts) {
    
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
    if (!isTRUE(input$automatically_update_figure_%widget_id%) || 
        length(input$data_source_%widget_id%) == 0 || 
        input$data_source_%widget_id% != "person") {
        return()
    }
    
    # Reset timeline synchronization variables
    reset_timeline_variables()
    
    # Execute code
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Auto-run code when visit detail selection changes
observe_event(m$selected_visit_detail, {
    # Check if auto-run is enabled and data source matches
    if (!isTRUE(input$automatically_update_figure_%widget_id%) || 
        length(input$data_source_%widget_id%) == 0 || 
        input$data_source_%widget_id% != "visit_detail") {
        return()
    }
    
    # Reset timeline synchronization variables
    reset_timeline_variables()
    
    # Execute code
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Helper function to reset timeline synchronization variables
reset_timeline_variables <- function() {
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
    
    # Show error message if no chart was generated
    if (length(fig) == 0) {
        output$error_message_%widget_id% <- renderUI(
            div(
                shiny.fluent::MessageBar(
                    i18np$t("no_data_to_display"), 
                    messageBarType = 5
                ), 
                style = "display: inline-block;"
            )
        )
        
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
    # If not in side-by-side mode, automatically switch to figure tab
    if (length(input$figure_and_settings_side_by_side_%widget_id%) > 0) {
        if (!input$figure_and_settings_side_by_side_%widget_id%) {
            shinyjs::click("figure_button_%widget_id%")
        }
    }
})
