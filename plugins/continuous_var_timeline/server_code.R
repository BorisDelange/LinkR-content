# ==========================================
# server_code.R - Code Editor Server Logic
# ==========================================
# 
# Handles code editor functionality including:
# - Code execution and figure generation
# - Auto-code generation from figure settings
# - Keyboard shortcuts and ACE editor interactions
# - Auto-execution triggers based on data updates
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

# Auto-execute code when widget first loads to show initial chart
shinyjs::delay(500, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-display_figure_%widget_id%', Math.random());")))

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
        
        # Generate R code for the current configuration
        generated_code <- generate_figure_code(
            data_source = data_source,
            concepts = selected_concepts %>% 
                dplyr::filter(concept_id %in% input$concepts_%widget_id%),
            synchronize_timelines = isTRUE(input$synchronize_timelines_%widget_id%)
        )
        
        # Update ACE editor with generated code
        shinyAce::updateAceEditor(session, "code_%widget_id%", value = generated_code)
        
        # Store code and trigger execution
        m$code_%widget_id% <- generated_code
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
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
# CODE GENERATION HELPER FUNCTION
# ======================================

# Generate R code based on current figure settings
generate_figure_code <- function(data_source, concepts, synchronize_timelines) {
    
    # ====================
    # BUILD CONCEPTS TABLE
    # ====================
    code <- paste0(
        "concepts <- tibble::tribble(\\n",
        "    ~concept_id, ~concept_name, ~domain_id, ~vocabulary_id"
    )
    
    # Add each concept as a row in the tibble
    if (nrow(concepts) > 0) {
        for (i in 1:nrow(concepts)) {
            row <- concepts[i, ]
            code <- paste0(
                code, ",\\n",
                "    ", row$concept_id, ", '", row$concept_name, "', '", 
                row$domain_id, "', '", row$vocabulary_id, "'"
            )
        }
    }
    code <- paste0(code, "\\n)")
    
    # ====================
    # INITIALIZE DATA STRUCTURES
    # ====================
    code <- paste0(
        code, "\\n\\n",
        "features <- list()\\n",
        "features_names <- c()\\n",
        "raw_data <- tibble::tibble()\\n",
        "data_datetimes_range <- c()\\n",
        "combined_features <- c()"
    )
    
    # ====================
    # BUILD SQL QUERY
    # ====================
    code <- paste0(
        code, "\\n\\n",
        generate_data_query(data_source)
    )
    
    # ====================
    # ADD DATE RANGE PROCESSING
    # ====================
    code <- paste0(
        code, "\\n\\n",
        generate_date_range_processing(data_source, synchronize_timelines)
    )
    
    # ====================
    # ADD DATA PROCESSING LOOP
    # ====================
    code <- paste0(
        code, "\\n\\n",
        generate_data_processing_loop()
    )
    
    # ====================
    # ADD CHART GENERATION
    # ====================
    code <- paste0(
        code, "\\n\\n",
        generate_chart_code(synchronize_timelines)
    )
    
    return(code)
}

# Generate SQL query based on data source
generate_data_query <- function(data_source) {
    paste0(
        "sql <- glue::glue_sql('\\n",
        "    SELECT \\n",
        "        measurement_concept_id AS concept_id,\\n",
        "        measurement_source_concept_id AS source_concept_id,\\n",
        "        measurement_datetime AS datetime,\\n",
        "        value_as_number\\n",
        "    FROM measurement \\n",
        "    WHERE ", data_source, "_id = {m$selected_", data_source, "} \\n",
        "    AND (measurement_concept_id IN ({concepts$concept_id*}) OR measurement_source_concept_id IN ({concepts$concept_id*}))\\n",
        "    UNION\\n",
        "    SELECT \\n",
        "        observation_concept_id AS concept_id,\\n",
        "        observation_source_concept_id AS source_concept_id,\\n",
        "        observation_datetime AS datetime, value_as_number\\n",
        "    FROM observation \\n",
        "    WHERE ", data_source, "_id = {m$selected_", data_source, "} \\n",
        "    AND (observation_concept_id IN ({concepts$concept_id*}) OR observation_source_concept_id IN ({concepts$concept_id*}))\\n",
        "', .con = d$con)\\n\\n",
        "raw_data <- DBI::dbGetQuery(d$con, sql) %>% tibble::as_tibble()"
    )
}

# Generate date range processing code
generate_date_range_processing <- function(data_source, synchronize_timelines) {
    
    # Data source specific date range queries
    date_query <- if (data_source == "person") {
        paste0(
            "if (!is.na(m$selected_person)){\\n",
            "    sql <- glue::glue_sql('\\n",
            "        SELECT \\n",
            "            MIN(visit_start_datetime) AS min_visit_start_datetime, \\n",
            "            MAX(visit_end_datetime) AS max_visit_end_datetime \\n",
            "        FROM visit_occurrence \\n",
            "        WHERE person_id = {m$selected_person} \\n",
            "    ', .con = d$con)\\n\\n",
            "    data_datetimes_range <- DBI::dbGetQuery(d$con, sql)\\n",
            "}"
        )
    } else {
        paste0(
            "if (!is.na(m$selected_visit_detail)){\\n",
            "    sql <- glue::glue_sql('\\n",
            "        SELECT \\n",
            "            MIN(visit_detail_start_datetime) AS min_visit_start_datetime, \\n",
            "            MAX(visit_detail_end_datetime) AS max_visit_end_datetime \\n",
            "        FROM visit_detail \\n",
            "        WHERE visit_detail_id = {m$selected_visit_detail} \\n",
            "    ', .con = d$con)\\n\\n",
            "    data_datetimes_range <- DBI::dbGetQuery(d$con, sql)\\n",
            "}"
        )
    }
    
    # Timeline synchronization logic
    sync_code <- if (synchronize_timelines) {
        paste0(
            "\\n",
            "if(!is.null(m$debounced_datetimes_timeline_%tab_id%)) if (length(m$debounced_datetimes_timeline_%tab_id%()) > 0) datetimes <- m$debounced_datetimes_timeline_%tab_id%()"
        )
    } else {
        ""
    }
    
    paste0(
        date_query, "\\n\\n",
        "if (length(data_datetimes_range) > 0){\\n",
        "    data_datetimes_range <- c(data_datetimes_range$min_visit_start_datetime, data_datetimes_range$max_visit_end_datetime)\\n",
        "    m$data_datetimes_range_%widget_id% <- data_datetimes_range\\n",
        "}\\n\\n",
        "datetimes <- data_datetimes_range",
        sync_code,
        "\\n\\nif (length(datetimes) > 0) m$datetimes_%widget_id% <- datetimes"
    )
}

# Generate data processing loop
generate_data_processing_loop <- function() {
    paste0(
        "for (concept_id in concepts$concept_id) {\\n",
        "    concept <- concepts %>% dplyr::filter(concept_id == !!concept_id)\\n\\n",
        "    if (nrow(concept) > 0){\\n",
        "        if (concept$domain_id %in% c('Measurement', 'Observation')) {\\n",
        "            data <- raw_data\\n\\n",
        "            if (nrow(data) > 0) {\\n",
        "                data <- data %>%\\n",
        "                    dplyr::filter(concept_id == !!concept_id | source_concept_id == !!concept_id) %>%\\n",
        "                    dplyr::select(datetime, value_as_number)\\n\\n",
        "                if (nrow(data) > 0) {\\n",
        "                    fake_data <- tibble::tibble(\\n",
        "                        datetime = c(data_datetimes_range[[1]] - lubridate::seconds(1), data_datetimes_range[[2]] + lubridate::seconds(1)),\\n",
        "                        value_as_number = c(NA, NA)\\n",
        "                    )\\n\\n",
        "                    data <- dplyr::bind_rows(fake_data, data)\\n",
        "                    data <- data %>% dplyr::arrange(datetime)\\n\\n",
        "                    features[[paste0('concept_', concept_id)]] <- xts::xts(data$value_as_number, data$datetime)\\n",
        "                    features_names <- c(features_names, concept$concept_name)\\n",
        "                }\\n",
        "            }\\n",
        "        }\\n",
        "    }\\n",
        "}\\n\\n",
        "if (length(features) > 0) combined_features <- do.call(merge, features)\\n",
        "if (length(features_names) > 0) colnames(combined_features) <- features_names"
    )
}

# Generate chart visualization code
generate_chart_code <- function(synchronize_timelines) {
    
    dygraph_call <- if (synchronize_timelines) {
        "dygraphs::dygraph(combined_features, group = 'tab_%tab_id%') %>%"
    } else {
        "dygraphs::dygraph(combined_features) %>%"
    }
    
    paste0(
        "if (length(combined_features) > 0){\\n    ",
        "fig <- \\n        ", dygraph_call, "\\n",
        "        dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2, useDataTimezone = TRUE) %>%\\n",
        "        dygraphs::dyRangeSelector(dateWindow = c(\\n",
        "            format(datetimes[[1]], '%Y-%m-%d %H:%M:%S'),\\n",
        "            format(datetimes[[2]], '%Y-%m-%d %H:%M:%S')\\n",
        "        )) %>%\\n",
        "        dygraphs::dyAxis('y', valueRange = c(0, NA))\\n",
        "}\\n\\n",
        "fig"
    )
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
# CODE EXECUTION ENGINE
# ======================================

# Main code execution handler
observe_event(input$run_code_%widget_id%, {
    
    fig <- character()
    
    # ====================
    # EXECUTE USER CODE
    # ====================
    tryCatch({
        # Execute the code stored in m$code_%widget_id%
        eval(parse(text = m$code_%widget_id%))
    }, error = function(e) {
        # Log error for debugging
        warning("Code execution error in widget %widget_id%: ", e$message)
        fig <<- character()
    })
    
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
    }
    
    # Display chart if generation was successful
    if (length(fig) > 0) {
        output$dygraph_%widget_id% <- dygraphs::renderDygraph(fig)
        
        shinyjs::hide("error_message_div_%widget_id%")
        shinyjs::show("dygraph_div_%widget_id%")
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
