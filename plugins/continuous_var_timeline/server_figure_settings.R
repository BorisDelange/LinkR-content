# ==========================================
# server_figure_settings.R - Figure Settings Logic
# ==========================================
# 
# Manages figure settings including loading/saving from user configurations,
# timeline synchronization, chart type management, and settings persistence
#
# ==========================================

# ======================================
# DYNAMIC UI UPDATES BASED ON SELECTIONS
# ======================================

# Update concepts dropdown when chart type changes
observe_event(input$chart_type_%widget_id%, {
    
    # Define allowed domains based on chart type
    if (input$chart_type_%widget_id% == "dygraphs") {
        allowed_domains <- c("Measurement", "Observation")
    } else if (input$chart_type_%widget_id% == "plotly") {
        allowed_domains <- c("Measurement", "Observation", "Condition", "Procedure", "Drug")
    } else {
        allowed_domains <- c("Measurement", "Observation")
    }
    
    # Filter concepts for the selected chart type
    filtered_concepts <- selected_concepts %>% 
        dplyr::filter(domain_id %in% allowed_domains)
    
    # Update concepts dropdown options
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "concepts_%widget_id%", 
        options = convert_tibble_to_list(
            filtered_concepts,
            key_col = "concept_id", 
            text_col = "concept_name"
        ),
        value = filtered_concepts$concept_id  # Pre-select all available concepts
    )
})

# Show/hide concept selection divs based on concepts_choice
observe_event(input$concepts_choice_%widget_id%, {
    
    if (input$concepts_choice_%widget_id% == "selected_concepts") {
        shinyjs::hide("concept_classes_div_%widget_id%")
        shinyjs::show("concepts_div_%widget_id%")
    }
    else if (input$concepts_choice_%widget_id% == "selected_concept_classes") {
        
        # Define allowed domains based on current chart type
        chart_type <- if (length(input$chart_type_%widget_id%) > 0) {
            input$chart_type_%widget_id%
        } else {
            "dygraphs"
        }
        
        if (chart_type == "dygraphs") {
            allowed_domains <- c("Measurement", "Observation")
        } else if (chart_type == "plotly") {
            allowed_domains <- c("Measurement", "Observation", "Condition", "Procedure", "Drug")
        } else {
            allowed_domains <- c("Measurement", "Observation")
        }
        
        # Update concept class IDs for the selected chart type
        concept_class_ids <- tibble::tibble(concept_class_id = character())
        if (length(d$dataset_concept) > 0) {
            if (nrow(d$dataset_concept) > 0) {
                concept_class_ids <- d$dataset_concept %>%
                    dplyr::filter(domain_id %in% allowed_domains) %>%
                    dplyr::distinct(concept_class_id) %>%
                    dplyr::arrange(concept_class_id)
            }
        }
        
        shiny.fluent::updateDropdown.shinyInput(
            session, 
            "concept_classes_%widget_id%", 
            options = convert_tibble_to_list(
                concept_class_ids, 
                key_col = "concept_class_id", 
                text_col = "concept_class_id"
            )
        )
        
        shinyjs::hide("concepts_div_%widget_id%")
        shinyjs::show("concept_classes_div_%widget_id%")
    }
    else {
        # Hide both if invalid selection
        sapply(c("concepts_div_%widget_id%", "concept_classes_div_%widget_id%"), shinyjs::hide)
    }
})

# ======================================
# LOAD FIGURE SETTINGS FROM DATABASE
# ======================================

# Handler for loading saved figure settings and code from selected user configuration
observe_event(input$load_figure_settings_%widget_id%, {
    
    # Get the selected user configuration ID
    link_id <- input$user_configuration_%widget_id%
    
    # Query database for all figure settings associated with this configuration
    sql <- glue::glue_sql(
        "SELECT name, value, value_num 
         FROM widgets_options 
         WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", 
        .con = m$db
    )
    figure_settings <- DBI::dbGetQuery(m$db, sql)
    
    code <- ""
    
    # Update UI components with saved values
    if (nrow(figure_settings) > 0) {
        # Process each saved setting and update corresponding UI element
        sapply(figure_settings$name, function(name) {
            
            # Extract value and numeric value for this setting
            value <- figure_settings %>% 
                dplyr::filter(name == !!name) %>% 
                dplyr::pull(value)
            value_num <- figure_settings %>% 
                dplyr::filter(name == !!name) %>% 
                dplyr::pull(value_num)
            
            # Update UI elements based on setting type
            if (name %in% c("data_source", "chart_type", "concepts_choice")) {
                # Update dropdown selections
                shiny.fluent::updateDropdown.shinyInput(
                    session, 
                    paste0(name, "_%widget_id%"), 
                    value = value
                )
            } 
            else if (name == "concepts") {
                # Update concepts multi-select dropdown
                # Convert comma-separated string back to numeric vector
                value <- as.numeric(unlist(strsplit(value, ", ")))
                shiny.fluent::updateDropdown.shinyInput(
                    session, 
                    paste0(name, "_%widget_id%"), 
                    value = value
                )
            }
            else if (name == "concept_classes") {
                # Update concept classes multi-select dropdown
                # Convert comma-separated string back to character vector
                value <- unlist(strsplit(value, ", "))
                shiny.fluent::updateDropdown.shinyInput(
                    session, 
                    paste0(name, "_%widget_id%"), 
                    value = value
                )
            }
            else if (name %in% c("synchronize_timelines", "automatically_update_figure")) {
                # Update toggle switches
                # Convert numeric value back to logical
                value <- as.logical(value_num)
                shiny.fluent::updateToggle.shinyInput(
                    session, 
                    paste0(name, "_%widget_id%"), 
                    value = value
                )
            }
            else if (name == "code") {
                # Update code editor content
                code <<- value
                m$code_%widget_id% <- value
                shinyAce::updateAceEditor(session, "code_%widget_id%", value = code)
            }
        })
    }
    
    # Auto-execute code if enabled
    if (length(input$run_code_at_user_configuration_load_%widget_id%) > 0) {
        if (input$run_code_at_user_configuration_load_%widget_id%) {
            shinyjs::delay(500, {
                shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))  
            })
        }
    }
})

# ======================================
# SAVE FIGURE SETTINGS TO DATABASE
# ======================================

# Observer for saving current figure settings and code to selected user configuration
observe_event(input$save_params_and_code_trigger_%widget_id%, {
    
    # Validate user configuration selection
    if (length(input$user_configuration_%widget_id%) == 0) {
        # If no configuration is selected, redirect to configuration management
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_user_configurations_tab_%widget_id%', Math.random());"))
        return()
    }
    
    link_id <- input$user_configuration_%widget_id%

    # Remove existing settings for this configuration to avoid duplicates
    sql_send_statement(
        m$db, 
        glue::glue_sql(
            "DELETE FROM widgets_options 
             WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", 
            .con = m$db
        )
    )
    
    # Prepare new settings data
    new_data <- tibble::tribble(
        ~name, ~value, ~value_num,
        "data_source", input$data_source_%widget_id%, NA_real_,
        "chart_type", input$chart_type_%widget_id%, NA_real_,
        "concepts_choice", input$concepts_choice_%widget_id%, NA_real_,
        "concepts", input$concepts_%widget_id% %>% toString(), NA_real_,
        "concept_classes", input$concept_classes_%widget_id% %>% toString(), NA_real_,
        "synchronize_timelines", NA_character_, as.integer(input$synchronize_timelines_%widget_id%),
        "automatically_update_figure", NA_character_, as.integer(input$automatically_update_figure_%widget_id%),
        "code", input$code_%widget_id%, NA_real_
    )
    
    # Add database metadata
    new_data <- new_data %>%
        dplyr::transmute(
            id = get_last_row(m$db, "widgets_options") + 1:nrow(new_data), 
            widget_id = %widget_id%, 
            person_id = NA_integer_, 
            link_id = link_id,
            category = "figure_settings", 
            name, 
            value, 
            value_num, 
            creator_id = m$user_id, 
            datetime = now(), 
            deleted = FALSE
        )
    
    # Insert new settings into database
    DBI::dbAppendTable(m$db, "widgets_options", new_data)
    
    # Show success message
    show_message_bar("modif_saved", "success")
})

# ======================================
# SAVE TRIGGERS
# ======================================

# Auto-save on startup (without notification)
shinyjs::delay(1000, {
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_trigger_%widget_id%', Math.random());"))
})

# Handle manual save button clicks
observe_event(input$save_params_and_code_%widget_id%, {
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_trigger_%widget_id%', Math.random());"))
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
        } else if (chart_type == "plotly") {
            shinyjs::runjs(sprintf(
                "document.getElementById('%s').style.paddingLeft = '80px';",
                ns("plot_%widget_id%")
            ))
        }
    } else {
        # Remove padding when synchronization is disabled
        if (chart_type == "dygraphs") {
            shinyjs::runjs(sprintf(
                "document.getElementById('%s').style.paddingLeft = '0px'; var event = new Event('resize'); window.dispatchEvent(event);",
                ns("dygraph_div_%widget_id%")
            ))
        } else if (chart_type == "plotly") {
            shinyjs::runjs(sprintf(
                "document.getElementById('%s').style.paddingLeft = '0px';",
                ns("plot_%widget_id%")
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
observe_event(plotly::event_data("plotly_relayout", source = "plot_%widget_id%"), {
    
    # Only process if timeline synchronization is enabled
    if (!input$synchronize_timelines_%widget_id%) return()
    
    relayout_data <- plotly::event_data("plotly_relayout", source = "plot_%widget_id%")
    
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
    
    # Check for significant timeline changes
    # Calculate time difference between current and synchronized timelines
    time_diff_start <- abs(
        as.numeric(m$debounced_datetimes_timeline_%tab_id%()[[1]]) - 
        as.numeric(m$datetimes_%widget_id%[[1]])
    )
    time_diff_end <- abs(
        as.numeric(m$debounced_datetimes_timeline_%tab_id%()[[2]]) - 
        as.numeric(m$datetimes_%widget_id%[[2]])
    )
    
    # Trigger code re-execution if timeline has changed significantly (>5 seconds)
    if (time_diff_start > 5 || time_diff_end > 5) {
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
    
}, ignoreInit = TRUE)
