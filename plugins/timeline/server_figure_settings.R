# ==========================================
# server_figure_settings.R - Figure Settings Logic
# ==========================================
# 
# Manages figure settings including loading/saving from user configurations,
# timeline synchronization, chart type management, and settings persistence
# Smart defaults based on concept selection and saved configurations
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
    
    # Get current selected concepts to preserve selection
    current_selected_concepts <- input$concepts_%widget_id%
    
    # Filter current selection to keep only concepts available in new chart type
    preserved_concepts <- current_selected_concepts[
        current_selected_concepts %in% filtered_concepts$concept_id
    ]
    
    # If no concepts can be preserved, select all available concepts
    final_selection <- if (length(preserved_concepts) > 0) {
        preserved_concepts
    } else {
        filtered_concepts$concept_id
    }
    
    # Update concepts dropdown options
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "concepts_%widget_id%", 
        options = convert_tibble_to_list(
            filtered_concepts,
            key_col = "concept_id", 
            text_col = "concept_name"
        ),
        value = final_selection
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
# CONCEPTS CHECK/UNCHECK BUTTONS
# ======================================

# Check all concepts
observe_event(input$concepts_check_all_%widget_id%, {
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
    
    # Filter concepts for the current chart type
    filtered_concepts <- selected_concepts %>% 
        dplyr::filter(domain_id %in% allowed_domains)
    
    # Update dropdown options and select all concepts
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "concepts_%widget_id%", 
        options = convert_tibble_to_list(
            filtered_concepts,
            key_col = "concept_id", 
            text_col = "concept_name"
        ),
        value = filtered_concepts$concept_id
    )
})

# Uncheck all concepts
observe_event(input$concepts_uncheck_all_%widget_id%, {
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
    
    # Filter concepts for the current chart type
    filtered_concepts <- selected_concepts %>% 
        dplyr::filter(domain_id %in% allowed_domains)
    
    # Update dropdown options and deselect all concepts
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "concepts_%widget_id%", 
        options = convert_tibble_to_list(
            filtered_concepts,
            key_col = "concept_id", 
            text_col = "concept_name"
        ),
        value = c()
    )
})

# ======================================
# SMART DEFAULTS BASED ON CONCEPT SELECTION
# ======================================

# Apply smart defaults when concepts are loaded or changed
apply_smart_defaults <- function(concepts_to_check, has_saved_config = FALSE) {
    
    # Only apply smart defaults if no saved configuration exists
    if (has_saved_config) {
        return()
    }
    
    # Get domain information for the selected concepts
    if (length(concepts_to_check) > 0) {
        concept_domains <- selected_concepts %>%
            dplyr::filter(concept_id %in% concepts_to_check) %>%
            dplyr::distinct(domain_id) %>%
            dplyr::pull(domain_id)
        
        # Determine optimal chart type based on concept domains
        if (length(concept_domains) == 1 && concept_domains[1] == "Measurement") {
            # Only measurements - use dygraphs
            optimal_chart_type <- "dygraphs"
        } else {
            # Mixed domains or non-measurement only - use plotly
            optimal_chart_type <- "plotly"
        }
        
        # Update chart type if it's different from current selection
        current_chart_type <- if (length(input$chart_type_%widget_id%) > 0) {
            input$chart_type_%widget_id%
        } else {
            "dygraphs"
        }
        
        if (current_chart_type != optimal_chart_type) {
            shiny.fluent::updateDropdown.shinyInput(
                session, 
                "chart_type_%widget_id%", 
                value = optimal_chart_type
            )
        }
    }
}

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
    has_saved_concepts <- FALSE
    loaded_concepts <- c()
    
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
                
                # Check if we have valid saved concepts
                if (length(value) > 0 && !any(is.na(value))) {
                    has_saved_concepts <<- TRUE
                    loaded_concepts <<- value
                    
                    shiny.fluent::updateDropdown.shinyInput(
                        session, 
                        paste0(name, "_%widget_id%"), 
                        value = value
                    )
                } else {
                    # No valid saved concepts - will use all available concepts
                    has_saved_concepts <<- FALSE
                }
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
    # No saved configuration found - trigger figure display with default settings
    else shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-display_figure_%widget_id%', Math.random());"))
    
    # Handle case where no saved concepts exist - select all and apply smart defaults
    if (!has_saved_concepts) {
        # Select all available concepts
        all_concepts <- selected_concepts$concept_id
        
        shiny.fluent::updateDropdown.shinyInput(
            session, 
            "concepts_%widget_id%", 
            value = all_concepts
        )
        
        # Apply smart defaults based on all available concepts
        shinyjs::delay(100, {
            apply_smart_defaults(all_concepts, has_saved_config = FALSE)
        })
    } else {
        # Apply smart defaults based on loaded concepts if needed
        shinyjs::delay(100, {
            apply_smart_defaults(loaded_concepts, has_saved_config = TRUE)
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
})

# ======================================
# SAVE TRIGGERS
# ======================================

# Auto-save on startup (without notification)
# shinyjs::delay(1000, {
    # shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_trigger_%widget_id%', Math.random());"))
# })

# Handle manual save button clicks
observe_event(input$save_params_and_code_%widget_id%, {
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_trigger_%widget_id%', Math.random());"))
    
    # Notify user
    show_message_bar("modif_saved", "success")
})
