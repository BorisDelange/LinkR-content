# ==========================================
# server_output_settings.R - Output Settings Logic
# ==========================================
# 
# Manages output settings including loading/saving from user configurations,
# timeline synchronization, chart type management, and settings persistence
# Smart defaults based on concept selection and saved configurations
#
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
