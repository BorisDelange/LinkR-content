# ==========================================
# server_output_settings.R - Data Table Configuration Server Logic
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 DATA TABLE PLUGIN - CUSTOMIZED IMPLEMENTATION  🔧                      ██
# ██                                                                            ██
# ██  This file handles server-side logic for the Data Table plugin            ██
# ██  configuration interface with OMOP concept selection.                     ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# DATA TABLE PLUGIN - OUTPUT SETTINGS SERVER FILE
# 
# This file manages server-side logic for the Data Table plugin configuration.
# It handles dynamic UI updates for concept selection, OMOP table filtering,
# and time-based aggregation settings.
# 
# KEY FUNCTIONALITY:
# - Dynamic concept selection UI (selected concepts vs concept classes)
# - OMOP table filtering for concept classes (Measurement/Observation only)
# - Cascading dropdown updates based on user selections
# - Input validation and error handling
# - Configuration persistence through centralized input definitions

# ======================================
# CENTRALIZED INPUT DEFINITIONS
# ======================================

# Define all inputs for this plugin in one centralized location.
# This configuration automatically generates the saving and loading logic for user configurations.
# 
# IMPORTANT: All UI elements must be added to ui_output_settings.R first, then registered here
# to enable automatic persistence of user preferences through the configuration system (server_user_configurations.R).
#
# STRUCTURE:
# Each input is defined as a list with the following required fields:
# - id: unique identifier (will be suffixed with _%widget_id%)
# - type: input type (see available types below)
# - default: default value when no configuration is loaded
#
# AVAILABLE INPUT TYPES:
# - "dropdown": Single selection dropdown (shiny.fluent::Dropdown)
# - "multiselect": Multiple selection dropdown (shiny.fluent::Dropdown with multiSelect = TRUE)
# - "text": Text input field (shiny.fluent::TextField)
# - "toggle": Boolean toggle switch (shiny.fluent::Toggle)
# - "code": Code editor (shinyAce::aceEditor)
# - "date": Date picker (shiny.fluent::DatePicker)
# - "number": Numeric input (shiny.fluent::SpinButton)
#
# WORKFLOW:
# 1. Create UI elements in ui_output_settings.R
# 2. Register all inputs here in all_inputs_%widget_id%
# 3. User configuration save/load is handled automatically
# 4. Add custom logic below for dynamic behavior and validation

all_inputs_%widget_id% <- list(
    list(id = "data_source", type = "dropdown", default = "person"),
    list(id = "concepts_choice", type = "dropdown", default = "selected_concepts"),
    list(id = "omop_table", type = "dropdown", default = "measurement"),
    list(id = "concept_classes", type = "multiselect", default = c()),
    list(id = "concepts", type = "multiselect", default = "all_available"),
    list(id = "column_organization", type = "dropdown", default = "regular_intervals"),
    list(id = "num_cols", type = "number", default = 8),
    list(id = "aggregate_fct", type = "dropdown", default = "mean"),
    list(id = "synchronize_timelines", type = "toggle", default = FALSE),
    list(id = "auto_update", type = "toggle", default = TRUE),
    list(id = "code", type = "code", default = "")
)

# ======================================
# CONDITIONAL UI DISPLAY LOGIC
# ======================================

# Show/hide column organization options based on column_organization selection
observe_event(input$column_organization_%widget_id%, {
    # Check if updates are locked (during configuration loading)
    if (isTRUE(input$update_lock_%widget_id%)) {
        return()
    }
    
    column_organization <- input$column_organization_%widget_id%
    
    if (!is.null(column_organization)) {
        if (column_organization == "regular_intervals") {
            # Show number of columns and aggregation function
            shinyjs::show("num_cols_div_%widget_id%")
            shinyjs::show("aggregate_fct_div_%widget_id%")
        } else if (column_organization == "by_timestamp") {
            # Hide number of columns and aggregation function
            shinyjs::hide("num_cols_div_%widget_id%")
            shinyjs::hide("aggregate_fct_div_%widget_id%")
        }
    }
})

# Show/hide concept selection UI based on concepts_choice selection
observe_event(input$concepts_choice_%widget_id%, {
    # Check if updates are locked (during configuration loading)
    if (isTRUE(input$update_lock_%widget_id%)) {
        return()
    }
    
    concepts_choice <- input$concepts_choice_%widget_id%
    
    if (!is.null(concepts_choice)) {
        if (concepts_choice == "selected_concepts") {
            # Show selected concepts dropdown, hide concept classes and OMOP table
            shinyjs::show("concepts_div_%widget_id%")
            shinyjs::hide("concept_classes_div_%widget_id%")
            shinyjs::hide("omop_table_div_%widget_id%")
        } else if (concepts_choice == "selected_concept_classes") {
            # Show concept classes and OMOP table, hide selected concepts
            shinyjs::show("concept_classes_div_%widget_id%")
            shinyjs::show("omop_table_div_%widget_id%")
            shinyjs::hide("concepts_div_%widget_id%")
            
            # Populate concept classes based on current OMOP table selection
            omop_table <- if (length(input$omop_table_%widget_id%) > 0) {
                input$omop_table_%widget_id%
            } else {
                "measurement"  # Default fallback
            }
            
            # Map OMOP tables to their corresponding domain_ids
            table_to_domain_mapping <- list(
                "measurement" = "Measurement",
                "observation" = "Observation"
            )
            
            # Get allowed domain based on selected OMOP table
            allowed_domain <- table_to_domain_mapping[[omop_table]]
            
            # Update concept class IDs for the selected OMOP table
            concept_class_ids <- tibble::tibble(concept_class_id = character())
            if (length(d$dataset_concept) > 0) {
                if (nrow(d$dataset_concept) > 0) {
                    concept_class_ids <- d$dataset_concept %>%
                        dplyr::filter(domain_id == allowed_domain) %>%
                        dplyr::distinct(concept_class_id) %>%
                        dplyr::arrange(concept_class_id)
                }
            }
            
            # Update the concept classes dropdown
            shiny.fluent::updateDropdown.shinyInput(
                session, 
                "concept_classes_%widget_id%", 
                options = convert_tibble_to_list(
                    concept_class_ids, 
                    key_col = "concept_class_id", 
                    text_col = "concept_class_id"
                )
            )
        }
    }
})

# Update concept classes dropdown based on OMOP table selection
observe_event(input$omop_table_%widget_id%, {
    # Check if updates are locked (during configuration loading)
    if (isTRUE(input$update_lock_%widget_id%)) {
        return()
    }
    
    # Only update if we're in concept classes mode
    concepts_choice <- if (length(input$concepts_choice_%widget_id%) > 0) {
        input$concepts_choice_%widget_id%
    } else {
        "selected_concepts"
    }
    
    if (concepts_choice == "selected_concept_classes") {
        
        # Get selected OMOP table
        omop_table <- if (length(input$omop_table_%widget_id%) > 0) {
            input$omop_table_%widget_id%
        } else {
            "measurement"  # Default fallback
        }
        
        # Map OMOP tables to their corresponding domain_ids
        table_to_domain_mapping <- list(
            "measurement" = "Measurement",
            "observation" = "Observation"
        )
        
        # Get allowed domain based on selected OMOP table
        allowed_domain <- table_to_domain_mapping[[omop_table]]
        
        # Update concept class IDs for the selected OMOP table
        concept_class_ids <- tibble::tibble(concept_class_id = character())
        if (length(d$dataset_concept) > 0) {
            if (nrow(d$dataset_concept) > 0) {
                concept_class_ids <- d$dataset_concept %>%
                    dplyr::filter(domain_id == allowed_domain) %>%
                    dplyr::distinct(concept_class_id) %>%
                    dplyr::arrange(concept_class_id)
            }
        }
        
        # Update the concept classes dropdown
        shiny.fluent::updateDropdown.shinyInput(
            session, 
            "concept_classes_%widget_id%", 
            options = convert_tibble_to_list(
                concept_class_ids, 
                key_col = "concept_class_id", 
                text_col = "concept_class_id"
            ),
            value = character(0)
        )
    }
})

# Select/unselect all concepts buttons
observe_event(input$concepts_check_all_%widget_id%, {
    # Check if updates are locked (during configuration loading)
    if (isTRUE(input$update_lock_%widget_id%)) {
        return()
    }
    
    # Get all available concept IDs for Measurement and Observation domains
    all_concept_ids <- selected_concepts %>%
        dplyr::filter(domain_id %in% c("Measurement", "Observation")) %>%
        dplyr::pull(concept_id)
    
    # Update dropdown to select all
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "concepts_%widget_id%", 
        value = all_concept_ids
    )
})

observe_event(input$concepts_uncheck_all_%widget_id%, {
    # Check if updates are locked (during configuration loading)
    if (isTRUE(input$update_lock_%widget_id%)) {
        return()
    }
    
    # Update dropdown to clear all selections
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "concepts_%widget_id%", 
        value = character(0)
    )
})

# Hide Display + Save button if user doesn't have save permissions
if (!("projects_widgets_settings" %in% user_accesses)) {
    shinyjs::hide("display_and_save_%widget_id%")
}

# ======================================
# INITIAL UI STATE SETUP
# ======================================

# Set initial visibility based on default values
shinyjs::delay(100, {
    # Column organization visibility
    column_organization <- input$column_organization_%widget_id%
    if (is.null(column_organization)) column_organization <- "regular_intervals"
    
    if (column_organization == "regular_intervals") {
        shinyjs::show("num_cols_div_%widget_id%")
        shinyjs::show("aggregate_fct_div_%widget_id%")
    } else if (column_organization == "by_timestamp") {
        shinyjs::hide("num_cols_div_%widget_id%")
        shinyjs::hide("aggregate_fct_div_%widget_id%")
    }
    
    # Concepts choice visibility
    concepts_choice <- input$concepts_choice_%widget_id%
    if (is.null(concepts_choice)) concepts_choice <- "selected_concepts"
    
    if (concepts_choice == "selected_concepts") {
        shinyjs::show("concepts_div_%widget_id%")
        shinyjs::hide("concept_classes_div_%widget_id%")
        shinyjs::hide("omop_table_div_%widget_id%")
    } else if (concepts_choice == "selected_concept_classes") {
        shinyjs::show("concept_classes_div_%widget_id%")
        shinyjs::show("omop_table_div_%widget_id%")
        shinyjs::hide("concepts_div_%widget_id%")
    }
})
