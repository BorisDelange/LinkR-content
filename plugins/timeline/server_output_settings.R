# ==========================================
# server_output_settings.R - Timeline Output Configuration Server Logic
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 REQUIRES CUSTOMIZATION - PLUGIN IMPLEMENTATION  🔧                     ██
# ██                                                                            ██
# ██  This file MUST be customized for your specific plugin.                    ██
# ██  Follow the template structure and implement your logic.                   ██
# ██  See comments and examples for guidance.                                   ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# TIMELINE PLUGIN - OUTPUT SETTINGS SERVER FILE
# 
# This file handles the server-side logic for the timeline output configuration interface.
# It manages user interactions with the no-code settings panel for medical timeline visualization.
# 
# CORE FUNCTIONALITY:
# - Timeline chart type management (dygraphs vs plotly)
# - Medical concept selection with OMOP domain filtering
# - Data source selection (patient vs visit level)
# - Timeline synchronization controls
# - Dynamic UI updates based on user selections
# 
# Manages output settings including loading/saving from user configurations,
# timeline synchronization, chart type management, and settings persistence
# Smart defaults based on concept selection and saved configurations
#

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

all_inputs_%widget_id% <- list(
    list(id = "data_source", type = "dropdown", default = "person"),
    list(id = "chart_type", type = "dropdown", default = "dygraphs"),
    list(id = "concepts_choice", type = "dropdown", default = "selected_concepts"),
    list(id = "concept_classes", type = "multiselect", default = c()),
    list(id = "omop_table", type = "multiselect", default = c("measurement")),
    list(id = "concepts", type = "multiselect", default = "all_available"),
    list(id = "synchronize_timelines", type = "toggle", default = FALSE),
    list(id = "automatically_update_output", type = "toggle", default = TRUE),
    list(id = "code", type = "code", default = "")
)

# ======================================
# DYNAMIC UI UPDATES BASED ON SELECTIONS
# ======================================

# Update concepts dropdown when chart type changes
observe_event(input$chart_type_%widget_id%, {
    
    # Define allowed domains based on chart type
    if (input$chart_type_%widget_id% == "dygraphs") {
        allowed_domains <- c("Measurement", "Observation")
        
        # For dygraphs: force concepts choice to "selected_concepts" and hide concept classes option
        shiny.fluent::updateDropdown.shinyInput(
            session, 
            "concepts_choice_%widget_id%", 
            options = list(
                list(key = "selected_concepts", text = i18np$t("selected_concepts"))
            ),
            value = "selected_concepts"
        )
        
        # Hide concept classes div and show concepts div
        shinyjs::hide("concept_classes_div_%widget_id%")
        shinyjs::show("concepts_div_%widget_id%")
        
    } else if (input$chart_type_%widget_id% == "plotly") {
        allowed_domains <- c("Measurement", "Observation", "Condition", "Procedure", "Drug")
        
        # For plotly: restore both options in concepts choice dropdown
        shiny.fluent::updateDropdown.shinyInput(
            session, 
            "concepts_choice_%widget_id%", 
            options = list(
                list(key = "selected_concept_classes", text = i18np$t("selected_concept_classes")),
                list(key = "selected_concepts", text = i18np$t("selected_concepts"))
            ),
            value = "selected_concepts"
        )
        
    } else {
        allowed_domains <- c("Measurement", "Observation")
    }
    
    # Filter concepts for the selected chart type
    filtered_concepts <- selected_concepts %>% dplyr::filter(domain_id %in% allowed_domains)
    
    # Update concepts dropdown options
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "concepts_%widget_id%", 
        options = convert_tibble_to_list(
            filtered_concepts,
            key_col = "concept_id", 
            text_col = "concept_name"
        )
    )
})

# Show/hide concept selection divs based on concepts_choice
observe_event(input$concepts_choice_%widget_id%, {
    
    if (input$concepts_choice_%widget_id% == "selected_concepts") {
        shinyjs::hide("concept_classes_div_%widget_id%")
        shinyjs::hide("omop_table_div_%widget_id%")
        shinyjs::show("concepts_div_%widget_id%")
    }
    else if (input$concepts_choice_%widget_id% == "selected_concept_classes") {
        
        # Get selected OMOP tables to determine relevant domain_ids
        omop_tables <- if (length(input$omop_table_%widget_id%) > 0) {
            input$omop_table_%widget_id%
        } else {
            c("measurement")  # Default fallback
        }
        
        # Map OMOP tables to their corresponding domain_ids
        table_to_domain_mapping <- list(
            "measurement" = "Measurement",
            "observation" = "Observation", 
            "procedure_occurrence" = "Procedure",
            "condition_occurrence" = "Condition",
            "drug_exposure" = "Drug"
        )
        
        # Get allowed domains based on selected OMOP tables
        allowed_domains <- unlist(table_to_domain_mapping[omop_tables])
        
        # Update concept class IDs for the selected OMOP tables
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
        shinyjs::show("omop_table_div_%widget_id%")
        shinyjs::show("concept_classes_div_%widget_id%")
    }
    else {
        # Hide all if invalid selection
        sapply(c("concepts_div_%widget_id%", "concept_classes_div_%widget_id%", "omop_table_div_%widget_id%"), shinyjs::hide)
    }
})

# Update concept classes when OMOP table selection changes
observe_event(input$omop_table_%widget_id%, {
    
    # Only update if we're in concept classes mode
    concepts_choice <- if (length(input$concepts_choice_%widget_id%) > 0) {
        input$concepts_choice_%widget_id%
    } else {
        "selected_concepts"
    }
    
    if (concepts_choice == "selected_concept_classes") {
        
        # Get selected OMOP tables
        omop_tables <- if (length(input$omop_table_%widget_id%) > 0) {
            input$omop_table_%widget_id%
        } else {
            c("measurement")  # Default fallback
        }
        
        # Map OMOP tables to their corresponding domain_ids
        table_to_domain_mapping <- list(
            "measurement" = "Measurement",
            "observation" = "Observation", 
            "procedure_occurrence" = "Procedure",
            "condition_occurrence" = "Condition",
            "drug_exposure" = "Drug"
        )
        
        # Get allowed domains based on selected OMOP tables
        allowed_domains <- unlist(table_to_domain_mapping[omop_tables])
        
        # Update concept class IDs for the selected OMOP tables
        concept_class_ids <- tibble::tibble(concept_class_id = character())
        if (length(d$dataset_concept) > 0) {
            if (nrow(d$dataset_concept) > 0) {
                concept_class_ids <- d$dataset_concept %>%
                    dplyr::filter(domain_id %in% allowed_domains) %>%
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
            value = c()  # Reset selection when table changes
        )
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
# PERMISSIONS AND UI VISIBILITY
# ======================================

# Hide Display + Save button if user doesn't have save permissions
if (!("projects_widgets_settings" %in% user_accesses)) {
    shinyjs::hide("display_and_save_%widget_id%")
}