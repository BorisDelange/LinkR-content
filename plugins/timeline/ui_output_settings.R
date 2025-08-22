# ==========================================
# ui_output_settings.R - Timeline Configuration Panel
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

# TIMELINE PLUGIN - OUTPUT SETTINGS UI FILE
# 
# This file defines the no-code configuration interface for the Timeline plugin.
# It provides user-friendly controls that automatically generate and modify the underlying
# R code for medical timeline visualization based on user selections.
# 
# Interactive panel for configuring timeline chart settings including:
# - Chart type selection (dygraphs vs plotly)
# - Medical concept selection from available OMOP data
# - Data source selection (patient vs visit level)
# - Timeline synchronization controls
# - Auto-update preferences
# 
# IMPORTANT NOTES:
# - AVOID using conditionalPanel() to show/hide UI elements. Instead, manage this logic
#   in the server file (server_output_settings.R) using shinyjs::show and shinyjs::hide functions.
#   This provides better control and maintains consistency with the reactive framework.
# - Each div element includes an ID attribute to enable dynamic show/hide functionality
#   using shinyjs::show() and shinyjs::hide() from the server logic. This allows for
#   conditional display of UI elements based on user selections.
# - When adding or removing configuration elements, make sure to update both the
#   CONFIGURATION LOADING FROM DATABASE and CONFIGURATION SAVING TO DATABASE sections
#   in server_user_configurations.R to ensure user choices are properly saved and restored.

div(
    # ====================
    # DATA SOURCE SELECTION
    # ====================
    div(
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("data_source_%widget_id%"), 
                options = list(
                    list(key = "person", text = i18np$t("patient_data")),
                    list(key = "visit_detail", text = i18np$t("stay_data"))
                ), 
                value = "person",                    # Default to patient data
                label = i18np$t("data_to_display")
            ),
            style = "width: 200px;"
        ),
        style = "padding-bottom: 15px; border-bottom: solid 1px #808080;"
    ),
    
    # ====================
    # CHART TYPE SELECTION
    # ====================
    div(
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("chart_type_%widget_id%"), 
                options = list(
                    list(key = "dygraphs", text = "dygraphs – Courbes continues (biologie, constantes)"),
                    list(key = "plotly", text = "plotly – Chronologie d'événements (traitements, actes)")
                ), 
                value = "dygraphs",                  # Default to dygraphs
                label = i18np$t("chart_type")
            ),
            style = "width: 350px;"
        ),
        style = "margin-top: 5px; padding-bottom: 15px; border-bottom: solid 1px #808080;"
    ),
    
    # ====================
    # CONCEPTS SELECTION METHOD
    # ====================
    div(
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("concepts_choice_%widget_id%"), 
                options = list(
                    list(key = "selected_concept_classes", text = i18np$t("selected_concept_classes")),
                    list(key = "selected_concepts", text = i18np$t("selected_concepts"))
                ), 
                value = "selected_concepts",         # Default to selected concepts
                label = i18np$t("concepts_to_display")
            ),
            style = "width: 200px;"
        ),
        style = "margin-top: 5px;"
    ),
    
    # ====================
    # CONCEPT CLASSES SELECTION (HIDDEN BY DEFAULT)
    # ====================
    shinyjs::hidden(
        div(
            id = ns("concept_classes_div_%widget_id%"),
            div(
                shiny.fluent::Dropdown.shinyInput(
                    ns("concept_classes_%widget_id%"), 
                    label = i18np$t("concept_classes"),
                    options = list(),               # Will be populated dynamically
                    multiSelect = TRUE
                ),
                style = "width: 200px;"
            ),
            style = "margin-top: 5px;"
        )
    ),
    
    # ====================
    # INDIVIDUAL CONCEPTS SELECTION
    # ====================
    div(
        id = ns("concepts_div_%widget_id%"),
        div(
            div(
                # Multi-select dropdown for medical concepts
                # Domain filter will be updated based on chart type
                shiny.fluent::Dropdown.shinyInput(
                    ns("concepts_%widget_id%"), 
                    label = i18np$t("concepts"),
                    
                    # Default to Measurement and Observation for dygraphs
                    options = convert_tibble_to_list(
                        selected_concepts %>% dplyr::filter(domain_id %in% c("Measurement", "Observation")),
                        key_col = "concept_id", 
                        text_col = "concept_name"
                    ),
                    
                    value = selected_concepts %>% 
                        dplyr::filter(domain_id %in% c("Measurement", "Observation")) %>%
                        dplyr::pull(concept_id),        # Pre-select available concepts
                    multiSelect = TRUE                  # Allow multiple selections
                ),
                style = "width: 200px;"
            ),
            div(
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("concepts_check_all_%widget_id%"), iconProps = list(iconName = "CheckboxComposite")), text = i18np$t("select_all_concepts")),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("concepts_uncheck_all_%widget_id%"), iconProps = list(iconName = "Checkbox")), text = i18np$t("unselect_all_concepts")),
                style = "margin: 27px 0 0 5px; display: flex;"
            ),
            class = "small_icon_button",
            style = "display: flex;"
        ),
        style = "margin-top: 5px;"
    ),
    
    # ====================
    # TIMELINE SYNCHRONIZATION
    # ====================
    div(
        shiny.fluent::Toggle.shinyInput(
            ns("synchronize_timelines_%widget_id%"), 
            label = i18np$t("synchronize_timelines"),
            value = FALSE
        ),
        style = "margin-top: 15px;"
    ),
    
    # ====================
    # AUTOMATIC OUTPUT UPDATES
    # ====================
    div(
        shiny.fluent::Toggle.shinyInput(
            ns("automatically_update_output_%widget_id%"), 
            label = i18np$t("automatically_update_output"),
            value = TRUE
        ),
        style = "margin-top: 15px;"
    ),
    
    # ====================
    # ACTION BUTTONS
    # ====================
    # Standard action buttons - customize labels as needed
    div(
        id = ns("action_buttons_div_%widget_id%"),
        # Default action - Generate/Update timeline only
        shiny.fluent::DefaultButton.shinyInput(
            ns("display_output_2_%widget_id%"), 
            i18np$t("display_output"), iconProps = list(iconName = "Play"),
            onClick = htmlwidgets::JS(paste0(
                "item => { Shiny.setInputValue('", id, "-display_output_%widget_id%', Math.random()); }"
            ))
        ),
        
        # Primary action - Display and Save (if user has permissions)
        # This button is shown/hidden based on user access in the main UI file
        shiny.fluent::PrimaryButton.shinyInput(
            ns("display_and_save_%widget_id%"), 
            i18np$t("display_and_save"), 
            iconProps = list(iconName = "SaveAs"),
            onClick = htmlwidgets::JS(paste0(
                "item => { ",
                "Shiny.setInputValue('", id, "-display_and_save_%widget_id%', Math.random()); ",
                "}"
            ))
        ),
        
        style = "margin-top: 20px; display: flex; gap: 10px; flex-wrap: wrap; padding-top: 15px; border-top: solid 1px #808080;"
    )
)