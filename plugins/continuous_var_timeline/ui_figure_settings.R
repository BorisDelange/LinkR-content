# ==========================================
# ui_figure_settings.R - Chart Configuration Panel
# ==========================================
# 
# Interactive panel for configuring chart settings including:
# - Medical concept selection from available data
# - Data source selection (patient vs visit level)
# - Timeline synchronization controls
# - Auto-update preferences
#
# ==========================================

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
    # CONCEPTS SELECTION
    # ====================
    div(
        id = ns("concepts_div_%widget_id%"),
        div(
            # Multi-select dropdown for medical concepts
            shiny.fluent::Dropdown.shinyInput(
                ns("concepts_%widget_id%"), 
                label = i18np$t("concepts"),
                
                # Filter concepts to only Measurement and Observation domains
                options = convert_tibble_to_list(
                    selected_concepts %>% 
                        dplyr::filter(domain_id %in% c("Measurement", "Observation")),
                    key_col = "concept_id", 
                    text_col = "concept_name"
                ),
                
                value = selected_concepts$concept_id,  # Pre-select all available concepts
                multiSelect = TRUE                     # Allow multiple selections
            ),
            style = "width: 200px;"
        ),
        style = "margin-top: 15px;"
    ),
    
    # ====================
    # TIMELINE SYNCHRONIZATION
    # ====================
    div(
        shiny.fluent::Toggle.shinyInput(
            ns("synchronize_timelines_%widget_id%"), 
            label = i18np$t("synchronize_timelines"),
            value = FALSE                           # Default to disabled
        ),
        style = "margin-top: 15px;"
    ),
    
    # ====================
    # AUTOMATIC FIGURE UPDATES
    # ====================
    div(
        shiny.fluent::Toggle.shinyInput(
            ns("automatically_update_figure_%widget_id%"), 
            label = i18np$t("automatically_update_figure"),
            value = TRUE                            # Default to enabled
        ),
        style = "margin-top: 15px;"
    ),
    
    # ====================
    # ACTION BUTTONS
    # ====================
    div(
        # Primary action - Display the chart
        shiny.fluent::PrimaryButton.shinyInput(
            ns("settings_display_figure_%widget_id%"), 
            i18np$t("display_figure"),
            onClick = htmlwidgets::JS(paste0(
                "item => { Shiny.setInputValue('", id, "-display_figure_%widget_id%', Math.random()); }"
            ))
        ),
        
        # Secondary action - Save current configuration
        shiny.fluent::DefaultButton.shinyInput(
            ns("save_settings_%widget_id%"), 
            i18np$t("save_figure_settings_and_code"),
            onClick = htmlwidgets::JS(paste0(
                "item => { Shiny.setInputValue('", id, "-save_params_and_code_%widget_id%', Math.random()); }"
            ))
        ),
        
        style = "margin-top: 20px; display: flex; gap: 10px; flex-wrap: wrap;"
    )
)
