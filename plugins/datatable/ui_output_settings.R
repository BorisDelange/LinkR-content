# ==========================================
# ui_output_settings.R - Data Table Configuration Interface
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 DATA TABLE PLUGIN - CUSTOMIZED IMPLEMENTATION  🔧                      ██
# ██                                                                            ██
# ██  This file provides the Data Table plugin configuration interface with    ██
# ██  OMOP concept selection and time-based aggregation settings.              ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# DATA TABLE PLUGIN - OUTPUT SETTINGS UI FILE
# 
# This file defines the configuration interface for the Data Table plugin.
# Users can select data sources, concepts, and configure time-based aggregation
# for displaying measurement data in tabular format with temporal grouping.
# 
# KEY FEATURES:
# - Data source selection (patient vs visit detail data)
# - OMOP concept filtering with selected concepts and concept classes
# - OMOP table selection limited to Measurement and Observation tables
# - Time-based column configuration for temporal aggregation
# - Statistical aggregation function selection
# - Timeline synchronization options

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
                value = "person",
                label = i18np$t("data_to_display")
            ),
            style = "width: 250px;"
        ),
        style = "padding-bottom: 15px; border-bottom: solid 1px #808080;"
    ),
    
    # ====================
    # CONCEPTS CONFIGURATION SECTION
    # ====================
    div(
        # Concepts selection method dropdown
        div(
            div(
                shiny.fluent::Dropdown.shinyInput(
                    ns("concepts_choice_%widget_id%"), 
                    options = list(
                        list(key = "selected_concepts", text = i18np$t("selected_concepts")),
                        list(key = "selected_concept_classes", text = i18np$t("selected_concept_classes"))
                    ), 
                    value = "selected_concepts",
                    label = i18np$t("concepts_to_display")
                ),
                style = "width: 250px;"
            ),
            style = "flex: 0 0 auto; margin-right: 15px;"
        ),
        
        # OMOP table selection (hidden by default, only for concept classes)
        shinyjs::hidden(
            div(
                id = ns("omop_table_div_%widget_id%"),
                div(
                    shiny.fluent::Dropdown.shinyInput(
                        ns("omop_table_%widget_id%"), 
                        label = i18np$t("omop_table"),
                        options = list(
                            list(key = "measurement", text = "Measurement"),
                            list(key = "observation", text = "Observation")
                        ),
                        value = "measurement"
                    ),
                    style = "width: 250px;"
                ),
                style = "flex: 0 0 auto; margin-right: 15px;"
            )
        ),
        
        # Concept classes selection (hidden by default)
        shinyjs::hidden(
            div(
                id = ns("concept_classes_div_%widget_id%"),
                div(
                    shiny.fluent::Dropdown.shinyInput(
                        ns("concept_classes_%widget_id%"), 
                        label = i18np$t("concept_classes"),
                        options = list(),
                        multiSelect = TRUE
                    ),
                    style = "width: 250px;"
                ),
                style = "flex: 0 0 auto;"
            )
        ),
        
        # Individual concepts selection
        div(
            id = ns("concepts_div_%widget_id%"),
            div(
                shiny.fluent::Dropdown.shinyInput(
                    ns("concepts_%widget_id%"), 
                    label = i18np$t("concepts"),
                    options = convert_tibble_to_list(
                        selected_concepts %>% dplyr::filter(domain_id %in% c("Measurement", "Observation")),
                        key_col = "concept_id", text_col = "concept_name"
                    ),
                    value = selected_concepts %>% 
                        dplyr::filter(domain_id %in% c("Measurement", "Observation")) %>%
                        dplyr::pull(concept_id),
                    multiSelect = TRUE
                ),
                style = "width: 250px;"
            ),
            div(
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("concepts_check_all_%widget_id%"), iconProps = list(iconName = "CheckboxComposite")), text = i18np$t("select_all_concepts")),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("concepts_uncheck_all_%widget_id%"), iconProps = list(iconName = "Checkbox")), text = i18np$t("unselect_all_concepts")),
                style = "margin: 27px 0 0 5px; display: flex;"
            ),
            class = "small_icon_button",
            style = "display: flex; flex: 0 0 auto;"
        ),
        
        style = "margin-top: 5px; display: flex; flex-wrap: wrap; gap: 5px; padding-bottom: 15px; border-bottom: solid 1px #808080;"
    ),
    
    # ====================
    # TIME-BASED AGGREGATION SETTINGS
    # ====================
    div(
        # Number of time columns
        div(
            div(i18np$t("num_cols"), style = "font-weight: 600; margin-bottom: 5px;"),
            div(
                shiny.fluent::SpinButton.shinyInput(
                    ns("num_cols_%widget_id%"), 
                    min = 1, max = 20, by = 1, value = 8
                ), 
                style = "width: 150px;"
            ),
            style = "flex: 0 0 auto; margin-right: 15px; margin-top: 5px;"
        ),
        # Aggregation function
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("aggregate_fct_%widget_id%"), 
                label = i18np$t("aggregated_function"), 
                value = "mean", 
                options = list(
                    list(key = "min", text = i18np$t("min")),
                    list(key = "mean", text = i18np$t("mean")),
                    list(key = "median", text = i18np$t("median")),
                    list(key = "max", text = i18np$t("max"))
                )
            ),
            style = "width: 200px;"
        ),
        style = "margin-top: 5px; display: flex; flex-wrap: wrap; gap: 5px; padding-bottom: 15px; border-bottom: solid 1px #808080;"
    ),
    
    # ====================
    # TIMELINE OPTIONS
    # ====================
    div(
        # Timeline synchronization toggle
        div(
            shiny.fluent::Toggle.shinyInput(
                ns("synchronize_timelines_%widget_id%"), 
                label = i18np$t("synchronize_timelines"),
                value = FALSE
            ),
            style = "flex: 0 0 auto;"
        ),
        
        # Automatic updates toggle
        div(
            shiny.fluent::Toggle.shinyInput(
                ns("automatically_update_output_%widget_id%"), 
                label = i18np$t("automatically_update_output"),
                value = TRUE
            ),
            style = "flex: 0 0 auto;"
        ),
        
        style = "margin-top: 15px; display: flex; flex-wrap: wrap; gap: 5px; align-items: center;"
    ),
    
    # ====================
    # ACTION BUTTONS
    # ====================
    div(
        id = ns("action_buttons_div_%widget_id%"),
        # Default action - Generate/Update output only
        shiny.fluent::DefaultButton.shinyInput(
            ns("display_output_2_%widget_id%"), 
            i18np$t("display_output"), iconProps = list(iconName = "Play"),
            onClick = htmlwidgets::JS(paste0(
                "item => { Shiny.setInputValue('", id, "-display_output_%widget_id%', Math.random()); }"
            ))
        ),
        
        # Primary action - Display and Save (if user has permissions)
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
