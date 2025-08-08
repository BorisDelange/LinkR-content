# ==========================================
# ui_output_settings.R - Output Configuration Panel
# ==========================================

div(
    # ====================
    # HEALTHCARE INDICATOR SELECTION
    # ====================
    # Select the main healthcare indicator to analyze
    div(
        id = ns("indicator_div_%widget_id%"),
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("indicator_%widget_id%"), 
                options = list(
                    list(key = "patient_count", text = i18np$t("patient_count")),
                    list(key = "admission_count", text = i18np$t("admission_count")),
                    list(key = "mortality", text = i18np$t("mortality")),
                    list(key = "average_length_of_stay", text = i18np$t("average_length_of_stay"))
                ), 
                value = "patient_count",
                label = i18np$t("indicator")
            ),
            style = "width: 250px;"
        )
    ),
    
    # ====================
    # INDICATOR TARGET SCOPE
    # ====================
    # Define the scope/target for the indicator analysis
    div(
        id = ns("indicator_scope_div_%widget_id%"),
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("indicator_scope_%widget_id%"), 
                options = list(
                    list(key = "hospitalization", text = i18np$t("hospitalization")),
                    list(key = "hospital_units", text = i18np$t("hospital_units"))
                ), 
                value = "hospitalization",
                label = i18np$t("indicator_scope")
            ),
            style = "width: 250px;"
        ),
        style = "padding-top: 10px;"
    ),
    
    # ====================
    # HOSPITAL UNIT SELECTION
    # ====================
    # Multi-select dropdown for choosing specific hospital units
    shinyjs::hidden(
        div(
            id = ns("hospital_unit_div_%widget_id%"),
            div(
                shiny.fluent::Dropdown.shinyInput(
                    ns("hospital_unit_%widget_id%"), 
                    label = i18np$t("hospital_unit"),
                    multiSelect = TRUE
                ),
                style = "width: 250px;"
            ),
            div(
                id = ns("hospital_unit_buttons_%widget_id%"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("hospital_unit_check_all_%widget_id%"), iconProps = list(iconName = "CheckboxComposite")), text = i18np$t("select_all_units")),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("hospital_unit_uncheck_all_%widget_id%"), iconProps = list(iconName = "Checkbox")), text = i18np$t("clear_selection")),
                style = "margin: 27px 0 0 5px; display: flex;"
            ),
            class = "small_icon_button",
            style = "display: flex; padding-top: 10px;"
        )
    ),
    
    # ====================
    # AUTOMATIC UPDATES TOGGLE
    # ====================
    # Enable/disable automatic result updates
    div(
        id = ns("auto_update_div_%widget_id%"),
        shiny.fluent::Toggle.shinyInput(
            ns("auto_update_%widget_id%"), 
            label = i18np$t("automatic_updates"),
            value = TRUE
        ),
        style = "margin-top: 15px; padding-top: 15px; border-top: solid 1px #808080;"
    ),
    
    # ====================
    # ACTION BUTTONS
    # ====================
    # Primary actions for displaying and saving results
    div(
        id = ns("action_buttons_div_%widget_id%"),
        # Primary action - Generate/Update output
        shiny.fluent::PrimaryButton.shinyInput(
            ns("display_output_2_%widget_id%"), 
            i18np$t("display_output"), iconProps = list(iconName = "Play"),
            onClick = htmlwidgets::JS(paste0(
                "item => { Shiny.setInputValue('", id, "-display_output_%widget_id%', Math.random()); }"
            ))
        ),
        
        # Secondary action - Save configuration (if user has permissions)
        shiny.fluent::DefaultButton.shinyInput(
            ns("save_output_settings_and_code_2_%widget_id%"), 
            i18np$t("save_output_settings_and_code"), iconProps = list(iconName = "Save"),
            onClick = htmlwidgets::JS(paste0(
                "item => { Shiny.setInputValue('", id, "-save_output_settings_and_code_%widget_id%', Math.random()); }"
            ))
        ),
        
        style = "margin-top: 15px; display: flex; gap: 10px; flex-wrap: wrap;"
    )
)
