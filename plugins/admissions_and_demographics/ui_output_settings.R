# ==========================================
# ui_output_settings.R - Output Configuration Panel
# ==========================================

div(
    # ====================
    # HEALTHCARE INDICATOR SELECTION
    # ====================
    # Responsive container for indicator and scope selection
    div(
        id = ns("indicator_and_scope_container_%widget_id%"),
        # Select the main healthcare indicator to analyze
        div(
            id = ns("indicator_div_%widget_id%"),
            div(
                shiny.fluent::Dropdown.shinyInput(
                    ns("indicator_%widget_id%"), 
                    options = list(
                        list(key = "patient_count", text = i18np$t("patient_count")),
                        list(key = "admission_count", text = i18np$t("admission_count")),
                        list(key = "admission_timeline", text = i18np$t("admission_timeline")),
                        # list(key = "readmission_rate", text = i18np$t("readmission_rate")),
                        # list(key = "admission_schedule", text = i18np$t("admission_schedule")),
                        list(key = "average_age", text = i18np$t("average_age")),
                        list(key = "gender", text = i18np$t("gender_distribution")),
                        list(key = "mortality_rate", text = i18np$t("mortality_rate")),
                        list(key = "average_length_of_stay", text = i18np$t("average_length_of_stay"))
                        # list(key = "bed_occupancy_rate", text = i18np$t("bed_occupancy_rate"))
                    ), 
                    value = "patient_count",
                    label = i18np$t("indicator")
                ),
                style = "width: 250px;"
            ),
            style = "flex: 0 0 auto; margin-right: 15px;"
        ),
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
            style = "flex: 0 0 auto;"
        ),
        style = "display: flex; flex-wrap: wrap; gap: 5px; padding-bottom: 15px; border-bottom: solid 1px #808080;"
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
                    label = i18np$t("hospital_units"),
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
    # LEGEND CONFIGURATION
    # ====================
    # Responsive container for legend text fields
    div(
        id = ns("legend_container_%widget_id%"),
        # Legend 1
        div(
            shiny.fluent::TextField.shinyInput(
                ns("legend_1_%widget_id%"),
                label = i18np$t("legend_1"),
                value = i18np$t("hospitalized_patients")
            ),
            style = "width: 250px; flex: 0 0 auto; margin-right: 15px;"
        ),
        # Legend 2
        div(
            shiny.fluent::TextField.shinyInput(
                ns("legend_2_%widget_id%"),
                label = i18np$t("legend_2"),
                value = ""
            ),
            style = "width: 250px; flex: 0 0 auto;"
        ),
        style = "display: flex; flex-wrap: wrap; gap: 5px; margin-top: 15px;"
    ),
    
    # ====================
    # TIMELINE CONFIGURATION
    # ====================
    # Responsive container for timeline configuration (hidden by default)
    shinyjs::hidden(
        div(
            id = ns("timeline_div_%widget_id%"),
            # All timeline fields on one responsive row
            div(
                # Timeline title
                div(
                    shiny.fluent::TextField.shinyInput(
                        ns("timeline_title_%widget_id%"),
                        label = i18np$t("plot_title"),
                        value = ""
                    ),
                    style = "width: 250px; flex: 0 0 auto; margin-right: 15px;"
                ),
                # X-axis label
                div(
                    id = ns("timeline_x_label_div_%widget_id%"),
                    shiny.fluent::TextField.shinyInput(
                        ns("timeline_x_label_%widget_id%"),
                        label = i18np$t("x_axis_legend"),
                        value = ""
                    ),
                    style = "width: 250px; flex: 0 0 auto; margin-right: 15px;"
                ),
                # Y-axis label
                div(
                    id = ns("timeline_y_label_div_%widget_id%"),
                    shiny.fluent::TextField.shinyInput(
                        ns("timeline_y_label_%widget_id%"),
                        label = i18np$t("y_axis_legend"),
                        value = ""
                    ),
                    style = "width: 250px; flex: 0 0 auto; margin-right: 15px;"
                ),
                # Number of bins
                div(
                    id = ns("timeline_nb_bins_div_%widget_id%"),
                    div(i18np$t("nb_bins"), style = "font-weight: 600; margin-bottom: 5px;"),
                    shiny.fluent::SpinButton.shinyInput(
                        ns("timeline_nb_bins_%widget_id%"),
                        value = 10,
                        min = 1,
                        max = 200,
                        step = 5
                    ),
                    style = "width: 250px; flex: 0 0 auto; margin-top: 5px;"
                ),
                style = "display: flex; flex-wrap: wrap; gap: 5px;"
            ),
            style = "margin-top: 15px; padding-top: 15px; border-top: solid 1px #808080;"
        )
    ),
    
    # ====================
    # HISTOGRAM BINS CONFIGURATION
    # ====================
    # Number of bins for histogram charts (hidden by default)
    shinyjs::hidden(
        div(
            id = ns("histogram_div_%widget_id%"),
            div(
                div(i18np$t("nb_bins"), style = "font-weight: 600; margin-bottom: 5px;"),
                shiny.fluent::SpinButton.shinyInput(
                    ns("histogram_nb_bins_%widget_id%"),
                    value = 10,
                    min = 5,
                    max = 100,
                    step = 5
                ),
                style = "width: 250px;"
            ),
            style = "margin-top: 15px; padding-top: 15px; border-top: solid 1px #808080;"
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
        style = "margin-top: 15px; padding: 15px 0; border-top: solid 1px #808080; border-bottom: solid 1px #808080;"
    ),
    
    # ====================
    # ACTION BUTTONS
    # ====================
    # Primary actions for displaying and saving results
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
        
        # Primary action - Display AND Save (if user has permissions)
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
        
        style = "margin-top: 15px; display: flex; gap: 10px; align-items: flex-start;"
    )
)
