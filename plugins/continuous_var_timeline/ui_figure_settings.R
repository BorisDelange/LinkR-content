# UI - Figure settings
#
# Insert the UI components for configuring the figure settings in this section.

div(
    div(
        shiny.fluent::Dropdown.shinyInput(
            ns("data_source_%widget_id%"), options = list(
                list(key = "person", text = i18np$t("patient_data")),
                list(key = "visit_detail", text = i18np$t("stay_data"))
            ), value = "person", label = i18np$t("data_to_display")
        ),
        style = "width: 200px;"
    ),
    div(
        id = ns("concepts_div_%widget_id%"),
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("concepts_%widget_id%"), label = i18np$t("concepts"),
                options = convert_tibble_to_list(
                    selected_concepts %>% dplyr::filter(domain_id %in% c("Measurement", "Observation")),
                    key_col = "concept_id", text_col = "concept_name"
                ),
                value = selected_concepts$concept_id,
                multiSelect = TRUE
            ),
            style = "width: 200px;"
        ),
        style = "display: flex; gap: 10px; padding-bottom: 15px; border-bottom: solid 1px #808080;"
    ),
    div(
        shiny.fluent::Toggle.shinyInput(ns("synchronize_timelines_%widget_id%"), label = i18np$t("synchronize_timelines")),
        style = "margin-top: 15px;"
    ),
    div(
        shiny.fluent::PrimaryButton.shinyInput(
            ns("settings_display_figure_%widget_id%"), i18np$t("display_figure"),
            onClick = htmlwidgets::JS(paste0("item => { Shiny.setInputValue('", id, "-display_figure_%widget_id%', Math.random()); }"))
        ),
        shiny.fluent::DefaultButton.shinyInput(
            ns("save_settings_%widget_id%"), i18np$t("save_figure_settings_and_code"),
            onClick = htmlwidgets::JS(paste0("item => { Shiny.setInputValue('", id, "-save_params_and_code_%widget_id%', Math.random()); }"))
        ),
        style = "margin-top: 15px; display: flex; gap: 5px;"
    )
)
