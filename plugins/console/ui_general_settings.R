# UI - General settings

div(
    tags$strong(i18np$t("display")), br(),
    div(
        shiny.fluent::Toggle.shinyInput(ns("show_settings_file_%widget_id%"), value = toggle_values$show_settings_file),
        tags$label(i18np$t("show_settings_file"), `for` = ns("show_settings_file_%widget_id%"), style = "margin-left: 5px;"),
        style = "display: flex; margin-top: 8px;" 
    ),
    div(
        shiny.fluent::Toggle.shinyInput(ns("figure_and_settings_side_by_side_%widget_id%"), value = toggle_values$figure_and_settings_side_by_side),
        tags$label(i18np$t("figure_and_settings_side_by_side"), `for` = ns("figure_and_settings_side_by_side_%widget_id%"), style = "margin-left: 5px;"),
        style = "display: flex; margin-top: 5px;" 
    ), br(),
    tags$strong(i18np$t("code_execution")), br(),
    div(
        shiny.fluent::Toggle.shinyInput(ns("run_code_at_settings_file_load_%widget_id%"), value = toggle_values$run_code_at_settings_file_load),
        tags$label(i18np$t("run_code_at_settings_file_load"), `for` = ns("run_code_at_settings_file_load_%widget_id%"), style = "margin-left: 5px;"),
        style = "display: flex; margin-top: 8px;" 
    ),
    div(
        shiny.fluent::Toggle.shinyInput(ns("run_code_on_data_update_%widget_id%"), value = toggle_values$run_code_on_data_update),
        tags$label(i18np$t("run_code_on_data_update"), `for` = ns("run_code_on_data_update_%widget_id%"), style = "margin-left: 5px;"),
        style = "display: flex; margin-top: 5px;" 
    ),
    style = "margin: 5px 10px;"
)
