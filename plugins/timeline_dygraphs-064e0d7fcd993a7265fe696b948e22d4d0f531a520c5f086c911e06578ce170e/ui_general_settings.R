# UI - General settings
#
# Include the UI components for the general settings in this section.
# If you add any fields here, ensure that you also update server_general_settings.R and ui_general_settings.R accordingly.

div(
    tags$strong(i18np$t("display")), br(),
    div(
        shiny.fluent::Toggle.shinyInput(ns("show_saved_file_%widget_id%"), value = toggle_values$show_saved_file),
        tags$label(i18np$t("show_saved_file"), `for` = ns("show_saved_file_%widget_id%"), style = "margin-left: 5px;"),
        style = "display: flex; margin-top: 8px;" 
    ),
    div(
        shiny.fluent::Toggle.shinyInput(ns("figure_and_settings_side_by_side_%widget_id%"), value = toggle_values$figure_and_settings_side_by_side),
        tags$label(i18np$t("figure_and_settings_side_by_side"), `for` = ns("figure_and_settings_side_by_side_%widget_id%"), style = "margin-left: 5px;"),
        style = "display: flex; margin-top: 5px;" 
    ),
    style = "margin: 5px 10px;"
)
