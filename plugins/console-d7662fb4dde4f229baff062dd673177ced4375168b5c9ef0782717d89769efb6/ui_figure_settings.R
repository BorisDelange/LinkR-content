div(
    div(
        shiny.fluent::Dropdown.shinyInput(
            ns("language_%widget_id%"), label = i18np$t("language"),
            options = list(
                list(key = "r", text = i18np$t("r")),
                list(key = "python", text = i18np$t("python"))
            ),
            value = "r"
        ), 
        style = "width: 200px;"
    ),
    div(
        shiny.fluent::Dropdown.shinyInput(
            ns("output_%widget_id%"), label = i18np$t("language"),
            options = list(
                list(key = "figure", text = i18np$t("figure_output")),
                list(key = "text", text = i18np$t("text_output"))
            ),
            value = "figure"
        ), 
        style = "width: 200px;"
    ),
    style = "display: flex; gap: 10px;"
)
