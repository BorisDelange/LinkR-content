# Options for patients var dropdown
var_choice_options <- list(
    list(key = "age", text = i18np$t("age")),
    list(key = "gender", text = i18np$t("gender")),
    list(key = "mortality", text = i18np$t("mortality")),
    list(key = "stays", text = i18np$t("stays")),
    list(key = "length_of_stay", text = i18np$t("length_of_stay")),
    list(key = "readmissions", text = i18np$t("readmissions"))
)

tagList(
    div(
        br(),
        div(shiny.fluent::Dropdown.shinyInput(ns("var_choice_%widget_id%"), options = var_choice_options, value = "age"), style = "width:300px"),
        div(
            style = "display:flex;",
            div(
                id = ns("split_layout_left_%widget_id%"),
                style = "padding-right:10px; width:50%;", br(),
                uiOutput(ns("title_%widget_id%")),
                div(
                    id = ns("table_and_plot_div_%widget_id%"),
                    div(id = ns("plot_div_%widget_id%"), plotOutput(ns("plot_%widget_id%")), style = "border:solid 2px #EFEEEE; width:70%;"),
                    div(id = ns("table_div_%widget_id%"), tableOutput(ns("table_%widget_id%")), style = "border:solid 2px #EFEEEE; margin-top:5px; width:30%;"),
                    style = "display:flex;"
                ),
                br(),
                div(
                    id = ns("plot_size_div_%widget_id%"),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        div(
                            div(strong(i18np$t("plot_width")), style = "margin-top:6px;"),
                            div(shiny.fluent::Slider.shinyInput(ns("plot_width_%widget_id%"), value = 100, min = 1, max = 100), style = "width:200px; margin-left:-8px; padding-top:4px;"),
                            style = "width:33%; max-width:200px;"
                        ),
                        div(
                            div(strong(i18np$t("plot_height")), style = "margin-top:6px;"),
                            div(shiny.fluent::Slider.shinyInput(ns("plot_height_%widget_id%"), value = 100, min = 1, max = 100), style = "width:200px; margin-left:-8px; padding-top:4px;"),
                            style = "width:33%; max-width:200px;"
                        ),
                        div(
                            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                                shiny.fluent::Toggle.shinyInput(ns("show_params_%widget_id%"), value = TRUE, style = "margin-top:5px;"),
                                div(class = "toggle_title", i18np$t("show_params"), style = "padding-top:5px;")
                            ),
                            style = "width:34%; max-width:300px; margin-top:15px;"
                        )
                    )
                )
            ),
            div(
                id = ns("split_layout_right_%widget_id%"),
                style = "padding-left:10px; width:50%; margin-top:60px;",
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    shiny.fluent::Toggle.shinyInput(ns("show_stats_%widget_id%"), value = TRUE, style = "margin-top:5px;"),
                    div(class = "toggle_title", i18np$t("show_stats"), style = "padding-top:5px;")
                ),
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    shiny.fluent::Toggle.shinyInput(ns("side_by_side_%widget_id%"), value = TRUE, style = "margin-top:5px;"),
                    div(class = "toggle_title", i18np$t("table_plot_side_by_side"), style = "padding-top:5px;")
                )
            )
        )
    ),
    div(
        id = ns("patients_tab_%widget_id%"),
    )
)
