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
                div(plotOutput(ns("plot_%widget_id%")), style = "border:solid 2px #EFEEEE;"),
                div(tableOutput(ns("table_%widget_id%")), style = "border:solid 2px #EFEEEE; margin-top:5px;")
            ),
            div(
                id = ns("split_layout_right_%widget_id%"),
                style = "padding-left:10px; width:50%;",
                
            )
        )
    ),
    div(
        id = ns("patients_tab_%widget_id%"),
    )
)
