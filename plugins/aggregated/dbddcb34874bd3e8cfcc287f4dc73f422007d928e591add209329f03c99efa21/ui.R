x_variables <- convert_tibble_to_list(selected_concepts, key_col = "concept_id", text_col = "concept_name")
y_variables <- convert_tibble_to_list(selected_concepts, key_col = "concept_id", text_col = "concept_name")

splitLayout(
    cellWidths = c("50%", "50%"),
    div(style = "padding-right:10px;",
        plotOutput(ns("plot_output_%widget_id%")), style = "border:dashed 1px; margin-top:10px;"
    ),
    div(style = "padding-left:10px;",
        shiny.fluent::Pivot(
            onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab_%widget_id%', item.props.id)")),
            shiny.fluent::PivotItem(id = "plot_parameters_%widget_id%", itemKey = "plot_parameters", headerText = i18np$t("plot_parameters")),
            shiny.fluent::PivotItem(id = "variables_%widget_id%", itemKey = "variables", headerText = i18np$t("variables"))
        ),
        conditionalPanel(
            condition = "input.current_tab_%widget_id% == 'plot_parameters_%widget_id%'", ns = ns, br(),
            shiny.fluent::Dropdown.shinyInput(ns("plot_function_%widget_id%"), label = i18np$t("plot_choice"),
                options = list(
                    list(key = "geom_histogram", text = "geom_histogram"),
                    list(key = "geom_bar", text = "geom_bar")
                ),
                value = "geom_histogram"),
            shiny.fluent::Dropdown.shinyInput(ns("plot_theme_%widget_id%"), label = i18np$t("theme"),
                options = list(
                    list(key = "theme_grey", text = "Grey"),
                    list(key = "theme_gray", text = "Gray"),
                    list(key = "theme_bw", text = "Black & white"),
                    list(key = "theme_linedraw", text = "Linedraw"),
                    list(key = "theme_light", text = "Light"),
                    list(key = "theme_dark", text = "Dark"),
                    list(key = "theme_minimal", text = "Minimal"),
                    list(key = "theme_classic", text = "Classic"),
                    list(key = "theme_void", text = "Void"),
                    list(key = "theme_test", text = "Test")
                ),
                value = "theme_bw"),
            conditionalPanel(
                condition = "input.plot_function_%widget_id% == 'geom_histogram'", ns = ns,
                shiny.fluent::TextField.shinyInput(ns("x_label_%widget_id%"), label = i18np$t("x_label")),
                shiny.fluent::TextField.shinyInput(ns("y_label_%widget_id%"), label = i18np$t("y_label")),
                shiny.fluent::TextField.shinyInput(ns("stat_%widget_id%"), label = "stat"),
                shiny.fluent::TextField.shinyInput(ns("bins_%widget_id%"), label = "bins")
            )
        ),
        conditionalPanel(
            condition = "input.current_tab_%widget_id% == 'variables_%widget_id%'", ns = ns, br(),
            shiny.fluent::Dropdown.shinyInput(ns("x_variable_%widget_id%"), options = x_variables, label = i18np$t("x_variable")),
            shiny.fluent::Dropdown.shinyInput(ns("y_variable_%widget_id%"), options = y_variables, label = i18np$t("y_variable"))
        ),
        br(), br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            shiny.fluent::PrimaryButton.shinyInput(ns("show_%widget_id%"), i18np$t("show")),
            shiny.fluent::DefaultButton.shinyInput(ns("save_%widget_id%"), i18np$t("save")), 
            shiny.fluent::Toggle.shinyInput(ns("hide_params_%widget_id%"), value = FALSE, style = "margin-top:5px;"),
            div(class = "toggle_title", i18np$t("hide_params"), style = "padding-top:5px;")
        ),
        br(), br(),
        textOutput(ns("test_%widget_id%")), br(), br(),
        tableOutput(ns("table_%widget_id%"))
    )
)
