splitLayout(
    cellWidths = c("50%", "50%"),
    div(style = "padding-right:10px;",
        plotOutput(ns("plot_output_%widget_id%")), style = "border:dashed 1px; margin-top:10px;"
    ),
    div(style = "padding-left:10px;",
        strong(i18np$t("plot_parameters"), style = "color:grey"), br(), br(),
        shiny.fluent::Dropdown.shinyInput(ns("plot_function_%widget_id%"), label = i18np$t("plot_choice"),
            options = list(
                list(key = "geom_histogram", text = "geom_histogram"),
                list(key = "geom_bar", text = "geom_bar")#,
                  # autres fonctions ici...
            )),
        conditionalPanel(
            condition = "input.plot_function_%widget_id% == 'geom_histogram'",
            shiny.fluent::Stack(
                shiny.fluent::TextField.shinyInput(ns("stat_%widget_id%"), label = "stat"),
                shiny.fluent::TextField.shinyInput(ns("bins_%widget_id%"), label = "bins")
                # autres paramètres spécifiques ici...
            )
        ),
        # autres conditionalPanel pour les autres fonctions ici...
        br(),
        strong(i18np$t("variables"), style = "color:grey"), br(), br(),
        shiny.fluent::Dropdown.shinyInput(ns("x_variable_%widget_id%"), 
            options = list(
                # variables possibles ici...
            ),
            label = i18np$t("x_variable")),
        shiny.fluent::Dropdown.shinyInput(ns("y_variable_%widget_id%"), 
            options = list(
                # variables possibles ici...
            ),
            label = i18np$t("y_variable")),
        br(), br(),
        shiny.fluent::PrimaryButton.shinyInput(ns("submit_%widget_id%"), i18np$t("show")
    )
)

