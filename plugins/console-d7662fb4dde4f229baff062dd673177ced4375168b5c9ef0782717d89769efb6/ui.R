tagList(
    div(
        shiny.fluent::IconButton.shinyInput(ns("figure_%widget_id%"), iconProps = list(iconName = "BarChart4"), title = i18np$t("show_figure")),
        shiny.fluent::IconButton.shinyInput(ns("figure_settings_%widget_id%"), iconProps = list(iconName = "AllApps"), title = i18np$t("show_figure_settings")),
        shiny.fluent::IconButton.shinyInput(ns("code_%widget_id%"), iconProps = list(iconName = "Code"), title = i18np$t("show_code_editor")),
        shiny.fluent::IconButton.shinyInput(ns("general_settings_%widget_id%"), iconProps = list(iconName = "Settings"), title = i18np$t("show_general_settings")),
        class = "widget_icon",
        style = "display: flex; color: #808080; border-bottom: solid grey 0.5px;"
    ),
    div(
        id = ns("figure_settings_code_div_%widget_id%"),
        shinyjs::hidden(
            div(
                id = ns("figure_settings_code_sidenav_%widget_id%"),
                shiny.fluent::IconButton.shinyInput(ns("display_figure_%widget_id%"), iconProps = list(iconName = "Play"), title = i18np$t("display_figure"), style = "margin: 0"),
                shiny.fluent::IconButton.shinyInput(ns("save_params_and_code_%widget_id%"), iconProps = list(iconName = "Save"), title = i18np$t("save_figure_settings_and_code"), style = "margin: 0"),
                class = "widget_icon",
                style = "border-right: solid grey 0.5px;"
            )
        ),
        div(
            id = ns("figure_div_%widget_id%"),
            %import_script('ui_figure.R')%,
            style = "height: 100%;"
        ),
        shinyjs::hidden(
            div(
                id = ns("figure_settings_div_%widget_id%"),
                %import_script('ui_figure_settings.R')%,
                style = "height: 100%; margin: 5px 10px;"
            )
        ),
        shinyjs::hidden(
            div(
                id = ns("code_div_%widget_id%"),
                shinyAce::aceEditor(
                    ns("code_%widget_id%"), value = "", mode = "r",
                    autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
                ),
                style = "height: 100%; width: 100%;"
            )
        ),
        style = "display: flex; height: calc(100% - 40px);"
    ),
    shinyjs::hidden(
        div(
            id = ns("general_settings_div_%widget_id%"),
            div(
                shiny.fluent::Toggle.shinyInput(ns("figure_and_settings_side_by_side_%widget_id%"), value = FALSE),
                tags$label(i18np$t("figure_and_settings_side_by_side"), `for` = ns("figure_and_settings_side_by_side_%widget_id%"), style = "margin-left: 5px;"),
                style = "display: flex;" 
            ),
            style = "margin-top: 5px; height: calc(100% - 45px);"
        )
    )
)
