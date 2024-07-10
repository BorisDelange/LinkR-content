tagList(
    shinyjs::hidden(
      div(
        id = ns("add_settings_file_modal_%widget_id%"),
        div(
            div(
                tags$h1(i18np$t("create_settings_file"), style = "font-size: 14px;"),
                shiny.fluent::IconButton.shinyInput(ns("close_add_settings_file_modal_%widget_id%"), iconProps = list(iconName = "ChromeClose")),
                style = "display: flex; justify-content: space-between;",
                class = "small_close_button"
            ),
            div(shiny.fluent::TextField.shinyInput(ns("settings_file_name_%widget_id%"), label = i18np$t("name")), style = "width: 200px;"),
            div(
                shiny.fluent::PrimaryButton.shinyInput(ns("add_settings_file_%widget_id%"), i18np$t("add")),
                style = "position: absolute; right: 10px; bottom: 3px;"
              ),
            style = "background: #fff; padding: 5px 10px 10px 15px; position: relative; width: 400px; height: 120px;"
        ),
        style = "display: flex; align-items: center; justify-content: center; position: absolute; left: 0; top: 0;  width: 100%; height: 100%; background-color: rgba(0,0,0,0.2); z-index: 1000;"
      )
    ),
    div(
        id = ns("saved_settings_sidenav_%widget_id%"),
        shiny.fluent::IconButton.shinyInput(ns("create_settings_file_%widget_id%"), iconProps = list(iconName = "Add"), title = i18np$t("create_settings_file"), style = "margin: 0"),
        class = "widget_icon",
        style = "border-right: solid grey 0.5px;"
    ),
    div(
        div(shiny.fluent::Dropdown.shinyInput(ns("saved_settings_%widget_id%"), label = i18np$t("file")), style = "width: 150px"),
        style = "margin: 5px 10px;"
    )
)
