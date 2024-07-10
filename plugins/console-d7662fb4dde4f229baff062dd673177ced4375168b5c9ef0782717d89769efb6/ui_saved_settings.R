tagList(

    # Add settings file modal
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
    
    # Delete settings file modal
    shinyjs::hidden(
      div(
        id = ns("delete_settings_file_modal_%widget_id%"),
        div(
            tags$h1(i18np$t("delete_settings_file_title"), style = "font-size: 14px;"),
            tags$p(i18np$t("delete_settings_file_text")),
            div(
                shiny.fluent::DefaultButton.shinyInput(ns("close_file_deletion_modal_%widget_id%"), i18np$t("dont_delete")),
                div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_file_deletion_%widget_id%"), i18np$t("delete")), class = "delete_button"),
                style = "position: absolute; right: 10px; bottom: 3px; display: flex; gap: 5px;"
            ),
            style = "background: #fff; padding: 5px 10px 10px 15px; position: relative; width: 400px; height: 120px;"
        ),
        style = "display: flex; align-items: center; justify-content: center; position: absolute; left: 0; top: 0;  width: 100%; height: 100%; background-color: rgba(0,0,0,0.2); z-index: 1000;"
      )
    ),
    
    # Sidenav
    div(
        id = ns("saved_settings_sidenav_%widget_id%"),
        shiny.fluent::IconButton.shinyInput(ns("create_settings_file_%widget_id%"), iconProps = list(iconName = "Add"), title = i18np$t("create_settings_file"), style = "margin: 0"),
        class = "widget_icon",
        style = "border-right: solid grey 0.5px;"
    ),
    
    # Dropdown and delete button
    div(
        div(shiny.fluent::Dropdown.shinyInput(ns("saved_settings_%widget_id%"), label = i18np$t("file"), options = dropdown_options, value = selected_file), style = "width: 200px"),
        shinyjs::hidden(
            div(
                id = ns("delete_saved_settings_file_div_%widget_id%"),
                shiny.fluent::PrimaryButton.shinyInput(ns("delete_saved_settings_file_%widget_id%"), i18np$t("delete")),
                class = "delete_button"
            )
        ),
        style = "margin: 5px 10px;"
    )
)
