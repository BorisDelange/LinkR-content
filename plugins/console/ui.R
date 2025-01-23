# UI - main file

if ("projects_console_access" %in% user_accesses){
    code_button <- shiny.fluent::IconButton.shinyInput(ns("code_button_%widget_id%"), iconProps = list(iconName = "Code"), title = i18np$t("show_code_editor"))
} else code_button <- ""

if ("projects_widgets_settings" %in% user_accesses){
    save_figure_settings_buttons <- shiny.fluent::IconButton.shinyInput(ns("save_params_and_code_%widget_id%"), iconProps = list(iconName = "Save"), title = i18np$t("save_figure_settings_and_code"), style = "margin: 0")
    save_general_settings_button <- shiny.fluent::IconButton.shinyInput(ns("save_general_settings_button_%widget_id%"), iconProps = list(iconName = "Save"), title = i18np$t("save_general_settings"), style = "margin: 0")
} else {
    save_figure_settings_buttons <- ""
    save_general_settings_button <- ""   
}

%import_script('ui_load_general_settings.R')%

tagList(
    div(
        shinyjs::hidden(
            div(
                id = ns("figure_button_div_%widget_id%"),
                shiny.fluent::IconButton.shinyInput(ns("figure_button_%widget_id%"), iconProps = list(iconName = "BarChart4"), title = i18np$t("show_figure"))
            )
        ),
        shiny.fluent::IconButton.shinyInput(ns("figure_settings_button_%widget_id%"), iconProps = list(iconName = "AllApps"), title = i18np$t("show_figure_settings")),
        code_button,
        shiny.fluent::IconButton.shinyInput(ns("general_settings_button_%widget_id%"), iconProps = list(iconName = "Settings"), title = i18np$t("show_general_settings")),
        uiOutput(
            ns("settings_files_ui_%widget_id%"),
            onclick = paste0("Shiny.setInputValue('", id, "-show_settings_files_tab_%widget_id%', Math.random())")
        ),
        class = "widget_icon",
        style = "display: flex; color: #808080; border-bottom: solid grey 0.5px; height: 28px; padding-top: 5px; font-size: 12px;"
    ),
    div(
        id = ns("figure_settings_code_div_%widget_id%"),
        div(
            id = ns("figure_settings_code_sidenav_%widget_id%"),
            shiny.fluent::IconButton.shinyInput(ns("display_figure_%widget_id%"), iconProps = list(iconName = "Play"), title = i18np$t("display_figure"), style = "margin: 0"),
            save_figure_settings_buttons,
            class = "widget_icon",
            style = "border-right: solid grey 0.5px; width: 25px;"
        ),
        div(
            id = ns("figure_div_%widget_id%"),
            %import_script('ui_figure.R')%,
            style = paste0("height: 100%; flex-basis: ", div_width, "; margin: 0 10px; flex: 1; box-sizing: border-box; min-width: 50px;"),
            class = "left-panel"
        ),
        div(
            id = ns("resizer_%widget_id%"),
            style = "width: 5px; cursor: col-resize; background-color: #ccc;",
            class = "resizer"
        ),
        div(
            id = ns("figure_settings_div_%widget_id%"),
            %import_script('ui_figure_settings.R')%,
            style = paste0("height: 100%; flex-basis: ", div_width, "%; margin: 5px 10px; overflow: auto; flex: 1; box-sizing: border-box;")
        ),
        shinyjs::hidden(
            div(
                id = ns("code_div_%widget_id%"),
                %import_script('ui_code.R')%,
                style = paste0("height: 100%; flex-basis: ", div_width, "%; overflow: auto; flex: 1; box-sizing: border-box;"),
                class = "right-panel"
            )
        ),
        style = "display: flex; height: calc(100% - 40px);"
    ),
    shinyjs::hidden(
        div(
            id = ns("general_settings_div_%widget_id%"),
            div(
                id = ns("general_settings_sidenav_%widget_id%"),
                save_general_settings_button,
                class = "widget_icon",
                style = "border-right: solid grey 0.5px; width: 25px;"
            ),
            %import_script('ui_general_settings.R')%,
            style = "display: flex; height: calc(100% - 40px);"
        )
    ),
    shinyjs::hidden(
        div(
            id = ns("settings_files_div_%widget_id%"),
            %import_script('ui_settings_files.R')%,
            style = "display: flex; height: calc(100% - 40px);"
        )
    )
)
