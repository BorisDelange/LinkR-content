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
        shiny.fluent::IconButton.shinyInput(ns("code_button_%widget_id%"), iconProps = list(iconName = "Code"), title = i18np$t("show_code_editor")),
        shiny.fluent::IconButton.shinyInput(ns("general_settings_button_%widget_id%"), iconProps = list(iconName = "Settings"), title = i18np$t("show_general_settings")),
        uiOutput(
            ns("saved_settings_ui_%widget_id%"),
            onclick = paste0("Shiny.setInputValue('", id, "-show_saved_settings_tab_%widget_id%', Math.random())")
        ),
        class = "widget_icon",
        style = "display: flex; color: #808080; border-bottom: solid grey 0.5px; height: 28px; padding-top: 5px; font-size: 12px; color: #808080;"
    ),
    div(
        id = ns("figure_settings_code_div_%widget_id%"),
        div(
            id = ns("figure_settings_code_sidenav_%widget_id%"),
            shiny.fluent::IconButton.shinyInput(ns("display_figure_%widget_id%"), iconProps = list(iconName = "Play"), title = i18np$t("display_figure"), style = "margin: 0"),
            shiny.fluent::IconButton.shinyInput(ns("save_params_and_code_%widget_id%"), iconProps = list(iconName = "Save"), title = i18np$t("save_figure_settings_and_code"), style = "margin: 0"),
            class = "widget_icon",
            style = "border-right: solid grey 0.5px;"
        ),
        div(
            id = ns("figure_div_%widget_id%"),
            %import_script('ui_figure.R')%,
            style = paste0("height: 100%; width: ", div_width, "; margin: 5px 10px; overflow: auto;")
        ),
        shinyjs::hidden(
            div(
                id = ns("figure_settings_div_%widget_id%"),
                %import_script('ui_figure_settings.R')%,
                style = paste0("height: 100%; width: ", div_width, "%; margin: 5px 10px; overflow: auto;")
            )
        ),
        div(
            id = ns("code_div_%widget_id%"),
            shinyAce::aceEditor(
                ns("code_%widget_id%"), value = "", mode = "r",
                hotkeys = list(
                    save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                    run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                    comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
                ),
                autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
            ),
            style = paste0("height: 100%; width: ", div_width, "%; overflow: auto;")
        ),
        style = "display: flex; height: calc(100% - 40px);"
    ),
    shinyjs::hidden(
        div(
            id = ns("general_settings_div_%widget_id%"),
            div(
                id = ns("general_settings_sidenav_%widget_id%"),
                shiny.fluent::IconButton.shinyInput(ns("save_general_settings_button_%widget_id%"), iconProps = list(iconName = "Save"), title = i18np$t("save_general_settings"), style = "margin: 0"),
                class = "widget_icon",
                style = "border-right: solid grey 0.5px;"
            ),
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
                ), br(),
                tags$strong(i18np$t("code_execution")), br(),
                div(
                    shiny.fluent::Toggle.shinyInput(ns("run_code_at_settings_file_load_%widget_id%"), value = toggle_values$run_code_at_patient_update),
                    tags$label(i18np$t("run_code_at_settings_file_load"), `for` = ns("run_code_at_settings_file_load_%widget_id%"), style = "margin-left: 5px;"),
                    style = "display: flex; margin-top: 8px;" 
                ),
                div(
                    shiny.fluent::Toggle.shinyInput(ns("run_code_at_patient_update_%widget_id%"), value = toggle_values$run_code_at_patient_update),
                    tags$label(i18np$t("run_code_at_patient_update"), `for` = ns("run_code_at_patient_update_%widget_id%"), style = "margin-left: 5px;"),
                    style = "display: flex; margin-top: 5px;" 
                ),
                style = "margin: 5px 10px;"
            ),
            style = "display: flex; height: calc(100% - 40px);"
        )
    ),
    shinyjs::hidden(
        div(
            id = ns("saved_settings_div_%widget_id%"),
            %import_script('ui_saved_settings.R')%,
            style = "display: flex; height: calc(100% - 40px);"
        )
    )
)
