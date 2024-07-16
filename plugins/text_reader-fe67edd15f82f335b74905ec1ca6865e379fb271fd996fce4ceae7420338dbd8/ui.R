%import_script('ui_load_general_settings.R')%

# Load dropdown options
if (length(selected_file) > 0) link_id <- selected_file else link_id <- 0
sql <- glue::glue_sql("SELECT id, value FROM widgets_options WHERE widget_id = %widget_id% AND link_id = {link_id} AND category = 'words_set'", .con = m$db)
m$words_sets_%widget_id% <- DBI::dbGetQuery(m$db, sql)
words_sets_options <- m$words_sets_%widget_id% %>% convert_tibble_to_list(key_col = "id", text_col = "value")

figure_settings_tab_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_figure_settings_tab_%widget_id%', this.id);
    Shiny.setInputValue('", id, "-current_figure_settings_tab_trigger_%widget_id%', Math.random());"
)

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
            #shiny.fluent::IconButton.shinyInput(ns("save_params_and_code_%widget_id%"), iconProps = list(iconName = "Save"), title = i18np$t("save_figure_settings_and_code"), style = "margin: 0"),
            class = "widget_icon",
            style = "border-right: solid grey 0.5px;"
        ),
        div(
            id = ns("figure_div_%widget_id%"),
            uiOutput(ns("notes_%widget_id%")),
            style = "width: 50%; margin: 0 10px; overflow-y: auto;"
        ),
        %import_script('ui_figure_settings.R')%,
        shinyjs::hidden(
            div(
                id = ns("code_div_%widget_id%"),
                shinyAce::aceEditor(
                    ns("code_editor_%widget_id%"), value = "", mode = "r",
                    autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
                ),
                style = "width: 50%; height: calc(100% - 10px); margin-top: 10px;"
            )
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
