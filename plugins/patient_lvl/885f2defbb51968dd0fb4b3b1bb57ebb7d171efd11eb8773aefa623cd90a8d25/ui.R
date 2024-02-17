# Get widget options
sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id%", .con = r$db)
widget_options <- DBI::dbGetQuery(m$db, sql)
notes <- widget_options %>% dplyr::filter(name == "script") %>% dplyr::select(id = value_num, name = value)
selected_script <- NULL
selected_script_result <- widget_options %>% dplyr::filter(name == "selected_script")
if (nrow(selected_script_result) > 0) if ((selected_script_result %>% dplyr::pull(value_num)) %in% notes$id) selected_script <- selected_script_result %>% dplyr::pull(value_num)

# notes_titles <- d$note %>% dplyr::distinct(note_title) %>% dplyr::collect() %>% dplyr::pull()

tagList(
    shiny.fluent::reactOutput(ns("delete_confirm_%widget_id%")),
    shiny.fluent::Pivot(
        id = ns("pivot_%widget_id%"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab_%widget_id%', item.props.id)")),
        shiny.fluent::PivotItem(id = "no_code_div_%widget_id%", itemKey = "notes", headerText = i18np$t("notes")),
        shiny.fluent::PivotItem(id = "scripts_management_div_%widget_id%", itemKey = "scripts_management", headerText = i18np$t("scripts_management"))
    ),
    div(
        id = ns("no_code_div_%widget_id%"),  br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            div(shiny.fluent::Dropdown.shinyInput(ns("script_choice_%widget_id%"),
                options = convert_tibble_to_list(notes, key_col = "id", text_col = "name"), value = selected_script), 
                style = "width:300px"),
            shiny.fluent::DefaultButton.shinyInput(ns("save_%widget_id%"), i18np$t("save")),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::Toggle.shinyInput(ns("hide_params_%widget_id%"), value = FALSE, style = "margin-top:5px;"),
                div(class = "toggle_title", i18np$t("hide_params"), style = "padding-top:5px;")
            )
        ),  br(),
        div(
            style = "display:flex;",
            div(
                id = ns("notes_div_%widget_id%"),
                style = "padding-right:10px; width:50%;",
                uiOutput(ns("notes_%widget_id%"))
            ),
            div(
                id = ns("params_div_%widget_id%"),
                style = "padding-left:10px; width:50%;",
                shiny.fluent::Pivot(
                    id = ns("params_pivot_%widget_id%"),
                    onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-params_current_tab_%widget_id%', item.props.id)")),
                    shiny.fluent::PivotItem(id = "notes_selection_div_%widget_id%", itemKey = "notes_selection", headerText = i18np$t("notes_selection")),
                    shiny.fluent::PivotItem(id = "word_div_%widget_id%", itemKey = "word_sets", headerText = i18np$t("word_sets")),
                    shiny.fluent::PivotItem(id = "sub_params_div_%widget_id%", itemKey = "sub_params", headerText = i18np$t("parameters"))
                ), br(),
                div(
                    id = ns("notes_selection_div_%widget_id%"),
                    div(
                        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                          div(shiny.fluent::Toggle.shinyInput(ns("show_all_notes_%widget_id%"), value = FALSE)),
                          div(i18np$t("show_all_notes"), style = "font-weight:bold; margin-bottom:5px;")
                        )
                    ),
                    DT::DTOutput(ns("notes_datatable_%widget_id%"))
                ),
                shinyjs::hidden(
                    div(
                        id = ns("word_sets_div_%widget_id%")
                    )
                ),
                shinyjs::hidden(
                    div(
                        id = ns("sub_params_div_%widget_id%"),
                        div(
                            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                              div(shiny.fluent::Toggle.shinyInput(ns("chronological_order_%widget_id%"), value = TRUE)),
                              div(i18np$t("chronological_order"), style = "font-weight:bold; margin-bottom:5px;")
                            )
                        ),
                        div(
                            shiny.fluent::Dropdown.shinyInput(ns("display_format_%widget_id%"), label = i18np$t("display_format"),
                                options = list(
                                    list(key = "html", text = i18np$t("html")),
                                    list(key = "raw", text = i18np$t("raw"))
                                ),
                                value = "html"
                            ),
                            style = "width:300px;"
                        )
                    )
                )
            )
        )
    ),
    shinyjs::hidden(
        div(
            id = ns("scripts_management_div_%widget_id%"), br(),
            div(
                id = ns("scripts_management_tab_%widget_id%"),
                shiny.fluent::Stack(
                    horizontal = TRUE, tokens = list(childrenGap = 10),
                    make_textfield(i18n = i18n, ns = ns, label = "name", id = "script_name_%widget_id%", width = "300px"),
                    div(shiny.fluent::PrimaryButton.shinyInput(ns("add_script_%widget_id%"), i18n$t("add")), style = "margin-top:38px;")
                ),
                DT::DTOutput(ns("scripts_management_datatable_%widget_id%")),
                shiny.fluent::Stack(
                    horizontal = TRUE, tokens = list(childrenGap = 10),
                    shiny.fluent::PrimaryButton.shinyInput(ns("save_scripts_%widget_id%"), i18n$t("save")),
                    shiny.fluent::DefaultButton.shinyInput(ns("delete_scripts_%widget_id%"), i18n$t("delete_selection"))
                )
            )
        )
    )
)
