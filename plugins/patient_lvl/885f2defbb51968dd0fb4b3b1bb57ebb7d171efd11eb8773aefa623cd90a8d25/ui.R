# Get widget options
sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id%", .con = r$db)
widget_options <- DBI::dbGetQuery(m$db, sql)
notes <- widget_options %>% dplyr::filter(name == "script") %>% dplyr::select(id = value_num, name = value)
selected_script <- NULL
selected_script_result <- widget_options %>% dplyr::filter(name == "selected_script")
if (nrow(selected_script_result) > 0) if ((selected_script_result %>% dplyr::pull(value_num)) %in% notes$id) selected_script <- selected_script_result %>% dplyr::pull(value_num)

# Load word sets
word_sets <- widget_options %>% dplyr::filter(name == "word_set") %>% dplyr::select(id = value_num, name = value)
word_sets_filter <- tibble::tibble(id = 0L, name = i18np$t("none")) %>% dplyr::bind_rows(word_sets)

tagList(
    shiny.fluent::reactOutput(ns("delete_confirm_%widget_id%")),
    shinyjs::hidden(
        div(
            id = ns("delete_word_set_modal_%widget_id%"),
            div(
                tags$strong(i18np$t("word_set_deletion_title")), br(),
                p(i18np$t("word_set_deletion_text")), br(),
                div(
                    shiny.fluent::DefaultButton.shinyInput(ns("close_delete_word_set_modal_%widget_id%"), i18n$t("dont_delete")),
                    shiny.fluent::PrimaryButton.shinyInput(ns("confirm_word_set_deletion_%widget_id%"), i18n$t("delete")),
                    style = "display: flex; justify-content: flex-end; gap: 5px;"
                ),
                style = "background: #fff; padding: 10px 10px 10px 15px; width: 400px; height: 115px;"
            ),
            style = "display: flex; align-items: center; justify-content: center; position: fixed; z-index: 10; left: 0; top: 0; width: 100%; height: 100%; background-color: rgba(0,0,0,0.2)"
        )
    ),
    shinyjs::hidden(
        div(
            id = ns("delete_word_modal_%widget_id%"),
            div(
                tags$strong(i18np$t("word_deletion_title")), br(),
                p(i18np$t("word_deletion_text")), br(),
                div(
                    shiny.fluent::DefaultButton.shinyInput(ns("close_delete_word_modal_%widget_id%"), i18n$t("dont_delete")),
                    shiny.fluent::PrimaryButton.shinyInput(ns("confirm_word_deletion_%widget_id%"), i18n$t("delete")),
                    style = "display: flex; justify-content: flex-end; gap: 5px;"
                ),
                style = "background: #fff; padding: 10px 10px 10px 15px; width: 400px; height: 115px;"
            ),
            style = "display: flex; align-items: center; justify-content: center; position: fixed; z-index: 10; left: 0; top: 0; width: 100%; height: 100%; background-color: rgba(0,0,0,0.2)"
        )
    ),
#     shiny.fluent::Pivot(
#         id = ns("pivot_%widget_id%"),
#         onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab_%widget_id%', item.props.id)")),
#         shiny.fluent::PivotItem(id = "no_code_div_%widget_id%", itemKey = "notes", headerText = i18np$t("notes")),
#         shiny.fluent::PivotItem(id = "scripts_management_div_%widget_id%", itemKey = "scripts_management", headerText = i18np$t("scripts_management"))
#     ),
    div(
        id = ns("no_code_div_%widget_id%"),  br(),
#         shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
#             div(shiny.fluent::Dropdown.shinyInput(ns("script_choice_%widget_id%"),
#                 options = convert_tibble_to_list(notes, key_col = "id", text_col = "name"), value = selected_script), 
#                 style = "width:300px"),
#             shiny.fluent::DefaultButton.shinyInput(ns("save_%widget_id%"), i18np$t("save")),
#             shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
#                 shiny.fluent::Toggle.shinyInput(ns("hide_params_%widget_id%"), value = FALSE, style = "margin-top:5px;"),
#                 div(class = "toggle_title", i18np$t("hide_params"), style = "padding-top:5px;")
#             )
#         ),  br(),
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
                    shiny.fluent::PivotItem(id = "word_sets_div_%widget_id%", itemKey = "word_sets", headerText = i18np$t("word_sets")),
                    shiny.fluent::PivotItem(id = "sub_params_div_%widget_id%", itemKey = "sub_params", headerText = i18np$t("parameters"))
                ),
                div(
                    id = ns("notes_selection_div_%widget_id%"),
                    DT::DTOutput(ns("notes_datatable_%widget_id%"))
                ),
                shinyjs::hidden(
                    div(
                        id = ns("word_sets_div_%widget_id%"),
                        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                            div(
                                div(class = "input_title", i18np$t("new_word_set_name")),
                                div(shiny.fluent::TextField.shinyInput(ns("new_word_set_name_%widget_id%")), style = "width:300px;")
                            ),
                            div(shiny.fluent::PrimaryButton.shinyInput(ns("new_word_set_add_%widget_id%"), i18n$t("add")), style = "margin-top:39px")
                        ),
                        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                            div(
                                div(class = "input_title", i18np$t("word_set")),
                                div(shiny.fluent::Dropdown.shinyInput(ns("word_set_%widget_id%"), options = convert_tibble_to_list(word_sets, key_col = "id", text_col = "name")), style = "width:300px")
                            ),
                            div(shiny.fluent::DefaultButton.shinyInput(ns("delete_word_set_%widget_id%"), i18n$t("delete")), style = "margin-top:39px")
                        ),
                        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                            div(
                                div(class = "input_title", i18np$t("new_word_name")),
                                div(shiny.fluent::TextField.shinyInput(ns("new_word_name_%widget_id%")), style = "width:300px")
                            ),
                            div(shiny.fluent::PrimaryButton.shinyInput(ns("add_word_%widget_id%"), i18n$t("add")), style = "margin-top:39px;")
                        ),
                        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                            div(
                                div(class = "input_title", i18np$t("word")),
                                div(shiny.fluent::Dropdown.shinyInput(ns("word_%widget_id%")), style = "width:300px")
                            ),
                            div(shiny.fluent::DefaultButton.shinyInput(ns("delete_word_%widget_id%"), i18n$t("delete")), style = "margin-top:39px;")
                        )
                    )
                ),
                shinyjs::hidden(
                    div(
                        id = ns("sub_params_div_%widget_id%"), br(),
                        div(
                            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                              div(shiny.fluent::Toggle.shinyInput(ns("show_all_notes_%widget_id%"), value = FALSE)),
                              div(i18np$t("show_all_notes"), style = "font-weight:bold; margin-bottom:5px;")
                            ),
                            style = "width:300px;"
                        ),
                        div(
                            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                              div(shiny.fluent::Toggle.shinyInput(ns("chronological_order_%widget_id%"), value = TRUE)),
                              div(i18np$t("chronological_order"), style = "font-weight:bold; margin-bottom:5px;")
                            ),
                            style = "width:300px;"
                        ), br(),
                        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                            div(
                                div(class = "input_title", i18np$t("filter_on_word_set")),
                                div(shiny.fluent::Dropdown.shinyInput(ns("word_set_filter_%widget_id%"), options = convert_tibble_to_list(word_sets_filter, key_col = "id", text_col = "name"), value = 0L), style = "width:300px;")
                            ),
                            div(shiny.fluent::PrimaryButton.shinyInput(ns("run_filter_%widget_id%"), i18np$t("run_filter")), style = "margin-top:39px;")
                        ),
                        div(
                            div(class = "input_title", i18np$t("display_format")),
                            shiny.fluent::Dropdown.shinyInput(ns("display_format_%widget_id%"),
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
    )#,
#     shinyjs::hidden(
#         div(
#             id = ns("scripts_management_div_%widget_id%"), br(),
#             div(
#                 id = ns("scripts_management_tab_%widget_id%"),
#                 shiny.fluent::Stack(
#                     horizontal = TRUE, tokens = list(childrenGap = 10),
#                     make_textfield(i18n = i18n, ns = ns, label = "name", id = "script_name_%widget_id%", width = "300px"),
#                     div(shiny.fluent::PrimaryButton.shinyInput(ns("add_script_%widget_id%"), i18n$t("add")), style = "margin-top:38px;")
#                 ),
#                 DT::DTOutput(ns("scripts_management_datatable_%widget_id%")),
#                 shiny.fluent::Stack(
#                     horizontal = TRUE, tokens = list(childrenGap = 10),
#                     shiny.fluent::PrimaryButton.shinyInput(ns("save_scripts_%widget_id%"), i18n$t("save")),
#                     shiny.fluent::DefaultButton.shinyInput(ns("delete_scripts_%widget_id%"), i18n$t("delete_selection"))
#                 )
#             )
#         )
#     )
)
