# UI - Figure settings
#
# Insert the UI components for configuring the figure settings in this section.
div(
    div(
        id = ns("figure_settings_tabs_%widget_id%"),
        tags$button(id = ns("select_notes_%widget_id%"), i18np$t("notes"), class = "widget_pivot_item selected_widget_pivot_item", onclick = figure_settings_tab_item_js),
        tags$button(id = ns("keyword_search_%widget_id%"), i18np$t("keyword_search"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
        tags$button(id = ns("chatbot_%widget_id%"), i18np$t("chatbot"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
        tags$button(id = ns("layout_%widget_id%"), i18np$t("layout"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
        class = "pivot"
    ),
    
    # Notes DataTable
    
    div(
        id = ns("select_notes_div_%widget_id%"),
        div(
            id = ns("error_message_div_%widget_id%"),
            div(shiny.fluent::MessageBar(i18np$t("select_a_patient"), messageBarType = 5), style = "display: inline-block;")
        ),
        DT::DTOutput(ns("notes_datatable_%widget_id%")), 
        style = "margin-top: 15px;"
    ),
    
    # Layout
    
    shinyjs::hidden(
        div(
            id = ns("layout_div_%widget_id%"),
            div(
                shiny.fluent::Toggle.shinyInput(ns("display_raw_text_%widget_id%"), value = FALSE),
                tags$label(i18np$t("display_raw_text"), `for` = ns("display_raw_text_%widget_id%"), style = "margin-left: 5px;"),
                style = "display: flex; margin-top: 15px;" 
            ),
            style = "height: 100%;"
        )
    ),
    
    # Keyword search
    
    shinyjs::hidden(
        div(
            id = ns("keyword_search_div_%widget_id%"),
            div(
                div(
                    div(shiny.fluent::Dropdown.shinyInput(ns("search_word_sets_%widget_id%"), label = i18np$t("search_word_sets"), multiSelect = TRUE), style = "width: 200px;"),
                    div(shiny.fluent::Toggle.shinyInput(ns("filter_notes_with_matches_%widget_id%"), label = i18np$t("filter_notes_with_matches")), style = "margin-top: 29px;"),
                    style = "display: flex; gap: 10px;"
                ),
                tags$hr(style = "border: none; border-top: 1px solid #ccc; height: 1px; margin: 20px 0 10px 0;"),
                div(
                    div(shiny.fluent::TextField.shinyInput(ns("word_set_name_%widget_id%"), label = i18np$t("create_word_set")), style = "width: 200px;"),
                    div(
                        shiny.fluent::IconButton.shinyInput(ns("create_word_set_%widget_id%"), iconProps = list(iconName = "Add")),
                        style = "margin-top: 26px;", class = "widget_icon"
                    ),
                    style = "display: flex; gap: 5px;"
                ),
                tags$hr(style = "border: none; border-top: 1px solid #ccc; height: 1px; margin: 20px 0 10px 0;"),
                div(
                    div(shiny.fluent::Dropdown.shinyInput(ns("edit_word_set_%widget_id%"), label = i18np$t("edit_word_set")), style = "width: 200px;"),
                    shinyjs::hidden(
                        div(
                            id = ns("delete_word_set_div_%widget_id%"),
                            shiny.fluent::IconButton.shinyInput(ns("delete_word_set_%widget_id%"), iconProps = list(iconName = "Delete")),
                            style = "margin-top: 26px;", class = "widget_icon"
                        )
                    ),
                    style = "display: flex; gap: 5px;"
                ),
                shinyjs::hidden(
                    div(
                        id = ns("edit_word_set_details_div_%widget_id%"),
                        div(
                            div(shiny.fluent::TextField.shinyInput(ns("word_name_%widget_id%"), label = i18np$t("add_new_word")), style = "width: 200px;"),
                            div(
                                shiny.fluent::IconButton.shinyInput(ns("add_new_word_%widget_id%"), iconProps = list(iconName = "Add")),
                                style = "margin-top: 26px;", class = "widget_icon"
                            ),
                            style = "display: flex; gap: 5px;"
                        ),
                        uiOutput(ns("words_list_%widget_id%"), style = "margin-top: 15px; display: flex; flex-wrap: wrap; gap: 5px;")
                    )
                ),
                style = "height: 100%; margin-top: 10px;"
            )
        )
    ),
    
    # Delete word set modal
    
    shinyjs::hidden(
      div(
        id = ns("delete_word_set_modal_%widget_id%"),
        div(
            tags$h1(i18np$t("delete_word_set_title"), style = "font-size: 14px;"),
            tags$p(i18np$t("delete_word_set_text")),
            div(
                shiny.fluent::DefaultButton.shinyInput(ns("close_word_set_deletion_modal_%widget_id%"), i18np$t("dont_delete")),
                div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_word_set_deletion_%widget_id%"), i18np$t("delete")), class = "delete_button"),
                style = "position: absolute; right: 10px; bottom: 8px; display: flex; gap: 5px;"
            ),
            style = "background: #fff; padding: 5px 10px 10px 15px; position: relative; width: 400px; height: 120px;"
        ),
        style = "display: flex; align-items: center; justify-content: center; position: absolute; left: 0; top: 0;  width: 100%; height: 100%; background-color: rgba(0,0,0,0.2); z-index: 1000;"
      )
    ),
    
    # Chatbot
    
    shinyjs::hidden(
        if(requireNamespace("ellmer", quietly = TRUE) && requireNamespace("ollamar", quietly = TRUE)){
            div(
                id = ns("chatbot_div_%widget_id%"),
                div(
                    div(
                        shiny.fluent::Dropdown.shinyInput(ns("llm_provider_%widget_id%"), options = list(
                                list(key = "ollama", text = "Ollama")
                            ),
                            label = i18np$t("llm_provider")
                        ),
                        style = "width: 150px;"
                    ),
                    div(
                        shiny.fluent::Dropdown.shinyInput(ns("llm_model_%widget_id%"), label = i18np$t("llm_model")),
                        style = "width: 150px;"
                    ),
                    div(
                        shiny.fluent::Dropdown.shinyInput(ns("include_notes_%widget_id%"), label = i18np$t("include_notes"), options = list(
                            list(key = "none", text = i18np$t("none")),
                            list(key = "selected_note", text = i18np$t("selected_note")),
                            list(key = "all_notes", text = i18np$t("all_notes"))
                        ), value = "none"),
                        style = "width: 150px;"
                    ),
                    style = "display: flex; gap: 10px; margin-top: 15px;" 
                ),
                div(
                    id = ns("chat_container_%widget_id%"),
                    div(
                        id = ns("chat_messages_%widget_id%"),
                        uiOutput(ns("chat_ui_%widget_id%")),
                        style = "flex-grow: 1; overflow-y: auto;"
                    ),
                    div(
                        id = ns("chat_input_%widget_id%"),
                        textAreaInput(ns("user_input_%widget_id%"), "", width = "calc(100% - 8px)", resize = "vertical"),
                        div(
                            shiny.fluent::DefaultButton.shinyInput(ns("clear_chat_%widget_id%"), i18np$t("clear_chat"), iconProps = list(iconName = "Refresh")), 
                            shiny.fluent::PrimaryButton.shinyInput(ns("send_message_%widget_id%"), i18np$t("send"), iconProps = list(iconName = "Play")), 
                            style = "margin-top: 10px; display: flex; justify-content: flex-end; gap: 5px;"
                        ),
                        style = "width: 100%; margin-top: auto; padding-bottom: 2px;"
                    ),
                    tags$script(HTML(paste0("
                        $(document).ready(function() {
                            $('#", ns("user_input_%widget_id%"), "').keydown(function(event) {
                                if (event.keyCode === 13 && !event.shiftKey) {
                                    event.preventDefault();
                                    
                                    setTimeout(function() {
                                        $('#", ns("send_message_%widget_id%"), "').click();
                                    }, 500);
                                    
                                    return false;
                                }
                                return true;
                            });
                        });
                    "))),
                    style = "display: flex; flex-direction: column; height: calc(100% - 70px); width: 100%;"
                ),
                style = "display: flex; flex-direction: column; height: calc(100% - 25px); width: 100%;"
            )
        } else {
            div(
                id = ns("chatbot_div_%widget_id%"),
                div(shiny.fluent::MessageBar(i18np$t("packages_ellmer_ollamar_needed"), messageBarType = 5), style = "display: inline-block; margin-top: 10px;")
            )
        }
    ),
    
    style = "height: 100%; display: flex; flex-direction: column;"
)
