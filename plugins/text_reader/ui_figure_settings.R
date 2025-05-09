# UI - Figure settings
#
# Insert the UI components for configuring the figure settings in this section.

div(
    div(
        id = ns("figure_settings_tabs_%widget_id%"),
        tags$button(id = ns("select_notes_%widget_id%"), i18np$t("notes"), class = "widget_pivot_item selected_widget_pivot_item", onclick = figure_settings_tab_item_js),
        tags$button(id = ns("filters_%widget_id%"), i18np$t("filters"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
        tags$button(id = ns("layout_%widget_id%"), i18np$t("layout"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
        tags$button(id = ns("chatbot_%widget_id%"), i18np$t("chatbot"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
        class = "pivot"
    ),
    
    # Notes DataTable
    
    div(
        id = ns("select_notes_div_%widget_id%"),
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
    
    # Chatbot
    
    shinyjs::hidden(
        div(
            id = ns("chatbot_div_%widget_id%"),
            div(
                div(
                    shiny.fluent::Dropdown.shinyInput(ns("llm_provider_%widget_id%"), options = list(
                            list(key = "ollama", text = "Ollama")
                        ),
                        value = "ollama", label = i18np$t("llm_provider")
                    ),
                    style = "width: 150px;"
                ),
                div(
                    shiny.fluent::Dropdown.shinyInput(ns("llm_model_%widget_id%"), label = i18np$t("llm_model")),
                    style = "width: 150px;"
                ),
                style = "display: flex; gap: 10px; margin-top: 15px;" 
            ),
            uiOutput(ns("chat_ui_%widget_id%")),
            div(
                div(textAreaInput(ns("user_input_%widget_id%"), "", width = "calc(100% - 8px)", resize = "vertical")),
                div(shiny.fluent::PrimaryButton.shinyInput(ns("send_message_%widget_id%"), "Submit"), style = "margin-top: 10px; display: flex; justify-content: flex-end;"),
                style = "position: absolute; bottom: 10px; width: calc(50% - 35px);"
            ),
            style = "height: 100%;"
        )
    ),
    
    style = "height: calc(100% - 45px);"
)
