div(
    id = ns("figure_settings_div_%widget_id%"),
    div(
        id = ns("figure_settings_tabs_%widget_id%"),
        tags$button(id = ns("select_notes_%widget_id%"), i18np$t("notes"), class = "widget_pivot_item selected_widget_pivot_item", onclick = figure_settings_tab_item_js),
        tags$button(id = ns("filters_%widget_id%"), i18np$t("filters"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
        tags$button(id = ns("words_sets_%widget_id%"), i18np$t("words_sets"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
        tags$button(id = ns("layout_%widget_id%"), i18np$t("layout"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
        class = "pivot"
    ),
    
    # Add a words set modal
    shinyjs::hidden(
      div(
        id = ns("add_words_set_modal_%widget_id%"),
        div(
            div(
                tags$h1(i18np$t("add_a_words_set"), style = "font-size: 14px;"),
                shiny.fluent::IconButton.shinyInput(ns("close_add_words_set_modal_%widget_id%"), iconProps = list(iconName = "ChromeClose")),
                style = "display: flex; justify-content: space-between;",
                class = "small_close_button"
            ),
            div(shiny.fluent::TextField.shinyInput(ns("new_words_set_name_%widget_id%"), label = i18np$t("name")), style = "width: 200px;"),
            div(
                shiny.fluent::PrimaryButton.shinyInput(ns("add_words_set_%widget_id%"), i18np$t("add")),
                style = "position: absolute; right: 10px; bottom: 8px;"
            ),
            style = "background: #fff; padding: 5px 10px 10px 15px; position: relative; width: 400px; height: 120px;"
        ),
        style = "display: flex; align-items: center; justify-content: center; position: absolute; left: 0; top: 0;  width: 100%; height: 100%; background-color: rgba(0,0,0,0.2); z-index: 1000;"
      )
    ),
    
    # Delete a words set modal
    shinyjs::hidden(
      div(
        id = ns("delete_words_set_modal_%widget_id%"),
        div(
            tags$h1(i18np$t("delete_a_words_set_title"), style = "font-size: 14px;"),
            tags$p(i18np$t("delete_a_words_set_text")),
            div(
                shiny.fluent::DefaultButton.shinyInput(ns("close_delete_words_set_modal_%widget_id%"), i18np$t("dont_delete")),
                div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_words_set_deletion_%widget_id%"), i18np$t("delete")), class = "delete_button"),
                style = "display: flex; gap: 5px; position: absolute; right: 10px; bottom: 8px;"
            ),
            style = "background: #fff; padding: 5px 10px 10px 15px; position: relative; width: 400px; height: 120px;"
        ),
        style = "display: flex; align-items: center; justify-content: center; position: absolute; left: 0; top: 0;  width: 100%; height: 100%; background-color: rgba(0,0,0,0.2); z-index: 1000;"
      )
    ),
    
    # Notes
    div(
        id = ns("select_notes_div_%widget_id%"),
        DT::DTOutput(ns("notes_datatable_%widget_id%")), 
        style = "margin-top: 15px;"
    ),
    
    # Filters
    shinyjs::hidden(
        div(
            id = ns("filters_div_%widget_id%"),
            div(
                shiny.fluent::Dropdown.shinyInput(
                    ns("filters_category_%widget_id%"), label = i18np$t("category"),
                    options = list(list(key = "words_set", text = i18np$t("words_set"))),
                    value = "words_set"
                ),
                style = "width: 200px; margin-top: 15px;"
            ),
            div(
                div(
                    shiny.fluent::Dropdown.shinyInput(ns("filters_words_set_%widget_id%"), label = i18np$t("words_set"), options = words_sets_options),
                    style = "width: 200px;"
                ),
                shinyjs::hidden(
                    div(
                        id = ns("filters_add_words_set_div_%widget_id%"),
                        create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("filters_add_words_set_%widget_id%"), iconProps = list(iconName = "Add")), text = i18np$t("add")),
                        class = "small_icon_button", style = "margin-top: 27px;"
                    )
                ),
                style = "display: flex; gap: 5px;"
            ),
            uiOutput(ns("filters_ui_%widget_id%"), style = "display: flex; flex-wrap: wrap; margin-top: 15px;"),
        )
    ),
    
    # Words sets
    shinyjs::hidden(
        div(
            id = ns("words_sets_div_%widget_id%"),
            div(
                id = ns("edit_words_sets_div_%widget_id%"),
                div(
                    shiny.fluent::Dropdown.shinyInput(ns("words_set_%widget_id%"), label = i18np$t("words_set"), options = words_sets_options),
                    style = "width: 200px;"
                ),
                create_hover_card(
                    ui = div(shiny.fluent::IconButton.shinyInput(ns("new_words_set_%widget_id%"), iconProps = list(iconName = "Add")), class = "small_icon_button", style = "margin: 27px 0 0 5px;"),
                    text = i18np$t("add_new_words_set")
                ),
                shinyjs::hidden(
                    div(
                        id = ns("delete_words_set_div_%widget_id%"),
                        create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("delete_words_set_%widget_id%"), iconProps = list(iconName = "Delete")), text = i18np$t("delete_this_words_set")),
                        class = "small_icon_button", style = "margin-top: 27px;"
                    )
                ),
                style = "display: flex;"
            ),
            shinyjs::hidden(
                div(
                    id = ns("words_set_details_div_%widget_id%"),
                    div(
                        div(
                            shiny.fluent::TextField.shinyInput(ns("new_word_%widget_id%"), label = i18np$t("new_word")),
                            style = "width: 200px;"
                        ),
                        create_hover_card(
                            ui = div(shiny.fluent::IconButton.shinyInput(ns("add_new_word_%widget_id%"), iconProps = list(iconName = "Add")), class = "small_icon_button", style = "margin-top: 27px;"),
                            text = i18np$t("add_new_word")
                        ),
                        style = "display: flex; gap: 5px;"
                    ),
                    uiOutput(ns("words_ui_%widget_id%"), style = "display: flex; flex-wrap: wrap; margin-top: 15px;")
                )
            ),
            style = "margin-top: 15px;"
        )
    ),
    
    # Layout
    shinyjs::hidden(
        div(
            id = ns("layout_div_%widget_id%"),
            div(
                shiny.fluent::Toggle.shinyInput(ns("remove_multiple_line_breaks_%widget_id%"), value = TRUE),
                tags$label(i18np$t("remove_multiple_line_breaks"), `for` = ns("remove_multiple_line_breaks_%widget_id%"), style = "margin-left: 5px;"),
                style = "display: flex; margin-top: 15px;" 
            ),
        )
    ),
    style = "width: 50%; height: 100%; margin-left: 10px; overflow: auto;"
)
