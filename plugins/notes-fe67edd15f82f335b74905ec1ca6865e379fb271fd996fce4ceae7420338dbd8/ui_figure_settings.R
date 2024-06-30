div(
    id = ns("figure_settings_div_%widget_id%"),
    div(
        id = ns("figure_settings_tabs_%widget_id%"),
        tags$button(id = ns("select_notes_%widget_id%"), i18np$t("notes"), class = "widget_pivot_item selected_widget_pivot_item", onclick = figure_settings_tab_item_js),
        tags$button(id = ns("words_sets_%widget_id%"), i18np$t("words_sets"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
        tags$button(id = ns("layout_%widget_id%"), i18np$t("layout"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
        class = "pivot"
    ),
    div(
        id = ns("select_notes_div_%widget_id%"),
        DT::DTOutput(ns("notes_datatable_%widget_id%")), 
        style = "margin-top: 15px;"
    ),
    shinyjs::hidden(
        div(
            id = ns("words_sets_div_%widget_id%"),
            div(
                id = ns("edit_words_sets_div_%widget_id%"),
                div(
                    shiny.fluent::Dropdown.shinyInput(ns("words_set_%widget_id%"), label = i18np$t("words_set")),
                    style = "width: 200px;"
                ),
                div(
                    shiny.fluent::IconButton.shinyInput(ns("new_words_set_%widget_id%"), iconProps = list(iconName = "Add")),
                    class = "small_icon_button",
                    style = "margin-top: 17px;"
                ),
                style = "display: flex; gap: 5px;"
            ),
            shinyjs::hidden(
                div(
                    id = ns("new_words_set_div_%widget_id%"),
                    div(
                        shiny.fluent::TextField.shinyInput(ns("new_words_set_name_%widget_id%"), label = i18np$t("words_set")),
                        style = "width: 200px;"
                    ),
                    div(
                        shiny.fluent::DefaultButton.shinyInput(ns("cancel_new_words_set_%widget_id%"), i18np$t("cancel")),
                        shiny.fluent::PrimaryButton.shinyInput(ns("add_new_words_set_%widget_id%"), i18np$t("add")),
                        style = "display: flex; gap: 5px;"
                    )
                )
            ),
            style = "margin-top: 15px;"
        )
    ),
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
    style = "width: 50%; height: 100%; margin-left: 10px;"
)
