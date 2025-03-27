# UI - Figure settings
#
# Insert the UI components for configuring the figure settings in this section.

div(
    div(
        id = ns("figure_settings_tabs_%widget_id%"),
        tags$button(id = ns("select_notes_%widget_id%"), i18np$t("notes"), class = "widget_pivot_item selected_widget_pivot_item", onclick = figure_settings_tab_item_js),
        tags$button(id = ns("filters_%widget_id%"), i18np$t("filters"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
        tags$button(id = ns("layout_%widget_id%"), i18np$t("layout"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
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
    )
)
