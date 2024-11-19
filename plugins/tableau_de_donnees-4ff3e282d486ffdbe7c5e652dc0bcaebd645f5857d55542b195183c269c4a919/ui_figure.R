# UI - Figure page
#
# Insert the UI components for the figure in this section.

div(
    shinyjs::hidden(
        div(
            id = ns("error_message_div_%widget_id%"),
            uiOutput(ns("error_message_%widget_id%")),
            style = "padding-top: 10px;"
        )
    ),
    div(DT::DTOutput(ns("datatable_%widget_id%")), style = "padding: 0 5px;"),
    style = "width: 100%; height: 100%; overflow: auto; box-sizing: border-box;"
)
