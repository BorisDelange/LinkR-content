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
    div(dygraphs::dygraphOutput(ns("dygraph_%widget_id%"), height = "100%", width = "100%"), style = "width: 100%; height: calc(100% - 10px); padding-top: 10px;"),
    style = "height: 100%; width: 100%; box-sizing: border-box; padding-right: 5px;"
)
