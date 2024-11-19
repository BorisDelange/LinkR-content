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
    # Div containing the figure output
    style = "width: 100%; height: 100%;"
)
