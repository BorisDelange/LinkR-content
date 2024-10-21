# UI - Figure page
#
# Insert the UI components for the figure in this section.

div(
    uiOutput(ns("figure_message_%widget_id%")),
    dygraphs::dygraphOutput(ns("dygraph_%widget_id%"), height = "100%", width = "100%"),
    style = "height: 100%; width: 100%;"
)
