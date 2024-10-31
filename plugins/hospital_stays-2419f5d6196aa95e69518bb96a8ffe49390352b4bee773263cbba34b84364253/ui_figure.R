# UI - Figure page
#
# Insert the UI components for the figure in this section.

div(
    plotOutput(ns("stays_plot_%widget_id%"), width = "100%", height = "100%"),
    style = "width: 100%; height: 100%;"
)
