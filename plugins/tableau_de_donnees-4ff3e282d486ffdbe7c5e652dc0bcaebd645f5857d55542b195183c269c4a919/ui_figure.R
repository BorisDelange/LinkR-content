# UI - Figure page
#
# Insert the UI components for the figure in this section.

div(
    uiOutput(ns("figure_message_%widget_id%")),
    DT::DTOutput(ns("datatable_%widget_id%")),
    style = "width: 100%; height: 100%; margin-top: 10px; overflow: auto;"
)
