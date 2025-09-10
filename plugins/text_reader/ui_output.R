# UI - Output display

div(
    div(
        id = ns("figure_div_%widget_id%"),
        uiOutput(ns("notes_%widget_id%"), style = "height: calc(100% - 15px);"),
        style = "height: 100%;"
    ),
    style = "width: 100%; padding: 5px; box-sizing: border-box; height: 100%; overflow-y: auto;"
)