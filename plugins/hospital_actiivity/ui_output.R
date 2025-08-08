# ==========================================
# ui_output.R - Output Display Interface
# ==========================================

div(
    div(
        id = ns("ui_output_div_%widget_id%"),
        uiOutput(ns("ui_output_%widget_id%"), style = "height: 100%;"),
        style = "width: 100%; height: 100%; padding: 10px; box-sizing: border-box;"
    ),
    
    shinyjs::hidden(
        div(
            id = ns("console_output_div_%widget_id%"),
            verbatimTextOutput(ns("console_output_%widget_id%")),
            style = "width: 100%; padding: 10px; box-sizing: border-box; font-family: monospace;"
        )
    ),
    
    style = "width: 100%; padding: 5px; box-sizing: border-box; height: 100%;"
)
