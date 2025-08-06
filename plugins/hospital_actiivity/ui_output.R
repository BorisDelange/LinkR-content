# ==========================================
# ui_output.R - Output Display Interface
# ==========================================

div(
    # DYNAMIC HTML OUTPUT (uiOutput)
    # Uncomment for dynamic content, reports, or conditional UI
    div(
        id = ns("ui_output_div_%widget_id%"),
        uiOutput(ns("ui_output_%widget_id%")),
        style = "width: 100%; padding: 10px; box-sizing: border-box;"
    ),
    
    # CONSOLE-STYLE TEXT OUTPUT (verbatimTextOutput)
    # Uncomment for statistical results, code output, or log messages
    shinyjs::hidden(
        div(
            id = ns("console_output_div_%widget_id%"),
            verbatimTextOutput(ns("console_output_%widget_id%")),
            style = "width: 100%; padding: 10px; box-sizing: border-box; font-family: monospace;"
        )
    ),
    
    # ====================
    # CONTAINER STYLING
    # ====================
    # Main container with scrollable overflow for large content
    style = "width: 100%; padding: 5px; box-sizing: border-box; height: 100%; overflow-y: auto;"
)
