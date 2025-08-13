# ==========================================
# ui_output.R - Output Display Interface
# ==========================================

div(
    # ====================
    # MAIN OUTPUT CONTAINER
    # ====================
    # Container for standard UI outputs (cards, tables, etc.)
    div(
        id = ns("ui_output_div_%widget_id%"),
        uiOutput(ns("ui_output_%widget_id%"), style = "height: 100%;"),
        style = "width: 100%; height: 100%; padding: 10px; box-sizing: border-box;"
    ),
    
    # ====================
    # PLOT OUTPUT CONTAINER
    # ====================
    # Container for Plotly visualizations (timelines, charts, etc.)
    shinyjs::hidden(
        div(
            id = ns("plotly_output_div_%widget_id%"),
            plotly::plotlyOutput(
                ns("plotly_output_%widget_id%"), 
                height = "400px",
                width = "100%"
            ),
            style = "width: 100%; padding: 10px; box-sizing: border-box;"
        )
    ),
    
    # ====================
    # CONSOLE OUTPUT CONTAINER
    # ====================
    # Container for debugging and console messages
    shinyjs::hidden(
        div(
            id = ns("console_output_div_%widget_id%"),
            verbatimTextOutput(ns("console_output_%widget_id%")),
            style = "width: 100%; padding: 10px; box-sizing: border-box; font-family: monospace;"
        )
    ),
    
    style = "width: 100%; padding: 5px; box-sizing: border-box; height: 100%;"
)
