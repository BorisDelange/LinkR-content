# ==========================================
# ui_figure.R - Figure Display Interface
# ==========================================

div(
    # ====================
    # ERROR MESSAGE PANEL
    # ====================
    # Hidden error message container (shown when errors occur)
    shinyjs::hidden(
        div(
            id = ns("error_message_div_%widget_id%"),
            uiOutput(ns("error_message_%widget_id%"))
        )
    ),
    
    # ====================
    # MAIN CHART DISPLAY
    # ====================
    # Interactive time series chart using dygraphs
    div(
        id = ns("dygraph_div_%widget_id%"),
        dygraphs::dygraphOutput(
            ns("dygraph_%widget_id%"), 
            height = "100%", 
            width = "100%"
        ),
        style = "width: 100%; height: calc(100% - 10px); padding-top: 10px; box-sizing: border-box;"
    ),
    
    # Container styling with scrollable overflow
    style = "width: 100%; padding: 5px; box-sizing: border-box; height: 100%; overflow-y: auto;"
)
