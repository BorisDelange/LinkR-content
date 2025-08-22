# ==========================================
# ui_output.R - Timeline Output Display Interface
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 REQUIRES CUSTOMIZATION - PLUGIN IMPLEMENTATION  🔧                     ██
# ██                                                                            ██
# ██  This file MUST be customized for your specific plugin.                    ██
# ██  Follow the template structure and implement your logic.                   ██
# ██  See comments and examples for guidance.                                   ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# TIMELINE PLUGIN - OUTPUT DISPLAY UI FILE
# 
# This file defines the output display interface for the Timeline plugin.
# It provides specialized containers for displaying timeline visualizations
# using both dygraphs (for continuous data) and plotly (for event data).
# 
# TIMELINE-SPECIFIC FEATURES:
# - Hybrid display supporting both dygraphs and plotly charts
# - Error message handling for no-data scenarios
# - Timeline synchronization support with proper padding
# - Responsive layout that adapts to container size

div(
    # ====================
    # ERROR MESSAGE PANEL
    # ====================
    # Hidden error message container (shown when errors occur)
    shinyjs::hidden(
        div(
            id = ns("error_message_div_%widget_id%"),
            uiOutput(ns("error_message_%widget_id%"), style = "height: 100%;"),
            style = "height: 100%;"
        )
    ),
    
    # ====================
    # DYGRAPHS CHART DISPLAY
    # ====================
    # Interactive time series chart using dygraphs (for continuous data)
    div(
        id = ns("dygraph_div_%widget_id%"),
        dygraphs::dygraphOutput(
            ns("dygraph_%widget_id%"), 
            height = "100%", 
            width = "100%"
        ),
        style = "width: 100%; height: calc(100% - 10px); padding-top: 10px; box-sizing: border-box;"
    ),
    
    # ====================
    # PLOTLY CHART DISPLAY
    # ====================
    # Interactive timeline chart using plotly (for event data)
    shinyjs::hidden(
        div(
            id = ns("plotly_div_%widget_id%"),
            plotly::plotlyOutput(
                ns("plotly_%widget_id%"), 
                height = "100%", 
                width = "100%"
            ),
            style = "width: 100%; height: calc(100% - 10px); padding-top: 10px; box-sizing: border-box;"
        )
    ),
    
    # Container styling with scrollable overflow
    style = "width: 100%; padding: 5px; box-sizing: border-box; height: 100%; overflow-y: auto;"
)