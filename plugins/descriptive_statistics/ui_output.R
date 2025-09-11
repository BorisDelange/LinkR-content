# ==========================================
# ui_output.R - Output Display Interface
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

# PLUGIN TEMPLATE - OUTPUT DISPLAY UI FILE
# 
# This file defines the output display interface for the widget plugin template.
# It provides a flexible container that can display various types of outputs depending
# on your plugin's functionality and requirements.
# 
# WHEN CREATING A NEW PLUGIN WITH THIS TEMPLATE:
# - Choose the appropriate output type(s) for your plugin's functionality
# - Remove unused output containers to keep the code clean
# - Customize styling and layout based on your specific needs
# - Consider using conditional display logic in server.R to show/hide different outputs
# 
# PLUGIN OUTPUT POSSIBILITIES:
# - Data Visualizations: plots, charts, graphs, maps
# - Statistical Analysis: summary tables, test results, model outputs
# - Data Tables: filtered/processed datasets, reports
# - Interactive Components: dynamic UI elements, dashboards
# - Text/HTML Content: formatted reports, documentation, alerts
# 
# COMMON OUTPUT TYPES AND USE CASES:
# 
# plotOutput() - Base R plots, ggplot2 charts
#   Use for: static plots, statistical charts, custom visualizations
# 
# plotly::plotlyOutput() - Interactive plotly charts  
#   Use for: interactive plots, dashboards, time series with zoom/pan
# 
# dygraphs::dygraphOutput() - Time series charts
#   Use for: financial data, sensor data, any time-based continuous data
# 
# DT::DTOutput() - Interactive data tables
#   Use for: displaying datasets, search/filter functionality, data exploration
# 
# leaflet::leafletOutput() - Interactive maps
#   Use for: geospatial data, location-based analysis, geographic visualizations
# 
# uiOutput()/htmlOutput() - Dynamic HTML content
#   Use for: formatted text, dynamic UI, conditional content, reports
# 
# verbatimTextOutput() - Console-style text output
#   Use for: code results, statistical summaries, log messages
# 
# tableOutput() - Simple HTML tables
#   Use for: basic tabular data, summary statistics, small datasets

div(
    # ====================
    # ERROR MESSAGE PANEL
    # ====================
    # Always include error handling - shows when plugin execution fails
    shinyjs::hidden(
        div(
            id = ns("error_message_div_%widget_id%"),
            uiOutput(ns("error_message_%widget_id%"))
        )
    ),
    
    # ====================
    # EXAMPLE OUTPUT CONTAINERS
    # ====================
    # Remove the outputs you don't need and customize the ones you keep
    
    # STATIC PLOT OUTPUT (Base R, ggplot2)
    # For static visualizations that adapt to container size
    shinyjs::hidden(
        div(
            id = ns("plot_div_%widget_id%"),
            plotOutput(
                ns("plot_%widget_id%"), 
                height = "100%",
                width = "100%"
            ),
            style = "width: 100%; height: 100%; padding: 10px; box-sizing: border-box;"
        )
    ),
    
    # INTERACTIVE PLOT OUTPUT (plotly)
    # Uncomment for interactive charts and dashboards
    # shinyjs::hidden(
    #     div(
    #         id = ns("plotly_div_%widget_id%"),
    #         plotly::plotlyOutput(
    #             ns("plotly_%widget_id%"), 
    #             height = "100%", 
    #             width = "100%"
    #         ),
    #         style = "width: 100%; height: calc(100% - 20px); padding: 10px; box-sizing: border-box;"
    #     )
    # ),
    
    # TIME SERIES CHART OUTPUT (dygraphs)
    # Uncomment for time series data visualization
    # div(
    #     id = ns("dygraph_div_%widget_id%"),
    #     dygraphs::dygraphOutput(
    #         ns("dygraph_%widget_id%"), 
    #         height = "100%", 
    #         width = "100%"
    #     ),
    #     style = "width: 100%; height: calc(100% - 20px); padding: 10px; box-sizing: border-box;"
    # ),
    
    # INTERACTIVE DATA TABLE OUTPUT (DT)
    # For data exploration and tabular display
    shinyjs::hidden(
        div(
            id = ns("datatable_div_%widget_id%"),
            DT::DTOutput(
                ns("datatable_%widget_id%"),
                height = "100%",
                width = "100%"
            ),
            style = "width: 100%; height: calc(100% - 20px); padding: 10px; box-sizing: border-box;"
        )
    ),
    
    # INTERACTIVE MAP OUTPUT (leaflet)
    # Uncomment for geospatial analysis and mapping
    # div(
    #     id = ns("map_div_%widget_id%"),
    #     leaflet::leafletOutput(
    #         ns("map_%widget_id%"),
    #         height = "100%",
    #         width = "100%"
    #     ),
    #     style = "width: 100%; height: calc(100% - 20px); padding: 10px; box-sizing: border-box;"
    # ),
    
    # DYNAMIC HTML OUTPUT (uiOutput)
    # Visualization helper UI (for variable analysis and recommendations)
    shinyjs::hidden(
        div(
            id = ns("visualization_helper_div_%widget_id%"),
            uiOutput(ns("visualization_helper_%widget_id%"), style = "height: calc(100% - 5px);"),
            style = "width: 100%; height: 100%; padding: 10px; box-sizing: border-box; overflow-y: auto;"
        )
    ),
    
    # Used for UI messages, cards, and error displays
    shinyjs::hidden(
        div(
            id = ns("dynamic_output_div_%widget_id%"),
            uiOutput(ns("dynamic_output_%widget_id%"), style = "height: calc(100% - 5px);"),
            style = "width: 100%; height: 100%; padding: 10px; box-sizing: border-box;"
        )
    ),
    
    # CONSOLE-STYLE TEXT OUTPUT (verbatimTextOutput)
    # Uncomment for statistical results, code output, or log messages
    # shinyjs::hidden(
    #     div(
    #         id = ns("text_output_div_%widget_id%"),
    #         verbatimTextOutput(ns("text_output_%widget_id%")),
    #         style = "width: 100%; padding: 10px; box-sizing: border-box; font-family: monospace;"
    #     )
    # ),
    
    # SIMPLE HTML TABLE OUTPUT (tableOutput)
    # For gtsummary tables and statistics output
    shinyjs::hidden(
        div(
            id = ns("table_div_%widget_id%"),
            uiOutput(ns("table_%widget_id%")),
            style = "width: 100%; padding: 10px; box-sizing: border-box; overflow: auto;"
        )
    ),
    
    # ====================
    # CONTAINER STYLING
    # ====================
    # Main container with scrollable overflow for large content
    style = "width: 100%; padding: 5px; box-sizing: border-box; height: 100%; overflow-y: auto;"
)
