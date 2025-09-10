# ==========================================
# ui_output.R - Output Display Interface
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 CONSOLE PLUGIN - CUSTOMIZED IMPLEMENTATION  🔧                         ██
# ██                                                                            ██
# ██  This file provides the Console plugin output display interface with      ██
# ██  support for multiple output types. Based on the plugin template.         ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# CONSOLE PLUGIN - OUTPUT DISPLAY UI FILE
# 
# This file defines the output display containers for the Console plugin.
# It provides specialized containers for each supported output type across multiple languages.
# 
# CONSOLE PLUGIN OUTPUT TYPES:
# - Console: Text output for R and Python code execution
# - UI: Dynamic Shiny UI components generated from R code
# - Figure: Static plots from R (ggplot2, base R, etc.)
# - Table: Basic HTML tables for data display
# - Datatable: Interactive tables with search and pagination
# - Dygraphs: Interactive time series charts
# - Plotly: Interactive visualizations and 3D plots
# - R Markdown: Formatted markdown with MathJax support
# - Matplotlib: Python plotting (placeholder for future implementation)
# 
# IMPORTANT NOTES:
# - All output containers are initially hidden and shown dynamically by server logic
# - Each container is optimized for its specific content type
# - Output routing is handled by server_code.R based on result type

div(
    # ====================
    # CONSOLE OUTPUT
    # ====================
    # Used for captured text output, errors, and warnings from code execution
    # Supports both R and Python console output
    shinyjs::hidden(
        div(
            id = ns("console_output_div_%widget_id%"),
            verbatimTextOutput(
                ns("console_output_%widget_id%"),
                placeholder = TRUE
            ),
            style = paste(
                "width: 100%;",
                "height: 100%;",
                "padding: 10px;",
                "box-sizing: border-box;",
                "font-family: 'Courier New', Consolas, monospace;",
                "font-size: 12px;",
                "background-color: #f8f8f8;",
                "border: 1px solid #ddd;",
                "border-radius: 4px;",
                "overflow-x: auto;",
                "overflow-y: auto;"
            )
        )
    ),
    
    # ====================
    # DYNAMIC UI OUTPUT
    # ====================
    # Used for R code that returns UI elements (renderUI results)
    # Allows dynamic generation of Shiny UI components
    shinyjs::hidden(
        div(
            id = ns("ui_output_div_%widget_id%"),
            uiOutput(ns("ui_output_%widget_id%"), style = "height: calc(100% - 5px);"),
            style = "width: 100%; height: 100%; padding: 10px; box-sizing: border-box;"
        )
    ),
    
    # ====================
    # FIGURE/PLOT OUTPUT
    # ====================
    # Used for static R plots (base R plots, ggplot2, etc.)
    # Displays rendered plot images
    shinyjs::hidden(
        div(
            id = ns("figure_output_div_%widget_id%"),
            plotOutput(
                ns("figure_output_%widget_id%"),
                height = "100%",
                width = "100%"
            ),
            style = "width: 100%; height: calc(100% - 20px); padding: 10px; box-sizing: border-box;"
        )
    ),
    
    # ====================
    # SIMPLE TABLE OUTPUT
    # ====================
    # Used for basic HTML table display from R data frames
    # Non-interactive table rendering
    shinyjs::hidden(
        div(
            id = ns("table_output_div_%widget_id%"),
            tableOutput(ns("table_output_%widget_id%")),
            style = paste(
                "width: 100%;",
                "height: 100%;",
                "padding: 10px;",
                "box-sizing: border-box;",
                "overflow: auto;",
                "max-height: 500px;"
            )
        )
    ),
    
    # ====================
    # INTERACTIVE DATA TABLE OUTPUT
    # ====================
    # Used for interactive data tables with search, filter, and pagination
    # Utilizes DT package for enhanced table functionality
    shinyjs::hidden(
        div(
            id = ns("datatable_output_div_%widget_id%"),
            DT::DTOutput(
                ns("datatable_output_%widget_id%"),
                height = "100%",
                width = "100%"
            ),
            style = paste(
                "width: 100%;",
                "height: calc(100% - 20px);",
                "padding: 10px;",
                "box-sizing: border-box;",
                "min-height: 300px;"
            )
        )
    ),
    
    # ====================
    # DYGRAPHS TIME SERIES OUTPUT
    # ====================
    # Used for interactive time series charts using the dygraphs library
    # Provides zoom, pan, and hover functionality for temporal data
    shinyjs::hidden(
        div(
            id = ns("dygraphs_output_div_%widget_id%"),
            dygraphs::dygraphOutput(
                ns("dygraphs_output_%widget_id%"),
                height = "100%",
                width = "100%"
            ),
            style = "width: 100%; height: calc(100% - 20px); padding: 10px; box-sizing: border-box;"
        )
    ),
    
    # ====================
    # PLOTLY INTERACTIVE PLOT OUTPUT
    # ====================
    # Used for interactive plots and visualizations using plotly
    # Supports 3D plots, animations, and advanced interactivity
    shinyjs::hidden(
        div(
            id = ns("plotly_output_div_%widget_id%"),
            plotly::plotlyOutput(
                ns("plotly_output_%widget_id%"),
                height = "100%",
                width = "100%"
            ),
            style = "width: 100%; height: calc(100% - 20px); padding: 10px; box-sizing: border-box;"
        )
    ),
    
    # ====================
    # R MARKDOWN OUTPUT
    # ====================
    # Used for rendered R Markdown content with MathJax support
    # Displays formatted markdown with mathematical expressions
    shinyjs::hidden(
        div(
            id = ns("rmarkdown_output_div_%widget_id%"),
            div(
                uiOutput(ns("rmarkdown_output_%widget_id%")),
                style = paste(
                    "background-color: white;",
                    "padding: 0 15px;",
                    "border: 1px solid #ddd;",
                    "border-radius: 4px;",
                    "box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                    "height: 100%;",
                    "overflow-x: auto;",
                    "overflow-y: auto;"
                )
            ),
            style = paste(
                "width: 100%;",
                "height: 100%;",
                "padding: 10px;",
                "box-sizing: border-box;",
                "overflow: hidden;"
            )
        )
    ),
    
    # ====================
    # MATPLOTLIB OUTPUT (PYTHON)
    # ====================
    # Placeholder for Python matplotlib output (implementation depends on Python integration)
    # Currently commented out as Python integration needs to be implemented
    # shinyjs::hidden(
    #     div(
    #         id = ns("matplotlib_output_div_%widget_id%"),
    #         plotOutput(
    #             ns("matplotlib_output_%widget_id%"),
    #             height = "100%",
    #             width = "100%"
    #         ),
    #         style = "width: 100%; height: calc(100% - 20px); padding: 10px; box-sizing: border-box;"
    #     )
    # ),
    
    # ====================
    # CONTAINER STYLING
    # ====================
    # Main container with scrollable overflow for large content
    # Ensures all outputs fit within the widget boundaries
    style = paste(
        "width: 100%;",
        "height: 100%;",
        "padding: 5px;",
        "box-sizing: border-box;",
        "overflow-y: auto;"
    )
)
