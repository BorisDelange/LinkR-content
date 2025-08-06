# ==========================================
# ui_output.R - Output Display Interface
# ==========================================

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
            uiOutput(ns("ui_output_%widget_id%")),
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
                    "padding: 15px;",
                    "border: 1px solid #ddd;",
                    "border-radius: 4px;",
                    "box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                    "height: 100%;"
                )
            ),
            style = paste(
                "width: 100%;",
                "height: calc(100% - 20px);",
                "padding: 10px;",
                "box-sizing: border-box;",
                "max-height: 600px;",
                "overflow-y: auto;"
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
