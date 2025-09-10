# ==========================================
# ui_output.R - Hospital Stays Output Display Interface
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 HOSPITAL STAYS PLUGIN - CUSTOMIZED IMPLEMENTATION  🔧                  ██
# ██                                                                            ██
# ██  This file defines the output display containers for the Hospital Stays    ██
# ██  plugin's timeline visualization and error handling.                       ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# HOSPITAL STAYS PLUGIN - OUTPUT DISPLAY UI FILE
# 
# This file defines the output display interface for the Hospital Stays plugin.
# It provides containers for displaying the interactive timeline visualization
# and handling error messages when patient data is not available.
# 
# HOSPITAL STAYS OUTPUT COMPONENTS:
# - Error message display for cases where no patient is selected or no data exists
# - Dynamic output container for plotly timeline charts (main output)
# - Static plot container for any ggplot2 fallback visualizations
# - Data table container for displaying tabular stay information if needed
# 
# HOSPITAL STAYS VISUALIZATION:
# - Primary output: Interactive plotly timeline showing hospital stays across units
# - Timeline displays chronological sequence of patient visits
# - Hover information shows stay duration, unit names, and dates
# - Automatic scaling and formatting based on data and language preferences
# 
# OUTPUT ROUTING:
# - Error messages: Patient selection errors, no data warnings
# - Plotly objects: Main timeline visualization (most common)
# - Other output types: Fallback support for tables, static plots if needed

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
    # UI OUTPUT CONTAINER (Always visible)
    # ====================
    # Main container for UI messages, cards, and error displays (like Admissions plugin)
    div(
        id = ns("ui_output_div_%widget_id%"),
        uiOutput(ns("ui_output_%widget_id%"), style = "height: calc(100% - 5px);"),
        style = "width: 100%; height: 100%; padding: 10px; box-sizing: border-box;"
    ),
    
    # ====================
    # HOSPITAL STAYS OUTPUT CONTAINERS
    # ====================
    # Containers needed for the Hospital Stays plugin output types
    
    # STATIC PLOT OUTPUT (ggplot2 fallback)
    # Hidden by default - shown if ggplot2 visualizations are generated
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
    
    # PLOTLY OUTPUT (Main container for hospital stays timeline)
    # Primary output container for hospital stays timeline visualization
    shinyjs::hidden(
        div(
            id = ns("plotly_output_div_%widget_id%"),
            plotly::plotlyOutput(
                ns("plotly_output_%widget_id%"),
                height = "100%",
                width = "100%"
            ),
            style = "width: 100%; height: 100%; padding: 10px; box-sizing: border-box;"
        )
    ),
    
    # DYNAMIC OUTPUT (Fallback for other content types)
    # Used for user-friendly messages and other dynamic content
    shinyjs::hidden(
        div(
            id = ns("dynamic_output_div_%widget_id%"),
            uiOutput(ns("dynamic_output_%widget_id%")),
            style = "width: 100%; height: calc(100% - 20px); padding: 10px; box-sizing: border-box;"
        )
    ),
    
    # INTERACTIVE DATA TABLE OUTPUT (Optional)
    # Hidden by default - can be used for tabular stay data if needed
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
    
    # SIMPLE HTML TABLE OUTPUT (Optional)
    # Hidden by default - can be used for summary tables if needed
    shinyjs::hidden(
        div(
            id = ns("table_div_%widget_id%"),
            tableOutput(ns("table_%widget_id%")),
            style = "width: 100%; padding: 10px; box-sizing: border-box; overflow: auto;"
        )
    ),
    
    # ====================
    # CONTAINER STYLING
    # ====================
    # Main container styling (like Admissions plugin - no overflow scroll)
    style = "width: 100%; padding: 5px; box-sizing: border-box; height: 100%;"
)
