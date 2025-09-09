# ==========================================
# ui_output.R - Hospital Indicators Output Display Interface
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  ⚠️  DO NOT MODIFY - CORE PLUGIN FRAMEWORK  ⚠️                             ██
# ██                                                                            ██
# ██  This file is part of the plugin framework and works automatically.        ██
# ██  Modifications are NOT required and may break functionality.               ██
# ██  Only modify if you have specific advanced requirements.                   ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# ADMISSIONS AND DEMOGRAPHICS PLUGIN - OUTPUT DISPLAY UI FILE
# 
# This file defines the output display containers for the Admissions and Demographics plugin.
# It provides specialized containers for healthcare analytics visualizations including
# hospital indicators, demographic charts, and medical statistics displays.
# 
# HOSPITAL-SPECIFIC OUTPUT CONTAINERS:
# - UI output container for indicator cards and statistics displays
# - Plot output container for Plotly medical visualizations (timelines, demographics)
# - Error message container for healthcare data validation feedback
# - Responsive layout optimized for hospital analytics and medical data presentation
# 
# OUTPUT TYPES SUPPORTED:
# - Patient count and admission statistics cards
# - Timeline charts for admission patterns
# - Demographic visualizations (age, gender distributions)
# - Hospital unit analysis displays
# - Mortality and length of stay indicators
# - Error handling for OMOP data integration issues

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
    # GGPLOT OUTPUT CONTAINER
    # ====================
    # Container for ggplot visualizations (pie charts, etc.)
    shinyjs::hidden(
        div(
            id = ns("ggplot_output_div_%widget_id%"),
            plotOutput(
                ns("ggplot_output_%widget_id%"), 
                height = "100%",
                width = "100%"
            ),
            style = "width: 100%; height: 100%; padding: 10px; box-sizing: border-box;"
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
