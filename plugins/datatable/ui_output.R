# ==========================================
# ui_output.R - Data Table Output Display Interface
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 DATA TABLE PLUGIN - CUSTOMIZED IMPLEMENTATION  🔧                      ██
# ██                                                                            ██
# ██  This file provides the Data Table plugin output display interface        ██
# ██  optimized for interactive data table visualization.                      ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# DATA TABLE PLUGIN - OUTPUT DISPLAY UI FILE
# 
# This file defines the output display interface for the Data Table plugin.
# It provides specialized containers for displaying time-based aggregated
# measurement data in interactive tabular format with temporal columns.
# 
# KEY FEATURES:
# - Interactive DT datatable with search and pagination
# - Error message handling for patient/visit selection
# - Dynamic UI messages for user feedback
# - Adaptive height management for optimal display
# - Proper styling for healthcare data presentation

div(
    # ====================
    # DYNAMIC UI MESSAGES
    # ====================
    # Used for user-friendly messages (patient selection, no data, errors)
    shinyjs::hidden(
        div(
            id = ns("dynamic_output_div_%widget_id%"),
            uiOutput(ns("dynamic_output_%widget_id%"), style = "height: calc(100% - 5px);"),
            style = "width: 100%; height: 100%; box-sizing: border-box;"
        )
    ),
    
    # ====================
    # INTERACTIVE DATA TABLE OUTPUT
    # ====================
    # Primary output for Data Table plugin - displays time-aggregated measurement data
    shinyjs::hidden(
        div(
            id = ns("datatable_div_%widget_id%"),
            DT::DTOutput(ns("datatable_%widget_id%")),
            style = "padding: 5px 5px 10px 5px; position: relative; z-index: 100;"
        )
    ),
    
    # ====================
    # DATETIME SLIDER CONTROLS
    # ====================
    # Optional timeline synchronization controls
    shinyjs::hidden(
        div(
            id = ns("datetime_slider_div_%widget_id%"),
            sliderInput(
                ns("datetime_slider_%widget_id%"),
                label = NULL,
                ticks = FALSE,
                min = as.Date("1970-01-01"),
                max = Sys.Date(),
                value = c(as.Date("1970-01-01"), Sys.Date()),
                timezone = "+0000",
                width = "100%"
            ),
            style = "width: calc(100% - 130px); padding-left: 130px; overflow: hidden; position: relative; top: -10px;"
        )
    ),
    
    # ====================
    # CONTAINER STYLING
    # ====================
    # Main container with scrollable overflow for table display
    style = "width: 100%; padding: 5px; box-sizing: border-box; height: 100%; overflow-y: auto;"
)
