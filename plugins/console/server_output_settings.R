# ==========================================
# server_output_settings.R - Output Configuration Server Logic
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 CONSOLE PLUGIN - CUSTOMIZED IMPLEMENTATION  🔧                         ██
# ██                                                                            ██
# ██  This file configures the Console plugin output settings with multi-      ██
# ██  language and output type support. Based on the plugin template structure.██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# CONSOLE PLUGIN - OUTPUT SETTINGS SERVER FILE
# 
# This file handles the server-side logic for the Console plugin output configuration.
# It manages the programming language selection and output type configuration interface.
# 
# CONSOLE PLUGIN CONFIGURATION:
# - Programming Language Selection (R/Python)
# - Output Type Selection (console, figure, table, datatable, etc.)
# - Auto-update toggle for automatic execution
# - Dynamic output type filtering based on selected programming language
# 
# CORE FUNCTIONALITY:
# - Centralized input definitions for configuration system
# - Dynamic UI updates based on user selections
# - Programming language-specific output type filtering
# - Input validation and error handling
# 
# UPDATE LOCK MECHANISM:
# - Available for preventing automatic field updates during configuration loading
# - Use input$update_lock_%widget_id% to prevent observe_event conflicts
# - Pattern: if (isTRUE(input$update_lock_%widget_id%)) { return() }
# - Currently not needed but ready for future use if observe_event conflicts arise

# ======================================
# CENTRALIZED INPUT DEFINITIONS
# ======================================

# Define all inputs for the Console plugin in one centralized location.
# This configuration automatically generates the saving and loading logic for user configurations.
# 
# CONSOLE PLUGIN INPUTS:
# - prog_language: Programming language selection (R or Python)
# - output: Output type selection (varies by language)
# - auto_update: Toggle for automatic code execution
#
# WORKFLOW:
# 1. User selects programming language
# 2. Output type dropdown is filtered based on language
# 3. User configuration save/load is handled automatically
# 4. Code execution is triggered based on auto_update setting

all_inputs_%widget_id% <- list(
    list(id = "prog_language", type = "dropdown", default = "r"),
    list(id = "output", type = "dropdown", default = "console"), 
    list(id = "auto_update", type = "toggle", default = TRUE),
    list(id = "code", type = "code", default = "")
)

# ======================================
# DYNAMIC OUTPUT TYPE FILTERING
# ======================================

# Define available output types for each programming language
output_types <- list()
output_types$r <- list(
    list(key = "console", text = i18np$t("console")),
    list(key = "ui", text = i18np$t("ui_html")),
    list(key = "figure", text = i18np$t("figure")),
    list(key = "table", text = i18np$t("table")),
    list(key = "datatable", text = i18np$t("datatable")),
    list(key = "dygraphs", text = i18np$t("dygraphs")),
    list(key = "plotly", text = i18np$t("plotly")),
    list(key = "rmarkdown", text = i18np$t("rmarkdown"))
)

output_types$python <- list(
    list(key = "console", text = i18np$t("console")),
    list(key = "matplotlib", text = i18np$t("matplotlib"))
)

# Update output type options when programming language changes
observe_event(input$prog_language_%widget_id%, {
    # ======================================
    # UPDATE LOCK MECHANISM
    # ======================================
    # Check if updates are locked (during configuration loading)
    # This prevents automatic field updates from overwriting values being loaded from saved configurations
    if (isTRUE(input$update_lock_%widget_id%)) {
        return()
    }
    
    if (!is.null(input$prog_language_%widget_id%)) {
        language <- input$prog_language_%widget_id%
        
        # Get available output types for selected language
        available_outputs <- output_types[[language]]
        
        # Update the output dropdown with language-specific options
        shiny.fluent::updateDropdown.shinyInput(
            session = session,
            inputId = "output_%widget_id%", 
            options = available_outputs,
            value = available_outputs[[1]]$key  # Default to first option
        )
    }
})

# ======================================
# CONDITIONAL UI DISPLAY LOGIC
# ======================================

# Hide Display + Save button if user doesn't have save permissions
if (!("projects_widgets_settings" %in% user_accesses)) {
    shinyjs::hide("display_and_save_%widget_id%")
}

# Optional: Add conditional UI logic based on selections
# Example: Show/hide specific settings based on output type
observe_event(input$output_%widget_id%, {
    # Check if updates are locked (during configuration loading)
    if (isTRUE(input$update_lock_%widget_id%)) {
        return()
    }
    
    output_type <- input$output_%widget_id%
    
    if (!is.null(output_type)) {
        # Add logic here if you need to show/hide UI elements
        # based on the selected output type
        
        # Example for future enhancements:
        # if (output_type == "rmarkdown") {
        #     shinyjs::show("markdown_options_div_%widget_id%")
        # } else {
        #     shinyjs::hide("markdown_options_div_%widget_id%")
        # }
    }
})

# ======================================
# LANGUAGE CHANGE VALIDATION
# ======================================

# Validate output type when language changes to ensure compatibility
observe_event(input$prog_language_%widget_id%, {
    # Check if updates are locked (during configuration loading)
    if (isTRUE(input$update_lock_%widget_id%)) {
        return()
    }
    
    if (!is.null(input$prog_language_%widget_id%) && !is.null(input$output_%widget_id%)) {
        language <- input$prog_language_%widget_id%
        current_output <- input$output_%widget_id%
        
        # Check if current output type is available for the selected language
        available_output_keys <- sapply(output_types[[language]], function(x) x$key)
        
        if (!(current_output %in% available_output_keys)) {
            # If current output is not available, reset to console
            shiny.fluent::updateDropdown.shinyInput(
                session = session,
                inputId = "output_%widget_id%",
                value = "console"
            )
        }
    }
})