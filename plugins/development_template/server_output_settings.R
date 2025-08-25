# ==========================================
# server_output_settings.R - Output Configuration Server Logic
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

# PLUGIN TEMPLATE - OUTPUT SETTINGS SERVER FILE
# 
# This file handles the server-side logic for the output configuration interface.
# It manages user interactions with the no-code settings panel.
# 
# WHEN CREATING A NEW PLUGIN WITH THIS TEMPLATE:
# - Customize the dynamic UI observers for your specific input elements
# - Implement validation logic for your parameter combinations
# 
# CORE FUNCTIONALITY:
# - Dynamic UI updates based on user selections
# - Bulk selection helpers (select all/clear all functionality)
# - Input validation and error handling
# - Conditional UI display using shinyjs::show() and shinyjs::hide() to dynamically
#   show or hide UI elements based on user selections (e.g., showing title inputs
#   only when certain output types are selected)
# 
# COMMON CONFIGURATION PATTERNS:
# 
# CONDITIONAL UI DISPLAY:
#   Show/hide settings based on other selections (e.g., output type affects available options)
# 
# CASCADING UPDATES:
#   Filter available options based on previous selections (e.g., dataset affects variables)
# 
# UPDATE LOCK MECHANISM:
#   Prevent automatic field updates during configuration loading using input$update_lock_%widget_id%
#   This solves conflicts where observe_event reactions overwrite values being loaded from saved configs
#   Pattern: Check if (isTRUE(input$update_lock_%widget_id%)) { return() } at start of observe_event

# ======================================
# CENTRALIZED INPUT DEFINITIONS
# ======================================

# Define all inputs for this plugin in one centralized location.
# This configuration automatically generates the saving and loading logic for user configurations.
# 
# IMPORTANT: All UI elements must be added to ui_output_settings.R first, then registered here
# to enable automatic persistence of user preferences through the configuration system (server_user_configurations.R).
#
# STRUCTURE:
# Each input is defined as a list with the following required fields:
# - id: unique identifier (will be suffixed with _%widget_id%)
# - type: input type (see available types below)
# - default: default value when no configuration is loaded
#
# AVAILABLE INPUT TYPES:
# - "dropdown": Single selection dropdown (shiny.fluent::Dropdown)
# - "multiselect": Multiple selection dropdown (shiny.fluent::Dropdown with multiSelect = TRUE)
# - "text": Text input field (shiny.fluent::TextField)
# - "toggle": Boolean toggle switch (shiny.fluent::Toggle)
# - "code": Code editor (shinyAce::aceEditor)
# - "date": Date picker (shiny.fluent::DatePicker)
# - "number": Numeric input (shiny.fluent::SpinButton)
#
# WORKFLOW:
# 1. Create UI elements in ui_output_settings.R
# 2. Register all inputs here in all_inputs_%widget_id%
# 3. User configuration save/load is handled automatically
# 4. Add custom logic below for dynamic behavior and validation

all_inputs_%widget_id% <- list(
    list(id = "output_type", type = "dropdown", default = "histogram"),
    list(id = "variables", type = "multiselect", default = c("Sepal.Length")),
    list(id = "plot_title",type = "text", default = i18np$t("data_analysis_results")),
    list(id = "auto_update", type = "toggle", default = TRUE),
    list(id = "code", type = "code", default = "")
    # Add more inputs as needed:
    # list(id = "start_date", type = "date", default = Sys.Date() - 30),
    # list(id = "alpha_level", type = "number", default = 0.05)
)

# ======================================
# CONDITIONAL UI DISPLAY LOGIC
# ======================================

# Show/hide plot title input based on output type selection
observe_event(input$output_type_%widget_id%, {
    # ======================================
    # UPDATE LOCK MECHANISM EXAMPLE
    # ======================================
    # Check if updates are locked (during configuration loading)
    # This prevents automatic field updates from overwriting values being loaded from saved configurations
    # Use this pattern when you have observe_event reactions that automatically update UI fields
    # and you want to prevent them from interfering with configuration loading
    if (isTRUE(input$update_lock_%widget_id%)) {
        return()
    }
    
    output_type <- input$output_type_%widget_id%
    
    if (!is.null(output_type)) {
        if (output_type == "histogram") {
            shinyjs::show("plot_title_div_%widget_id%")
        } else {
            shinyjs::hide("plot_title_div_%widget_id%")
        }
    }
})

# Hide Display + Save button if user doesn't have save permissions
if (!("projects_widgets_settings" %in% user_accesses)) {
    shinyjs::hide("display_and_save_%widget_id%")
}

# ======================================
# VARIABLES CHECK/UNCHECK BUTTONS
# ======================================

observe_event(input$variables_check_all_%widget_id%, {
    all_variables <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
    shiny.fluent::updateDropdown.shinyInput(session, "variables_%widget_id%", value = all_variables)
})

observe_event(input$variables_uncheck_all_%widget_id%, {
    shiny.fluent::updateDropdown.shinyInput(session, "variables_%widget_id%", value = character(0))
})
