# ==========================================
# ui_output_settings.R - Hospital Stays Output Configuration Panel
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 HOSPITAL STAYS PLUGIN - SIMPLIFIED IMPLEMENTATION  🔧                  ██
# ██                                                                            ██
# ██  This file currently has minimal functionality as the plugin               ██
# ██  doesn't have UI parameters yet. Ready for future enhancements.            ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# HOSPITAL STAYS PLUGIN - OUTPUT SETTINGS UI FILE
# 
# This file defines the (minimal) no-code configuration interface for the Hospital Stays plugin.
# Currently, it only provides the auto-update toggle as there are no specific parameters
# for the timeline visualization yet.
# 
# HOSPITAL STAYS VISUALIZATION:
# - Displays a timeline of patient hospital stays across different care sites
# - Uses plotly for interactive timeline visualization
# - Automatically generated based on selected patient's visit_detail data
# - Shows chronological sequence of stays with hover information
# 
# CURRENT CONFIGURATION OPTIONS:
# - Auto-update toggle: Controls whether visualization updates when patient changes
# - (Future expansion: date filtering, care site selection, display options)
# 
# FUTURE EXPANSION POSSIBILITIES:
# - Date range filtering for specific time periods
# - Care site selection/filtering controls
# - Color customization for different unit types
# - Timeline display options (zoom, grouping)
# - Export options for the timeline visualization

div(
    # ====================
    # HOSPITAL STAYS CONFIGURATION
    # ====================
    # Currently minimal - just the auto-update toggle
    # Future expansion can add more controls here
    
    div(
        id = ns("auto_update_div_%widget_id%"),
        shiny.fluent::Toggle.shinyInput(
            ns("auto_update_%widget_id%"), 
            label = i18np$t("automatic_updates"),
            value = TRUE
        ),
        style = "margin-top: 15px; margin-bottom: 15px;"
    ),
    
    # ====================
    # ACTION BUTTONS
    # ====================
    # Standard action buttons for Hospital Stays plugin
    div(
        id = ns("action_buttons_div_%widget_id%"),
        # Default action - Generate/Update hospital stays timeline
        shiny.fluent::DefaultButton.shinyInput(
            ns("display_output_2_%widget_id%"), 
            i18np$t("display_output"), iconProps = list(iconName = "Play"),
            onClick = htmlwidgets::JS(paste0(
                "item => { Shiny.setInputValue('", id, "-display_output_%widget_id%', Math.random()); }"
            ))
        ),
        
        # Primary action - Display and Save (if user has permissions)
        # This button is shown/hidden based on user access in the main UI file
        shiny.fluent::PrimaryButton.shinyInput(
            ns("display_and_save_%widget_id%"), 
            i18np$t("display_and_save"), 
            iconProps = list(iconName = "SaveAs"),
            onClick = htmlwidgets::JS(paste0(
                "item => { ",
                "Shiny.setInputValue('", id, "-display_and_save_%widget_id%', Math.random()); ",
                "}"
            ))
        ),
        
        style = "margin-top: 15px; display: flex; gap: 10px; flex-wrap: wrap; padding-top: 15px; border-top: solid 1px #808080;"
    )
)
