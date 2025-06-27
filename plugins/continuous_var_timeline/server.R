# ==========================================
# Server - Main file
# ==========================================

# ======================================
# TAB NAVIGATION SYSTEM
# ======================================

# Define all available tabs in the widget interface
tabs <- c("figure", "figure_settings", "code", "general_settings")

# Main tab switching logic - triggered when user clicks navigation buttons
observe_event(input$current_tab_trigger_%widget_id%, {
    
    # Get the requested tab from user input
    tab <- input$current_tab_%widget_id%
    
    # ====================
    # MAIN LAYOUT CONTROL
    # ====================
    
    # Hide or show the main content area based on selected tab
    if (tab == "general_settings") {
        # General settings uses full screen, hide main content area
        shinyjs::hide("figure_settings_code_div_%widget_id%")
    } else {
        # All other tabs use the main content area
        shinyjs::show("figure_settings_code_div_%widget_id%")
    }
    
    # ====================
    # HIDE INACTIVE TABS
    # ====================
    
    # Hide all tabs except the currently selected one
    inactive_tabs <- setdiff(c("figure_settings", "code", "general_settings"), tab)
    sapply(paste0(inactive_tabs, "_div_%widget_id%"), shinyjs::hide)
    
    # Always hide settings files panel (shown separately via different trigger)
    shinyjs::hide("settings_files_div_%widget_id%")
    
    # ====================
    # SHOW ACTIVE TAB (WITH ACCESS CONTROL)
    # ====================
    
    # Show the requested tab, but only if user has appropriate access
    # Code tab requires console access, other tabs are always accessible
    if (tab != "code" || (tab == "code" && "projects_widgets_console" %in% user_accesses)) {
        shinyjs::show(paste0(tab, "_div_%widget_id%"))
    }
    
    # ====================
    # FIGURE DISPLAY LOGIC
    # ====================
    
    # For figure_settings and code tabs, conditionally show the figure panel
    if (tab %in% c("figure_settings", "code")) {
        
        # Check if side-by-side layout is enabled
        if (length(input$figure_and_settings_side_by_side_%widget_id%) > 0 && 
            input$figure_and_settings_side_by_side_%widget_id%) {
            # Side-by-side mode: show figure alongside settings/code
            shinyjs::show("figure_div_%widget_id%")
        } else {
            # Full-width mode: hide figure panel
            shinyjs::hide("figure_div_%widget_id%")
            
            # Adjust widths in full-width mode
            shinyjs::runjs(paste0("
                var figureDiv = $('#", id, "-figure_div_%widget_id%');
                var figureSettingsDiv = $('#", id, "-figure_settings_div_%widget_id%');
                var codeDiv = $('#", id, "-code_div_%widget_id%');
                
                if ('", tab, "' === 'code') {
                    // Full-width code
                    figureDiv.css('flex', '0');
                    codeDiv.css('flex', '1');
                    figureSettingsDiv.css('flex', '0');
                } else if ('", tab, "' === 'figure_settings') {
                    // Full-width settings
                    figureDiv.css('flex', '0');
                    figureSettingsDiv.css('flex', '1');
                    codeDiv.css('flex', '0');
                }
            "))
        }
        
        # Show the action buttons sidebar for these tabs
        shinyjs::show("figure_settings_code_sidenav_%widget_id%")
        
    } else {
        # For other tabs, hide the action buttons sidebar
        shinyjs::hide("figure_settings_code_sidenav_%widget_id%")
        
        # Hide figure panel unless we're on the figure tab itself
        if (tab != "figure") {
            shinyjs::hide("figure_div_%widget_id%")
        }
    }
    
    # ====================
    # ACE EDITOR FIX
    # ====================
    
    # Trigger window resize event to prevent scroll issues in ACE code editor
    # This ensures the editor renders properly after tab switches
    shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
})

# ======================================
# IMPORT SERVER MODULES
# ======================================

# Import specialized server logic for each functional area
# Each file handles the server-side logic for its corresponding UI component

# Figure settings server logic (chart configuration, data source selection)
%import_script('server_figure_settings.R')%

# Code editor server logic (syntax highlighting, code execution, save/load)
%import_script('server_code.R')%

# Settings files server logic (file management, create/delete operations)
%import_script('server_settings_files.R')%

# General settings server logic (toggle preferences, layout options)
%import_script('server_general_settings.R')%
