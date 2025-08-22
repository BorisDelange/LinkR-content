# ==========================================
# Server - Main file
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 OPTIONAL CUSTOMIZATION - PLUGIN ENHANCEMENT  🔧                        ██
# ██                                                                            ██
# ██  This file provides default functionality that works out-of-the-box.       ██
# ██  Customize only if you need specific features or modifications.            ██
# ██  Safe to use as-is for standard plugin requirements.                       ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# TIMELINE PLUGIN - SERVER MAIN FILE
# 
# This file serves as the main server logic controller for the Timeline plugin.
# It manages tab navigation and coordinates between different functional modules.
# 
# WHEN CUSTOMIZING THIS TIMELINE PLUGIN:
# Add your new server script files in the "IMPORT SERVER MODULES" section at the bottom
# of this file using the %import_script('your_new_server_file.R')% syntax.
# 
# TAB SYSTEM OVERVIEW:
# - output: Displays the execution result of the code (timeline visualization)
# - output_settings: No-code interface with graphical controls to configure the timeline parameters
# - code: Code editor where users can write/modify code. Connected to output_settings in a 
# low-code approach where output_settings parameters automatically generate code
# - user_configurations: Interface to create, save, and manage different configurations 
# (saves both output_settings parameters and code content)

# ======================================
# TAB NAVIGATION SYSTEM
# ======================================

# Define all available tabs in the widget interface
tabs <- c("output", "output_settings", "code", "user_configurations")

# Main tab switching logic - triggered when user clicks navigation buttons
observe_event(input$current_tab_trigger_%widget_id%, {
    
    tab <- input$current_tab_%widget_id%
    
    # ====================
    # MAIN LAYOUT CONTROL
    # ====================
    
    # Hide or show main content area based on selected tab
    if (tab == "user_configurations") {
        # User configurations uses full screen, hide main content area
        shinyjs::hide("output_settings_code_div_%widget_id%")
    } else {
        # All other tabs use the main content area
        shinyjs::show("output_settings_code_div_%widget_id%")
    }
    
    # ====================
    # HIDE INACTIVE TABS
    # ====================
    
    # Hide all tabs except the currently selected one
    inactive_tabs <- setdiff(c("output_settings", "code", "user_configurations"), tab)
    sapply(paste0(inactive_tabs, "_div_%widget_id%"), shinyjs::hide)
    
    # ====================
    # SHOW ACTIVE TAB
    # ====================
    
    # Show the requested tab, but only if user has appropriate access
    # Code tab requires console access, other tabs are always accessible
    if (tab != "code" || (tab == "code" && "projects_widgets_console" %in% user_accesses)) {
        shinyjs::show(paste0(tab, "_div_%widget_id%"))
    }
    
    # ====================
    # UNIFIED PANEL LOGIC
    # ====================
    
    if (tab %in% c("output_settings", "code")) {
        
        # Check side-by-side mode state
        side_by_side <- length(input$output_and_settings_side_by_side_%widget_id%) > 0 && input$output_and_settings_side_by_side_%widget_id%
        
        if (side_by_side) {
            # Side-by-side mode: let panel_layout_manager handle everything
            shinyjs::show("output_div_%widget_id%")
            shinyjs::runjs("if (typeof setPanelWidths_%widget_id% === 'function') setPanelWidths_%widget_id%();")
        } else {
            # Full-width mode: use settings_container structure
            shinyjs::hide("output_div_%widget_id%")
            
            shinyjs::runjs(paste0("
                var outputDiv = $('#", id, "-output_div_%widget_id%');
                var settingsContainer = $('#", id, "-settings_container_%widget_id%');
                var outputSettingsDiv = $('#", id, "-output_settings_div_%widget_id%');
                var codeDiv = $('#", id, "-code_div_%widget_id%');
                
                // Full-width mode with proper sizing
                outputDiv.css('flex', '0');
                settingsContainer.css('flex', '1');
                
                if ('", tab, "' === 'code') {
                    // Full-width code editor
                    outputSettingsDiv.hide();
                    codeDiv.show().css('flex', '1');
                } else if ('", tab, "' === 'output_settings') {
                    // Full-width settings panel
                    outputSettingsDiv.show().css('flex', '1');
                    codeDiv.hide();
                }
            "))
        }
        
        # Show the action buttons sidebar for these tabs
        shinyjs::show("output_settings_code_sidenav_%widget_id%")
        
    } else if (tab == "output") {
        # Output-only mode
        shinyjs::runjs(paste0("
            var outputDiv = $('#", id, "-output_div_%widget_id%');
            var settingsContainer = $('#", id, "-settings_container_%widget_id%');
            
            outputDiv.css('flex', '1');
            settingsContainer.css('flex', '0');
        "))
        shinyjs::hide("output_settings_code_sidenav_%widget_id%")
        
    } else {
        # For other tabs (user_configurations), hide sidebar and output
        shinyjs::hide("output_settings_code_sidenav_%widget_id%")
        if (tab != "output") {
            shinyjs::hide("output_div_%widget_id%")
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
# USER CONFIGURATIONS TAB HANDLER
# ======================================

# Observer for showing the user configurations management interface
observe_event(input$show_user_configurations_tab_%widget_id%, {
    # Switch to user configurations tab
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-current_tab_%widget_id%', 'user_configurations');"))
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-current_tab_trigger_%widget_id%', Math.random());"))
})

# ======================================
# IMPORT SERVER MODULES
# ======================================

# Import specialized server logic for each functional area
# Each file handles the server-side logic for its corresponding UI component

# Output settings server logic (chart configuration, data source selection)
%import_script('server_output_settings.R')%

# Code editor server logic (syntax highlighting, code execution, save/load)
%import_script('server_code.R')%
%import_script('server_code_dygraphs.R')%
%import_script('server_code_plotly.R')%

# User configurations server logic (configuration management, create/delete operations)
%import_script('server_user_configurations.R')%

# Layout management server logic (side-by-side toggle, panel resizing)
%import_script('server_layout_manager.R')%
