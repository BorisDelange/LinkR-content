# ==========================================
# server_layout_manager.R - Dynamic Layout Management
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

# PLUGIN TEMPLATE - LAYOUT MANAGER SERVER FILE
# 
# This file manages the dynamic layout system for the widget plugin template.
# It provides a sophisticated resizable panel interface that enhances user experience
# by allowing flexible workspace arrangements.
# 
# WHEN CREATING A NEW PLUGIN WITH THIS TEMPLATE:
# - This file should work without modification for most plugins
# - The layout system automatically adapts to different panel configurations
# - Panel width preferences are automatically saved to the database
# - No customization typically needed unless adding new panel types
# 
# FEATURES PROVIDED:
# - Side-by-side vs full-width layout modes for optimal screen real estate usage
# - Interactive drag-to-resize functionality with visual feedback
# - Persistent panel width memory that remembers user preferences
# - Automatic database storage of layout preferences
# - Responsive design that works on different screen sizes
# - Smooth transitions between different layout modes
# 
# LAYOUT MODES:
# 
# SIDE-BY-SIDE MODE:
#   - Output panel + Settings/Code panel displayed simultaneously
#   - Interactive resizer handle allows width adjustment
#   - Optimal for users who want to see configuration and results together
#   - Panel widths are remembered separately for settings vs code tabs
# 
# FULL-WIDTH MODE:
#   - Single panel takes full width with tab-based navigation
#   - Better for users with limited screen space
#   - Navigation buttons allow switching between output, settings, and code
# 
# DATABASE INTEGRATION:
# - Layout preferences stored in widgets_options table
# - Automatically loaded on widget initialization
# - Changes saved immediately without user intervention
# - No additional database setup required

# ======================================
# JAVASCRIPT PANEL LAYOUT SYSTEM
# ======================================

# Create the JavaScript system for managing panel layouts and resizing
panel_layout_manager <- paste0("
    // ==========================================
    // PANEL WIDTH MEMORY SYSTEM
    // ==========================================
    
    // Initialize persistent memory for panel width preferences
    if (!window.panelMemory_%widget_id%) {
        window.panelMemory_%widget_id% = {
            code: '50%',        // Default width for code editor panel
            settings: '25%'     // Default width for settings panel
        };
    }
    
    // ==========================================
    // DYNAMIC PANEL WIDTH MANAGEMENT
    // ==========================================
    
    // Global function to set panel widths based on current active tab
    window.setPanelWidths_%widget_id% = function() {
        var currentTab = Shiny.shinyapp.$inputValues['", id, "-current_tab_%widget_id%'];
        var outputDiv = $('#", id, "-output_div_%widget_id%');
        var settingsContainer = $('#", id, "-settings_container_%widget_id%');
        var outputSettingsDiv = $('#", id, "-output_settings_div_%widget_id%');
        var codeDiv = $('#", id, "-code_div_%widget_id%');
        
        // Only apply panel sizing in side-by-side mode
        var sideBySide = Shiny.shinyapp.$inputValues['", id, "-output_and_settings_side_by_side_%widget_id%'];
        if (!sideBySide) return;
        
        if (currentTab === 'code') {
            // Code mode: Output panel + Code editor side by side
            outputDiv.css('flex', '1');  // Output takes remaining space
            settingsContainer.css('flex', '0 0 ' + window.panelMemory_%widget_id%.code);
            
            // Show code editor, hide settings panel
            outputSettingsDiv.hide();
            codeDiv.show().css('flex', '1');
            
        } else if (currentTab === 'output_settings') {
            // Settings mode: Output panel + Settings panel side by side
            outputDiv.css('flex', '1');  // Output takes remaining space
            settingsContainer.css('flex', '0 0 ' + window.panelMemory_%widget_id%.settings);
            
            // Show settings panel, hide code editor
            outputSettingsDiv.show().css('flex', '1');
            codeDiv.hide();
        }
    };
    
    // Apply initial panel layout
    window.setPanelWidths_%widget_id%();
    
    // ==========================================
    // TAB CHANGE LISTENER
    // ==========================================
    
    // Listen for tab changes and adjust panel layout accordingly
    $(document).off('shiny:inputchanged.widths_%widget_id%').on('shiny:inputchanged.widths_%widget_id%', function(event) {
        if (event.name === '", id, "-current_tab_%widget_id%') {
            // Add delay to ensure DOM is ready after tab change
            setTimeout(function() { 
                window.setPanelWidths_%widget_id%(); 
            }, 100);
        }
    });
    
    // ==========================================
    // INTERACTIVE PANEL RESIZING
    // ==========================================
    
    // Initialize drag-to-resize functionality (only once per widget)
    if (!window.resizingInitialized_%widget_id%) {
        var container = document.getElementById('", id, "-output_settings_code_div_%widget_id%');
        var isResizing = false;
        var lastDownX = 0;
        
        // Get references to panels and resizer handle
        var leftPanel = container.querySelector('.left-panel');  // Output panel
        var settingsContainer = document.getElementById('", id, "-settings_container_%widget_id%');
        var resizer = container.querySelector('.resizer');  // Drag handle
        
        // Utility function to trigger window resize events for proper chart rendering
        function triggerResizeEvent() {
            var event = new Event('resize');
            window.dispatchEvent(event);
        }
        
        // Mouse event handlers for drag-to-resize functionality
        if (resizer) {
            // Start resizing when user clicks on resizer handle
            resizer.addEventListener('mousedown', function(e) {
                isResizing = true;
                lastDownX = e.clientX;
                document.addEventListener('mousemove', resizePanels);
                document.addEventListener('mouseup', stopResizing);
            });
        }
        
        // Handle panel resizing during mouse drag
        function resizePanels(e) {
            if (!isResizing) return;
            
            var containerWidth = container.offsetWidth;
            var deltaX = e.clientX - lastDownX;
            var currentTab = Shiny.shinyapp.$inputValues['", id, "-current_tab_%widget_id%'];
            
            // Calculate new panel sizes based on mouse movement
            var leftWidth = leftPanel.offsetWidth + deltaX;
            var leftPercent = (leftWidth / containerWidth) * 100;
            var rightPercent = 100 - leftPercent - 0.5; // Reserve 0.5% for resizer handle
            
            // Enforce minimum panel sizes to prevent panels from becoming unusable
            if (leftPercent > 15 && rightPercent > 15) {
                leftPanel.style.flex = '0 0 ' + leftPercent + '%';
                settingsContainer.style.flex = '0 0 ' + rightPercent + '%';
                
                // Save the width preference for the current active tab
                if (currentTab === 'code') {
                    window.panelMemory_%widget_id%.code = rightPercent + '%';
                    console.log('Saved code panel width:', rightPercent + '%');
                } else if (currentTab === 'output_settings') {
                    window.panelMemory_%widget_id%.settings = rightPercent + '%';
                    console.log('Saved settings panel width:', rightPercent + '%');
                }
            }
            
            lastDownX = e.clientX;
            triggerResizeEvent();  // Ensure charts and other components re-render properly
        }
        
        // Clean up when user stops resizing
        function stopResizing(e) {
            isResizing = false;
            document.removeEventListener('mousemove', resizePanels);
            document.removeEventListener('mouseup', stopResizing);
            triggerResizeEvent();  // Final resize event
            
            // Log final saved widths for debugging
            var currentTab = Shiny.shinyapp.$inputValues['", id, "-current_tab_%widget_id%'];
            if (currentTab === 'code') {
                console.log('Final code panel width saved:', window.panelMemory_%widget_id%.code);
            } else if (currentTab === 'output_settings') {
                console.log('Final settings panel width saved:', window.panelMemory_%widget_id%.settings);
            }
        }
        
        // Mark resizing system as initialized to prevent duplicate event listeners
        window.resizingInitialized_%widget_id% = true;
    }
")

# ======================================
# LOAD SIDE-BY-SIDE PREFERENCE FROM DATABASE
# ======================================

# Query database for saved side-by-side layout preference
sql <- glue::glue_sql(
    "SELECT value_num 
     FROM widgets_options 
     WHERE widget_id = %widget_id% AND category = 'general_settings' AND name = 'output_and_settings_side_by_side'", 
    .con = m$db
)
side_by_side_result <- DBI::dbGetQuery(m$db, sql)

# Set default or load saved preference
side_by_side <- TRUE  # Default to side-by-side mode
if (nrow(side_by_side_result) > 0) {
    saved_value <- side_by_side_result %>% dplyr::pull(value_num) %>% as.logical()
    if (!is.na(saved_value)) {
        side_by_side <- saved_value
    }
}

if (!side_by_side){
    shinyjs::delay(1000, {
        shinyjs::show("output_button_div_%widget_id%")
        shinyjs::hide("resizer_%widget_id%")
        shinyjs::hide("output_settings_code_div_%widget_id%")
    })
}

# Initialize the layout state in JavaScript
shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-output_and_settings_side_by_side_%widget_id%', ", tolower(side_by_side), ");"))

# Initialize the panel layout system with a slight delay to ensure DOM is ready
shinyjs::delay(1000, shinyjs::runjs(panel_layout_manager))

# ======================================
# SIDE-BY-SIDE TOGGLE BUTTON HANDLER
# ======================================

# Observer for handling side-by-side layout toggle button clicks
observe_event(input$side_by_side_button_%widget_id%, {
    
    # ==========================================
    # TOGGLE SIDE-BY-SIDE STATE
    # ==========================================
    
    # Get current state and toggle it
    current_side_by_side <- TRUE
    if (length(input$output_and_settings_side_by_side_%widget_id%) > 0) {
        current_side_by_side <- input$output_and_settings_side_by_side_%widget_id%
    }
    new_side_by_side <- !current_side_by_side
    
    # Update the JavaScript state
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-output_and_settings_side_by_side_%widget_id%', ", tolower(new_side_by_side), ");"))
    
    # ==========================================
    # HANDLE TAB SWITCHING LOGIC
    # ==========================================
    
    # When enabling side-by-side mode, switch to appropriate tab
    if (length(input$current_tab_%widget_id%) == 0 ||
        (input$current_tab_%widget_id% == "output" && new_side_by_side)) {
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-current_tab_%widget_id%', 'output_settings');"))
    }
    
    # When disabling side-by-side mode, switch to output-only view
    if (!new_side_by_side) {
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-current_tab_%widget_id%', 'output');"))
    }
    
    # Trigger tab change event to update UI
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-current_tab_trigger_%widget_id%', Math.random());"))
    
    # ==========================================
    # APPLY UI LAYOUT CHANGES
    # ==========================================
    
    if (new_side_by_side) {
        # Enable side-by-side mode
        shinyjs::runjs(panel_layout_manager)  # Reinitialize the layout system
        shinyjs::hide("output_button_div_%widget_id%")  # Hide output button (not needed in side-by-side)
        shinyjs::show("resizer_%widget_id%")  # Show drag-to-resize handle
        
        # Force application of correct panel widths after a short delay
        shinyjs::delay(150, shinyjs::runjs("window.setPanelWidths_%widget_id%();"))
        
    } else {
        # Enable full-width mode
        shinyjs::show("output_button_div_%widget_id%")  # Show output button for navigation
        shinyjs::hide("resizer_%widget_id%")  # Hide resize handle (not needed in full-width)
    }
    
    # ==========================================
    # PERSIST PREFERENCE TO DATABASE
    # ==========================================
    
    # Remove existing side-by-side preference
    sql_send_statement(
        m$db, 
        glue::glue_sql(
            "DELETE FROM widgets_options 
             WHERE widget_id = %widget_id% AND category = 'general_settings' AND name = 'output_and_settings_side_by_side'", 
            .con = m$db
        )
    )
    
    # Create new preference record
    new_preference <- tibble::tibble(
        id = get_last_row(m$db, "widgets_options") + 1L,
        widget_id = %widget_id%,
        person_id = NA_integer_,
        link_id = NA_integer_,
        category = "general_settings",
        name = "output_and_settings_side_by_side",
        value = NA_character_,
        value_num = as.integer(new_side_by_side),  # Store as 1 (TRUE) or 0 (FALSE)
        creator_id = m$user_id,
        datetime = now(),
        deleted = FALSE
    )
    
    # Save the new preference to database
    DBI::dbAppendTable(m$db, "widgets_options", new_preference)
})
