# ==========================================
# Server - General Settings Logic
# ==========================================

# ======================================
# SIDE-BY-SIDE LAYOUT MANAGEMENT
# ======================================

# Handler for toggling between side-by-side and full-width modes
observe_event(input$figure_and_settings_side_by_side_%widget_id%, {
    
    # ====================
    # ENABLE SIDE-BY-SIDE MODE
    # ====================
    if (input$figure_and_settings_side_by_side_%widget_id%) {
        
        # Complex JavaScript for advanced panel management with width memory
        shinyjs::runjs(paste0("
            // ====================
            // INITIALIZE WIDTH MEMORY SYSTEM
            // ====================
            // Store panel widths to maintain user preferences when switching tabs
            if (!window.panelMemory_%widget_id%) {
                window.panelMemory_%widget_id% = {
                    code: '50%',        // Default width for code panel
                    settings: '20%'     // Default width for settings panel
                };
            }
            
            // ====================
            // DYNAMIC PANEL WIDTH MANAGEMENT
            // ====================
            // Function to apply appropriate panel widths based on active tab
            function setPanelWidths_%widget_id%() {
                var currentTab = Shiny.shinyapp.$inputValues['", id, "-current_tab_%widget_id%'];
                var figureDiv = $('#", id, "-figure_div_%widget_id%');
                var figureSettingsDiv = $('#", id, "-figure_settings_div_%widget_id%');
                var codeDiv = $('#", id, "-code_div_%widget_id%');
                
                if (currentTab === 'code') {
                    // Code mode: Figure + Code editor side by side
                    figureDiv.css('flex', '1');  // Figure takes remaining space
                    codeDiv.css('flex', '0 0 ' + window.panelMemory_%widget_id%.code);
                    figureSettingsDiv.css('flex', '0 0 ' + window.panelMemory_%widget_id%.settings);
                } else if (currentTab === 'figure_settings') {
                    // Settings mode: Figure + Settings panel side by side
                    figureDiv.css('flex', '1');  // Figure takes remaining space
                    figureSettingsDiv.css('flex', '0 0 ' + window.panelMemory_%widget_id%.settings);
                    codeDiv.css('flex', '0 0 ' + window.panelMemory_%widget_id%.code);
                }
            }
            
            // Apply initial panel widths
            setPanelWidths_%widget_id%();
            
            // ====================
            // TAB CHANGE LISTENER
            // ====================
            // Automatically adjust panel widths when user switches tabs
            $(document).off('shiny:inputchanged.widths_%widget_id%').on('shiny:inputchanged.widths_%widget_id%', function(event) {
                if (event.name === '", id, "-current_tab_%widget_id%') {
                    setPanelWidths_%widget_id%();
                }
            });
            
            // ====================
            // INTERACTIVE PANEL RESIZING
            // ====================
            // Enable drag-to-resize functionality between figure and side panels
            if (!window.resizingInitialized_%widget_id%) {
                var container = document.getElementById('", id, "-figure_settings_code_div_%widget_id%');
                var isResizing = false;
                var lastDownX = 0;
                
                // Get references to all panels and resizer
                var leftPanel = container.querySelector('.left-panel');  // Figure panel
                var figureSettingsPanel = document.getElementById('", id, "-figure_settings_div_%widget_id%');
                var codePanel = document.getElementById('", id, "-code_div_%widget_id%');
                var resizer = container.querySelector('.resizer');  // Drag handle
                
                // Utility function to trigger resize events for proper chart rendering
                function triggerResizeEvent() {
                    var event = new Event('resize');
                    window.dispatchEvent(event);
                }
                
                // ====================
                // MOUSE RESIZE HANDLERS
                // ====================
                
                // Start resizing on mouse down
                resizer.addEventListener('mousedown', function(e) {
                    isResizing = true;
                    lastDownX = e.clientX;
                    document.addEventListener('mousemove', resizePanels);
                    document.addEventListener('mouseup', stopResizing);
                });
                
                // Handle panel resizing during mouse drag
                function resizePanels(e) {
                    if (!isResizing) return;
                    
                    var containerWidth = container.offsetWidth;
                    var deltaX = e.clientX - lastDownX;
                    var currentTab = Shiny.shinyapp.$inputValues['", id, "-current_tab_%widget_id%'];
                    
                    // Calculate new panel sizes
                    var leftWidth = leftPanel.offsetWidth + deltaX;
                    var leftPercent = (leftWidth / containerWidth) * 100;
                    var rightPercent = 100 - leftPercent - 0.5; // 0.5% reserved for resizer
                    
                    // Enforce minimum panel sizes (10% each)
                    if (leftPercent > 10 && rightPercent > 10) {
                        leftPanel.style.flex = '0 0 ' + leftPercent + '%';
                        
                        // Apply resize to appropriate right panel based on current tab
                        if (currentTab === 'code') {
                            codePanel.style.flex = '0 0 ' + rightPercent + '%';
                            // Remember code panel width for future tab switches
                            window.panelMemory_%widget_id%.code = rightPercent + '%';
                        } else {
                            figureSettingsPanel.style.flex = '0 0 ' + rightPercent + '%';
                            // Remember settings panel width for future tab switches
                            window.panelMemory_%widget_id%.settings = rightPercent + '%';
                        }
                    }
                    
                    lastDownX = e.clientX;
                    triggerResizeEvent();  // Ensure charts re-render properly
                }
                
                // Stop resizing on mouse up
                function stopResizing(e) {
                    isResizing = false;
                    document.removeEventListener('mousemove', resizePanels);
                    document.removeEventListener('mouseup', stopResizing);
                    triggerResizeEvent();  // Final resize event
                }
                
                // Mark resizing as initialized to prevent duplicate event listeners
                window.resizingInitialized_%widget_id% = true;
            }
        "))
        
        # ====================
        # UI ELEMENT VISIBILITY CHANGES
        # ====================
        # Hide figure button (not needed in side-by-side mode)
        shinyjs::hide("figure_button_div_%widget_id%")
        # Show resizer handle for drag-to-resize functionality
        shinyjs::show("resizer_%widget_id%")
    }
    
    # ====================
    # DISABLE SIDE-BY-SIDE MODE (FULL-WIDTH)
    # ====================
    else {
        # Reset all panels to default flex values
        shinyjs::runjs(paste0("
            // Get current active tab
            var currentTab = Shiny.shinyapp.$inputValues['", id, "-current_tab_%widget_id%'];
            var figureDiv = $('#", id, "-figure_div_%widget_id%');
            var figureSettingsDiv = $('#", id, "-figure_settings_div_%widget_id%');
            var codeDiv = $('#", id, "-code_div_%widget_id%');
            
            // Smart reset based on active tab
            if (currentTab === 'code') {
                // Full-width code mode
                figureDiv.css('flex', '0');              // Hide figure
                codeDiv.css('flex', '1');                // Code takes full width
                figureSettingsDiv.css('flex', '0');      // Hide settings
            } else if (currentTab === 'figure_settings') {
                // Full-width settings mode  
                figureDiv.css('flex', '0');              // Hide figure
                figureSettingsDiv.css('flex', '1');      // Settings takes full width
                codeDiv.css('flex', '0');                // Hide code
            } else if (currentTab === 'figure') {
                // Full-width figure mode
                figureDiv.css('flex', '1');              // Figure takes full width
                figureSettingsDiv.css('flex', '0');      // Hide settings
                codeDiv.css('flex', '0');                // Hide code
            } else {
                // Default values for other tabs
                figureDiv.css('flex', '1');
                figureSettingsDiv.css('flex', '0 0 20%');
                codeDiv.css('flex', '0 0 50%');
            }
        "))
        
        # ====================
        # UI ELEMENT VISIBILITY CHANGES
        # ====================
        # Show figure button (needed for navigation in full-width mode)
        shinyjs::show("figure_button_div_%widget_id%")
        # Hide resizer handle (not needed in full-width mode)
        shinyjs::hide("resizer_%widget_id%")
    }
})

# ======================================
# GENERAL SETTINGS PERSISTENCE
# ======================================

# Handler for saving general application settings to database
observe_event(input$save_general_settings_%widget_id%, {
    
    # ====================
    # CLEAR EXISTING SETTINGS
    # ====================
    # Remove old general settings but preserve selected file ID
    sql_send_statement(
        m$db, 
        glue::glue_sql(
            "DELETE FROM widgets_options 
             WHERE widget_id = %widget_id% AND category = 'general_settings' AND name != 'selected_file_id'", 
            .con = m$db
        )
    )
    
    # ====================
    # PREPARE NEW SETTINGS DATA
    # ====================
    
    file_id <- input$settings_file_%widget_id%
    new_data <- tibble::tibble(name = character(), value = character(), value_num = integer())
    
    # Define all general settings that need to be saved
    general_settings_vec <- c(
        "show_settings_file", 
        "figure_and_settings_side_by_side", 
        "run_code_on_data_update", 
        "run_code_at_settings_file_load"
    )
    
    # ====================
    # PROCESS EACH TOGGLE SETTING
    # ====================
    
    sapply(general_settings_vec, function(name) {
        # Default to FALSE (0) if toggle not found or unchecked
        toggle_value <- 0L
        input_name <- paste0(name, "_%widget_id%")
        
        # Check if input exists and is TRUE
        if (length(input[[input_name]]) > 0) {
            if (input[[input_name]]) {
                toggle_value <- 1L
            }
        }
        
        # Add this setting to the data collection
        new_data <<- new_data %>% 
            dplyr::bind_rows(
                tibble::tibble(
                    name = name, 
                    value = NA_character_, 
                    value_num = toggle_value
                )
            )
    })
    
    # ====================
    # ADD DATABASE METADATA
    # ====================
    
    # Transform data with required database fields
    new_data <- new_data %>%
        dplyr::transmute(
            id = get_last_row(m$db, "widgets_options") + 1:length(general_settings_vec), 
            widget_id = %widget_id%, 
            person_id = NA_integer_, 
            link_id = NA_integer_,  # General settings not linked to specific files
            category = "general_settings", 
            name, 
            value, 
            value_num, 
            creator_id = m$user_id, 
            datetime = now(), 
            deleted = FALSE
        )
    
    # ====================
    # SAVE TO DATABASE
    # ====================
    
    # Insert new settings into database
    DBI::dbAppendTable(m$db, "widgets_options", new_data)
})
