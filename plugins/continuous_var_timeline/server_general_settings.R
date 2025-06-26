# Server - General settings

# Settings / editor side-by-side with figure

# Side-by-side mode management
observe_event(input$figure_and_settings_side_by_side_%widget_id%, {
    
    if (input$figure_and_settings_side_by_side_%widget_id%){
        # Enable side-by-side mode with width memorization
        shinyjs::runjs(paste0("
            // Initialize width memory for each tab
            if (!window.panelMemory_%widget_id%) {
                window.panelMemory_%widget_id% = {
                    code: '50%',        // Default width for code panel
                    settings: '20%'     // Default width for settings panel
                };
            }
            
            // Function to set panel widths based on active tab
            function setPanelWidths_%widget_id%() {
                var currentTab = Shiny.shinyapp.$inputValues['", id, "-current_tab_%widget_id%'];
                var figureDiv = $('#", id, "-figure_div_%widget_id%');
                var figureSettingsDiv = $('#", id, "-figure_settings_div_%widget_id%');
                var codeDiv = $('#", id, "-code_div_%widget_id%');
                
                if (currentTab === 'code') {
                    // Code mode: Apply memorized width for code panel
                    figureDiv.css('flex', '1');
                    codeDiv.css('flex', '0 0 ' + window.panelMemory_%widget_id%.code);
                    figureSettingsDiv.css('flex', '0 0 ' + window.panelMemory_%widget_id%.settings);
                } else if (currentTab === 'figure_settings') {
                    // Settings mode: Apply memorized width for settings panel
                    figureDiv.css('flex', '1');
                    figureSettingsDiv.css('flex', '0 0 ' + window.panelMemory_%widget_id%.settings);
                    codeDiv.css('flex', '0 0 ' + window.panelMemory_%widget_id%.code);
                }
            }
            
            // Apply initial widths
            setPanelWidths_%widget_id%();
            
            // Listen for tab changes
            $(document).off('shiny:inputchanged.widths_%widget_id%').on('shiny:inputchanged.widths_%widget_id%', function(event) {
                if (event.name === '", id, "-current_tab_%widget_id%') {
                    setPanelWidths_%widget_id%();
                }
            });
            
            // Initialize resizing functionality
            if (!window.resizingInitialized_%widget_id%) {
                var container = document.getElementById('", id, "-figure_settings_code_div_%widget_id%');
                var isResizing = false;
                var lastDownX = 0;
                
                var leftPanel = container.querySelector('.left-panel');
                var figureSettingsPanel = document.getElementById('", id, "-figure_settings_div_%widget_id%');
                var codePanel = document.getElementById('", id, "-code_div_%widget_id%');
                var resizer = container.querySelector('.resizer');
                
                function triggerResizeEvent() {
                    var event = new Event('resize');
                    window.dispatchEvent(event);
                }
                
                resizer.addEventListener('mousedown', function(e) {
                    isResizing = true;
                    lastDownX = e.clientX;
                    document.addEventListener('mousemove', resizePanels);
                    document.addEventListener('mouseup', stopResizing);
                });
                
                function resizePanels(e) {
                    if (!isResizing) return;
                    
                    var containerWidth = container.offsetWidth;
                    var deltaX = e.clientX - lastDownX;
                    var currentTab = Shiny.shinyapp.$inputValues['", id, "-current_tab_%widget_id%'];
                    
                    var leftWidth = leftPanel.offsetWidth + deltaX;
                    var leftPercent = (leftWidth / containerWidth) * 100;
                    var rightPercent = 100 - leftPercent - 0.5; // 0.5% for resizer
                    
                    if (leftPercent > 10 && rightPercent > 10) {
                        leftPanel.style.flex = '0 0 ' + leftPercent + '%';
                        
                        if (currentTab === 'code') {
                            codePanel.style.flex = '0 0 ' + rightPercent + '%';
                            // Store the new width in memory for code panel
                            window.panelMemory_%widget_id%.code = rightPercent + '%';
                        } else {
                            figureSettingsPanel.style.flex = '0 0 ' + rightPercent + '%';
                            // Store the new width in memory for settings panel
                            window.panelMemory_%widget_id%.settings = rightPercent + '%';
                        }
                    }
                    
                    lastDownX = e.clientX;
                    triggerResizeEvent();
                }
                
                function stopResizing(e) {
                    isResizing = false;
                    document.removeEventListener('mousemove', resizePanels);
                    document.removeEventListener('mouseup', stopResizing);
                    triggerResizeEvent();
                }
                
                window.resizingInitialized_%widget_id% = true;
            }
        "))
        
        shinyjs::hide("figure_button_div_%widget_id%")
        shinyjs::show("resizer_%widget_id%")
    }
    else {
        # Disable side-by-side mode
        shinyjs::runjs(paste0("
            $('#", id, "-figure_div_%widget_id%').css('flex', '1');
            $('#", id, "-figure_settings_div_%widget_id%').css('flex', '0 0 20%');
            $('#", id, "-code_div_%widget_id%').css('flex', '0 0 50%');
        "))
        
        shinyjs::show("figure_button_div_%widget_id%")
        shinyjs::hide("resizer_%widget_id%")
    }
})

# Save general settings in db

observe_event(input$save_general_settings_%widget_id%, {
    
    # Delete old rows
    sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND category = 'general_settings' AND name != 'selected_file_id'", .con = m$db))
    
    file_id <- input$settings_file_%widget_id%
    new_data <- tibble::tibble(name = character(), value = character(), value_num = integer())
    
    general_settings_vec <- c("show_settings_file", "figure_and_settings_side_by_side", "run_code_on_data_update", "run_code_at_settings_file_load")
    
    sapply(general_settings_vec, function(name){
        toggle_value <- 0L
        input_name <- paste0(name, "_%widget_id%")
        if (length(input[[input_name]]) > 0) if (input[[input_name]]) toggle_value <- 1L
        new_data <<- new_data %>% dplyr::bind_rows(tibble::tibble(name = name, value = NA_character_, value_num = toggle_value))
    })
    
    new_data <-
        new_data %>%
        dplyr::transmute(
            id = get_last_row(m$db, "widgets_options") + 1:(length(general_settings_vec)), widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
            category = "general_settings", name, value, value_num, creator_id = m$user_id, datetime = now(), deleted = FALSE
        )
    
    DBI::dbAppendTable(m$db, "widgets_options", new_data)
})
