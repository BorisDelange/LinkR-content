# ==========================================
# Server - Figure Settings Logic
# ==========================================

# ======================================
# LOAD FIGURE SETTINGS FROM DATABASE
# ======================================

# Handler for loading saved figure settings and code
observe_event(input$load_figure_settings_%widget_id%, {
    
    # Get the selected settings file ID
    link_id <- input$settings_file_%widget_id%
    
    # Query database for all figure settings associated with this file
    sql <- glue::glue_sql(
        "SELECT name, value, value_num 
         FROM widgets_options 
         WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", 
        .con = m$db
    )
    figure_settings <- DBI::dbGetQuery(m$db, sql)
    
    code <- ""
    
    # ====================
    # UPDATE UI COMPONENTS WITH SAVED VALUES
    # ====================
    
    if (nrow(figure_settings) > 0) {
        # Process each saved setting and update corresponding UI element
        sapply(figure_settings$name, function(name) {
            
            # Extract value and numeric value for this setting
            value <- figure_settings %>% 
                dplyr::filter(name == !!name) %>% 
                dplyr::pull(value)
            value_num <- figure_settings %>% 
                dplyr::filter(name == !!name) %>% 
                dplyr::pull(value_num)
            
            # Update UI elements based on setting type
            if (name == "data_source") {
                # Update data source dropdown (person/visit_detail)
                shiny.fluent::updateDropdown.shinyInput(
                    session, 
                    paste0(name, "_%widget_id%"), 
                    value = value
                )
            } 
            else if (name == "concepts") {
                # Update concepts multi-select dropdown
                # Convert comma-separated string back to numeric vector
                value <- as.numeric(unlist(strsplit(value, ", ")))
                shiny.fluent::updateDropdown.shinyInput(
                    session, 
                    paste0(name, "_%widget_id%"), 
                    value = value
                )
            }
            else if (name == "synchronize_timelines") {
                # Update timeline synchronization toggle
                # Convert numeric value back to logical
                value <- as.logical(value_num)
                shiny.fluent::updateToggle.shinyInput(
                    session, 
                    paste0(name, "_%widget_id%"), 
                    value = value
                )
            }
            else if (name == "code") {
                # Update code editor content
                code <<- value  # Use <<- to assign to parent scope
                m$code_%widget_id% <- value
                shinyAce::updateAceEditor(session, "code_%widget_id%", value = code)
            }
        })
    }
    
    # ====================
    # AUTO-EXECUTE CODE IF ENABLED
    # ====================
    
    # Check if auto-run setting is enabled and execute code after loading
    if (length(input$run_code_at_settings_file_load_%widget_id%) > 0) {
        if (input$run_code_at_settings_file_load_%widget_id%) {
            shinyjs::delay(500, {
                shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))  
            })
        }
    }
})

# ======================================
# SAVE FIGURE SETTINGS TO DATABASE
# ======================================

# Function to save current figure settings and code
save_params_and_code_%widget_id% <- function(notification = TRUE) {
    
    # ====================
    # VALIDATE SETTINGS FILE SELECTION
    # ====================
    
    # If no settings file is selected, redirect to file management
    if (length(input$settings_file_%widget_id%) == 0) {
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_settings_files_tab_%widget_id%', Math.random());"))
    }
    
    # ====================
    # SAVE SETTINGS TO DATABASE
    # ====================
    
    if (length(input$settings_file_%widget_id%) > 0) {
        
        link_id <- input$settings_file_%widget_id%
    
        # Remove existing settings for this file to avoid duplicates
        sql_send_statement(
            m$db, 
            glue::glue_sql(
                "DELETE FROM widgets_options 
                 WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", 
                .con = m$db
            )
        )
        
        # ====================
        # PREPARE NEW SETTINGS DATA
        # ====================
        
        # Create tibble with all current settings values
        new_data <- tibble::tribble(
            ~name, ~value, ~value_num,
            "data_source", input$data_source_%widget_id%, NA_real_,
            "concepts", input$concepts_%widget_id% %>% toString(), NA_real_,  # Convert vector to comma-separated string
            "synchronize_timelines", NA_character_, as.integer(input$synchronize_timelines_%widget_id%),
            "code", input$code_%widget_id%, NA_real_
        )
        
        # Add database metadata (IDs, timestamps, etc.)
        new_data <- new_data %>%
            dplyr::transmute(
                id = get_last_row(m$db, "widgets_options") + 1:nrow(new_data), 
                widget_id = %widget_id%, 
                person_id = NA_integer_, 
                link_id = link_id,
                category = "figure_settings", 
                name, 
                value, 
                value_num, 
                creator_id = m$user_id, 
                datetime = now(), 
                deleted = FALSE
            )
        
        # Insert new settings into database
        DBI::dbAppendTable(m$db, "widgets_options", new_data)
        
        # ====================
        # USER NOTIFICATION
        # ====================
        
        # Show success message if notifications are enabled
        if (notification) {
            show_message_bar("modif_saved", "success")
        }
    }
}

# ====================
# AUTO-SAVE ON STARTUP
# ====================

# Save default settings when widget first loads (without notification)
shinyjs::delay(1000, save_params_and_code_%widget_id%(notification = FALSE))

# ====================
# SAVE BUTTON HANDLER
# ====================

# Handle manual save button clicks
observe_event(input$save_params_and_code_%widget_id%, {
    save_params_and_code_%widget_id%()
})

# ======================================
# TIMELINE SYNCHRONIZATION SYSTEM
# ======================================

# ====================
# INITIALIZE TIMELINE VARIABLES
# ====================

# Create reactive timeline variables if they don't exist
if (length(m$datetimes_timeline_%tab_id%) == 0) {
    # Main timeline reactive value
    m$datetimes_timeline_%tab_id% <- reactiveVal()
    
    # Debounced version to prevent excessive updates (500ms delay)
    m$debounced_datetimes_timeline_%tab_id% <- reactive(m$datetimes_timeline_%tab_id%()) %>% debounce(500)
}

# ====================
# HANDLE TIMELINE SYNC TOGGLE
# ====================

# Adjust chart padding when timeline synchronization is toggled
observe_event(input$synchronize_timelines_%widget_id%, {
    
    if (input$synchronize_timelines_%widget_id%) {
        # Add left padding to align with synchronized timeline
        shinyjs::runjs(sprintf(
            "document.getElementById('%s').style.paddingLeft = '80px'; var event = new Event('resize'); window.dispatchEvent(event);",
            ns("dygraph_div_%widget_id%")
        ))
    } else {
        # Remove padding when synchronization is disabled
        shinyjs::runjs(sprintf(
            "document.getElementById('%s').style.paddingLeft = '0px'; var event = new Event('resize'); window.dispatchEvent(event);",
            ns("dygraph_div_%widget_id%")
        ))
    }
})

# ====================
# CAPTURE TIMELINE CHANGES
# ====================

# Monitor chart date window changes and broadcast to other widgets
observe_event(input$dygraph_%widget_id%_date_window, {
    
    # Only process if timeline synchronization is enabled
    if (!input$synchronize_timelines_%widget_id%) return()
    
    # Convert date window from JavaScript format to R POSIXct
    datetime_values <- as.POSIXct(
        input$dygraph_%widget_id%_date_window, 
        format = "%Y-%m-%dT%H:%M:%OSZ", 
        tz = "UTC"
    )
    
    # Update the shared timeline reactive value
    m$datetimes_timeline_%tab_id%(datetime_values)
})

# ====================
# RESPOND TO EXTERNAL TIMELINE CHANGES
# ====================

# Listen for timeline changes from other synchronized widgets
observe_event(m$debounced_datetimes_timeline_%tab_id%(), {
    
    # Only process if synchronization is enabled and data is available
    if (!input$synchronize_timelines_%widget_id% || 
        length(m$debounced_datetimes_timeline_%tab_id%()) == 0 || 
        length(m$datetimes_%widget_id%) == 0) {
        return()
    }
    
    # ====================
    # CHECK FOR SIGNIFICANT TIMELINE CHANGES
    # ====================
    
    # Calculate time difference between current and synchronized timelines
    # Only trigger update if difference is greater than 5 seconds
    time_diff_start <- abs(
        as.numeric(m$debounced_datetimes_timeline_%tab_id%()[[1]]) - 
        as.numeric(m$datetimes_%widget_id%[[1]])
    )
    time_diff_end <- abs(
        as.numeric(m$debounced_datetimes_timeline_%tab_id%()[[2]]) - 
        as.numeric(m$datetimes_%widget_id%[[2]])
    )
    
    # Trigger code re-execution if timeline has changed significantly
    if (time_diff_start > 5 || time_diff_end > 5) {
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
    
}, ignoreInit = TRUE)  # Ignore initial execution to prevent unnecessary updates
