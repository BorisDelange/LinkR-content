# ==========================================
# Server - Settings Files Management
# ==========================================

# ======================================
# UI STYLING CONFIGURATION
# ======================================

# Define consistent styling for settings file UI elements
settings_files_ui_style <- paste0(
    "display: inline-block; color: white; max-width: 250px; border-radius: 8px; padding: 1px 5px; align-items: center;",
    "height: 18px; font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; cursor: pointer; margin: 2px 5px;"
)

# Initialize UI with "no file selected" state
output$settings_files_ui_%widget_id% <- renderUI(
    div(
        i18np$t("no_settings_file_selected"), 
        style = paste0(settings_files_ui_style, "background-color: #606060ab;")  # Gray background for inactive state
    )
)

# ======================================
# SETTINGS FILE VISIBILITY CONTROL
# ======================================

# Toggle visibility of settings file UI element based on general setting
observe_event(input$show_settings_file_%widget_id%, {
    if (input$show_settings_file_%widget_id%) {
        shinyjs::show("settings_files_ui_%widget_id%")
    } else {
        shinyjs::hide("settings_files_ui_%widget_id%")
    }
})

# Show settings files management panel (hide all other tabs)
observe_event(input$show_settings_files_tab_%widget_id%, {
    # Hide all main content tabs
    sapply(c(paste0(tabs, "_div_%widget_id%"), "figure_settings_code_div_%widget_id%"), shinyjs::hide)
    
    # Show file management interface
    shinyjs::show("settings_files_div_%widget_id%")
})

# ======================================
# CREATE SETTINGS FILE MODAL CONTROL
# ======================================

# Show modal dialog for creating new settings file
observe_event(input$create_settings_file_%widget_id%, {
    shinyjs::show("add_settings_file_modal_%widget_id%")
})

# Close create settings file modal
observe_event(input$close_add_settings_file_modal_%widget_id%, {
    shinyjs::hide("add_settings_file_modal_%widget_id%")
})

# ======================================
# ADD SETTINGS FILE FUNCTIONALITY
# ======================================

# Function to create new settings file with validation
add_settings_file_%widget_id% <- function(file_name, notification = TRUE) {
    
    # ====================
    # VALIDATE FILE NAME
    # ====================
    
    # Check if name is empty or invalid
    empty_name <- TRUE
    if (length(file_name) > 0) {
        if (!is.na(file_name) && file_name != "") {
            empty_name <- FALSE
        }
    }
    
    if (empty_name) {
        # Show error message for empty name
        shiny.fluent::updateTextField.shinyInput(
            session, 
            "settings_file_name_%widget_id%", 
            errorMessage = i18np$t("provide_valid_name")
        )
        return()   
    }
    
    # Clear any existing error message
    shiny.fluent::updateTextField.shinyInput(
        session, 
        "settings_file_name_%widget_id%", 
        errorMessage = NULL
    )
    
    # ====================
    # CHECK FOR DUPLICATE NAMES
    # ====================
    
    # Query existing file names for this widget
    sql <- glue::glue_sql(
        "SELECT value FROM widgets_options 
         WHERE widget_id = %widget_id% AND category = 'settings_files' AND name = 'file_name'", 
        .con = m$db
    )
    files_names <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
    
    # Check if name already exists (case-insensitive, special chars removed)
    name_already_used <- remove_special_chars(file_name) %in% remove_special_chars(files_names)
    
    if (name_already_used) {
        # Show error message for duplicate name
        shiny.fluent::updateTextField.shinyInput(
            session, 
            "settings_file_name_%widget_id%", 
            errorMessage = i18np$t("name_already_used")
        )
        return()
    }
    
    # ====================
    # CREATE NEW SETTINGS FILE
    # ====================
    
    # Generate new unique ID
    new_id <- get_last_row(m$db, "widgets_options") + 1
    
    # Prepare database record for new settings file
    new_data <- tibble::tibble(
        id = new_id, 
        widget_id = %widget_id%, 
        person_id = NA_integer_, 
        link_id = NA_integer_,
        category = "settings_files", 
        name = "file_name", 
        value = file_name, 
        value_num = NA_real_, 
        creator_id = m$user_id, 
        datetime = now(), 
        deleted = FALSE
    )
    
    # Insert new file record into database
    DBI::dbAppendTable(m$db, "widgets_options", new_data)
    
    # ====================
    # UPDATE UI STATE
    # ====================
    
    # Update local filenames cache
    m$settings_filenames_%widget_id% <- m$settings_filenames_%widget_id% %>% 
        dplyr::bind_rows(tibble::tibble(id = new_id, name = file_name))
    
    # Clear input field
    shiny.fluent::updateTextField.shinyInput(session, "settings_file_name_%widget_id%", value = "")
    
    # Update dropdown with new file and select it
    dropdown_options <- convert_tibble_to_list(m$settings_filenames_%widget_id%, key_col = "id", text_col = "name")
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "settings_file_%widget_id%", 
        options = dropdown_options, 
        value = new_id
    )
    
    # Reset code editor to empty state for new file
    shinyAce::updateAceEditor(session, "code_%widget_id%", value = "")
    
    # Close modal
    shinyjs::hide("add_settings_file_modal_%widget_id%")
    
    # ====================
    # USER NOTIFICATION
    # ====================
    
    # Show success message if notifications enabled
    if (notification) {
        show_message_bar("new_settings_file_added", "success")
    }
}

# ====================
# AUTO-CREATE DEFAULT FILE
# ====================

# Create default settings file when widget loads (without notification)
shinyjs::delay(300, add_settings_file_%widget_id%(i18np$t("settings_1"), notification = FALSE))

# ====================
# CONFIRM FILE CREATION
# ====================

# Handle user confirmation to create new file
observe_event(input$add_settings_file_%widget_id%, {
    add_settings_file_%widget_id%(input$settings_file_name_%widget_id%)
})

# ======================================
# SETTINGS FILE SELECTION HANDLING
# ======================================

# Handle user selecting a different settings file
observe_event(input$settings_file_%widget_id%, {
    
    # ====================
    # UPDATE UI ELEMENTS
    # ====================
    
    # Show delete button (now that a file is selected)
    shinyjs::show("delete_settings_file_div_%widget_id%")

    # Get selected file information
    file_id <- input$settings_file_%widget_id%
    filename <- m$settings_filenames_%widget_id% %>% 
        dplyr::filter(id == file_id) %>% 
        dplyr::pull(name)
    
    # Update UI to show selected file name with active styling
    output$settings_files_ui_%widget_id% <- renderUI(
        div(
            filename, 
            style = paste0(settings_files_ui_style, "background-color: #1d94ce;")  # Blue background for active state
        )
    )
    
    # ====================
    # PERSIST SELECTION TO DATABASE
    # ====================
    
    # Remove any existing selected file record
    sql_send_statement(
        m$db, 
        glue::glue_sql(
            "DELETE FROM widgets_options 
             WHERE widget_id = %widget_id% AND category = 'general_settings' AND name = 'selected_file_id'", 
            .con = m$db
        )
    )
    
    # Save new selection to database
    new_data <- tibble::tibble(
        id = get_last_row(m$db, "widgets_options") + 1, 
        widget_id = %widget_id%, 
        person_id = NA_integer_, 
        link_id = file_id,  # Reference to selected settings file
        category = "general_settings", 
        name = "selected_file_id", 
        value = NA_character_, 
        value_num = NA_real_, 
        creator_id = m$user_id, 
        datetime = now(), 
        deleted = FALSE
    )
    DBI::dbAppendTable(m$db, "widgets_options", new_data)
    
    # ====================
    # LOAD SETTINGS FROM FILE
    # ====================
    
    # Trigger loading of figure settings and code from selected file
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_figure_settings_%widget_id%', Math.random());"))
})

# ======================================
# DELETE SETTINGS FILE FUNCTIONALITY
# ======================================

# Show confirmation modal for file deletion
observe_event(input$delete_settings_file_%widget_id%, {
    # Only show modal if a file is actually selected
    if (length(input$settings_file_%widget_id%) == 0) return()
    
    shinyjs::show("delete_settings_file_modal_%widget_id%")
})

# Close delete confirmation modal
observe_event(input$close_file_deletion_modal_%widget_id%, {
    shinyjs::hide("delete_settings_file_modal_%widget_id%")
})

# Handle confirmed file deletion
observe_event(input$confirm_file_deletion_%widget_id%, {
    
    file_id <- input$settings_file_%widget_id%
    
    # ====================
    # DELETE FROM DATABASE
    # ====================
    
    # Remove the settings file record
    sql_send_statement(
        m$db, 
        glue::glue_sql("DELETE FROM widgets_options WHERE id = {file_id}", .con = m$db)
    )
    
    # Remove selected file reference from general settings
    sql_send_statement(
        m$db, 
        glue::glue_sql(
            "DELETE FROM widgets_options 
             WHERE widget_id = %widget_id% AND category = 'general_settings' AND name = 'selected_file_id'", 
            .con = m$db
        )
    )
    
    # ====================
    # UPDATE UI STATE
    # ====================
    
    # Update local filenames cache
    m$settings_filenames_%widget_id% <- m$settings_filenames_%widget_id% %>% 
        dplyr::filter(id != file_id)
    
    # Update dropdown to remove deleted file
    dropdown_options <- convert_tibble_to_list(m$settings_filenames_%widget_id%, key_col = "id", text_col = "name")
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "settings_file_%widget_id%", 
        options = dropdown_options, 
        value = NULL  # No file selected
    )
    
    # Close deletion modal
    shinyjs::hide("delete_settings_file_modal_%widget_id%")
    
    # Reset UI to "no file selected" state
    output$settings_files_ui_%widget_id% <- renderUI(
        div(
            i18np$t("no_settings_file_selected"), 
            style = paste0(settings_files_ui_style, "background-color: #606060ab;")
        )
    )
    
    # Hide delete button (no file to delete)
    shinyjs::hide("delete_settings_file_div_%widget_id%")
    
    # ====================
    # USER NOTIFICATION
    # ====================
    
    # Show warning message about file deletion
    show_message_bar("settings_file_delete", "warning")
})
