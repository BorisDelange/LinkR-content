# ==========================================
# server_user_configurations.R - User Configuration Management
# ==========================================
# 
# Manages user configuration files including creation, deletion, selection,
# and persistence of configuration preferences in the database
#
# ==========================================

# ======================================
# UI STYLING CONFIGURATION
# ======================================

# Define consistent styling for user configuration UI elements
user_configurations_ui_style <- paste0(
    "display: inline-block; color: white; max-width: 250px; border-radius: 8px; padding: 1px 5px; align-items: center;",
    "height: 18px; font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; cursor: pointer; margin: 2px 5px;"
)

# Initialize UI with "no configuration selected" state
output$user_configurations_ui_%widget_id% <- renderUI(
    div(
        i18np$t("no_user_configuration_selected"), 
        style = paste0(user_configurations_ui_style, "background-color: #606060ab;")
    )
)

# ======================================
# USER CONFIGURATION VISIBILITY CONTROL
# ======================================

# Toggle visibility of user configuration UI element based on general setting
observe_event(input$show_user_configurations_%widget_id%, {
    if (input$show_user_configurations_%widget_id%) {
        shinyjs::show("user_configurations_ui_%widget_id%")
    } else {
        shinyjs::hide("user_configurations_ui_%widget_id%")
    }
})

# Show user configurations management panel
observe_event(input$show_user_configurations_tab_%widget_id%, {
    # Hide all main content tabs
    sapply(c(paste0(tabs, "_div_%widget_id%"), "figure_settings_code_div_%widget_id%"), shinyjs::hide)
    
    # Show configuration management interface
    shinyjs::show("user_configurations_div_%widget_id%")
})

# ======================================
# CREATE USER CONFIGURATION MODAL CONTROL
# ======================================

# Show modal dialog for creating new user configuration
observe_event(input$create_user_configuration_%widget_id%, {
    shinyjs::show("add_user_configuration_modal_%widget_id%")
})

# Close create user configuration modal
observe_event(input$close_add_user_configuration_modal_%widget_id%, {
    shinyjs::hide("add_user_configuration_modal_%widget_id%")
})

# ======================================
# ADD USER CONFIGURATION FUNCTIONALITY
# ======================================

# Function to create new user configuration with validation
add_user_configuration_%widget_id% <- function(configuration_name, notification = TRUE) {
    
    # Validate configuration name
    empty_name <- TRUE
    if (length(configuration_name) > 0) {
        if (!is.na(configuration_name) && configuration_name != "") {
            empty_name <- FALSE
        }
    }
    
    if (empty_name) {
        # Show error message for empty name
        shiny.fluent::updateTextField.shinyInput(
            session, 
            "user_configuration_name_%widget_id%", 
            errorMessage = i18np$t("provide_valid_name")
        )
        return()   
    }
    
    # Clear any existing error message
    shiny.fluent::updateTextField.shinyInput(
        session, 
        "user_configuration_name_%widget_id%", 
        errorMessage = NULL
    )
    
    # Check for duplicate names
    sql <- glue::glue_sql(
        "SELECT value FROM widgets_options 
         WHERE widget_id = %widget_id% AND category = 'user_configurations' AND name = 'configuration_name'", 
        .con = m$db
    )
    configuration_names <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
    
    # Check if name already exists (case-insensitive, special chars removed)
    name_already_used <- remove_special_chars(configuration_name) %in% remove_special_chars(configuration_names)
    
    if (name_already_used) {
        # Show error message for duplicate name
        shiny.fluent::updateTextField.shinyInput(
            session, 
            "user_configuration_name_%widget_id%", 
            errorMessage = i18np$t("name_already_used")
        )
        return()
    }
    
    # Create new user configuration
    new_id <- get_last_row(m$db, "widgets_options") + 1
    
    # Prepare database record for new user configuration
    new_data <- tibble::tibble(
        id = new_id, 
        widget_id = %widget_id%, 
        person_id = NA_integer_, 
        link_id = NA_integer_,
        category = "user_configurations", 
        name = "configuration_name", 
        value = configuration_name, 
        value_num = NA_real_, 
        creator_id = m$user_id, 
        datetime = now(), 
        deleted = FALSE
    )
    
    # Insert new configuration record into database
    DBI::dbAppendTable(m$db, "widgets_options", new_data)
    
    # Update local configurations cache
    m$user_configurations_%widget_id% <- m$user_configurations_%widget_id% %>% 
        dplyr::bind_rows(tibble::tibble(id = new_id, name = configuration_name))
    
    # Clear input field
    shiny.fluent::updateTextField.shinyInput(session, "user_configuration_name_%widget_id%", value = "")
    
    # Update dropdown with new configuration and select it
    dropdown_options <- convert_tibble_to_list(m$user_configurations_%widget_id%, key_col = "id", text_col = "name")
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "user_configuration_%widget_id%", 
        options = dropdown_options, 
        value = new_id
    )
    
    # Reset code editor to empty state for new configuration
    shinyAce::updateAceEditor(session, "code_%widget_id%", value = "")
    
    # Close modal
    shinyjs::hide("add_user_configuration_modal_%widget_id%")
    
    # Show success message if notifications enabled
    if (notification) {
        show_message_bar("new_user_configuration_added", "success")
    }
}

# Auto-create default configuration when widget loads
shinyjs::delay(300, add_user_configuration_%widget_id%(i18np$t("configuration_1"), notification = FALSE))

# Handle user confirmation to create new configuration
observe_event(input$add_user_configuration_%widget_id%, {
    add_user_configuration_%widget_id%(input$user_configuration_name_%widget_id%)
})

# ======================================
# USER CONFIGURATION SELECTION HANDLING
# ======================================

# Handle user selecting a different configuration
observe_event(input$user_configuration_%widget_id%, {
    
    # Show delete button (now that a configuration is selected)
    shinyjs::show("delete_user_configuration_div_%widget_id%")

    # Get selected configuration information
    configuration_id <- input$user_configuration_%widget_id%
    configuration_name <- m$user_configurations_%widget_id% %>% 
        dplyr::filter(id == configuration_id) %>% 
        dplyr::pull(name)
    
    # Update UI to show selected configuration name with active styling
    output$user_configurations_ui_%widget_id% <- renderUI(
        div(
            configuration_name, 
            style = paste0(user_configurations_ui_style, "background-color: #FF8C42;")
        )
    )
    
    # Remove any existing selected configuration record
    sql_send_statement(
        m$db, 
        glue::glue_sql(
            "DELETE FROM widgets_options 
             WHERE widget_id = %widget_id% AND category = 'general_settings' AND name = 'selected_configuration_id'", 
            .con = m$db
        )
    )
    
    # Save new selection to database
    new_data <- tibble::tibble(
        id = get_last_row(m$db, "widgets_options") + 1, 
        widget_id = %widget_id%, 
        person_id = NA_integer_, 
        link_id = configuration_id,
        category = "general_settings", 
        name = "selected_configuration_id", 
        value = NA_character_, 
        value_num = NA_real_, 
        creator_id = m$user_id, 
        datetime = now(), 
        deleted = FALSE
    )
    DBI::dbAppendTable(m$db, "widgets_options", new_data)
    
    # Trigger loading of figure settings and code from selected configuration
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_figure_settings_%widget_id%', Math.random());"))
})

# ======================================
# DELETE USER CONFIGURATION FUNCTIONALITY
# ======================================

# Show confirmation modal for configuration deletion
observe_event(input$delete_user_configuration_%widget_id%, {
    # Only show modal if a configuration is actually selected
    if (length(input$user_configuration_%widget_id%) == 0) return()
    
    shinyjs::show("delete_user_configuration_modal_%widget_id%")
})

# Close delete confirmation modal
observe_event(input$close_configuration_deletion_modal_%widget_id%, {
    shinyjs::hide("delete_user_configuration_modal_%widget_id%")
})

# Handle confirmed configuration deletion
observe_event(input$confirm_configuration_deletion_%widget_id%, {
    
    configuration_id <- input$user_configuration_%widget_id%
    
    # Remove the user configuration record
    sql_send_statement(
        m$db, 
        glue::glue_sql("DELETE FROM widgets_options WHERE id = {configuration_id}", .con = m$db)
    )
    
    # Remove selected configuration reference from general settings
    sql_send_statement(
        m$db, 
        glue::glue_sql(
            "DELETE FROM widgets_options 
             WHERE widget_id = %widget_id% AND category = 'general_settings' AND name = 'selected_configuration_id'", 
            .con = m$db
        )
    )
    
    # Update local configurations cache
    m$user_configurations_%widget_id% <- m$user_configurations_%widget_id% %>% 
        dplyr::filter(id != configuration_id)
    
    # Update dropdown to remove deleted configuration
    dropdown_options <- convert_tibble_to_list(m$user_configurations_%widget_id%, key_col = "id", text_col = "name")
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "user_configuration_%widget_id%", 
        options = dropdown_options, 
        value = NULL
    )
    
    # Close deletion modal
    shinyjs::hide("delete_user_configuration_modal_%widget_id%")
    
    # Reset UI to "no configuration selected" state
    output$user_configurations_ui_%widget_id% <- renderUI(
        div(
            i18np$t("no_user_configuration_selected"), 
            style = paste0(user_configurations_ui_style, "background-color: #606060AB;")
        )
    )
    
    # Hide delete button (no configuration to delete)
    shinyjs::hide("delete_user_configuration_div_%widget_id%")
    
    # Show warning message about configuration deletion
    show_message_bar("user_configuration_delete", "warning")
})
