# ==========================================
# server_user_configurations.R - User Configuration Management
# ==========================================
# 
# Manages user configuration files including creation, deletion, selection, renaming,
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
    sapply(c(paste0(tabs, "_div_%widget_id%"), "output_settings_code_div_%widget_id%"), shinyjs::hide)
    
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
# RENAME USER CONFIGURATION MODAL CONTROL
# ======================================

# Show modal dialog for renaming existing user configuration
observe_event(input$rename_user_configuration_%widget_id%, {
    # Get current configuration name to pre-fill the text field
    configuration_id <- input$user_configuration_%widget_id%
    configuration_name <- m$user_configurations_%widget_id% %>% 
        dplyr::filter(id == configuration_id) %>% 
        dplyr::pull(name)
    
    # Pre-fill the rename text field with current name
    shiny.fluent::updateTextField.shinyInput(
        session, 
        "user_configuration_name_rename_%widget_id%", 
        value = configuration_name
    )
    
    # Show rename modal
    shinyjs::show("rename_user_configuration_modal_%widget_id%")
})

# Close rename user configuration modal
observe_event(input$close_rename_user_configuration_modal_%widget_id%, {
    shinyjs::hide("rename_user_configuration_modal_%widget_id%")
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
            "user_configuration_name_add_%widget_id%", 
            errorMessage = i18np$t("provide_valid_name")
        )
        return()   
    }
    
    # Clear any existing error message
    shiny.fluent::updateTextField.shinyInput(
        session, 
        "user_configuration_name_add_%widget_id%", 
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
            "user_configuration_name_add_%widget_id%", 
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
    shiny.fluent::updateTextField.shinyInput(session, "user_configuration_name_add_%widget_id%", value = "")
    
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
shinyjs::delay(500, add_user_configuration_%widget_id%(i18np$t("configuration_1"), notification = FALSE))

# Handle user confirmation to create new configuration
observe_event(input$add_user_configuration_%widget_id%, {
    add_user_configuration_%widget_id%(input$user_configuration_name_add_%widget_id%)
})

# ======================================
# RENAME USER CONFIGURATION FUNCTIONALITY
# ======================================

# Handle user confirmation to save renamed configuration
observe_event(input$save_user_configuration_rename_%widget_id%, {
    
    new_configuration_name <- input$user_configuration_name_rename_%widget_id%
    
    # Validate configuration name
    empty_name <- TRUE
    if (length(new_configuration_name) > 0) {
        if (!is.na(new_configuration_name) && new_configuration_name != "") {
            empty_name <- FALSE
        }
    }
    
    if (empty_name) {
        # Show error message for empty name
        shiny.fluent::updateTextField.shinyInput(
            session, 
            "user_configuration_name_rename_%widget_id%", 
            errorMessage = i18np$t("provide_valid_name")
        )
        return()   
    }
    
    # Clear any existing error message
    shiny.fluent::updateTextField.shinyInput(
        session, 
        "user_configuration_name_rename_%widget_id%", 
        errorMessage = NULL
    )
    
    # Get current configuration ID
    configuration_id <- input$user_configuration_%widget_id%
    
    # Check for duplicate names (excluding current configuration)
    sql <- glue::glue_sql(
        "SELECT value FROM widgets_options 
         WHERE widget_id = %widget_id% AND category = 'user_configurations' 
         AND name = 'configuration_name' AND id != {configuration_id}", 
        .con = m$db
    )
    other_configuration_names <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
    
    # Check if name already exists (case-insensitive, special chars removed)
    name_already_used <- remove_special_chars(new_configuration_name) %in% remove_special_chars(other_configuration_names)
    
    if (name_already_used) {
        # Show error message for duplicate name
        shiny.fluent::updateTextField.shinyInput(
            session, 
            "user_configuration_name_rename_%widget_id%", 
            errorMessage = i18np$t("name_already_used")
        )
        return()
    }
    
    # Update configuration name in database
    sql_send_statement(
        m$db, 
        glue::glue_sql(
            "UPDATE widgets_options 
             SET value = {new_configuration_name}, datetime = {now()} 
             WHERE id = {configuration_id}", 
            .con = m$db
        )
    )
    
    # Update local configurations cache
    m$user_configurations_%widget_id% <- m$user_configurations_%widget_id% %>% 
        dplyr::mutate(name = ifelse(id == configuration_id, new_configuration_name, name))
    
    # Update dropdown options with new name
    dropdown_options <- convert_tibble_to_list(m$user_configurations_%widget_id%, key_col = "id", text_col = "name")
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "user_configuration_%widget_id%", 
        options = dropdown_options, 
        value = configuration_id  # Keep same configuration selected
    )
    
    # Update UI to show new configuration name with active styling
    output$user_configurations_ui_%widget_id% <- renderUI(
        div(
            new_configuration_name, 
            style = paste0(user_configurations_ui_style, "background-color: #FF8C42;")
        )
    )
    
    # Close modal
    shinyjs::hide("rename_user_configuration_modal_%widget_id%")
    
    # Show success message
    show_message_bar("user_configuration_renamed", "success")
}, ignoreInit = TRUE)

# ======================================
# USER CONFIGURATION SELECTION HANDLING
# ======================================

# Handle user selecting a different configuration
observe_event(input$user_configuration_%widget_id%, {
    
    # Show rename and delete buttons (now that a configuration is selected)
    sapply(c("rename_user_configuration_div_%widget_id%", "delete_user_configuration_div_%widget_id%"), shinyjs::show)

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
    
    # Trigger loading of output settings and code from selected configuration
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_configuration_%widget_id%', Math.random());"))
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
    
    # Hide rename and delete buttons (no configuration to modify)
    sapply(c("rename_user_configuration_div_%widget_id%", "delete_user_configuration_div_%widget_id%"), shinyjs::hide)
    
    # Show warning message about configuration deletion
    show_message_bar("user_configuration_delete", "warning")
})

# ======================================
# CONFIGURATION LOADING FROM DATABASE
# ======================================

# Handler for loading saved output settings and code from selected user configuration
observe_event(input$load_configuration_%widget_id%, {
    
    # Get the selected user configuration ID
    link_id <- input$user_configuration_%widget_id%
    
    # Query database for all output settings associated with this configuration
    sql <- glue::glue_sql(
        "SELECT name, value, value_num 
         FROM widgets_options 
         WHERE widget_id = %widget_id% AND category = 'output_settings' AND link_id = {link_id}", 
        .con = m$db
    )
    output_settings <- DBI::dbGetQuery(m$db, sql)
    
    code <- ""
    has_saved_concepts <- FALSE
    auto_update <- FALSE
    
    # Update UI components with saved values
    if (nrow(output_settings) > 0) {
        # Process each saved setting and update corresponding UI element
        sapply(output_settings$name, function(name) {
            
            # Extract value and numeric value for this setting
            value <- output_settings %>% 
                dplyr::filter(name == !!name) %>% 
                dplyr::pull(value)
            value_num <- output_settings %>% 
                dplyr::filter(name == !!name) %>% 
                dplyr::pull(value_num)
            
            # Update UI elements based on setting type
            if (name %in% c("data_source", "chart_type", "concepts_choice")) {
                # Update dropdown selections
                shiny.fluent::updateDropdown.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            } 
            else if (name == "concepts") {
                # Update concepts multi-select dropdown
                # Convert comma-separated string back to numeric vector
                value <- as.numeric(unlist(strsplit(value, ", ")))
                
                # Check if we have valid saved concepts
                if (length(value) > 0 && !any(is.na(value))) {
                    has_saved_concepts <<- TRUE
                    shiny.fluent::updateDropdown.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
                } else {
                    # No valid saved concepts - will use all available concepts
                    has_saved_concepts <<- FALSE
                }
            }
            else if (name == "concept_classes") {
                # Update concept classes multi-select dropdown
                # Convert comma-separated string back to character vector
                value <- unlist(strsplit(value, ", "))
                shiny.fluent::updateDropdown.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            }
            else if (name %in% c("synchronize_timelines", "automatically_update_output")) {
                # Update toggle switches
                # Convert numeric value back to logical
                value <- as.logical(value_num)
                shiny.fluent::updateToggle.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
                if (name == "automatically_update_output" && !is.na(value)) auto_update <<- value
            }
            else if (name == "code") {
                # Update code editor content
                code <<- value
                m$code_%widget_id% <- value
                shinyAce::updateAceEditor(session, "code_%widget_id%", value = code)
            }
        })
    }
    # No saved configuration found - trigger output display with default settings
    else shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-display_output_%widget_id%', Math.random());"))
    
    # Handle case where no saved concepts exist - select all
    if (!has_saved_concepts) {
        # Select all available concepts
        all_concepts <- selected_concepts$concept_id
        
        if (length(all_concepts) > 0) {
            concept_domains <- selected_concepts %>%
                dplyr::filter(concept_id %in% all_concepts) %>%  # Use all_concepts instead of concepts_to_check
                dplyr::pull(domain_id) %>%
                unique()
        }
    }
    
    # Auto-execute code if enabled
    if (auto_update) {
        shinyjs::delay(500, {
            shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))  
        })
    }
})

# ======================================
# CONFIGURATION SAVING TO DATABASE
# ======================================

# Observer for saving current output settings and code to selected user configuration
observe_event(input$save_output_settings_and_code_trigger_%widget_id%, {
    
    # Validate user configuration selection
    if (length(input$user_configuration_%widget_id%) == 0) {
        # If no configuration is selected, redirect to configuration management
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_user_configurations_tab_%widget_id%', Math.random());"))
        return()
    }
    
    link_id <- input$user_configuration_%widget_id%

    # Remove existing settings for this configuration to avoid duplicates
    sql_send_statement(
        m$db, 
        glue::glue_sql(
            "DELETE FROM widgets_options 
             WHERE widget_id = %widget_id% AND category = 'output_settings' AND link_id = {link_id}", 
            .con = m$db
        )
    )
    
    # Prepare new settings data
    new_data <- tibble::tribble(
        ~name, ~value, ~value_num,
        "data_source", input$data_source_%widget_id%, NA_real_,
        "chart_type", input$chart_type_%widget_id%, NA_real_,
        "concepts_choice", input$concepts_choice_%widget_id%, NA_real_,
        "concepts", input$concepts_%widget_id% %>% toString(), NA_real_,
        "concept_classes", input$concept_classes_%widget_id% %>% toString(), NA_real_,
        "synchronize_timelines", NA_character_, as.integer(input$synchronize_timelines_%widget_id%),
        "automatically_update_output", NA_character_, as.integer(input$automatically_update_output_%widget_id%),
        "code", input$code_%widget_id%, NA_real_
    )
    
    # Add database metadata
    new_data <- new_data %>%
        dplyr::transmute(
            id = get_last_row(m$db, "widgets_options") + 1:nrow(new_data), 
            widget_id = %widget_id%, 
            person_id = NA_integer_, 
            link_id = link_id,
            category = "output_settings", 
            name, 
            value, 
            value_num, 
            creator_id = m$user_id, 
            datetime = now(), 
            deleted = FALSE
        )
    
    # Insert new settings into database
    DBI::dbAppendTable(m$db, "widgets_options", new_data)
})

# ======================================
# SAVE TRIGGERS
# ======================================

# Handle manual save button clicks
observe_event(input$save_output_settings_and_code_%widget_id%, {
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_output_settings_and_code_trigger_%widget_id%', Math.random());"))
    
    # Notify user
    show_message_bar("modif_saved", "success")
})
