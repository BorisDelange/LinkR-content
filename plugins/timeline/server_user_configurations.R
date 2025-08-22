# ==========================================
# server_user_configurations.R - User Configurations Server Logic
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

# PLUGIN TEMPLATE - USER CONFIGURATIONS SERVER FILE
# 
# This file handles the server-side logic for user configuration management.
# It provides comprehensive functionality for creating, selecting, renaming, 
# deleting, persisting, loading, and saving user configuration presets, 
# allowing users to save and quickly switch between different analysis scenarios.
# 
# WHEN CREATING A NEW PLUGIN WITH THIS TEMPLATE:
# - This file should work without modification for most plugins
# - The configuration system automatically integrates with your plugin's settings
# - Database operations are handled automatically by the template framework
# - Validation and UI styling are already implemented
# - No customization typically needed unless adding special configuration features
# 
# CORE FUNCTIONALITY:
# - Create new configuration presets with validation
# - Select and load existing configurations from database
# - Rename configurations with validation and duplicate checking
# - Delete configurations with confirmation dialogs
# - Automatic database persistence of all configuration operations
# - Save current widget settings to selected configuration
# - Load saved configuration settings and apply to UI components
# - Manual and automatic save triggers for configuration updates
# - UI state management and visual feedback
# - Integration with the main widget's settings system
# 
# USER WORKFLOW:
# 1. User configures widget settings in output_settings panel
# 2. User creates a new configuration to save current settings
# 3. User can switch between configurations via dropdown
# 4. Selected configurations are automatically loaded from database
# 5. User can rename configurations to better organize their presets
# 6. User can manually save changes to current configuration
# 7. User can delete configurations they no longer need
# 8. All settings persist across sessions via database storage
# 
# DATABASE INTEGRATION:
# - All operations automatically saved to widgets_options table
# - Configurations linked to specific widgets via widget_id
# - Settings persistence handled by automated save/load mechanisms
# - Configuration loading retrieves all saved widget settings
# - Configuration saving stores current UI state to database
# - Configuration renaming updates database records in real-time
# - Save triggers handle both manual and automatic persistence
# - No additional setup required - works out of the box
# 
# CONFIGURATION PERSISTENCE FEATURES:
# - Automatic loading of saved settings when configuration selected
# - Smart defaults applied when no saved configuration exists
# - Support for multiple setting types (dropdowns, toggles, text, code)
# - Manual save triggers via user action
# - Automatic save capabilities for real-time persistence
# - Cross-session configuration availability
# - Configuration name validation and duplicate prevention
# - Seamless renaming with immediate UI updates

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
    
    # Focus on the text field after a small delay to ensure modal is visible
    shinyjs::delay(50, {
        shinyjs::runjs(paste0("document.getElementById('", id, "-user_configuration_name_add_%widget_id%').focus();"))
    })
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
shinyjs::delay(1000, add_user_configuration_%widget_id%(i18np$t("configuration_1"), notification = FALSE))

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

# Load saved configuration settings from database
observe_event(input$load_configuration_%widget_id%, {
    
    # Get selected configuration ID
    configuration_id <- input$user_configuration_%widget_id%
    
    if (is.null(configuration_id)) {
        return()
    }
    
    # Query database for saved settings
    sql <- glue::glue_sql(
        "SELECT name, value, value_num 
         FROM widgets_options 
         WHERE widget_id = %widget_id% AND category = 'output_settings' AND link_id = {configuration_id}", 
        .con = m$db
    )
    saved_settings <- DBI::dbGetQuery(m$db, sql)
    
    # Initialize tracking variables
    auto_update <- FALSE
    loaded_input_ids <- character(0)
    
    # Apply saved settings to UI using automated logic
    if (nrow(saved_settings) > 0) {
        
        # Process each saved setting
        for (i in 1:nrow(saved_settings)) {
            setting_name <- saved_settings$name[i]
            setting_value <- saved_settings$value[i]
            setting_value_num <- saved_settings$value_num[i]
            
            # Find matching input configuration
            input_config <- Filter(function(x) x$id == setting_name, all_inputs_%widget_id%)
            
            if (length(input_config) > 0) {
                input_def <- input_config[[1]]
                input_id <- paste0(setting_name, "_%widget_id%")
                
                # Track which inputs were loaded from saved settings
                loaded_input_ids <- c(loaded_input_ids, setting_name)
                
                # Apply setting based on input type
                switch(
                    input_def$type,
                    "dropdown" = {
                        shiny.fluent::updateDropdown.shinyInput(session, input_id, value = setting_value)
                    },
                    "multiselect" = {
                        if (!is.na(setting_value) && setting_value != "") {
                            variables_vector <- unlist(strsplit(setting_value, ", ?"))
                            shiny.fluent::updateDropdown.shinyInput(session, input_id, value = variables_vector)
                        }
                    },
                    "text" = {
                        if (!is.na(setting_value)) {
                            shiny.fluent::updateTextField.shinyInput(session, input_id, value = setting_value)
                        }
                    },
                    "toggle" = {
                        toggle_value <- as.logical(setting_value_num)
                        if (!is.na(toggle_value)) {
                            shiny.fluent::updateToggle.shinyInput(session, input_id, value = toggle_value)
                            if (setting_name == "auto_update" && toggle_value) auto_update <- TRUE
                        }
                    },
                    "code" = {
                        if (!is.na(setting_value)) {
                            m[[paste0("code_%widget_id%")]] <- setting_value
                            shinyAce::updateAceEditor(session, input_id, value = setting_value)
                        }
                    },
                    "date" = {
                        if (!is.na(setting_value) && setting_value != "") {
                            date_value <- as.Date(setting_value)
                            shiny.fluent::updateDatePicker.shinyInput(session, input_id, value = date_value)
                        }
                    },
                    "number" = {
                        if (!is.na(setting_value_num)) {
                            shiny.fluent::updateSpinButton.shinyInput(session, input_id, value = setting_value_num)
                        }
                    }
                )
            }
        }
    }
    
    # Apply default values for inputs that weren't loaded from saved settings
    for (input_def in all_inputs_%widget_id%) {
        input_id_short <- input_def$id
        input_id_full <- paste0(input_id_short, "_%widget_id%")
        
        # Skip if this input was already loaded from saved settings
        if (input_id_short %in% loaded_input_ids) {
            next
        }
        
        # Apply default value based on input type
        switch(
            input_def$type,
            "dropdown" = {
                shiny.fluent::updateDropdown.shinyInput(session, input_id_full, value = input_def$default)
            },
            "multiselect" = {
                # Special case for concepts: select all available concepts by default
                if (input_id_short == "concepts" && input_def$default == "all_available") {
                    # Get chart type to determine available domains
                    chart_type <- if (exists("input") && length(input$chart_type_%widget_id%) > 0) {
                        input$chart_type_%widget_id%
                    } else {
                        "dygraphs"  # default
                    }
                    
                    allowed_domains <- if (chart_type == "dygraphs") {
                        c("Measurement", "Observation")
                    } else if (chart_type == "plotly") {
                        c("Measurement", "Observation", "Condition", "Procedure", "Drug")
                    } else {
                        c("Measurement", "Observation")
                    }
                    
                    # Select all available concepts for the current chart type
                    if (exists("selected_concepts")) {
                        available_concepts <- selected_concepts %>% 
                            dplyr::filter(domain_id %in% allowed_domains) %>%
                            dplyr::pull(concept_id)
                        shiny.fluent::updateDropdown.shinyInput(session, input_id_full, value = available_concepts)
                    } else {
                        shiny.fluent::updateDropdown.shinyInput(session, input_id_full, value = c())
                    }
                } else {
                    shiny.fluent::updateDropdown.shinyInput(session, input_id_full, value = input_def$default)
                }
            },
            "text" = {
                shiny.fluent::updateTextField.shinyInput(session, input_id_full, value = input_def$default)
            },
            "toggle" = {
                shiny.fluent::updateToggle.shinyInput(session, input_id_full, value = input_def$default)
                # Check if auto_update default is TRUE
                if (input_id_short == "auto_update" && isTRUE(input_def$default)) {
                    auto_update <- TRUE
                }
            },
            "code" = {
                m[[paste0("code_%widget_id%")]] <- input_def$default
                shinyAce::updateAceEditor(session, input_id_full, value = input_def$default)
            },
            "date" = {
                # Handle date defaults (could be Date object or function call)
                default_date <- if (is.function(input_def$default)) {
                    input_def$default()
                } else {
                    input_def$default
                }
                shiny.fluent::updateDatePicker.shinyInput(session, input_id_full, value = default_date)
            },
            "number" = {
                shiny.fluent::updateSpinButton.shinyInput(session, input_id_full, value = input_def$default)
            }
        )
    }
    
    if (nrow(saved_settings) == 0) {
        # No saved configuration found - trigger output display with default settings
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-display_output_%widget_id%', Math.random());"))
    }
    
    # Auto-execute if enabled (either from saved settings or defaults)
    else if (auto_update) {
        shinyjs::delay(500, {
            shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
        })
    }
})

# ======================================
# CONFIGURATION SAVING TO DATABASE
# ======================================

# Save current settings to database
observe_event(input$save_configuration_trigger_%widget_id%, {
    
    # Validate configuration selection
    configuration_id <- input$user_configuration_%widget_id%
    if (is.null(configuration_id)) {
        # Redirect to configuration management if none selected
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_user_configurations_tab_%widget_id%', Math.random());"))
        return()
    }
    
    # Remove existing settings to avoid duplicates
    sql_send_statement(
        m$db, 
        glue::glue_sql(
            "DELETE FROM widgets_options 
             WHERE widget_id = %widget_id% AND category = 'output_settings' AND link_id = {configuration_id}", 
            .con = m$db
        )
    )
    
    # Generate settings to save automatically from all_inputs configuration
    settings_list <- list()
    
    for (input_def in all_inputs_%widget_id%) {
        input_id <- paste0(input_def$id, "_%widget_id%")
        input_value <- input[[input_id]]
        
        # Prepare value and value_num based on input type
        value_char <- NA_character_
        value_num <- NA_real_
        
        switch(input_def$type,
            "dropdown" = {
                value_char <- if(is.null(input_value)) "" else as.character(input_value)
            },
            "multiselect" = {
                value_char <- if(is.null(input_value)) "" else paste(input_value, collapse = ", ")
            },
            "text" = {
                value_char <- if(is.null(input_value)) "" else as.character(input_value)
            },
            "toggle" = {
                value_num <- as.numeric(isTRUE(input_value))
            },
            "code" = {
                value_char <- if(is.null(input_value)) "" else as.character(input_value)
            },
            "date" = {
                value_char <- if(is.null(input_value)) "" else as.character(input_value)
            },
            "number" = {
                value_num <- if(is.null(input_value)) as.numeric(input_def$default) else as.numeric(input_value)
            }
        )
        
        # Add to settings list
        settings_list[[length(settings_list) + 1]] <- data.frame(
            name = input_def$id,
            value = value_char,
            value_num = value_num,
            stringsAsFactors = FALSE
        )
    }
    
    # Combine all settings into a single dataframe
    settings_to_save <- do.call(rbind, settings_list)
    
    # Add database metadata
    settings_with_metadata <- settings_to_save %>%
        dplyr::transmute(
            id = get_last_row(m$db, "widgets_options") + 1:nrow(settings_to_save), 
            widget_id = %widget_id%, 
            person_id = NA_integer_, 
            link_id = configuration_id,
            category = "output_settings", 
            name, 
            value, 
            value_num, 
            creator_id = m$user_id, 
            datetime = now(), 
            deleted = FALSE
        )
    
    # Save to database
    DBI::dbAppendTable(m$db, "widgets_options", settings_with_metadata)
})

# ======================================
# SAVE TRIGGERS
# ======================================

# Handle manual save button clicks
observe_event(input$save_output_settings_and_code_%widget_id%, {
    # Trigger the save process
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_configuration_trigger_%widget_id%', Math.random());"))
    
    # Notify user
    show_message_bar("modif_saved", "success")
})

# ======================================
# DISPLAY AND SAVE BUTTON HANDLER
# ======================================

# Create a reactive value to track when save should happen after display
save_after_display_%widget_id% <- reactiveVal(FALSE)

# Handle the combined Display + Save button
observe_event(input$display_and_save_%widget_id%, {
    # Set flag to save after display completes
    save_after_display_%widget_id%(TRUE)
    
    # Trigger the display output action (which will generate the code)
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-display_output_%widget_id%', Math.random());"))
})
