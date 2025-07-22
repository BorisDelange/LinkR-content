# ==========================================
# server_output_settings.R - Output Configuration Server Logic
# ==========================================

# PLUGIN TEMPLATE - OUTPUT SETTINGS SERVER FILE
# 
# This file handles the server-side logic for the output configuration interface.
# It manages user interactions with the no-code settings panel, coordinates
# automatic R code generation, and handles configuration persistence.
# 
# WHEN CREATING A NEW PLUGIN WITH THIS TEMPLATE:
# - Customize the dynamic UI observers for your specific input elements
# - Implement validation logic for your parameter combinations
# - Add settings loading/saving logic for your configuration parameters
# - Modify bulk selection helpers based on your multi-select inputs
# - Add any plugin-specific helper functions for data processing
# 
# CORE FUNCTIONALITY:
# - Dynamic UI updates based on user selections
# - Bulk selection helpers (select all/clear all functionality)
# - Smart defaults application based on data context
# - Configuration loading and saving to database
# - Input validation and error handling
# - Integration with automatic code generation system
# 
# COMMON CONFIGURATION PATTERNS:
# 
# CONDITIONAL UI DISPLAY:
#   Show/hide settings based on other selections (e.g., output type affects available options)
# 
# CASCADING UPDATES:
#   Filter available options based on previous selections (e.g., dataset affects variables)
# 
# BULK SELECTION HELPERS:
#   Select all/clear all buttons for multi-select dropdowns
# 
# SMART DEFAULTS:
#   Automatically configure optimal settings based on data characteristics
# 
# CONFIGURATION PERSISTENCE:
#   Save/load user configuration presets to/from database

# ======================================
# DYNAMIC UI UPDATES BASED ON SELECTIONS
# ======================================

# Example: Update available variables when output type changes
observe_event(input$output_type_%widget_id%, {
    
    selected_type <- input$output_type_%widget_id%
    
    if (!is.null(selected_type)) {
        
        # Example: Filter available variables based on output type
        if (selected_type == "plot") {
            # Show plot-suitable variables (numeric columns from iris)
            available_variables <- list(
                list(key = "Sepal.Length", text = "Sepal Length"),
                list(key = "Sepal.Width", text = "Sepal Width"),
                list(key = "Petal.Length", text = "Petal Length"),
                list(key = "Petal.Width", text = "Petal Width")
            )
        } else if (selected_type == "table") {
            # Show all variables for table display (including categorical)
            available_variables <- list(
                list(key = "Sepal.Length", text = "Sepal Length"),
                list(key = "Sepal.Width", text = "Sepal Width"),
                list(key = "Petal.Length", text = "Petal Length"),
                list(key = "Petal.Width", text = "Petal Width"),
                list(key = "Species", text = "Species")
            )
        } else if (selected_type == "summary") {
            # Show numeric variables for summary statistics
            available_variables <- list(
                list(key = "Sepal.Length", text = "Sepal Length"),
                list(key = "Sepal.Width", text = "Sepal Width"),
                list(key = "Petal.Length", text = "Petal Length"),
                list(key = "Petal.Width", text = "Petal Width")
            )
        } else {
            # Default: all numeric variables
            available_variables <- list(
                list(key = "Sepal.Length", text = "Sepal Length"),
                list(key = "Sepal.Width", text = "Sepal Width"),
                list(key = "Petal.Length", text = "Petal Length"),
                list(key = "Petal.Width", text = "Petal Width")
            )
        }
        
        # Get current selection to preserve valid choices
        current_selection <- input$variables_%widget_id%
        available_keys <- sapply(available_variables, function(x) x$key)
        preserved_selection <- current_selection[current_selection %in% available_keys]
        
        # If no variables can be preserved, select first available
        final_selection <- if (length(preserved_selection) > 0) {
            preserved_selection
        } else {
            available_keys[1]  # Select first variable as default
        }
        
        # Update variables dropdown
        shiny.fluent::updateDropdown.shinyInput(
            session, 
            "variables_%widget_id%", 
            options = available_variables,
            value = final_selection
        )
    }
})

# ======================================
# VARIABLES CHECK/UNCHECK BUTTONS
# ======================================

# Select all variables helper
observe_event(input$variables_check_all_%widget_id%, {
    
    # Get current output type to determine available variables
    output_type <- input$output_type_%widget_id%
    if (is.null(output_type)) output_type <- "plot"  # Default
    
    # Get available variables for current output type
    if (output_type == "plot") {
        all_variables <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
        variable_options <- list(
            list(key = "Sepal.Length", text = "Sepal Length"),
            list(key = "Sepal.Width", text = "Sepal Width"),
            list(key = "Petal.Length", text = "Petal Length"),
            list(key = "Petal.Width", text = "Petal Width")
        )
    } else if (output_type == "table") {
        all_variables <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
        variable_options <- list(
            list(key = "Sepal.Length", text = "Sepal Length"),
            list(key = "Sepal.Width", text = "Sepal Width"),
            list(key = "Petal.Length", text = "Petal Length"),
            list(key = "Petal.Width", text = "Petal Width"),
            list(key = "Species", text = "Species")
        )
    } else {
        all_variables <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
        variable_options <- list(
            list(key = "Sepal.Length", text = "Sepal Length"),
            list(key = "Sepal.Width", text = "Sepal Width"),
            list(key = "Petal.Length", text = "Petal Length"),
            list(key = "Petal.Width", text = "Petal Width")
        )
    }
    
    # Update dropdown to select all available variables
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "variables_%widget_id%", 
        options = variable_options,
        value = all_variables
    )
})

# Clear all variables helper
observe_event(input$variables_uncheck_all_%widget_id%, {
    
    # Get current output type to maintain correct options
    output_type <- input$output_type_%widget_id%
    if (is.null(output_type)) output_type <- "plot"  # Default
    
    # Get available variables for current output type
    if (output_type == "plot") {
        variable_options <- list(
            list(key = "Sepal.Length", text = "Sepal Length"),
            list(key = "Sepal.Width", text = "Sepal Width"),
            list(key = "Petal.Length", text = "Petal Length"),
            list(key = "Petal.Width", text = "Petal Width")
        )
    } else if (output_type == "table") {
        variable_options <- list(
            list(key = "Sepal.Length", text = "Sepal Length"),
            list(key = "Sepal.Width", text = "Sepal Width"),
            list(key = "Petal.Length", text = "Petal Length"),
            list(key = "Petal.Width", text = "Petal Width"),
            list(key = "Species", text = "Species")
        )
    } else {
        variable_options <- list(
            list(key = "Sepal.Length", text = "Sepal Length"),
            list(key = "Sepal.Width", text = "Sepal Width"),
            list(key = "Petal.Length", text = "Petal Length"),
            list(key = "Petal.Width", text = "Petal Width")
        )
    }
    
    # Update dropdown options and clear selection
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "variables_%widget_id%", 
        options = variable_options,
        value = character(0)
    )
})

# ======================================
# SMART DEFAULTS APPLICATION
# ======================================

# Apply smart defaults based on variable selection
apply_smart_defaults <- function(selected_variables = c(), has_saved_config = FALSE) {
    
    # Only apply smart defaults if no saved configuration exists
    if (has_saved_config) {
        return()
    }
    
    # Example: Determine optimal output type based on selected variables
    if (length(selected_variables) > 0) {
        
        # Check if Species is selected (categorical variable)
        has_categorical <- "Species" %in% selected_variables
        num_variables <- length(selected_variables[selected_variables != "Species"])
        
        # Example logic for determining optimal settings
        if (has_categorical || num_variables > 3) {
            optimal_output_type <- "table"
            optimal_show_legend <- FALSE
        } else if (num_variables <= 2) {
            optimal_output_type <- "plot"
            optimal_show_legend <- num_variables > 1
        } else {
            optimal_output_type <- "summary"
            optimal_show_legend <- TRUE
        }
        
        # Update UI with optimal settings
        current_output_type <- input$output_type_%widget_id%
        if (is.null(current_output_type) || current_output_type != optimal_output_type) {
            shiny.fluent::updateDropdown.shinyInput(
                session, 
                "output_type_%widget_id%", 
                value = optimal_output_type
            )
        }
        
        current_show_legend <- input$show_legend_%widget_id%
        if (is.null(current_show_legend) || current_show_legend != optimal_show_legend) {
            shiny.fluent::updateToggle.shinyInput(
                session, 
                "show_legend_%widget_id%", 
                value = optimal_show_legend
            )
        }
    }
}

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
    loaded_code <- ""
    has_saved_variables <- FALSE
    loaded_variables <- character(0)
    
    # Apply saved settings to UI
    if (nrow(saved_settings) > 0) {
        
        # Process each saved setting
        for (i in 1:nrow(saved_settings)) {
            setting_name <- saved_settings$name[i]
            setting_value <- saved_settings$value[i]
            setting_value_num <- saved_settings$value_num[i]
            
            # Update UI based on setting type
            if (setting_name == "output_type") {
                # Update output type dropdown
                shiny.fluent::updateDropdown.shinyInput(
                    session, 
                    "output_type_%widget_id%", 
                    value = setting_value
                )
            }
            else if (setting_name == "variables") {
                # Handle multi-select variables
                if (!is.na(setting_value) && setting_value != "") {
                    # Convert saved string back to vector (assuming comma-separated)
                    variables_vector <- unlist(strsplit(setting_value, ", ?"))
                    has_saved_variables <- TRUE
                    loaded_variables <- variables_vector
                    
                    shiny.fluent::updateDropdown.shinyInput(
                        session, 
                        "variables_%widget_id%", 
                        value = variables_vector
                    )
                }
            }
            else if (setting_name %in% c("show_legend", "interactive_mode", "auto_update")) {
                # Handle boolean toggles
                toggle_value <- as.logical(setting_value_num)
                if (!is.na(toggle_value)) {
                    shiny.fluent::updateToggle.shinyInput(
                        session, 
                        paste0(setting_name, "_%widget_id%"), 
                        value = toggle_value
                    )
                }
            }
            else if (setting_name == "code") {
                # Handle code editor content
                if (!is.na(setting_value)) {
                    loaded_code <- setting_value
                    m$code_%widget_id% <- setting_value
                    shinyAce::updateAceEditor(session, "code_%widget_id%", value = setting_value)
                }
            }
            # Add more setting types as needed for your plugin
        }
    }
    
    # Handle case where no saved variables exist
    if (!has_saved_variables) {
        # Select default variables (first variable as default)
        default_variables <- c("Sepal.Length")
        
        shiny.fluent::updateDropdown.shinyInput(
            session, 
            "variables_%widget_id%", 
            value = default_variables
        )
        
        # Apply smart defaults based on default variables
        shinyjs::delay(100, {
            apply_smart_defaults(default_variables, has_saved_config = FALSE)
        })
    } else {
        # Apply smart defaults based on loaded variables
        shinyjs::delay(100, {
            apply_smart_defaults(loaded_variables, has_saved_config = TRUE)
        })
    }
    
    # Auto-execute if enabled
    if (isTRUE(input$auto_update_%widget_id%)) {
        shinyjs::delay(500, {
            shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-display_output_%widget_id%', Math.random());"))
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
    
    # Prepare settings data for iris dataset plugin
    settings_to_save <- tibble::tribble(
        ~name, ~value, ~value_num,
        # Dropdown selections
        "output_type", input$output_type_%widget_id%, NA_real_,
        
        # Multi-select variables (convert to comma-separated string)
        "variables", if(is.null(input$variables_%widget_id%)) "" else paste(input$variables_%widget_id%, collapse = ", "), NA_real_,
        
        # Boolean toggles (store as 0/1 in value_num)
        "show_legend", NA_character_, as.numeric(isTRUE(input$show_legend_%widget_id%)),
        "interactive_mode", NA_character_, as.numeric(isTRUE(input$interactive_mode_%widget_id%)),
        "auto_update", NA_character_, as.numeric(isTRUE(input$auto_update_%widget_id%)),
        
        # Code content
        "code", input$code_%widget_id%, NA_real_
    )
    
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
# SAVE TRIGGERS AND USER FEEDBACK
# ======================================

# Handle manual save button clicks
observe_event(input$save_output_settings_and_code_%widget_id%, {
    # Trigger the save process
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_configuration_trigger_%widget_id%', Math.random());"))
    
    # Provide user feedback
    show_message_bar("modif_saved", "success")
})

# Optional: Auto-save on startup (uncomment if desired)
# shinyjs::delay(1000, {
#     shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_configuration_trigger_%widget_id%', Math.random());"))
# })
