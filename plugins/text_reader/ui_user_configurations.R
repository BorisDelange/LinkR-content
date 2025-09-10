# ==========================================
# ui_user_configurations.R - User Configuration Management Interface
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

# TEXT READER PLUGIN - USER CONFIGURATIONS UI FILE
# 
# This file defines the user configuration management interface for the text reader plugin.
# It provides functionality for users to save, load, rename, and manage different search and display 
# configuration presets for clinical note analysis and medical text processing.
# 
# TEXT READER CONFIGURATION FEATURES:
# - Save word set combinations for different clinical research scenarios
# - Store search and filter preferences for medical text analysis
# - Manage display settings for clinical note formatting
# - Quick switching between research configurations and patient cohorts
# - Template configurations for common medical text analysis workflows
# 
# CLINICAL WORKFLOW INTEGRATION:
# - Research scenario configurations (oncology, cardiology, etc.)
# - Patient cohort-specific search templates
# - Medical terminology word set collections
# - Clinical documentation review presets
# - Healthcare provider workflow configurations
# 
# USER WORKFLOW FOR MEDICAL TEXT ANALYSIS:
# 1. User configures word sets and search parameters for specific medical analysis
# 2. User clicks "Create Configuration" to save clinical search template
# 3. User can switch between medical research configurations via dropdown
# 4. User can rename configurations to match clinical research projects
# 5. User can delete outdated medical analysis configurations
# 
# DATABASE INTEGRATION:
# - Configurations are automatically saved to the widgets_options table
# - Each configuration stores all current widget parameters and code
# - Configuration names are validated and updated in real-time
# - The dropdown options are populated from database queries in ui.R
# - No additional database setup required - handled by the template framework

tagList(
    # ====================
    # CREATE AND RENAME CONFIGURATION MODALS
    # ====================
    # Generate modals for both creating and renaming configurations
    lapply(c("add", "rename"), function(action) {
        # Determine modal-specific properties
        modal_id <- paste0(action, "_user_configuration_modal_%widget_id%")
        close_button_id <- paste0("close_", action, "_user_configuration_modal_%widget_id%")
        text_field_id <- paste0("user_configuration_name_", action, "_%widget_id%")
        
        # Different button IDs for add vs rename to avoid conflicts
        if (action == "add") {
            action_button_id <- "add_user_configuration_%widget_id%"
        } else {
            action_button_id <- "save_user_configuration_rename_%widget_id%"
        }
        
        # Set title, button text, and icon based on action
        if (action == "add") {
            title_key <- "create_user_configuration"
            button_text <- i18np$t("add")
            button_icon <- "Add"
        } else {
            title_key <- "rename_user_configuration"
            button_text <- i18np$t("save")
            button_icon <- "Save"
        }
        
        # Hidden modal dialog for adding/renaming user configurations
        shinyjs::hidden(
            div(
                id = ns(modal_id),
                div(
                    # Modal content container
                    div(
                        # Modal header with title and close button
                        div(
                            tags$h1(i18np$t(title_key), style = "font-size: 14px;"),
                            shiny.fluent::IconButton.shinyInput(
                                ns(close_button_id), 
                                iconProps = list(iconName = "ChromeClose")
                            ),
                            style = "display: flex; justify-content: space-between;",
                            class = "small_close_button"
                        ),
                        
                        # Configuration name input field
                        div(
                            shiny.fluent::TextField.shinyInput(
                                ns(text_field_id), 
                                label = i18np$t("configuration_name")
                            ), 
                            style = "width: 200px;"
                        ),
                        bind_enter_key_to_button(input_id = ns(text_field_id), button_id = ns(action_button_id)),
                        
                        # Action button positioned at bottom right
                        div(
                            shiny.fluent::PrimaryButton.shinyInput(
                                ns(action_button_id), 
                                button_text,
                                iconProps = list(iconName = button_icon)
                            ),
                            style = "position: absolute; right: 10px; bottom: 8px;"
                        ),
                        
                        # Modal content styling
                        style = "background: #fff; padding: 5px 10px 10px 15px; position: relative; width: 400px; height: 120px;"
                    ),
                    
                    # Modal overlay styling
                    style = "display: flex; align-items: center; justify-content: center; position: absolute; left: 0; top: 0; width: 100%; height: 100%; background-color: rgba(0,0,0,0.2); z-index: 1000;"
                )
            )
        )
    }),
    
    # ====================
    # DELETE CONFIGURATION MODAL
    # ====================
    # Hidden confirmation modal for deleting user configurations
    shinyjs::hidden(
        div(
            id = ns("delete_user_configuration_modal_%widget_id%"),
            div(
                # Confirmation dialog content
                div(
                    # Dialog title
                    tags$h1(i18np$t("delete_user_configuration_title"), style = "font-size: 14px;"),
                    
                    # Warning message
                    tags$p(i18np$t("delete_user_configuration_text")),
                    
                    # Action buttons (Cancel + Delete)
                    div(
                        # Cancel button
                        shiny.fluent::DefaultButton.shinyInput(
                            ns("close_configuration_deletion_modal_%widget_id%"), 
                            i18np$t("dont_delete"), iconProps = list(iconName = "Cancel")
                        ),
                        
                        # Delete confirmation button (styled as dangerous action)
                        div(
                            shiny.fluent::PrimaryButton.shinyInput(
                                ns("confirm_configuration_deletion_%widget_id%"), 
                                i18np$t("delete"), iconProps = list(iconName = "Delete")
                            ), 
                            class = "delete_button"
                        ),
                        
                        style = "position: absolute; right: 10px; bottom: 8px; display: flex; gap: 5px;"
                    ),
                    
                    # Dialog content styling
                    style = "background: #fff; padding: 5px 10px 10px 15px; position: relative; width: 400px; height: 120px;"
                ),
                
                # Dialog overlay styling
                style = "display: flex; align-items: center; justify-content: center; position: absolute; left: 0; top: 0; width: 100%; height: 100%; background-color: rgba(0,0,0,0.2); z-index: 1000;"
            )
        )
    ),
    
    # ====================
    # MAIN CONTENT AREA
    # ====================
    div(
        # Create new configuration button
        div(
            shiny.fluent::PrimaryButton.shinyInput(
                ns("create_user_configuration_%widget_id%"), 
                i18np$t("create_user_configuration"),
                iconProps = list(iconName = "Add")
            ),
            style = "margin-bottom: 5px;"
        ),
        
        # Configuration selection and actions
        div(
            # User configuration selection dropdown
            div(
                shiny.fluent::Dropdown.shinyInput(
                    ns("user_configuration_%widget_id%"), 
                    label = i18np$t("configuration"), 
                    options = dropdown_options,     # Populated from database query in ui.R
                    value = selected_file           # Currently selected configuration ID
                ), 
                style = "width: 200px"
            ),
            
            # Rename button (hidden by default, shown when configuration is selected)
            shinyjs::hidden(
                div(
                    id = ns("rename_user_configuration_div_%widget_id%"),
                    shiny.fluent::DefaultButton.shinyInput(
                        ns("rename_user_configuration_%widget_id%"), 
                        i18np$t("rename"),
                        iconProps = list(iconName = "Edit")
                    ),
                    class = "rename_button",
                    style = "margin-top: 26px;"
                )
            ),
            
            # Delete button (hidden by default, shown when configuration is selected)
            shinyjs::hidden(
                div(
                    id = ns("delete_user_configuration_div_%widget_id%"),
                    shiny.fluent::PrimaryButton.shinyInput(
                        ns("delete_user_configuration_%widget_id%"), 
                        i18np$t("delete"),
                        iconProps = list(iconName = "Delete")
                    ),
                    class = "delete_button",
                    style = "margin-top: 26px;"
                )
            ),
            
            style = "display: flex; gap: 5px;"
        ),
        
        # Main content area styling
        style = "margin: 10px;"
    )
)
