# ==========================================
# ui_user_configurations.R - User Configuration Management Interface
# ==========================================
# 
# Interface for managing user configurations - appears when clicking the configuration icon
# Allows users to select, create, and delete saved configuration presets
#
# ==========================================

tagList(
    # ====================
    # CREATE CONFIGURATION MODAL
    # ====================
    # Hidden modal dialog for adding new user configurations
    shinyjs::hidden(
        div(
            id = ns("add_user_configuration_modal_%widget_id%"),
            div(
                # Modal content container
                div(
                    # Modal header with title and close button
                    div(
                        tags$h1(i18np$t("create_user_configuration"), style = "font-size: 14px;"),
                        shiny.fluent::IconButton.shinyInput(
                            ns("close_add_user_configuration_modal_%widget_id%"), 
                            iconProps = list(iconName = "ChromeClose")
                        ),
                        style = "display: flex; justify-content: space-between;",
                        class = "small_close_button"
                    ),
                    
                    # Configuration name input field
                    div(
                        shiny.fluent::TextField.shinyInput(
                            ns("user_configuration_name_%widget_id%"), 
                            label = i18np$t("configuration_name")
                        ), 
                        style = "width: 200px;"
                    ),
                    
                    # Add button positioned at bottom right
                    div(
                        shiny.fluent::PrimaryButton.shinyInput(
                            ns("add_user_configuration_%widget_id%"), 
                            i18np$t("add")
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
    ),
    
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
                            i18np$t("dont_delete")
                        ),
                        
                        # Delete confirmation button (styled as dangerous action)
                        div(
                            shiny.fluent::PrimaryButton.shinyInput(
                                ns("confirm_configuration_deletion_%widget_id%"), 
                                i18np$t("delete")
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
            
            # Delete button (hidden by default, shown when configuration is selected)
            shinyjs::hidden(
                div(
                    id = ns("delete_user_configuration_div_%widget_id%"),
                    shiny.fluent::DefaultButton.shinyInput(
                        ns("delete_user_configuration_%widget_id%"), 
                        i18np$t("delete"),
                        iconProps = list(iconName = "Delete"),
                        style = "background-color: #d13438; color: white; border-color: #d13438;"
                    ),
                    style = "margin-top: 26px;"
                )
            ),
            
            style = "display: flex; gap: 5px;"
        ),
        
        # Main content area styling
        style = "margin: 10px;"
    )
)
