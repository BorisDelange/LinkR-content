# ==========================================
# ui_settings_files.R - Settings File Management Interface
# ==========================================

# Interface for managing settings files - appears when clicking the file icon
# Allows users to select, create, and delete configuration files

tagList(
    # ====================
    # CREATE SETTINGS FILE MODAL
    # ====================
    # Hidden modal dialog for adding new settings files
    shinyjs::hidden(
        div(
            id = ns("add_settings_file_modal_%widget_id%"),
            div(
                # Modal content container
                div(
                    # Modal header with title and close button
                    div(
                        tags$h1(i18np$t("create_settings_file"), style = "font-size: 14px;"),
                        shiny.fluent::IconButton.shinyInput(
                            ns("close_add_settings_file_modal_%widget_id%"), 
                            iconProps = list(iconName = "ChromeClose")
                        ),
                        style = "display: flex; justify-content: space-between;",
                        class = "small_close_button"
                    ),
                    
                    # File name input field
                    div(
                        shiny.fluent::TextField.shinyInput(
                            ns("settings_file_name_%widget_id%"), 
                            label = i18np$t("file_name")
                        ), 
                        style = "width: 200px;"
                    ),
                    
                    # Add button positioned at bottom right
                    div(
                        shiny.fluent::PrimaryButton.shinyInput(
                            ns("add_settings_file_%widget_id%"), 
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
    # DELETE SETTINGS FILE MODAL
    # ====================
    # Hidden confirmation modal for deleting settings files
    shinyjs::hidden(
        div(
            id = ns("delete_settings_file_modal_%widget_id%"),
            div(
                # Confirmation dialog content
                div(
                    # Dialog title
                    tags$h1(i18np$t("delete_settings_file_title"), style = "font-size: 14px;"),
                    
                    # Warning message
                    tags$p(i18np$t("delete_settings_file_text")),
                    
                    # Action buttons (Cancel + Delete)
                    div(
                        # Cancel button
                        shiny.fluent::DefaultButton.shinyInput(
                            ns("close_file_deletion_modal_%widget_id%"), 
                            i18np$t("dont_delete")
                        ),
                        
                        # Delete confirmation button (styled as dangerous action)
                        div(
                            shiny.fluent::PrimaryButton.shinyInput(
                                ns("confirm_file_deletion_%widget_id%"), 
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
    # SIDEBAR NAVIGATION
    # ====================
    # Left sidebar with action buttons
    div(
        id = ns("settings_files_sidenav_%widget_id%"),
        
        # Create new settings file button
        shiny.fluent::IconButton.shinyInput(
            ns("create_settings_file_%widget_id%"), 
            iconProps = list(iconName = "Add"), 
            title = i18np$t("create_settings_file"), 
            style = "margin: 0"
        ),
        
        # Sidebar styling
        class = "widget_icon",
        style = "border-right: solid grey 0.5px; padding-left: 5px;"
    ),
    
    # ====================
    # MAIN CONTENT AREA
    # ====================
    # File selection dropdown and delete button
    div(
        # Settings file selection dropdown
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("settings_file_%widget_id%"), 
                label = i18np$t("file"), 
                options = dropdown_options,     # Populated from database query above
                value = selected_file           # Currently selected file ID
            ), 
            style = "width: 200px"
        ),
        
        # Delete button (hidden by default, shown when file is selected)
        shinyjs::hidden(
            div(
                id = ns("delete_settings_file_div_%widget_id%"),
                shiny.fluent::IconButton.shinyInput(
                    ns("delete_settings_file_%widget_id%"), 
                    iconProps = list(iconName = "Delete")
                ),
                style = "margin-top: 26px;", 
                class = "widget_icon"
            )
        ),
        
        # Main content area styling
        style = "display: flex; gap: 5px; margin: 5px 10px;"
    )
)
