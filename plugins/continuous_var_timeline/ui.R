# ==========================================
# UI - Main interface file
# ==========================================

# ======================================
# ACCESS-CONTROLLED BUTTON CONFIGURATION
# ======================================

# Code editor button - only shown if user has console access
if ("projects_widgets_console" %in% user_accesses) {
    code_button <- shiny.fluent::IconButton.shinyInput(
        ns("code_button_%widget_id%"), 
        iconProps = list(iconName = "Code"), 
        title = i18np$t("show_code_editor"),
        onClick = htmlwidgets::JS(paste0("item => {",
            "Shiny.setInputValue('", id, "-current_tab_trigger_%widget_id%', Math.random());",
            "Shiny.setInputValue('", id, "-current_tab_%widget_id%', 'code');",
        "}"))
    )
} else {
    code_button <- ""
}

# Save buttons - only shown if user has settings access permissions
if ("projects_widgets_settings" %in% user_accesses) {
    # Save figure settings and code button
    save_figure_settings_buttons <- shiny.fluent::IconButton.shinyInput(
        ns("save_params_and_code_%widget_id%"), 
        iconProps = list(iconName = "Save"), 
        title = i18np$t("save_figure_settings_and_code"), 
        style = "margin: 0"
    )
    
    # Save general settings button
    save_general_settings_button <- shiny.fluent::IconButton.shinyInput(
        ns("save_general_settings_button_%widget_id%"), 
        iconProps = list(iconName = "Save"), 
        title = i18np$t("save_general_settings"), 
        style = "margin: 0"
    )
} else {
    save_figure_settings_buttons <- ""
    save_general_settings_button <- ""   
}

# ======================================
# GENERAL SETTINGS IMPORT
# ======================================

%import_script('ui_load_general_settings.R')%

# ======================================
# MAIN UI STRUCTURE
# ======================================

tagList(
    # ====================
    # TOP NAVIGATION BAR
    # ====================
    div(
        # Hidden figure button (shown conditionally via JavaScript)
        shinyjs::hidden(
            div(
                id = ns("figure_button_div_%widget_id%"),
                shiny.fluent::IconButton.shinyInput(
                    ns("figure_button_%widget_id%"), 
                    iconProps = list(iconName = "BarChart4"), 
                    title = i18np$t("show_figure"),
                    onClick = htmlwidgets::JS(paste0("item => {",
                        "Shiny.setInputValue('", id, "-current_tab_trigger_%widget_id%', Math.random());",
                        "Shiny.setInputValue('", id, "-current_tab_%widget_id%', 'figure');",
                    "}"))
                )
            )
        ),
        
        # Figure settings button
        shiny.fluent::IconButton.shinyInput(
            ns("figure_settings_button_%widget_id%"), 
            iconProps = list(iconName = "AllApps"), 
            title = i18np$t("show_figure_settings"),
            onClick = htmlwidgets::JS(paste0("item => {",
                "Shiny.setInputValue('", id, "-current_tab_trigger_%widget_id%', Math.random());",
                "Shiny.setInputValue('", id, "-current_tab_%widget_id%', 'figure_settings');",
            "}"))
        ),
        
        # Code button (conditional based on user access)
        code_button,
        
        # General settings button
        shiny.fluent::IconButton.shinyInput(
            ns("general_settings_button_%widget_id%"), 
            iconProps = list(iconName = "Settings"), 
            title = i18np$t("show_general_settings"),
            onClick = htmlwidgets::JS(paste0("item => {",
                "Shiny.setInputValue('", id, "-current_tab_trigger_%widget_id%', Math.random());",
                "Shiny.setInputValue('", id, "-current_tab_%widget_id%', 'general_settings');",
            "}"))
        ),
        
        # Settings files UI
        uiOutput(
            ns("settings_files_ui_%widget_id%"),
            onclick = paste0("Shiny.setInputValue('", id, "-show_settings_files_tab_%widget_id%', Math.random())")
        ),
        
        # Top navigation bar styling
        class = "widget_icon data_widget_top_icons",
        style = "display: flex; color: #808080; border-bottom: solid grey 0.5px; height: 28px; padding: 5px 0 0 5px; font-size: 12px;"
    ),
    
    # ====================
    # MAIN CONTENT AREA
    # ====================
    div(
        id = ns("figure_settings_code_div_%widget_id%"),
        
        # Left sidebar with action buttons
        div(
            id = ns("figure_settings_code_sidenav_%widget_id%"),
            
            # Display figure button
            shiny.fluent::IconButton.shinyInput(
                ns("display_figure_%widget_id%"), 
                iconProps = list(iconName = "Play"), 
                title = i18np$t("display_figure"), 
                style = "margin: 0"
            ),
            
            # Save figure settings button (conditional)
            save_figure_settings_buttons,
            
            # Sidebar styling
            class = "widget_icon",
            style = "border-right: solid grey 0.5px; width: 25px; padding-left: 5px;"
        ),
        
        # Figure display area (left panel)
        div(
            id = ns("figure_div_%widget_id%"),
            %import_script('ui_figure.R')%,
            style = "height: 100%; flex: 1; box-sizing: border-box; min-width: 50px;",
            class = "left-panel"
        ),
        
        # Resizable divider
        div(
            id = ns("resizer_%widget_id%"),
            style = "width: 5px; cursor: col-resize; background-color: #ccc; flex: 0 0 5px;",
            class = "resizer"
        ),
        
        # Figure settings panel (right side)
        div(
            id = ns("figure_settings_div_%widget_id%"),
            %import_script('ui_figure_settings.R')%,
            style = "height: 100%; padding: 0 8px; overflow: auto; flex: 0 0 20%; box-sizing: border-box;"
        ),
        
        # Code editor panel (hidden by default)
        shinyjs::hidden(
            div(
                id = ns("code_div_%widget_id%"),
                %import_script('ui_code.R')%,
                style = "height: 100%; overflow: auto; flex: 0 0 50%; box-sizing: border-box;",
                class = "right-panel"
            )
        ),
        
        # Main content area styling
        style = "display: flex; height: calc(100% - 34px);",
        class = "data_widget_settings_code_panel"
    ),
    
    # ====================
    # HIDDEN PANELS 
    # ====================
    
    # General settings panel
    shinyjs::hidden(
        div(
            id = ns("general_settings_div_%widget_id%"),
            %import_script('ui_general_settings.R')%,
            style = "height: calc(100% - 40px);"
        )
    ),
    
    # Settings files panel
    shinyjs::hidden(
        div(
            id = ns("settings_files_div_%widget_id%"),
            %import_script('ui_settings_files.R')%,
            style = "display: flex; height: calc(100% - 40px);"
        )
    )
)
