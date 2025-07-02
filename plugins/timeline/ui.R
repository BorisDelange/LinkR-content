# ==========================================
# UI - Main interface file
# ==========================================

# ======================================
# LOAD USER CONFIGURATIONS FROM DATABASE
# ======================================

# Query available user configurations for this widget
sql <- glue::glue_sql(
    "SELECT id, value AS name 
     FROM widgets_options 
     WHERE widget_id = %widget_id% AND category = 'user_configurations' AND name = 'configuration_name'", 
    .con = m$db
)
m$user_configurations_%widget_id% <- DBI::dbGetQuery(m$db, sql)

# Convert query results to dropdown format (list of key-text pairs)
if (nrow(m$user_configurations_%widget_id%) > 0) {
    dropdown_options <- convert_tibble_to_list(
        m$user_configurations_%widget_id%, 
        key_col = "id", 
        text_col = "name"
    )
} else {
    # No user configurations found - initialize empty dropdown
    dropdown_options <- list()
}

# Query for currently selected user configuration ID
sql <- glue::glue_sql(
    "SELECT link_id 
     FROM widgets_options 
     WHERE widget_id = %widget_id% AND category = 'general_settings' AND name = 'selected_configuration_id'", 
    .con = m$db
)
selected_configuration_result <- DBI::dbGetQuery(m$db, sql)

# Set selected configuration or default to NULL
if (nrow(selected_configuration_result) > 0) {
    selected_file <- selected_configuration_result %>% dplyr::pull(link_id)
} else {
    selected_file <- NULL
}

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
        
        # Side by side button
        shiny.fluent::IconButton.shinyInput(
            ns("side_by_side_button_%widget_id%"), 
            iconProps = list(iconName = "DockLeft"), 
            title = i18np$t("figure_and_settings_side_by_side")
        ),
        
        # User configurations UI
        uiOutput(
            ns("user_configurations_ui_%widget_id%"),
            onclick = paste0("Shiny.setInputValue('", id, "-show_user_configurations_tab_%widget_id%', Math.random())")
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
        
        # Settings container (settings panel + sidebar)
        div(
            id = ns("settings_container_%widget_id%"),
            style = "height: 100%; display: flex; flex: 0 0 20%; box-sizing: border-box;",
            
            # Figure settings panel
            div(
                id = ns("figure_settings_div_%widget_id%"),
                %import_script('ui_figure_settings.R')%,
                style = "height: 100%; padding: 0 8px; overflow: auto; flex: 1; box-sizing: border-box;"
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
            
            # Right sidebar with action buttons
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
                style = "border-left: solid grey 0.5px; width: 25px; flex: 0 0 25px;"
            )
        ),
        
        # Main content area styling
        style = "display: flex; height: calc(100% - 34px);",
        class = "data_widget_settings_code_panel"
    ),
    
    # ====================
    # HIDDEN PANELS 
    # ====================
    
    # User configurations panel
    shinyjs::hidden(
        div(
            id = ns("user_configurations_div_%widget_id%"),
            %import_script('ui_user_configurations.R')%,
            style = "display: flex; height: calc(100% - 40px);"
        )
    )
)
