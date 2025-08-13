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
    # Save output settings and code button
    save_output_settings_buttons <- shiny.fluent::IconButton.shinyInput(
        ns("save_output_settings_and_code_%widget_id%"), 
        iconProps = list(iconName = "Save"), 
        title = i18np$t("save_output_settings_and_code"), 
        style = "margin: 0"
    )
} else {
    save_output_settings_buttons <- ""
}

# ======================================
# MAIN UI STRUCTURE
# ======================================

tagList(
    div(
        style = "height: 100%; width: 100%; position: relative;",
        id = ns("main_container_%widget_id%"),
        
        # ====================
        # TOP NAVIGATION BAR
        # ====================
        div(
            id = ns("navigation_bar_%widget_id%"),
            
            # Hidden output button (shown conditionally via JavaScript)
            shinyjs::hidden(
                div(
                    id = ns("output_button_div_%widget_id%"),
                    shiny.fluent::IconButton.shinyInput(
                        ns("output_button_%widget_id%"), 
                        iconProps = list(iconName = "BarChart4"), 
                        title = i18np$t("show_output"),
                        onClick = htmlwidgets::JS(paste0("item => {",
                            "Shiny.setInputValue('", id, "-current_tab_trigger_%widget_id%', Math.random());",
                            "Shiny.setInputValue('", id, "-current_tab_%widget_id%', 'output');",
                        "}"))
                    )
                )
            ),
            
            # Output settings button
            shiny.fluent::IconButton.shinyInput(
                ns("output_settings_button_%widget_id%"), 
                iconProps = list(iconName = "AllApps"), 
                title = i18np$t("show_output_settings"),
                onClick = htmlwidgets::JS(paste0("item => {",
                    "Shiny.setInputValue('", id, "-current_tab_trigger_%widget_id%', Math.random());",
                    "Shiny.setInputValue('", id, "-current_tab_%widget_id%', 'output_settings');",
                "}"))
            ),
            
            # Code button (conditional based on user access)
            code_button,
            
            # Side by side button
            shiny.fluent::IconButton.shinyInput(
                ns("side_by_side_button_%widget_id%"), 
                iconProps = list(iconName = "DockLeft"), 
                title = i18np$t("output_and_settings_side_by_side")
            ),
            
            # User configurations UI
            uiOutput(
                ns("user_configurations_ui_%widget_id%"),
                onclick = paste0("Shiny.setInputValue('", id, "-show_user_configurations_tab_%widget_id%', Math.random())")
            ),
            
            # Navigation bar styling and unified mouse handling
            class = "widget_icon data_widget_top_icons top-navigation-bar",
            
            style = paste0(
                "color: #808080; border-bottom: solid grey 0.5px; height: 0px; padding: 5px 0 0 5px; font-size: 12px; display: flex; opacity: 0; position: relative; z-index: 100;",
                "transition: height 0.3s ease-in-out, opacity 0.3s ease-in-out; overflow: hidden;"
            )
        ),
        
        # Expanded hover detection area (behind navigation bar)
        div(
            style = "position: absolute; top: 0; left: 0; width: 400px; height: 40px; z-index: 99; pointer-events: none;",
            class = "navigation-hover-area"
        ),
        
        # ====================
        # MAIN CONTENT AREA
        # ====================
        
        div(
            id = ns("output_settings_code_div_%widget_id%"),
            
            # Output display area (left panel)
            div(
                id = ns("output_div_%widget_id%"),
                %import_script('ui_output.R')%,
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
                
                # Output settings panel
                div(
                    id = ns("output_settings_div_%widget_id%"),
                    %import_script('ui_output_settings.R')%,
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
                    id = ns("output_settings_code_sidenav_%widget_id%"),
                    
                    # Display output button
                    shiny.fluent::IconButton.shinyInput(
                        ns("display_output_%widget_id%"), 
                        iconProps = list(iconName = "Play"), 
                        title = i18np$t("display_output"), 
                        style = "margin: 0"
                    ),
                    
                    # Save output settings button (conditional)
                    save_output_settings_buttons,
                    
                    # Sidebar styling
                    class = "widget_icon",
                    style = "border-left: solid grey 0.5px; width: 25px; flex: 0 0 25px;"
                )
            ),
            
            # Main content area styling
            style = "display: flex; height: 100%; transition: height 0.3s ease-in-out;",
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
    ),
    
    # Unified navigation control script
    tags$script(HTML(paste0("
        (function() {
            const containerId = '", ns("main_container_%widget_id%"), "';
            const navId = '", ns("navigation_bar_%widget_id%"), "';
            
            let showTimeout = null;
            let hideTimeout = null;
            
            function clearAllTimeouts() {
                if (showTimeout) {
                    clearTimeout(showTimeout);
                    showTimeout = null;
                }
                if (hideTimeout) {
                    clearTimeout(hideTimeout);
                    hideTimeout = null;
                }
            }
            
            function showNavigation() {
                const container = document.getElementById(containerId);
                if (!container) return;
                
                const nav = document.getElementById(navId);
                const content = container.querySelector('.data_widget_settings_code_panel');
                if (!nav || !content) return;
                
                clearAllTimeouts();
                
                nav.style.height = '28px';
                nav.style.opacity = '1';
                content.style.height = 'calc(100% - 34px)';
                
                setTimeout(() => {
                    window.dispatchEvent(new Event('resize'));
                }, 320);
            }
            
            function hideNavigation() {
                const container = document.getElementById(containerId);
                if (!container) return;
                
                const nav = document.getElementById(navId);
                const content = container.querySelector('.data_widget_settings_code_panel');
                if (!nav || !content) return;
                
                clearAllTimeouts();
                
                nav.style.height = '0px';
                nav.style.opacity = '0';
                content.style.height = '100%';
                
                setTimeout(() => {
                    window.dispatchEvent(new Event('resize'));
                }, 320);
            }
            
            function scheduleShow(delay = 100) {
                clearAllTimeouts();
                showTimeout = setTimeout(showNavigation, delay);
            }
            
            function scheduleHide(delay = 5000) {
                clearAllTimeouts();
                hideTimeout = setTimeout(() => {
                    const container = document.getElementById(containerId);
                    const hoverArea = container?.querySelector('.navigation-hover-area');
                    const nav = document.getElementById(navId);
                    
                    // Only hide if not hovering over navigation area
                    if (!hoverArea?.matches(':hover') && !nav?.matches(':hover')) {
                        hideNavigation();
                    }
                }, delay);
            }
            
            // Wait for DOM to be ready
            function initializeNavigation() {
                const container = document.getElementById(containerId);
                if (!container) {
                    setTimeout(initializeNavigation, 100);
                    return;
                }
                
                const hoverArea = container.querySelector('.navigation-hover-area');
                const nav = document.getElementById(navId);
                
                if (!hoverArea || !nav) {
                    setTimeout(initializeNavigation, 100);
                    return;
                }
                
                // Hover area events (using pointer events)
                hoverArea.style.pointerEvents = 'auto';
                hoverArea.addEventListener('mouseenter', () => scheduleShow());
                hoverArea.addEventListener('mouseleave', (e) => {
                    // Only schedule hide if not moving to navigation bar
                    if (!e.relatedTarget?.closest('#' + navId)) {
                        scheduleHide();
                    }
                });
                
                // Navigation bar events  
                nav.addEventListener('mouseenter', clearAllTimeouts);
                nav.addEventListener('mouseleave', () => scheduleHide());
                
                // Auto-hide on initial load
                scheduleHide();
            }
            
            initializeNavigation();
        })();
    ")))
)
