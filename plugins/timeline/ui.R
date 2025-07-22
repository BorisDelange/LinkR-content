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
   
   # Save general settings button
   save_general_settings_button <- shiny.fluent::IconButton.shinyInput(
       ns("save_general_settings_button_%widget_id%"), 
       iconProps = list(iconName = "Save"), 
       title = i18np$t("save_general_settings"), 
       style = "margin: 0"
   )
} else {
   save_output_settings_buttons <- ""
   save_general_settings_button <- ""   
}

# ======================================
# MAIN UI STRUCTURE
# ======================================

tagList(
   div(
       style = "height: 100%; width: 100%; overflow: auto; position: relative;",
       
       # Hover trigger zone at top-left (limited width to match navigation bar area)
       div(
           style = "position: absolute; top: 0; left: 0; width: 400px; height: 15px; z-index: 10;",
           onmouseenter = "
               var container = this.parentElement;
               var nav = container.querySelector('.top-navigation-bar');
               var content = container.querySelector('.data_widget_settings_code_panel');
               
               // Clear any existing hide timeout
               if (container.hideTimeout) {
                   clearTimeout(container.hideTimeout);
                   container.hideTimeout = null;
               }
               
               // Show navigation bar with animation if it's hidden
               if (nav.style.display !== 'flex') {
                   content.style.height = 'calc(100% - 34px)';
                   nav.style.display = 'flex';
                   nav.style.height = '0px';
                   nav.style.opacity = '0';
                   nav.offsetHeight;
                   nav.style.height = '28px';
                   nav.style.opacity = '1';
                   
                   setTimeout(function() {
                       var event = new Event('resize');
                       window.dispatchEvent(event);
                   }, 320);
               }
           "
       ),
       
       # ====================
       # TOP NAVIGATION BAR
       # ====================
       div(
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
           
           # Top navigation bar styling - initially visible with higher z-index
           class = "widget_icon data_widget_top_icons top-navigation-bar",
           style = paste0(
               "color: #808080; border-bottom: solid grey 0.5px; height: 28px; padding: 5px 0 0 5px; font-size: 12px; display: flex; opacity: 1; position: relative; z-index: 100;",
               "transition: height 0.3s ease-in-out, opacity 0.3s ease-in-out; overflow: hidden;"),
           
           # Handle mouse enter on navigation bar - cancel hide timeout
           onmouseenter = "
               var nav = this;
               var container = this.parentElement;
               
               // Clear any existing hide timeout when re-entering navigation bar
               if (container.hideTimeout) {
                   clearTimeout(container.hideTimeout);
                   container.hideTimeout = null;
               }
           ",
           
           # Handle mouseleave on navigation bar with 5000ms delay before hiding
           onmouseleave = "
               var nav = this;
               var container = this.parentElement;
               var content = container.querySelector('.data_widget_settings_code_panel');
               
               // Set timeout to hide navigation bar after 5000ms
               container.hideTimeout = setTimeout(function() {
                   // Double-check if mouse is still outside navigation area
                   if (!nav.matches(':hover') && !container.querySelector('.top-navigation-bar:hover')) {
                       nav.style.height = '0px';
                       nav.style.opacity = '0';
                       content.style.height = '100%';
                       
                       setTimeout(function() {
                           nav.style.display = 'none';
                           var event = new Event('resize');
                           window.dispatchEvent(event);
                       }, 320);
                   }
                   container.hideTimeout = null;
               }, 5000);
           "
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
           
           # Main content area styling - initially adjusted for visible navigation bar
           style = "display: flex; height: calc(100% - 34px); transition: height 0.3s ease-in-out;",
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
   
   # Auto-hide script that runs after DOM is loaded
   tags$script(HTML(paste0("
       setTimeout(function() {
           var containers = document.querySelectorAll('[style*=\"height: 100%; width: 100%; overflow: auto; position: relative;\"]');
           containers.forEach(function(container) {
               var nav = container.querySelector('.top-navigation-bar');
               var content = container.querySelector('.data_widget_settings_code_panel');
               
               if (nav && content && nav.style.display === 'flex') {
                   // Auto-hide after 5 seconds if not hovering
                   setTimeout(function() {
                       if (!nav.matches(':hover')) {
                           nav.style.height = '0px';
                           nav.style.opacity = '0';
                           content.style.height = '100%';
                           
                           setTimeout(function() {
                               nav.style.display = 'none';
                               var event = new Event('resize');
                               window.dispatchEvent(event);
                           }, 320);
                       }
                   }, 5000);
               }
           });
       }, 100);
   ")))
)
