# ==========================================
# ui_general_settings.R - Application Configuration Panel
# ==========================================

# Note: Any fields added here must also be updated in:
# - server_general_settings.R (server logic)
# - ui_general_settings.R (this file)

div(
    # ====================
    # DISPLAY SETTINGS SECTION
    # ====================
    div(
        tags$strong(i18np$t("display")), 
        br(),
        
        # Show/hide settings file toggle
        div(
            shiny.fluent::Toggle.shinyInput(
                ns("show_settings_file_%widget_id%"), 
                value = toggle_values$show_settings_file,
                onClick = htmlwidgets::JS(paste0(
                    "item => {Shiny.setInputValue('", id, "-save_general_settings_%widget_id%', Math.random());}"
                ))
            ),
            tags$label(
                i18np$t("show_settings_file"), 
                `for` = ns("show_settings_file_%widget_id%"), 
                style = "margin-left: 5px;"
            ),
            style = "display: flex; margin-top: 8px;" 
        ),
        
        # Side-by-side layout toggle
        div(
            shiny.fluent::Toggle.shinyInput(
                ns("figure_and_settings_side_by_side_%widget_id%"), 
                value = toggle_values$figure_and_settings_side_by_side,
                onClick = htmlwidgets::JS(paste0(
                    "item => {Shiny.setInputValue('", id, "-save_general_settings_%widget_id%', Math.random());}"
                ))
            ),
            tags$label(
                i18np$t("figure_and_settings_side_by_side"), 
                `for` = ns("figure_and_settings_side_by_side_%widget_id%"), 
                style = "margin-left: 5px;"
            ),
            style = "display: flex; margin-top: 5px;" 
        )
    ),
    
    # ====================
    # CODE EXECUTION SETTINGS SECTION
    # ====================
    div(
        tags$strong(i18np$t("code_execution")), 
        br(),
        
        # Auto-run code when settings file loads
        div(
            shiny.fluent::Toggle.shinyInput(
                ns("run_code_at_settings_file_load_%widget_id%"), 
                value = toggle_values$run_code_at_settings_file_load,
                onClick = htmlwidgets::JS(paste0(
                    "item => {Shiny.setInputValue('", id, "-save_general_settings_%widget_id%', Math.random());}"
                ))
            ),
            tags$label(
                i18np$t("run_code_at_settings_file_load"), 
                `for` = ns("run_code_at_settings_file_load_%widget_id%"), 
                style = "margin-left: 5px;"
            ),
            style = "display: flex; margin-top: 8px;" 
        ),
        
        # Auto-run code when data updates
        div(
            shiny.fluent::Toggle.shinyInput(
                ns("run_code_on_data_update_%widget_id%"), 
                value = toggle_values$run_code_on_data_update,
                onClick = htmlwidgets::JS(paste0(
                    "item => {Shiny.setInputValue('", id, "-save_general_settings_%widget_id%', Math.random());}"
                ))
            ),
            tags$label(
                i18np$t("run_code_on_data_update"), 
                `for` = ns("run_code_on_data_update_%widget_id%"), 
                style = "margin-left: 5px;"
            ),
            style = "display: flex; margin-top: 5px;" 
        ),
        
        style = "margin-top: 10px;"
    ),
    
    # Panel container styling
    style = "padding: 10px;"
)
