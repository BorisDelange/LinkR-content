# ==========================================
# ui_output_settings.R - Output Configuration Panel
# ==========================================

div(
    div(
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("prog_language_%widget_id%"), label = i18np$t("language"),
                options = list(
                    list(key = "r", text = i18np$t("r")),
                    list(key = "python", text = i18np$t("python"))
                ),
                value = "r"
            ), 
            style = "width: 150px;"
        ),
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("output_%widget_id%"), label = i18np$t("output"),
                options = list(
                    list(key = "console", text = i18np$t("console")),
                    list(key = "ui", text = i18np$t("ui_html")),
                    list(key = "figure", text = i18np$t("figure")),
                    list(key = "table", text = i18np$t("table")),
                    list(key = "datatable", text = i18np$t("datatable")),
                    list(key = "dygraphs", text = i18np$t("dygraphs")),
                    list(key = "plotly", text = i18np$t("plotly")),
                    list(key = "rmarkdown", text = i18np$t("rmarkdown"))
                ),
                value = "console"
            ), 
            style = "width: 150px;"
        ),
        style = "display: flex; gap: 10px;"
    ),
    div(
        id = ns("action_buttons_div_%widget_id%"),
        
        # Auto-update output
        div(
            div(
                id = ns("auto_update_div_%widget_id%"),
                shiny.fluent::Toggle.shinyInput(
                    ns("auto_update_%widget_id%"), 
                    label = i18np$t("auto_update_output"),
                    value = TRUE
                )
            ),
            style = "display: flex; align-items: center;"
        ),
        
        # Primary action - Generate/Update output
        shiny.fluent::PrimaryButton.shinyInput(
            ns("display_output_2_%widget_id%"), 
            i18np$t("display_output"), iconProps = list(iconName = "Play"),
            onClick = htmlwidgets::JS(paste0(
                "item => { Shiny.setInputValue('", id, "-display_output_%widget_id%', Math.random()); }"
            ))
        ),
        
        # Secondary action - Save configuration (if user has permissions)
        # This button is shown/hidden based on user access in the main UI file
        shiny.fluent::DefaultButton.shinyInput(
            ns("save_output_settings_and_code_2_%widget_id%"), 
            i18np$t("save_output_settings_and_code"), iconProps = list(iconName = "Save"),
            onClick = htmlwidgets::JS(paste0(
                "item => { Shiny.setInputValue('", id, "-save_output_settings_and_code_%widget_id%', Math.random()); }"
            ))
        ),
        
        style = "margin-top: 15px; display: flex; gap: 10px; flex-wrap: wrap; padding-top: 15px; border-top: solid 1px #808080;"
    ),
    
    # ====================
    # ADDITIONAL CONFIGURATION EXAMPLES
    # ====================
    # Add more sections as needed for your specific plugin:
    #
    # STATISTICAL PARAMETERS:
    # - Confidence intervals, significance levels, test types, sample sizes
    #
    # ADVANCED FILTERING OPTIONS:
    # - Numerical ranges, categorical filters, regex patterns, exclusion rules
    #
    # DISPLAY PREFERENCES:
    # - Chart dimensions, font sizes, themes, grid options, annotation styles
    #
    # EXPORT SETTINGS:
    # - File formats (PNG, PDF, SVG), resolution, DPI, compression options
    #
    # PERFORMANCE OPTIONS:
    # - Data sampling limits, processing timeouts, caching preferences, parallel processing
    #
    # INTERACTIVE FEATURES:
    # - Tooltips, zoom controls, brush selection, click actions, hover effects
    #
    # REMEMBER: When adding new configuration elements, update server_user_configurations.R
    # in both the CONFIGURATION LOADING and CONFIGURATION SAVING sections to ensure
    # user preferences are properly persisted and restored across sessions.
)
