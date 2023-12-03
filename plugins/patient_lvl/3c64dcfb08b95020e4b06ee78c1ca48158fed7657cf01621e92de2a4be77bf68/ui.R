# Get widget options
sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id%", .con = r$db)
widget_options <- DBI::dbGetQuery(m$db, sql)
scripts <- widget_options %>% dplyr::filter(name == "script") %>% dplyr::select(id = value_num, name = value)
selected_script <- NULL
selected_script_type_choicegroup <- "r"
selected_script_type <- "r"
selected_script_code <- ""
selected_script_result <- widget_options %>% dplyr::filter(name == "selected_script")
if (nrow(selected_script_result) > 0) if ((selected_script_result %>% dplyr::pull(value_num)) %in% scripts$id){
    selected_script <- selected_script_result %>% dplyr::pull(value_num)
    script_options <- widget_options %>% dplyr::filter(name %in% c("script_type", "script_code") & link_id == selected_script)
    
    if (nrow(script_options) > 0){
        selected_script_type_choicegroup <- script_options %>% dplyr::filter(name == "script_type") %>% dplyr::pull(value)
        
        if (selected_script_type_choicegroup == "rmarkdown") selected_script_type <- "markdown" else "r"
        selected_script_code <- script_options %>% dplyr::filter(name == "script_code") %>% dplyr::pull(value)
    }
}

# aceEditor div : show editor if user has access to the console
ace_editor_div <- div(br(), shiny.fluent::MessageBar(i18np$t("unauthorized_access_to_console"), messageBarType = 5), br())
if (length(m$user_accesses) > 0) if ("data_console" %in% m$user_accesses) ace_editor_div <- div(
    div(
        shinyAce::aceEditor(
            ns("script_code_%widget_id%"), "", mode = selected_script_type, value = selected_script_code,
                code_hotkeys = list(
                    "r", list(
                      run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                      run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                      save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S")
                    )
                ),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
        ), 
    style = "width: 100%;"),
    shiny.fluent::Stack(
        horizontal = TRUE, tokens = list(childrenGap = 10),
        shiny.fluent::PrimaryButton.shinyInput(ns("run_code_%widget_id%"), i18n$t("run_code")),
        shiny.fluent::DefaultButton.shinyInput(ns("save_code_%widget_id%"), i18n$t("save"))
    ), br()
)

tagList(
    shiny.fluent::reactOutput(ns("delete_confirm_%widget_id%")),
    shiny.fluent::Pivot(
        id = ns("pivot_%widget_id%"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab_%widget_id%', item.props.id)")),
        shiny.fluent::PivotItem(id = "script_div_%widget_id%", itemKey = "script", headerText = i18np$t("script")),
        shiny.fluent::PivotItem(id = "scripts_management_div_%widget_id%", itemKey = "scripts_management", headerText = i18np$t("scripts_management"))
    ),
    div(
        id = ns("script_div_%widget_id%"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            div(shiny.fluent::Dropdown.shinyInput(ns("script_choice_%widget_id%"), 
                options = convert_tibble_to_list(scripts, key_col = "id", text_col = "name"), value = selected_script), style = "width:300px;"),
            div(style = "width:10px"),
            shiny.fluent::ChoiceGroup.shinyInput(
                ns("script_type_%widget_id%"), className = "inline_choicegroup",
                options = list(
                    list(key = "r", text = i18np$t("r")),
                    list(key = "rmarkdown", text = i18np$t("rmarkdown")),
                    list(key = "plot", text = i18np$t("plot"))
                ),
                value = selected_script_type_choicegroup
            )
        ),
        ace_editor_div,
        div(
            id = ns("r_script_result_div_%widget_id%"),
            verbatimTextOutput(ns("r_script_result_%widget_id%")), 
            style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"
        ),
        shinyjs::hidden(
            div(
                id = ns("rmarkdown_script_result_div_%widget_id%"),
                uiOutput(ns("rmarkdown_script_result_%widget_id%"))
            )
        ),
        shinyjs::hidden(
            div(
                id = ns("plot_script_result_div_%widget_id%"),
                div(id = ns("plot_div_%widget_id%"), plotOutput(ns("plot_script_result_%widget_id%")), style = "width:50%;"), br(),
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 0),
                    div(strong(i18np$t("plot_width"))),
                    div(shiny.fluent::Slider.shinyInput(ns("plot_width_%widget_id%"), value = 50, min = 1, max = 100), style = "width:300px")
                )
                
            )
        )
    ),
    shinyjs::hidden(
        div(
            id = ns("scripts_management_div_%widget_id%"),
            shiny.fluent::Stack(
                horizontal = TRUE, tokens = list(childrenGap = 10),
                make_textfield(i18n = i18n, ns = ns, label = "name", id = "script_name_%widget_id%", width = "300px"),
                div(shiny.fluent::PrimaryButton.shinyInput(ns("add_script_%widget_id%"), i18n$t("add")), style = "margin-top:38px;")
            ),
            DT::DTOutput(ns("scripts_management_datatable_%widget_id%")),
            shiny.fluent::Stack(
                horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("save_scripts_%widget_id%"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("delete_scripts_%widget_id%"), i18n$t("delete_selection"))
            )
        )
    )
)
