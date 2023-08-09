tagList(
    shiny.fluent::Pivot(
        id = ns("pivot_%widget_id%"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab_%widget_id%', item.props.id)")),
        shiny.fluent::PivotItem(id = "script_%widget_id%", itemKey = "script", headerText = i18np$t("script")),
        shiny.fluent::PivotItem(id = "scripts_management_%widget_id%", itemKey = "scripts_management", headerText = i18np$t("scripts_management"))
    ),
    conditionalPanel(
        condition = "input.current_tab_%widget_id% == 'script_%widget_id%' || input.current_tab_%widget_id% == null", ns = ns, br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            div(shiny.fluent::Dropdown.shinyInput(ns("script_choice_%widget_id%")), style = "width:300px;"),
            div(style = "width:10px"),
            shiny.fluent::ChoiceGroup.shinyInput(
                ns("script_type_%widget_id%"), className = "inline_choicegroup",
                options = list(
                    list(key = "r", text = i18np$t("r")),
                    list(key = "rmarkdown", text = i18np$t("rmarkdown")),
                    list(key = "plot", text = i18np$t("plot"))
                ),
                value = "r"
            )
        ),
        div(shinyAce::aceEditor(
            ns("script_code_%widget_id%"), "", mode = "r", 
                code_hotkeys = list(
                    "r", list(
                      run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                      run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER")
                    )
                ),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
            ), style = "width: 100%;"),
        shiny.fluent::Stack(
            horizontal = TRUE, tokens = list(childrenGap = 10),
            shiny.fluent::PrimaryButton.shinyInput(ns("run_code_%widget_id%"), i18n$t("run_code")),
            shiny.fluent::DefaultButton.shinyInput(ns("save_code_%widget_id%"), i18n$t("save"))
        ), br(),
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
                plotOutput(ns("plot_script_result_%widget_id%"))
            )
        )
    ),
    conditionalPanel(
        condition = "input.current_tab_%widget_id% == 'scripts_management_%widget_id%' || input.current_tab_%widget_id% == null", ns = ns, br(),
        DT::DTOutput(ns("scripts_management_datatable_%widget_id%"))
    )
)
