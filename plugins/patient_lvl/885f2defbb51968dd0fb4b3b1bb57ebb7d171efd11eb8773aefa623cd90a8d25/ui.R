notes_titles <- d$note %>% dplyr::distinct(note_title) %>% dplyr::collect() %>% dplyr::pull()

tagList(
    shiny.fluent::reactOutput(ns("delete_confirm_%widget_id%")),
    shiny.fluent::Pivot(
        id = ns("pivot_%widget_id%"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab_%widget_id%', item.props.id)")),
        shiny.fluent::PivotItem(id = "notes_%widget_id%", itemKey = "notes", headerText = i18np$t("notes")),
        shiny.fluent::PivotItem(id = "code_%widget_id%", itemKey = "code", headerText = i18np$t("code")),
        shiny.fluent::PivotItem(id = "scripts_management_%widget_id%", itemKey = "scripts_management", headerText = i18np$t("scripts_management")),
        shiny.fluent::PivotItem(id = "word_sets_%widget_id%", itemKey = "word_sets", headerText = i18np$t("word_sets"))
    ),
    conditionalPanel(
        condition = "input.current_tab_%widget_id% == 'notes_%widget_id%' || input.current_tab_%widget_id% == null", ns = ns, br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
            div(
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  div(shiny.fluent::Toggle.shinyInput(ns("show_all_notes_%widget_id%"), value = TRUE)),
                  div(i18np$t("show_all_notes"), style = "font-weight:bold; margin-bottom:5px;")
                )
            ),
            div(
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  div(shiny.fluent::Toggle.shinyInput(ns("chronological_order_%widget_id%"), value = TRUE)),
                  div(i18np$t("chronological_order"), style = "font-weight:bold; margin-bottom:5px;")
                )
            )
        ),
        uiOutput(ns("notes_%widget_id%"))
    ),
    conditionalPanel(
        condition = "input.current_tab_%widget_id% == 'word_sets_%widget_id%' || input.current_tab_%widget_id% == null", ns = ns, br(),
        div(
            
        )
    )
)
