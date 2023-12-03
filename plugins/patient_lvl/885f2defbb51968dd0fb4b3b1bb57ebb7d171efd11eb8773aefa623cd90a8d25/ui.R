# Get widget options
sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id%", .con = r$db)
widget_options <- DBI::dbGetQuery(m$db, sql)
notes <- widget_options %>% dplyr::filter(name == "script") %>% dplyr::select(id = value_num, name = value)
selected_script <- NULL
selected_script_result <- widget_options %>% dplyr::filter(name == "selected_script")
if (nrow(selected_script_result) > 0) if ((selected_script_result %>% dplyr::pull(value_num)) %in% notes$id) selected_script <- selected_script_result %>% dplyr::pull(value_num)

# notes_titles <- d$note %>% dplyr::distinct(note_title) %>% dplyr::collect() %>% dplyr::pull()

tagList(
    shiny.fluent::reactOutput(ns("delete_confirm_%widget_id%")),
    shiny.fluent::Pivot(
        id = ns("pivot_%widget_id%"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab_%widget_id%', item.props.id)")),
        shiny.fluent::PivotItem(id = "notes_div_%widget_id%", itemKey = "notes", headerText = i18np$t("notes")),
        shiny.fluent::PivotItem(id = "code_div_%widget_id%", itemKey = "code", headerText = i18np$t("code")),
        shiny.fluent::PivotItem(id = "word_sets_div_%widget_id%", itemKey = "word_sets", headerText = i18np$t("word_sets")),
        shiny.fluent::PivotItem(id = "scripts_management_div_%widget_id%", itemKey = "scripts_management", headerText = i18np$t("scripts_management"))
    ),
    div(
        id = ns("notes_div_%widget_id%"),  br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            div(shiny.fluent::Dropdown.shinyInput(ns("script_choice_%widget_id%"),
                options = convert_tibble_to_list(notes, key_col = "id", text_col = "name"), value = selected_script), 
                style = "width:300px"),
            shiny.fluent::DefaultButton.shinyInput(ns("save_%widget_id%"), i18np$t("save")),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::Toggle.shinyInput(ns("hide_params_%widget_id%"), value = FALSE, style = "margin-top:5px;"),
                div(class = "toggle_title", i18np$t("hide_params"), style = "padding-top:5px;")
            )
        ),  br(),
        div(
            id = ns("params_div_%widget_id%"),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                div(
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                      div(shiny.fluent::Toggle.shinyInput(ns("show_all_notes_%widget_id%"), value = FALSE)),
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
            DT::DTOutput(ns("notes_datatable_%widget_id%"))
        ),
        uiOutput(ns("notes_%widget_id%"))
    ),
    shinyjs::hidden(
        div(
            id = ns("code_div_%widget_id%"), br(),
            div(
                
            )
        )
    ),
    shinyjs::hidden(
        div(
            id = ns("word_sets_div_%widget_id%"), br(),
            div(
                
            )
        )
    ),
    shinyjs::hidden(
        div(
            id = ns("scripts_management_div_%widget_id%"), br(),
            div(
                id = ns("scripts_management_tab_%widget_id%"),
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
)
