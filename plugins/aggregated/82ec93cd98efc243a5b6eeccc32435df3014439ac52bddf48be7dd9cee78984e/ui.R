# Options for patients var dropdown
var_choice_options <- list(
    list(key = "age", text = i18np$t("age")),
    list(key = "gender", text = i18np$t("gender")),
    list(key = "mortality", text = i18np$t("mortality")),
    list(key = "admissions", text = i18np$t("admissions")),
    list(key = "hospital_units", text = i18np$t("hospital_units")),
    list(key = "length_of_stay", text = i18np$t("length_of_stay")),
    list(key = "readmissions", text = i18np$t("readmissions"))
)

if (d$visit_detail %>% dplyr::count() %>% dplyr::pull() > 0){
    min_visit_detail_start_datetime <- d$visit_detail %>% dplyr::summarize(minimum = min(visit_detail_start_datetime, na.rm = TRUE)) %>% dplyr::pull(minimum)
    max_visit_detail_end_datetime <- d$visit_detail %>% dplyr::summarize(maximum = max(visit_detail_end_datetime, na.rm = TRUE)) %>% dplyr::pull(maximum)
    
    # Get various hospital units
    hospital_units_options <-
        d$visit_detail %>%
        dplyr::count(visit_detail_concept_id, sort = TRUE) %>%
        dplyr::collect() %>%
        dplyr::left_join(
            d$dataset_all_concepts %>% dplyr::select(visit_detail_concept_id = concept_id_1, unit_name = concept_name_1),
            by = "visit_detail_concept_id"
        )
} else {
    min_visit_detail_start_datetime <- ""
    max_visit_detail_end_datetime <- ""
}

tagList(
    div(
        br(),
        div(shiny.fluent::Dropdown.shinyInput(ns("var_choice_%widget_id%"), options = var_choice_options, value = "age"), style = "width:300px"),
        div(
            style = "display:flex;",
            div(
                id = ns("split_layout_left_%widget_id%"),
                style = "padding-right:10px; width:50%;", br(),
                uiOutput(ns("title_%widget_id%")),
                div(
                    id = ns("table_and_plot_div_%widget_id%"),
                    div(id = ns("plot_div_%widget_id%"), plotOutput(ns("plot_%widget_id%")), style = "border:solid 2px #EFEEEE; width:70%;"),
                    div(id = ns("table_div_%widget_id%"), tableOutput(ns("table_%widget_id%")), style = "border:solid 2px #EFEEEE; margin-left:5px; width:30%;"),
                    style = "display:flex;"
                ),
                br(),
                div(
                    id = ns("plot_size_div_%widget_id%"),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        div(
                            div(strong(i18np$t("plot_width")), style = "margin-top:6px;"),
                            div(shiny.fluent::Slider.shinyInput(ns("plot_width_%widget_id%"), value = 100, min = 1, max = 100), style = "width:200px; margin-left:-8px; padding-top:4px;"),
                            style = "width:33%; max-width:200px;"
                        ),
                        div(
                            div(strong(i18np$t("plot_height")), style = "margin-top:6px;"),
                            div(shiny.fluent::Slider.shinyInput(ns("plot_height_%widget_id%"), value = 100, min = 1, max = 100), style = "width:200px; margin-left:-8px; padding-top:4px;"),
                            style = "width:33%; max-width:200px;"
                        ),
                        div(
                            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                                shiny.fluent::Toggle.shinyInput(ns("show_params_%widget_id%"), value = TRUE, style = "margin-top:5px;"),
                                div(class = "toggle_title", i18np$t("show_params"), style = "padding-top:5px;")
                            ),
                            style = "width:34%; max-width:300px; margin-top:15px;"
                        )
                    )
                )
            ),
            div(
                id = ns("split_layout_right_%widget_id%"),
                style = "padding-left:10px; width:50%; margin-top:60px;",
                shiny.fluent::Stack(
                    horizontal = TRUE, tokens = list(childrenGap = 10),
                    div(
                        div(strong(i18np$t("hospital_units")), style = "margin-bottom:10px;"),
                        shiny.fluent::Dropdown.shinyInput(ns("hospital_units_%widget_id%"), 
                            options = hospital_units_options %>% convert_tibble_to_list(key_col = "visit_detail_concept_id", text_col = "unit_name"),
                            value = hospital_units_options %>% dplyr::pull(visit_detail_concept_id),
                            multiSelect = TRUE), 
                        style = "width:50%;"
                    ),
                    div(
                        shiny.fluent::DefaultButton.shinyInput(ns("check_all_hospital_units_%widget_id%"), i18np$t("check_all")),
                        style = "margin-top:28px;"
                    ),
                    div(
                        shiny.fluent::DefaultButton.shinyInput(ns("uncheck_all_hospital_units_%widget_id%"), i18np$t("uncheck_all")),
                        style = "margin-top:28px;"
                    )
                ), br(),
                shiny.fluent::Stack(
                    horizontal = TRUE, tokens = list(childrenGap = 10),
                    div(
                        div(strong(i18np$t("start_datetime")), style = "margin-bottom:10px;"),
                        shiny.fluent::DatePicker.shinyInput(ns("start_datetime_%widget_id%"), value = min_visit_detail_start_datetime),
                        style = "width:50%;"
                    ),
                    div(
                        div(strong(i18np$t("end_datetime")), style = "margin-bottom:10px;"),
                        shiny.fluent::DatePicker.shinyInput(ns("end_datetime_%widget_id%"), value = max_visit_detail_end_datetime),
                        style = "width:50%;"
                    )
                ),
                shinyjs::hidden(
                    div(
                        id = ns("admissions_type_div_%widget_id%"),
                        br(),
                        shiny.fluent::ChoiceGroup.shinyInput(ns("admissions_type_%widget_id%"), value = "hospital", 
                            options = list(
                                list(key = "hospital", text = i18np$t("hospital_admissions")),
                                list(key = "unit", text = i18np$t("hospital_units_admissions"))
                            ), className = "inline_choicegroup")
                    )
                ),
                div(shiny.fluent::PrimaryButton.shinyInput(ns("show_plot_%widget_id%"), i18np$t("show_plot")), style = "margin-top:28px;"),
                br(), hr(), br(),
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    shiny.fluent::Toggle.shinyInput(ns("show_stats_%widget_id%"), value = TRUE, style = "margin-top:5px;"),
                    div(class = "toggle_title", i18np$t("show_stats"), style = "padding-top:5px;")
                ),
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    shiny.fluent::Toggle.shinyInput(ns("side_by_side_%widget_id%"), value = TRUE, style = "margin-top:5px;"),
                    div(class = "toggle_title", i18np$t("table_plot_side_by_side"), style = "padding-top:5px;")
                )
            )
        )
    ),
    div(
        id = ns("patients_tab_%widget_id%"),
    )
)

