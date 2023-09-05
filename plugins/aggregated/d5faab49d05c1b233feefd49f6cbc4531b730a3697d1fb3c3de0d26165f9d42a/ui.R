visits_occurrences_datetimes <- list()
visits_occurrences_datetimes$min_start <- min(d$visit_occurrence$visit_start_date, na.rm = TRUE)
visits_occurrences_datetimes$max_end <- max(d$visit_occurrence$visit_end_date, na.rm = TRUE)

visit_concepts <- d$visit_occurrence %>% dplyr::group_by(visit_concept_id) %>% dplyr::summarize(visit_concept_name = dplyr::first(visit_concept_name)) %>% dplyr::ungroup()

tagList(
    # Select datetime & units
    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
        div(shiny.fluent::DatePicker.shinyInput(ns("visits_occurrences_start_datetime_%widget_id%"), 
            label = i18np$t("visit_occurrences_start_date"), value = visits_occurrences_datetimes$min_start), style = "width:300px;"),
        div(shiny.fluent::DatePicker.shinyInput(ns("visits_occurrences_end_datetime_%widget_id%"), 
            label = i18np$t("visit_occurrences_end_date"), value = visits_occurrences_datetimes$max_end), style = "width:300px;"),
        div(shiny.fluent::Dropdown.shinyInput(ns("visit_concepts_%widget_id%"), label = i18np$t("hospital_units"),
            options = convert_tibble_to_list(visit_concepts, key_col = "visit_concept_id", text_col = "visit_concept_name"),
            value = visit_concepts$visit_concept_id, multiSelect = TRUE), style = "width:300px;"),
        div(shiny.fluent::PrimaryButton.shinyInput(ns("show_%widget_id%"), i18np$t("show")), style = "margin-top:29px;")
    ),
    
    # Show results
)
