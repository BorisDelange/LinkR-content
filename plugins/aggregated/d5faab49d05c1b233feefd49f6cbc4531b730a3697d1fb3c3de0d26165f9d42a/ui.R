visits_occurrences_datetimes <- list()
visits_occurrences_datetimes$min_start <- min(d$visit_occurrence$visit_start_datetime, na.rm = TRUE)
visits_occurrences_datetimes$max_end <- max(d$visit_occurrence$visit_end_datetime, na.rm = TRUE)

visit_concepts <- d$visit_detail %>% dplyr::group_by(visit_detail_concept_id) %>% dplyr::summarize(visit_detail_concept_name = dplyr::first(visit_detail_concept_name)) %>% dplyr::ungroup()

# TO DO
### Add a dropdown for hospital

tagList(
    # Select category
    shiny.fluent::Pivot(
        id = ns("pivot_%widget_id%"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab_%widget_id%', item.props.id)")),
        shiny.fluent::PivotItem(id = "demography", itemKey = "demography", headerText = i18np$t("demography")),
        shiny.fluent::PivotItem(id = "unit", itemKey = "unit", headerText = i18np$t("hospital_unit")),
        shiny.fluent::PivotItem(id = "treatments", itemKey = "treatments", headerText = i18np$t("treatments")),
        shiny.fluent::PivotItem(id = "diagnoses", itemKey = "diagnoses", headerText = i18np$t("diagnoses"))
    ), br(),
    # Select datetime & units
    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
        div(shiny.fluent::DatePicker.shinyInput(ns("visit_details_start_date_%widget_id%"), 
            label = i18np$t("visit_details_start_date"), value = visits_occurrences_datetimes$min_start), style = "width:300px;"),
        div(shiny.fluent::DatePicker.shinyInput(ns("visit_details_end_date_%widget_id%"), 
            label = i18np$t("visit_details_end_date"), value = visits_occurrences_datetimes$max_end), style = "width:300px;"),
        div(shiny.fluent::Dropdown.shinyInput(ns("visit_details_concepts_%widget_id%"), label = i18np$t("hospital_units"),
            options = convert_tibble_to_list(visit_concepts, key_col = "visit_detail_concept_id", text_col = "visit_detail_concept_name"),
            value = visit_concepts$visit_detail_concept_id, multiSelect = TRUE), style = "width:300px;"),
        div(shiny.fluent::PrimaryButton.shinyInput(ns("show_%widget_id%"), i18np$t("show")), style = "margin-top:29px;")
    ), br(),
    
    # Show results
    
    # Demography
    ### Age, gender, number of distinct patients, proportion of death
    conditionalPanel("input.current_tab_%widget_id% == 'demography' || input.current_tab_%widget_id% == null", ns = ns,
        div(
            style = "display:flex;",
            div(
                div(toupper(i18np$t("age")), style = paste0("background-color:#377EB8; color:white; font-weight:bold; font-size:18px; text-align:center; display:inline-block;",
                    "position:relative; right:50%; left:50%; transform: translate(-50%, -50%); margin-top:15px; padding:0px 5px 0px 5px;")), br(),
                plotOutput(ns("demography_age_plot_%widget_id%")),
                style = "width:33%; margin-right:10px; border:1px solid; padding:5px; position:relative;"
            ),
            div(
                div(toupper(i18np$t("gender")), style = paste0("background-color:#377EB8; color:white; font-weight:bold; font-size:18px; text-align:center; display:inline-block;",
                    "position:relative; right:50%; left:50%; transform: translate(-50%, -50%); margin-top:15px; padding:0px 5px 0px 5px;")), br(),
                plotOutput(ns("demography_gender_plot_%widget_id%")),
                style = "width:34%; margin-right:10px; border:1px solid; padding:5px; position:relative;"
            ),
            div(
                div(toupper(i18np$t("mortality")), style = paste0("background-color:#377EB8; color:white; font-weight:bold; font-size:18px; text-align:center; display:inline-block;",
                    "position:relative; right:50%; left:50%; transform: translate(-50%, -50%); margin-top:15px; padding:0px 5px 0px 5px;")), br(),
                plotOutput(ns("demography_mortality_plot_%widget_id%")),
                style = "width:33%; border:1px solid; padding:5px; position:relative;"
            )
        ), br(),
        div(
            style = "display:flex;",
            div(
                style = "width:33%; height:100px; margin-right:10px; border:1px solid; padding:5px; position:relative;",
                uiOutput(ns("demography_age_ui_%widget_id%"))
            ),
            div(
                style = "width:34%; height:100px; margin-right:10px; border:1px solid; padding:5px; position:relative;",
                uiOutput(ns("demography_gender_ui_%widget_id%"))
            ),
            div(
                style = "width:33%; height:100px; border:1px solid; padding:5px; position:relative;"
            )
        )
    ),
    
    # Unit
    ### Number of admissions, length of stay, bed occupancy
    conditionalPanel("input.current_tab_%widget_id% == 'unit'", ns = ns,
        div("Unit")
    ),
    
    # Treatments
    ### Patients under mechanical ventilation, duration of MV, under RRT, under ECMO, tidal volume
    conditionalPanel("input.current_tab_%widget_id% == 'treatments'", ns = ns,
        div("Treatments")
    ),
    
    # Diagnoses
    ### Admission diagnoses
    conditionalPanel("input.current_tab_%widget_id% == 'diagnoses'", ns = ns,
        div("Diagnoses")
    )
)
