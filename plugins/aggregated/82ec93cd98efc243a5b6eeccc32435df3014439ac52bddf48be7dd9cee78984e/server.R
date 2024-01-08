# Stats functions
stats_functions <- list(
    "mean" = function(x) mean(x, na.rm = TRUE) %>% round(1),
    "iq1"  = function(x) quantile(x, probs = 0.25, na.rm = TRUE) %>% round(1),
    "median"  = function(x) median(x, na.rm = TRUE) %>% round(1),
    "iq3"  = function(x) quantile(x, probs = 0.75, na.rm = TRUE) %>% round(1),
    "min"  = function(x) min(x, na.rm = TRUE) %>% round(1),
    "max"  = function(x) max(x, na.rm = TRUE) %>% round(1),
    "missing_data" = function(x) sum(is.na(x)) %>% round(0),
    "n_rows" = function(x) length(x) %>% round(0)
)

# Stats functions per variable
var_stats_functions <- list(
    "age" = c("n_rows", "missing_data", "mean", "iq1", "median", "iq3", "min", "max"),
    "gender" = c("n_rows", "missing_data"),
    "mortality" = character(),
    "stays" = character(),
    "length_of_stay" = character(),
    "readmissions" = character()
)

# When a var is selected

observeEvent(input$var_choice_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$var_choice_%widget_id%"))
    
    var_choice <- input$var_choice_%widget_id%
    
    # Requires that data is loaded
    req(d$person %>% dplyr::count() %>% dplyr::pull() > 0)
    req(m$omop_version)
        
    result_ui <- tagList()
    result_plot <- ggplot2::ggplot()
    result_table <- tibble::tibble(var = character(), value = numeric())
    
    data <- tibble::tibble()
    
    if (m$omop_version %in% c("5.3", "5.4")){
        data <-
            d$visit_detail %>%
            dplyr::select(person_id, visit_detail_id, visit_detail_start_datetime, visit_detail_end_datetime) %>%
            dplyr::left_join(
                d$person %>%
                    dplyr::mutate(birth_datetime = dplyr::case_when(
                        is.na(birth_datetime) ~ lubridate::ymd_hms(paste0(year_of_birth, "-01-01 00:00:00")),
                        TRUE ~ birth_datetime
                    )) %>%
                    dplyr::select(person_id, birth_datetime, gender_concept_id),
                by = "person_id"
            ) %>%
            dplyr::relocate(gender_concept_id, .after = "person_id") %>%
            dplyr::left_join(
                d$death %>% dplyr::select(person_id, death_datetime),
                by = "person_id"
            )
    }
    else if (m$omop_version == "6.0"){
        data <-
            d$visit_detail %>%
            dplyr::select(person_id, visit_detail_id, visit_detail_start_datetime, visit_detail_end_datetime) %>%
            dplyr::left_join(
                d$person %>% dplyr::select(person_id, birth_datetime, death_datetime, gender_concept_id),
                by = "person_id"
            ) %>%
            dplyr::relocate(gender_concept_id, .after = "person_id")
    }
    
    data <-
        data %>%
        dplyr::collect() %>%
        dplyr::mutate(age = lubridate::interval(birth_datetime, visit_detail_start_datetime) / lubridate::years(1)) %>%
        dplyr::left_join(
            d$dataset_all_concepts %>% dplyr::filter(domain_id == "Gender") %>% dplyr::select(gender_concept_name = concept_name_1, gender_concept_id = concept_id_1),
            by = "gender_concept_id"
        )
    
    # Age
    if (var_choice == "age"){
        result_plot <- 
            data %>%
            dplyr::filter(!is.na(age)) %>%
            ggplot2::ggplot(ggplot2::aes(x = age)) +
            ggplot2::geom_histogram(bins = 50, fill = "#377EB8", color = "#FFFFFF") +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = i18np$t("age"), y = i18np$t("stay_age_occurrences")) +
            ggplot2::theme(
                axis.title.x = ggplot2::element_text(color = "gray40"),
                axis.title.y = ggplot2::element_text(color = "gray40"),
                panel.grid.minor.x = ggplot2::element_blank()
            ) +
            ggplot2::scale_x_continuous(limits = c(-1, NA), breaks = seq(0.000, max(data$age, na.rm = TRUE), by = 10))
            
        # Apply stats functions
        for (stat_name in var_stats_functions[[var_choice]]){
            stat_value <- stats_functions[[stat_name]](data$age)
            result_table <- result_table %>% dplyr::bind_rows(
                tibble::tibble(var = i18np$t(stat_name), value = stat_value)
            )
        }
        
        result_table <-
            result_table %>%
            dplyr::mutate(
                value = dplyr::case_when(
                    var %in% c(i18np$t("missing_data"), i18np$t("n_rows")) ~ as.character(as.integer(value)),
                    TRUE ~ as.character(value)
                )
            )
    }
    
    # Gender
    else if (var_choice == "gender"){
        result_plot <-
            data %>%
            dplyr::filter(!is.na(gender_concept_name)) %>%
            dplyr::select(gender = gender_concept_name) %>%
            dplyr::group_by(gender) %>%
            dplyr::summarize(count = dplyr::n()) %>%
            dplyr::mutate(percentage = count / sum(count)) %>%
            ggplot2::ggplot(ggplot2::aes(x = "", y = count, fill = gender)) +
            ggplot2::geom_bar(stat = "identity", width = 1, color = "#FFFFFF") +
            ggplot2::geom_text(ggplot2::aes(label = scales::percent(percentage)), position = ggplot2::position_stack(vjust = 0.5), color = "gray40") +
            ggplot2::coord_polar("y") +
            ggplot2::theme_minimal() +
            ggplot2::scale_fill_brewer(palette = "Blues", name = i18np$t("gender")) +
            ggplot2::labs(x = "", y = "") +
            ggplot2::theme(
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank(),
                panel.grid  = ggplot2::element_blank()
            )
        
        # Apply stats functions
        for (stat_name in var_stats_functions[[var_choice]]){
            stat_value <- stats_functions[[stat_name]](data$gender_concept_id)
            result_table <- result_table %>% dplyr::bind_rows(
                tibble::tibble(var = i18np$t(stat_name), value = stat_value)
            )
        }
    }
    
    output$title_%widget_id% <- renderUI(
        div(toupper(i18np$t(input$var_choice_%widget_id%)), style = paste0("background-color:#377EB8; color:white; font-weight:bold; font-size:18px; text-align:center; display:inline-block;",
            "position:relative; right:50%; left:50%; transform: translate(-50%, -50%); margin-top:15px; padding:0px 5px 0px 5px;"))
    )
    
    result_table <- setNames(result_table, c(i18np$t("variable"), i18np$t("value")))
    
    output$plot_%widget_id% <- renderPlot(result_plot)
    output$table_%widget_id% <- renderTable(result_table)
})
