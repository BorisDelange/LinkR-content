# TO DO
# - Problem with datetime / geographic zone (the selected date in DatePicker is not the good one : it is the next day, due to geographic zone / + 2 hours...)

# Show plots

observeEvent(input$show_%widget_id%, {
    %req%
    
    req(nrow(d$visit_detail) > 0)
    
    data <- list()
    
    print(input$visit_details_concepts_%widget_id%)
    
    # Demography
    
    data$demography <-
        d$visit_detail %>%
        dplyr::filter(
            visit_detail_start_date >= lubridate::ymd_hms(input$visit_details_start_date_%widget_id%),
            visit_detail_end_date <= lubridate::ymd_hms(input$visit_details_end_date_%widget_id%),
            visit_detail_concept_id %in% input$visit_details_concepts_%widget_id%
        ) %>%
        dplyr::select(person_id, visit_detail_id, visit_detail_start_datetime, visit_detail_end_datetime) %>%
        dplyr::left_join(
            d$person %>% dplyr::select(person_id, birth_datetime, gender_concept_name, death_datetime),
            by = "person_id"
        ) %>%
        dplyr::mutate(age = lubridate::interval(birth_datetime, visit_detail_start_datetime) / lubridate::years(1))
    
    ### Create age plot & uiOutput
    output$demography_age_plot_%widget_id% <- renderPlot({
        %req%
        data$demography %>%
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
            ggplot2::scale_x_continuous(limits = c(-1, NA), breaks = seq(0.000, max(data$demography$age, na.rm = TRUE), by = 10))

    })
    
    output$demography_age_ui_%widget_id% <- renderUI({
        %req%
        div(
            span(strong(i18np$t('missing_data')), " : ", style = "display:inline-block; width:150px;"), 
                nrow(data$demography %>% dplyr::filter(is.na(age))), " / ", nrow(data$demography),
                paste0(" (", round((nrow(data$demography %>% dplyr::filter(is.na(age))) / nrow(data$demography)), 1), " %)"), br(),
            span(strong(i18np$t('mean')), " : ", style = "display:inline-block; width:150px;"), round(mean(data$demography$age, na.rm = TRUE), 1), br(),
            span(strong(i18np$t('median')), " : ", style = "display:inline-block; width:150px;"), round(median(data$demography$age, na.rm = TRUE), 1), br(),
            span(strong(i18np$t('min')), " : ", style = "display:inline-block; width:150px;"), round(min(data$demography$age, na.rm = TRUE), 1), br(),
            span(strong(i18np$t('max')), " : ", style = "display:inline-block; width:150px;"), round(max(data$demography$age, na.rm = TRUE), 1)
        )
    })
    
    ### Create gender plot & uiOutput
    output$demography_gender_plot_%widget_id% <- renderPlot({
        %req%
        data$demography %>%
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
    })
    
    output$demography_gender_ui_%widget_id% <- renderUI({
        %req%
        div(
            span(strong(i18np$t('missing_data')), " : ", style = "display:inline-block; width:150px;"), 
                nrow(data$demography %>% dplyr::filter(is.na(gender_concept_name))), " / ", nrow(data$demography),
                paste0(" (", round((nrow(data$demography %>% dplyr::filter(is.na(gender_concept_name))) / nrow(data$demography)), 1), " %)"), br()
        )
    })
    
    ### Create mortality plot & uiOutput
    output$demography_mortality_plot_%widget_id% <- renderPlot({
        %req%
        data$demography %>%
            dplyr::mutate(status = dplyr::case_when(
                !is.na(death_datetime) & death_datetime <= visit_detail_end_datetime ~ i18np$t("dead"),
                TRUE ~ i18np$t("alive")
            )) %>%
            dplyr::group_by(gender_concept_name, status) %>%
            dplyr::summarize(count = dplyr::n()) %>%
            dplyr::mutate(percentage = count / sum(count) * 100) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(gender_concept_name, desc(status)) %>%
            dplyr::group_by(gender_concept_name) %>%
            dplyr::mutate(cum_percentage = cumsum(percentage) - 0.5 * percentage) %>%
            ggplot2::ggplot(ggplot2::aes(x = gender_concept_name, y = percentage, fill = status)) +
            ggplot2::geom_bar(stat = "identity", width = 0.6) +
            ggplot2::geom_text(ggplot2::aes(y = cum_percentage, label = sprintf("%.1f%%", percentage)), color = "gray40") +
            ggplot2::scale_fill_brewer(palette = "Blues", name = i18np$t("status")) +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = i18np$t("gender"), y = i18np$t("percentage")) +
            ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
            ggplot2::theme(
                axis.title.x = ggplot2::element_text(color = "gray40"),
                axis.title.y = ggplot2::element_text(color = "gray40"),
                plot.title = ggplot2::element_blank(),
                legend.text = ggplot2::element_text(color = "gray40"),
                legend.title = ggplot2::element_text(color = "gray40")
            )
    })
    
    # Unit
    
    # Treatments
    
    # Diagnoses
})
