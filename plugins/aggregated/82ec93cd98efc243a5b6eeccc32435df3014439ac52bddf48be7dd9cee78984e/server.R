# Config
use_debounce <- TRUE

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
    "age" = c("missing_data", "n_rows", "min", "iq1", "median", "mean", "iq3", "max"),
    "gender" = c("n_rows", "missing_data"),
    "mortality" = c("n_rows"),
    "hospital_units" = c("n_rows", "missing_data"),
    "admissions" = c("n_rows"),
    "length_of_stay" = character(),
    "readmissions" = character()
)

# -------------------------
# --- Show / hide divs ----
# -------------------------

divs <- c("var_choice_%widget_id%", "split_layout_right_%widget_id%", "plot_size_div_%widget_id%")

# Additional buttons for this plugin
output$additional_buttons_%widget_id% <- renderUI(actionButton(ns("maximize_fig_%widget_id%"), "", icon = icon("maximize")))

# Maximize fig
observeEvent(input$maximize_fig_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$maximize_fig_%widget_id%"))
    shinyjs::runjs(glue::glue("$('#{id}-split_layout_left_%widget_id%').css('width', '100%');"))
    shinyjs::delay(100, {
        output$additional_buttons_%widget_id% <- renderUI(actionButton(ns("minimize_fig_%widget_id%"), "", icon = icon("minimize")))
        sapply(divs, shinyjs::hide)
        m$create_plot_type_%widget_id% <- "show_plot"
        m$create_plot_trigger_%widget_id% <- Sys.time()
    })
})

# Minimize fig
observeEvent(input$minimize_fig_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$minimize_fig_%widget_id%"))
    shinyjs::runjs(glue::glue("$('#{id}-split_layout_left_%widget_id%').css('width', '50%');"))
    shinyjs::delay(100, {
        output$additional_buttons_%widget_id% <- renderUI(actionButton(ns("maximize_fig_%widget_id%"), "", icon = icon("maximize")))
        sapply(divs, shinyjs::show)
        m$create_plot_type_%widget_id% <- "show_plot"
        m$create_plot_trigger_%widget_id% <- Sys.time()
    })
})

# Show or hide stats table
observeEvent(input$show_stats_%widget_id%, {
    %req%
    req(input$show_stats_%widget_id%)
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$show_stats_%widget_id%"))
    
    if (input$show_stats_%widget_id%) shinyjs::show("table_div_%widget_id%")
    else shinyjs::hide("table_div_%widget_id%")
})

# Hide parameters div
observeEvent(input$show_params_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$show_params_%widget_id%"))

    if (input$show_params_%widget_id%){
        shinyjs::runjs(glue::glue("$('#{id}-split_layout_left_%widget_id%').css('width', '50%');"))
        shinyjs::show("split_layout_right_%widget_id%")
    }
    else {
        shinyjs::hide("split_layout_right_%widget_id%")
        shinyjs::runjs(glue::glue("$('#{id}-split_layout_left_%widget_id%').css('width', '100%');"))
    }
    
    # Reload plot
    m$render_results_%widget_id% <- Sys.time()  
})

# Table & plot side by side
observeEvent(input$side_by_side_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$side_by_side_%widget_id%"))
    
    if (input$side_by_side_%widget_id%){
        shinyjs::runjs(glue::glue("$('#{id}-table_and_plot_div_%widget_id%').css('display', 'flex');"))
        shinyjs::runjs(glue::glue("$('#{id}-plot_div_%widget_id%').css('width', '70%');"))
        shinyjs::runjs(glue::glue("$('#{id}-table_div_%widget_id%').css('width', '30%');"))
        shinyjs::runjs(glue::glue("$('#{id}-table_div_%widget_id%').css('margin-top', '0');"))
        shinyjs::runjs(glue::glue("$('#{id}-table_div_%widget_id%').css('margin-left', '5px');"))
    }
    else {
        shinyjs::runjs(glue::glue("$('#{id}-table_and_plot_div_%widget_id%').css('display', 'block');"))
        shinyjs::runjs(glue::glue("$('#{id}-plot_div_%widget_id%').css('width', '100%');"))
        shinyjs::runjs(glue::glue("$('#{id}-table_div_%widget_id%').css('width', '100%');"))
        shinyjs::runjs(glue::glue("$('#{id}-table_div_%widget_id%').css('margin-top', '5px');"))
        shinyjs::runjs(glue::glue("$('#{id}-table_div_%widget_id%').css('margin-left', '0');"))
    }
    
    # Reload plot
    m$render_results_%widget_id% <- Sys.time()  
})

# Table & plot width & height

# sapply(c("width", "height"), function(name){
#     
#     if (use_debounce) observeEvent(input[[paste0("plot_", name, "_%widget_id%")]], {
#         %req%
#         if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$plot_", name, "_%widget_id%"))
#         
#         if (name == "width") value <- paste0(input$plot_width_%widget_id%, "%")
#         else value <- input$plot_height_%widget_id% * 0.01 * 400
#         
#         shinyjs::runjs(glue::glue("$('#{id}-plot_div_%widget_id%').css('{name}', '{value}');"))
#         m$render_results_%widget_id% <- Sys.time()
#     }) %>% debounce(1000)
#     
#     else {
#         m[[paste0("debounce_plot_", name, "_%widget_id%")]] <- Sys.time()
#         observeEvent(input[[paste0("plot_", name, "_%widget_id%")]], {
#             %req%
#             if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$plot_", name, "_%widget_id%"))
#             
#             if (name == "width") value <- paste0(input$plot_width_%widget_id%, "%")
#             else value <- input$plot_height_%widget_id% * 0.01 * 400
#             
#             # Manual debounce (it seems debounce doesn't work everywhere)
#             
#             if (difftime(Sys.time(), m[[paste0("debounce_plot_", name, "_%widget_id%")]], units = "secs") > 1){
#                 m[[paste0("debounce_plot_", name, "_%widget_id%")]] <- Sys.time()
#                 
#                 shinyjs::runjs(glue::glue("$('#{id}-plot_div_%widget_id%').css('{name}', '{value}');"))
#                 if (name == "width") shinyjs::runjs(glue::glue("$('#{id}-table_div_%widget_id%').css('width', '{100 - input$plot_width_%widget_id%}%');"))
#                 
#                 m$create_plot_type_%widget_id% <- "show_plot"
#                 m$create_plot_trigger_%widget_id% <- Sys.time()
#             }
#         })
#     }
# })

# ----------------------------
# --- Render plot & table ----
# ----------------------------

observeEvent(input$check_all_hospital_units_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$check_all_hospital_units_%widget_id%"))
    shiny.fluent::updateDropdown.shinyInput(session, "hospital_units_%widget_id%", value = d$visit_detail %>% dplyr::distinct(visit_detail_concept_id) %>% dplyr::pull())
})
observeEvent(input$uncheck_all_hospital_units_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$uncheck_all_hospital_units_%widget_id%"))
    shiny.fluent::updateDropdown.shinyInput(session, "hospital_units_%widget_id%", value = integer(0))
})

observeEvent(input$var_choice_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$var_choice_%widget_id%"))
    
    # Show / hide admissions type choiceGroup
    if (input$var_choice_%widget_id% == "admissions") shinyjs::show("admissions_type_div_%widget_id%")
    else shinyjs::hide("admissions_type_div_%widget_id%")
    
    m$render_results_%widget_id% <- Sys.time()
})

observeEvent(input$show_plot_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$show_plot_%widget_id%"))
    
    m$render_results_%widget_id% <- Sys.time()
})

observeEvent(m$render_results_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer r$render_results_%widget_id%"))
    
    var_choice <- input$var_choice_%widget_id%
    
    # Requires that data is loaded
    req(d$person %>% dplyr::count() %>% dplyr::pull() > 0)
    req(d$visit_detail %>% dplyr::count() %>% dplyr::pull() > 0)
    req(length(m$omop_version) > 0)
    req(length(input$hospital_units_%widget_id%) > 0)
        
    result_ui <- tagList()
    plot_result <- ggplot2::ggplot()
    table_result <- tibble::tibble(var = character(), value = numeric())
    
    data <- list()
    
    if (m$omop_version %in% c("5.3", "5.4")){
        req(d$death %>% dplyr::count() %>% dplyr::pull() > 0)
        
        data$stays <-
            d$visit_detail %>%
            dplyr::select(person_id, visit_detail_id, visit_occurrence_id, visit_detail_concept_id, visit_detail_start_datetime, visit_detail_end_datetime) %>%
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
        data$stays <-
            d$visit_detail %>%
            dplyr::select(person_id, visit_detail_id, visit_occurrence_id, visit_detail_concept_id, visit_detail_start_datetime, visit_detail_end_datetime) %>%
            dplyr::left_join(
                d$person %>% dplyr::select(person_id, birth_datetime, death_datetime, gender_concept_id),
                by = "person_id"
            ) %>%
            dplyr::relocate(gender_concept_id, .after = "person_id")
    }
    
    m$temp <- input$hospital_units_%widget_id%
    
    data$stays <-
        data$stays %>%
        dplyr::collect() %>%
        # Filter on selected hospital units
        dplyr::filter(visit_detail_concept_id %in% input$hospital_units_%widget_id%) %>%
        # Add hospital unit names
        dplyr::left_join(
            d$dataset_all_concepts %>% dplyr::select(visit_detail_concept_id = concept_id_1, unit_name = concept_name_1),
            by = "visit_detail_concept_id"
        ) %>%
        dplyr::mutate(age = lubridate::interval(birth_datetime, visit_detail_start_datetime) / lubridate::years(1)) %>%
        dplyr::left_join(
            d$dataset_all_concepts %>% dplyr::filter(domain_id == "Gender") %>% dplyr::select(gender_concept_name = concept_name_1, gender_concept_id = concept_id_1),
            by = "gender_concept_id"
        )
        
    # Filter on selected start_datetime & end_datetime
    # For admissions, filter on visit_detail_start_datetime only
    if (var_choice == "admissions") data$stays <-
        data$stays %>%
        dplyr::filter(
            visit_detail_start_datetime >= as.POSIXct(input$start_datetime_%widget_id%),
            visit_detail_start_datetime <= as.POSIXct(input$end_datetime_%widget_id%),
        )
    else data$stays <-
        data$stays %>%
        dplyr::filter(
            visit_detail_start_datetime >= as.POSIXct(input$start_datetime_%widget_id%),
            visit_detail_end_datetime <= as.POSIXct(input$end_datetime_%widget_id%),
        )
        
    data$patients <-
        data$stays %>%
        # Keep only the first visit by patient
        dplyr::arrange(person_id, visit_detail_start_datetime) %>%
        dplyr::group_by(person_id) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
    
    # Age
    if (var_choice == "age"){
        
        plot_result <- 
            data$patients %>%
            dplyr::filter(!is.na(age)) %>%
            ggplot2::ggplot(ggplot2::aes(x = age)) +
            ggplot2::geom_histogram(bins = 50, fill = "#377EB8", color = "#FFFFFF") +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = i18np$t("age"), y = i18np$t("patients_occurrences")) +
            ggplot2::theme(
                axis.title.x = ggplot2::element_text(color = "gray40"),
                axis.title.y = ggplot2::element_text(color = "gray40"),
                panel.grid.minor.x = ggplot2::element_blank()
            ) +
            ggplot2::scale_x_continuous(limits = c(-1, NA), breaks = seq(0.0, max(data$patients$age, na.rm = TRUE), by = 10))
            
        # Apply stats functions
        for (stat_name in var_stats_functions[[var_choice]]){
            stat_value <- stats_functions[[stat_name]](data$patients$age)
            table_result <- table_result %>% dplyr::bind_rows(
                tibble::tibble(var = i18np$t(stat_name), value = stat_value)
            )
        }
        
        table_result <-
            table_result %>%
            dplyr::mutate(
                value = dplyr::case_when(
                    var %in% c(i18np$t("missing_data"), i18np$t("n_rows")) ~ as.character(as.integer(value)),
                    TRUE ~ as.character(value)
                ),
                var = dplyr::case_when(var == i18np$t("n_rows") ~ i18np$t("n_unique_patients"), TRUE ~ var)
            )
    }
    
    # Gender
    else if (var_choice == "gender"){
    
        plot_result <-
            data$patients %>%
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
            stat_value <- stats_functions[[stat_name]](data$patients$gender_concept_id)
            table_result <- table_result %>% dplyr::bind_rows(
                tibble::tibble(var = i18np$t(stat_name), value = stat_value)
            )
        }
        
        table_result <-
            table_result %>%
            dplyr::mutate(
                value = dplyr::case_when(
                    var %in% c(i18np$t("missing_data"), i18np$t("n_rows")) ~ as.character(as.integer(value)),
                    TRUE ~ as.character(value)
                ),
                var = dplyr::case_when(var == i18np$t("n_rows") ~ i18np$t("n_unique_patients"), TRUE ~ var)
            )
    }
    
    # Mortality
    else if (var_choice == "mortality"){
    
        plot_result <-
            data$patients %>%
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
        
        # Apply stats functions
        for (stat_name in var_stats_functions[[var_choice]]){
            stat_value <- stats_functions[[stat_name]](data$patients$death_datetime)
            table_result <- table_result %>% dplyr::bind_rows(
                tibble::tibble(var = i18np$t(stat_name), value = stat_value)
            )
        }
        
        table_result <-
            table_result %>%
            dplyr::mutate(
                value = dplyr::case_when(
                    var == i18np$t("n_rows") ~ as.character(as.integer(value)),
                    TRUE ~ as.character(value)
                ),
                var = dplyr::case_when(var == i18np$t("n_rows") ~ i18np$t("n_unique_patients"), TRUE ~ var)
            )
    }
    
    # Hospital units
    else if (var_choice == "hospital_units"){
        
        plot_result <-
            data$stays %>%
            dplyr::filter(!is.na(unit_name)) %>%
            dplyr::count(unit_name, sort = TRUE) %>%
            dplyr::top_n(10, wt = n) %>%
            ggplot2::ggplot(ggplot2::aes(x = reorder(unit_name, n), y = n)) +
            ggplot2::geom_bar(stat = "identity", fill = "#377EB8", color = "#FFFFFF") +
            ggplot2::coord_flip() +
            ggplot2::labs(x = i18np$t("hospital_unit"), y = i18np$t("stay_occurrences")) +
            ggplot2::theme_minimal()
        
        # Apply stats functions
        for (stat_name in var_stats_functions[[var_choice]]){
            stat_value <- stats_functions[[stat_name]](data$stays$unit_name)
            table_result <- table_result %>% dplyr::bind_rows(
                tibble::tibble(var = i18np$t(stat_name), value = stat_value)
            )
        }
        
        table_result <-
            table_result %>%
            dplyr::mutate(
                value = dplyr::case_when(
                    var %in% c(i18np$t("missing_data"), i18np$t("n_rows")) ~ as.character(as.integer(value)),
                    TRUE ~ as.character(value)
                ),
                var = dplyr::case_when(var == i18np$t("n_rows") ~ i18np$t("n_stays"), TRUE ~ var)
            ) %>%
            # Add visit_detail min & max datetimes
            dplyr::bind_rows(
                tibble::tribble(
                    ~var, ~value,
                    i18np$t("min_start_datetime"), data$stays$visit_detail_start_datetime %>% as.Date() %>% min(na.rm = TRUE) %>% format_datetime(language = language, type = "date") %>% as.character(),
                    i18np$t("max_end_datetime"), data$stays$visit_detail_end_datetime %>% as.Date() %>% max(na.rm = TRUE) %>% format_datetime(language = language, type = "date") %>% as.character()
                )
            )
    }
    
    # Admissions
    else if (var_choice == "admissions"){
        
        # If we want to see hospital admissions, group by visit_occurrence_id (corresponds to one hospital admission)
        if (input$admissions_type_%widget_id% == "hospital"){
            data$stays <-
                data$stays %>%
                dplyr::group_by(visit_occurrence_id) %>%
                dplyr::summarize(visit_detail_start_datetime = min(visit_detail_start_datetime)) %>%
                dplyr::ungroup()
        }
        
        plot_result <-
            data$stays %>%
            ggplot2::ggplot(ggplot2::aes(x = visit_detail_start_datetime)) +
            ggplot2::geom_histogram(bins = 30, fill = "#377EB8", color = "#FFFFFF") +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = i18np$t("date"), y = i18np$t("admissions_occurrences"))
            
        # Apply stats functions
        for (stat_name in var_stats_functions[[var_choice]]){
            stat_value <- stats_functions[[stat_name]](data$stays$visit_detail_start_datetime)
            table_result <- table_result %>% dplyr::bind_rows(
                tibble::tibble(var = i18np$t(stat_name), value = stat_value)
            )
        }
        
        # Calculate number of admissions per day
        admissions_per_day <-
            data$stays %>%
            dplyr::mutate(date = as.Date(visit_detail_start_datetime)) %>%
            dplyr::group_by(date) %>%
            dplyr::summarize(num_admissions = dplyr::n()) %>%
            dplyr::ungroup() %>%
            dplyr::pull(num_admissions) %>%
            mean() %>%
            round(1)
        
        table_result <-
            table_result %>%
            dplyr::mutate(
                value = dplyr::case_when(
                    var == i18np$t("n_rows") ~ as.character(as.integer(value)),
                    TRUE ~ as.character(value)
                ),
                var = dplyr::case_when(var == i18np$t("n_rows") ~ i18np$t("n_admissions"), TRUE ~ var)
            ) %>%
            # Add visit_detail min & max datetimes
            dplyr::bind_rows(
                tibble::tribble(
                    ~var, ~value,
                    i18np$t("mean_admissions_per_day"), as.character(admissions_per_day),
                    i18np$t("min_start_datetime"), data$stays$visit_detail_start_datetime %>% as.Date() %>% min(na.rm = TRUE) %>% format_datetime(language = language, type = "date") %>% as.character(),
                    i18np$t("max_end_datetime"), data$stays$visit_detail_start_datetime %>% as.Date() %>% max(na.rm = TRUE) %>% format_datetime(language = language, type = "date") %>% as.character()
                )
            )
    }
    
    output$title_%widget_id% <- renderUI(
        div(toupper(i18np$t(input$var_choice_%widget_id%)), style = paste0("background-color:#377EB8; color:white; font-weight:bold; font-size:18px; text-align:center; display:inline-block;",
            "position:relative; right:50%; left:50%; transform: translate(-50%, -50%); margin-top:15px; padding:0px 5px 0px 5px;"))
    )
    
    table_result <- setNames(table_result, c(i18np$t("variable"), i18np$t("value")))
    
    output$plot_%widget_id% <- renderPlot(plot_result)
    output$table_%widget_id% <- renderTable(table_result)
})

# Idea for a Sankey diagram ?
# data <-
#     d$visit_detail %>%
#     dplyr::select(person_id, visit_detail_id, visit_detail_concept_id, visit_detail_start_datetime, visit_detail_end_datetime) %>%
#     dplyr::collect() %>%
#     dplyr::arrange(person_id, visit_detail_start_datetime) %>%
#     dplyr::group_by(person_id) %>%
#     dplyr::mutate(
#         preceding_visit_detail_concept_id = dplyr::lag(visit_detail_concept_id),
#         preceding_visit_detail_end_datetime = dplyr::lag(visit_detail_end_datetime)
#     ) %>%
#     dplyr::ungroup() %>%
#     dplyr::filter(
#         visit_detail_start_datetime == preceding_visit_detail_end_datetime,
#         visit_detail_concept_id != preceding_visit_detail_concept_id
#     ) %>%
#     dplyr::left_join(
#         d$dataset_all_concepts %>% dplyr::select(visit_detail_concept_id = concept_id_1, unit_name = concept_name_1),
#         by = "visit_detail_concept_id"
#     ) %>%
#     dplyr::left_join(
#         d$dataset_all_concepts %>% dplyr::select(preceding_visit_detail_concept_id = concept_id_1, preceding_unit_name = concept_name_1),
#         by = "preceding_visit_detail_concept_id"
#     ) %>%
#     dplyr::select(preceding_visit_detail_end_datetime, preceding_unit_name, visit_detail_start_datetime, unit_name) %>%
#     dplyr::count(source = preceding_unit_name, target = unit_name) %>%
#     dplyr::arrange(dplyr::desc(n)) %>%
#     dplyr::slice(1:10)
# 
# nodes <- tibble::tibble(name = unique(c(as.character(data$source), as.character(data$target))))
# links <-
#     data %>%
#     dplyr::mutate(
#         source = match(source, nodes$name) - 1,
#         target = match(target, nodes$name) - 1
#     )
#     
# networkD3::sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", Value = "n", NodeID = "name")

