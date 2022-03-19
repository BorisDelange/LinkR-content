##########################################
# Translations                           #
##########################################

new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
  "EN", "vistime_required", "Library vistime required to display the graph, please install it with install.packages('vistime')",
  "FR", "vistime_required", "Le package vistime est nécessaire pour afficher le graphique, merci de l'installer avec install.packages('vistime')",
  "EN", "plotly_required", "Library plotly required to display the graph, please install it with install.packages('plotly')",
  "FR", "plotly_required", "Le package plotly est nécessaire pour afficher le graphique, merci de l'installer avec install.packages('plotly')",
  "EN", "scales_required", "Library plotly required to display the graph, please install it with install.packages('scales')",
  "FR", "scales_required", "Le package plotly est nécessaire pour afficher le graphique, merci de l'installer avec install.packages('scales')",
  "EN", "no_data", "No data available",
  "FR", "no_data", "Pas de données disponibles",
)

##########################################
# Show / hide divs                       #
##########################################

divs_%group_id%_%study_id% <- c("div_figure_%group_id%_%study_id%", "div_settings_%group_id%_%study_id%")

observeEvent(input$div_%group_id%_%study_id%, {
  
  sapply(divs_%group_id%_%study_id% %>% setdiff(., input$div_%group_id%_%study_id%), shinyjs::hide)
  shinyjs::show(input$div_%group_id%_%study_id%)
})

##########################################
# Reload vistime                         #
##########################################

observeEvent(input$data_choice_%group_id%_%study_id%, shinyjs::hide("div_vistime_%group_id%_%study_id%"))

observeEvent(r$data_patient, shinyjs::hide("div_vistime_%group_id%_%study_id%"))
observeEvent(r$data_stay, shinyjs::hide("div_vistime_%group_id%_%study_id%"))

# Refresh only if we want graphs to be synchronized
#if (isTruthy(r$timeline_%module_id%_%study_id%)){
#    observeEvent(r$timeline_%module_id%_%study_id%(), {
#        if (length(r$sync_timeline_%module_id%_%study_id%) > 0){
#            if (r$sync_timeline_%module_id%_%study_id%) r$reload_%group_id%_%study_id% <- Sys.time()
#        }
#    })
#}

# Refresh also is we do not want synchronization, to restore min & max values for vistime timeline
#observeEvent(r$sync_timeline_%module_id%_%study_id%, r$reload_%group_id%_%study_id% <- Sys.time())

#observeEvent(r$reload_%group_id%_%study_id%, {

observeEvent(input$show_data_%group_id%_%study_id%, {
  
  if (!requireNamespace("vistime", quietly = TRUE)) output$message_bar_1_%group_id%_%study_id% <- renderUI(
    div(shiny.fluent::MessageBar(translate(language, "vistime_required", new_words), messageBarType = 3), style = "margin-top:10px;"))
  if (!requireNamespace("plotly", quietly = TRUE)) output$message_bar_2_%group_id%_%study_id% <- renderUI(
    div(shiny.fluent::MessageBar(translate(language, "plotly_required", new_words), messageBarType = 3), style = "margin-top:10px;"))
  if (!requireNamespace("scales", quietly = TRUE)) output$message_bar_3_%group_id%_%study_id% <- renderUI(
    div(shiny.fluent::MessageBar(translate(language, "scales_required", new_words), messageBarType = 3), style = "margin-top:10px;"))
  
  tryCatch({
    
    req(input$data_choice_%group_id%_%study_id%)
    req(!is.na(r$chosen_patient))
    
    # Accept labs_vitals & orders
    
    data_temp <- list()
    data_temp$orders <- tibble::tibble()
    data_temp$labs_vitals <- tibble::tibble()
    
    if (input$data_choice_%group_id%_%study_id% == "all_stays"){
      
      if (nrow(r$data_patient$labs_vitals) > 0){
        
        data_temp$labs_vitals <- 
          r$data_patient$labs_vitals %>%
          dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id")) %>%
          dplyr::mutate(unit = dplyr::case_when((thesaurus_item_unit != "" & !is.na(thesaurus_item_unit)) ~ thesaurus_item_unit, TRUE ~ unit)) %>%
          dplyr::select(-thesaurus_item_unit)
      }
      
      if (nrow(r$data_patient$orders) > 0){  
        
        data_temp$orders <- 
          r$data_patient$orders %>%
          dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id"))
      }
    }
    
    if (input$data_choice_%group_id%_%study_id% == "current_stay"){
      
      if (nrow(r$data_stay$labs_vitals) > 0){
        
        data_temp$labs_vitals <- 
          r$data_stay$labs_vitals %>%
          dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id")) %>%
          dplyr::mutate(unit = dplyr::case_when((thesaurus_item_unit != "" & !is.na(thesaurus_item_unit)) ~ thesaurus_item_unit, TRUE ~ unit)) %>%
          dplyr::select(-thesaurus_item_unit)
      }
      
      if (nrow(r$data_stay$orders) > 0){
        
        data_temp$orders <- 
          r$data_stay$orders %>%
          dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id"))
      }
    }
    
    if (nrow(data_temp$labs_vitals) > 0){
      
      suppressWarnings(data_temp$labs_vitals <-
          data_temp$labs_vitals %>%
          dplyr::mutate(datetime_start = lubridate::round_date(datetime_start, unit = "10 mins"), datetime_stop = lubridate::round_date(datetime_stop, unit = "10 mins")) %>%
          dplyr::transmute(
            start = datetime_start, 
            end = ifelse(!is.na(datetime_stop), datetime_stop, lubridate::ymd_hms("")), 
            group = paste0(display_name, " (", unit, ")"), color = colour,
            event = round(value_num, input$num_after_comma_%group_id%_%study_id%)) %>%
          dplyr::mutate_at(c("start", "end"), as.character) %>%
          dplyr::mutate(color = scales::seq_gradient_pal("yellow", "red")(scales::rescale(event))))
    }
    
    if (nrow(data_temp$orders) > 0){
      
      # If continuous = 1, keep datetime_start & datetime_end, take rate value
      # If continuous = 1 & datetime_start = datetime_stop, delete value (useless)
      # If continuous = 0, keep only datetime_start, take amount value
      
      data_temp$orders <-
        data_temp$orders %>%
        dplyr::mutate(datetime_start = lubridate::round_date(datetime_start, unit = "10 mins"), datetime_stop = lubridate::round_date(datetime_stop, unit = "10 mins")) %>%
        dplyr::filter(continuous == 0 | (continuous == 1 & datetime_start != datetime_stop)) %>%
        dplyr::transmute(
          continuous,
          start = datetime_start, 
          end = dplyr::case_when(continuous == 1 ~ datetime_stop, TRUE ~ datetime_start), 
          display_name, rate_unit, amount_unit, color = colour,
          event = dplyr::case_when(continuous == 1 ~ round(rate, input$num_after_comma_%group_id%_%study_id%), TRUE ~ round(amount, input$num_after_comma_%group_id%_%study_id%))) %>%
        dplyr::mutate_at(c("start", "end"), as.character)
      
      suppressWarnings(data_temp$orders_continuous <- 
          data_temp$orders %>%
          dplyr::filter(continuous == 1) %>%
          dplyr::mutate(group = paste0(display_name, " (", rate_unit, ")"), color = scales::seq_gradient_pal("yellow", "red")(scales::rescale(event))) %>%
          dplyr::mutate(rescaled = scales::rescale(event)))
      
      suppressWarnings(data_temp$orders <-
          data_temp$orders %>%
          dplyr::filter(continuous == 0) %>%
          dplyr::mutate(group = paste0(display_name, " (", amount_unit, ")"), color = scales::seq_gradient_pal("yellow", "red")(scales::rescale(event))) %>%
          dplyr::mutate(rescaled = scales::rescale(event)) %>%
          dplyr::bind_rows(data_temp$orders_continuous) %>%
          dplyr::select(-continuous, -rate_unit, -amount_unit, -display_name))
    }
    
    # If there's no data to show
    if (nrow(data_temp$labs_vitals) == 0 & nrow(data_temp$orders) == 0){
      output$message_bar_3_%group_id%_%study_id% <- renderUI(div(shiny.fluent::MessageBar(translate(language, "no_data", new_words), messageBarType = 0), style = "margin-top:10px;"))
      shinyjs::hide("div_vistime_%group_id%_%study_id%")
    }
    
    # Require vistime, plotly & scales libraries
    if (requireNamespace("vistime", quietly = TRUE) & requireNamespace("plotly", quietly = TRUE) & requireNamespace("scales", quietly = TRUE)){
      
      if (nrow(data_temp$orders) > 0 | nrow(data_temp$labs_vitals) > 0){
        shinyjs::show("div_vistime_%group_id%_%study_id%")
        output$message_bar_3_%group_id%_%study_id% <- renderUI("")
      }
      
      data_temp_merge <- tibble::tibble()
      if (nrow(data_temp$orders) == 0 & nrow(data_temp$labs_vitals) > 0) data_temp_merge <- data_temp$labs_vitals
      if (nrow(data_temp$orders) > 0 & nrow(data_temp$labs_vitals) == 0) data_temp_merge <- data_temp$orders
      if (nrow(data_temp$orders) > 0 & nrow(data_temp$labs_vitals) > 0) data_temp_merge <- data_temp$labs_vitals %>% dplyr::bind_rows(data_temp$orders)
      
      # Check if data is not empty
      if (nrow(data_temp_merge) > 0){
        
        if (input$data_choice_%group_id%_%study_id% == "all_stays"){
          datetime_start <- min(r$data_patient$stays$admission_datetime)
          datetime_stop <- max(r$data_patient$stays$discharge_datetime)
        }
        
        if (length(r$chosen_stay) > 0){ # For the tests in plugins creation zone
          if (input$data_choice_%group_id%_%study_id% == "current_stay" & !is.na(r$chosen_stay)){
            datetime_start <- min(r$data_stay$stay$admission_datetime)
            datetime_stop <- min(r$data_stay$stay$discharge_datetime)
          }
        }
        
        # If a synchronization between plots exists
        
        if (isTruthy(r$timeline_%module_id%_%study_id%)){
          
          # Synchronize only if synchronization toggle is set to TRUE
          if (r$sync_timeline_%module_id%_%study_id%){
            datetime_start <- lubridate::ymd_hms(stringr::str_sub(stringr::str_replace(r$timeline_%module_id%_%study_id%()$start, "T", " "), 1, -6))
            datetime_stop <- lubridate::ymd_hms(stringr::str_sub(stringr::str_replace(r$timeline_%module_id%_%study_id%()$stop, "T", " "), 1, -6))
          }
        }
        
        output$plotly_%group_id%_%study_id% <- plotly::renderPlotly({
          
          req(length(datetime_start) > 0)
          
          vistime::vistime(data_temp_merge, linewidth = 30) %>%
            plotly::layout(
              height = 160,
              xaxis = list(
                range = c(datetime_start, datetime_stop),
                tickfont = list(size = 10)),
              margin = list(l = 100, r = 60, b = 0, t = 0, pad = 0),
              yaxis = list(side = "left", tickfont = list(size = 10))
            ) %>%
            plotly::config(displayModeBar = F) -> p
          
          pp <- plotly::plotly_build(p)
          
          for (i in 1:length(pp$x$data)){
            if(pp$x$data[[i]]$mode == "text") pp$x$data[[i]]$textfont$size <- 12
          }
          pp
          
        })
      }
    }
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
  
})