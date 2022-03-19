##########################################
# Translations                           #
##########################################

new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
  "EN", "item_name", "Item name",
  "FR", "item_name", "Nom de l'item",
  "EN", "datetime", "Datetime",
  "FR", "datetime", "Date & heure",
  "EN", "datetime_start", "Start",
  "FR", "datetime_start", "Début",
  "EN", "datetime_stop", "End",
  "FR", "datetime_stop", "Fin",
  "EN", "value", "Value",
  "FR", "value", "Valeur",
  "EN", "value_num", "Numeric value",
  "FR", "value_num", "Valeur numérique",
  "EN", "unit", "Unit",
  "FR", "unit", "Unité",
  "EN", "route", "Route",
  "FR", "route", "Voie",
  "EN", "continuous", "Continuous",
  "FR", "continuous", "Adm. continue",
  "EN", "amount", "Amount",
  "FR", "amount", "Quantité",
  "EN", "amount_unit", "Unit",
  "FR", "amount_unit", "Unité",
  "EN", "rate", "Rate",
  "FR", "rate", "Débit",
  "EN", "rate_unit", "Rate unit",
  "FR", "rate_unit", "Unité",
  "EN", "concentration", "Concentration",
  "FR", "concentration", "Unité",
  "EN", "concentration_unit", "Unit",
  "FR", "concentration_unit", "Unité",
  "EN", "no_data", "No data available",
  "FR", "no_data", "Pas de données disponibles"
)

##########################################
# Reload datatable                       #
##########################################

observeEvent(input$data_choice_%group_id%_%study_id%, r$reload_%group_id%_%study_id% <- Sys.time())

observeEvent(r$data_patient, r$reload_%group_id%_%study_id% <- Sys.time())
observeEvent(r$data_stay, r$reload_%group_id%_%study_id% <- Sys.time())

observeEvent(r$reload_%group_id%_%study_id%, {
  
  tryCatch({
    
    req(input$data_choice_%group_id%_%study_id%)
    req(!is.na(r$chosen_patient))
    
    ##########################################
    # Data processing                        #
    ##########################################
    
    data_temp_labs_vitals <- tibble::tibble()
    data_temp_orders <- tibble::tibble()
    
    if (input$data_choice_%group_id%_%study_id% == "all_stays"){
      
      if (nrow(r$data_patient$labs_vitals) > 0){
        
        data_temp_labs_vitals <-
          r$data_patient$labs_vitals %>%
          dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id")) %>%
          dplyr::mutate_at("datetime_start", as.character) %>%
          dplyr::mutate(unit = dplyr::case_when((thesaurus_item_unit != "" & !is.na(thesaurus_item_unit)) ~ thesaurus_item_unit, TRUE ~ unit)) %>%
          dplyr::select(display_name, value, value_num, unit, datetime_start)
      }
      
      if (nrow(r$data_patient$orders) > 0){
        
        data_temp_orders <-
          r$data_patient$orders %>%
          dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id")) %>%
          dplyr::mutate_at(c("datetime_start", "datetime_stop"), as.character) %>%
          dplyr::select(display_name, datetime_start, datetime_stop, route, continuous, amount, amount_unit, rate, rate_unit, concentration, concentration_unit)
      }
    } 
    
    if (input$data_choice_%group_id%_%study_id% == "current_stay"){
      
      if (nrow(r$data_stay$labs_vitals) > 0){
        
        data_temp_labs_vitals <- 
          r$data_stay$labs_vitals %>%
          dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id")) %>%
          dplyr::mutate_at("datetime_start", as.character) %>%
          dplyr::mutate(unit = dplyr::case_when((thesaurus_item_unit != "" & !is.na(thesaurus_item_unit)) ~ thesaurus_item_unit, TRUE ~ unit)) %>%
          dplyr::select(display_name, value, value_num, unit, datetime_start)
      }
      
      if (nrow(r$data_stay$orders) > 0){
        
        data_temp_orders <-
          r$data_stay$orders %>%
          dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id")) %>%
          dplyr::mutate_at(c("datetime_start", "datetime_stop"), as.character) %>%
          dplyr::select(display_name, datetime_start, datetime_stop, route, continuous, amount, amount_unit, rate, rate_unit, concentration, concentration_unit)
      }
    }
    
    # Unit col
    if (nrow(data_temp_labs_vitals) > 0) data_temp_labs_vitals <- data_temp_labs_vitals %>% dplyr::mutate(unit = ifelse(unit == "NULL", "", unit), value = ifelse(value == "NULL", "", value))
    if (nrow(data_temp_orders) > 0) data_temp_orders <- data_temp_orders %>% dplyr::mutate(route = ifelse(route == "NULL", "", route), amount_unit = ifelse(amount_unit == "NULL", "", amount_unit),
      rate_unit = ifelse(rate_unit == "NULL", "", rate_unit), concentration_unit = ifelse(concentration_unit == "NULL", "", concentration_unit))
    
    if (nrow(data_temp_labs_vitals) == 0 & nrow(data_temp_orders) == 0){
      output$message_bar_1_%group_id%_%study_id% <- renderUI(div(br(), shiny.fluent::MessageBar(translate(language, "no_data", new_words), messageBarType = 0), style = "margin-top:10px;"))
      shinyjs::show("message_bar_1_%group_id%_%study_id%")
      sapply(c("datatable_labs_vitals_%group_id%_%study_id%", "datatable_orders_%group_id%_%study_id%"), shinyjs::hide)
    }
    if (nrow(data_temp_labs_vitals) > 0) shinyjs::show("datatable_labs_vitals_%group_id%_%study_id%") else shinyjs::hide("datatable_labs_vitals_%group_id%_%study_id%")
    if (nrow(data_temp_orders) > 0) shinyjs::show("datatable_orders_%group_id%_%study_id%") else shinyjs::hide("datatable_orders_%group_id%_%study_id%")
    if (nrow(data_temp_labs_vitals) > 0 | nrow(data_temp_orders) > 0){
      output$message_bar_1_%group_id%_%study_id% <- renderUI("")
      shinyjs::hide("message_bar_1_%group_id%_%study_id%")
    } 
    
    ##########################################
    # Render datatable                       #
    ##########################################
    
    col_names_labs_vitals <- c(translate(language, "item_name", new_words), translate(language, "value", new_words), translate(language, "value_num", new_words), 
      translate(language, "unit", new_words), translate(language, "datetime", new_words))
    
    sortable_cols_labs_vitals <- c("display_name", "datetime_start", "value", "value_num")
    centered_cols_labs_vitals <- c("datetime_start", "unit")
    column_widths_labs_vitals = c("datetime_start" = "180px", "value" = "150px", "value_num" = "150px", "unit" = "100px")
    searchable_cols_labs_vitals <- c("display_name", "datetime_start", "value_num", "value")
    factorize_cols_labs_vitals <- c("display_name")
    
    render_datatable(
      data = data_temp_labs_vitals,
      output = output,
      r = r,
      ns = ns,
      language = language,
      output_name = "datatable_labs_vitals_%group_id%_%study_id%",
      col_names = col_names_labs_vitals,
      page_length = 10,
      sortable_cols = sortable_cols_labs_vitals,
      centered_cols = centered_cols_labs_vitals,
      column_widths = column_widths_labs_vitals,
      filter = TRUE,
      searchable_cols = searchable_cols_labs_vitals,
      factorize_cols = factorize_cols_labs_vitals
    )
    
    col_names_orders <- c(translate(language, "item_name", new_words), translate(language, "datetime_start", new_words), translate(language, "datetime_stop", new_words),
      translate(language, "route", new_words), translate(language, "continuous", new_words), translate(language, "amount", new_words), translate(language, "amount_unit", new_words),
      translate(language, "rate", new_words), translate(language, "rate_unit", new_words), translate(language, "concentration", new_words), translate(language, "concentration_unit", new_words))
    
    sortable_cols_orders <- c("display_name", "datetime_start", "datetime_stop", "route", "continuous", "amount", "amount_unit", "rate", "rate_unit", "concentration", "concentration_unit")
    centered_cols_orders <- c("datetime_start", "datetime_stop", "amount_unit", "rate_unit", "concentration_unit", "continuous")
    column_widths_orders = c("display_name" = "200px", "datetime_start" = "180px", "datetime_stop" = "180px")
    searchable_cols_orders <- c("display_name", "datetime_start", "datetime_stop", "rate", "rate_unit", "amount", "amount_unit", "concentration", "concentration_unit")
    factorize_cols_orders <- c("display_name")
    
    render_datatable(
      data = data_temp_orders,
      output = output,
      r = r,
      ns = ns,
      language = language,
      output_name = "datatable_orders_%group_id%_%study_id%",
      col_names = col_names_orders,
      page_length = 10,
      sortable_cols = sortable_cols_orders,
      centered_cols = centered_cols_orders,
      column_widths = column_widths_orders,
      filter = TRUE,
      searchable_cols = searchable_cols_orders,
      factorize_cols = factorize_cols_orders
    )
    
  },
    error = function(e) "")
  #error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
  #    error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language),
  #warning = function(w) if (nchar(w[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
  #    error_name = "%group_id% - run server code", category = "Warning", error_report = toString(w), language = language))
})