##########################################
# Translations                           #
##########################################

new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
  "EN", "dygraphs_required", "Library dygraphs required to display the graph, please install it with install.packages('dygraphs')",
  "FR", "dygraphs_required", "Le package dygraphs est nécessaire pour afficher le graphique, merci de l'installer avec install.packages('dygraphs')",
  "EN", "xts_required", "Library xts required to display the graph, please install it with install.packages('xts')",
  "FR", "xts_required", "Le package xts est nécessaire pour afficher le graphique, merci de l'installer avec install.packages('xts')",
  "EN", "admission", "Admission",
  "FR", "admission", "Admission",
  "EN", "discharge", "Discharge",
  "FR", "discharge", "Discharge",
  "EN", "no_data", "No data available",
  "FR", "no_data", "Pas de données disponibles",
  "EN", "orders_data_not_suitable", "This plugin is not suitable for orders data",
  "FR", "orders_data_not_suitable", "Ce plugin n'est pas adapté pour des données de type prescription / administration"
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
# Reload dygraph                         #
##########################################

observeEvent(input$data_choice_%group_id%_%study_id%, r$reload_%group_id%_%study_id% <- Sys.time())
observeEvent(input$show_stays_%group_id%_%study_id%, r$reload_%group_id%_%study_id% <- Sys.time())
observeEvent(input$draw_points_%group_id%_%study_id%, r$reload_%group_id%_%study_id% <- Sys.time())

observeEvent(r$data_patient, r$reload_%group_id%_%study_id% <- Sys.time())
observeEvent(r$data_stay, r$reload_%group_id%_%study_id% <- Sys.time())

# Synchronize timeline
observeEvent(input$sync_timeline_%group_id%_%study_id%, r$sync_timeline_%module_id%_%study_id% <- input$sync_timeline_%group_id%_%study_id%)

temp_timeline_%module_id%_%study_id% <- function(x){ list(start = input$dygraph_%group_id%_%study_id%_date_window[[1]], stop = input$dygraph_%group_id%_%study_id%_date_window[[2]])}
r$timeline_%module_id%_%study_id% <- temp_timeline_%module_id%_%study_id% %>% debounce(500) %>% throttle(100)

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
          dplyr::mutate(unit = dplyr::case_when((thesaurus_item_unit != "" & !is.na(thesaurus_item_unit)) ~ thesaurus_item_unit, TRUE ~ unit)) %>%
          dplyr::select(-thesaurus_item_unit)
      }
      
      if (nrow(r$data_patient$orders) > 0){
        
        data_temp_orders <-
          r$data_patient$orders %>%
          dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id"))
      }
    } 
    
    if (input$data_choice_%group_id%_%study_id% == "current_stay"){
      
      if (nrow(r$data_stay$labs_vitals) > 0){
        
        data_temp_labs_vitals <- 
          r$data_stay$labs_vitals %>%
          dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id")) %>%
          dplyr::mutate(unit = dplyr::case_when((thesaurus_item_unit != "" & !is.na(thesaurus_item_unit)) ~ thesaurus_item_unit, TRUE ~ unit)) %>%
          dplyr::select(-thesaurus_item_unit)
      }
      
      if (nrow(r$data_stay$orders) > 0){
        
        data_temp_orders <-
          r$data_stay$orders %>%
          dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id"))
      }
    }
    
    # If there's no data to show
    if (nrow(data_temp_labs_vitals) == 0){
      output$message_bar_3_%group_id%_%study_id% <- renderUI(div(shiny.fluent::MessageBar(translate(language, "no_data", new_words), messageBarType = 0), style = "margin-top:10px;"))
      shinyjs::hide("div_dygraph_%group_id%_%study_id%")
    }
    
    # If there are orders data, notify that this plugin is not suitable for these data
    if (nrow(data_temp_orders) > 0) output$message_bar_4_%group_id%_%study_id% <- renderUI(
      div(shiny.fluent::MessageBar(translate(language, "orders_data_not_suitable", new_words), messageBarType = 0), style = "margin-top:10px;"))
    
    # Unit col
    if (nrow(data_temp_labs_vitals) > 0) data_temp_labs_vitals <- data_temp_labs_vitals %>% dplyr::mutate(unit = ifelse(unit == "NULL", "", unit))
    
    # Remove duplicates if exist
    # Group by display_name, so if we have multiple identical items from distinct thesaurus, only one legend will appear for both
    
    if (nrow(data_temp_labs_vitals) > 0){
      
      output$message_bar_3_%group_id%_%study_id% <- renderUI("")
      
      if (requireNamespace("dygraphs", quietly = TRUE) & requireNamespace("xts", quietly = TRUE)){
        
        data_temp_items <-
          data_temp_labs_vitals %>%
          dplyr::group_by(item_id, display_name, colour, unit) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup()
        
        data_temp_labs_vitals <- 
          data_temp_labs_vitals %>%
          dplyr::group_by(datetime_start, display_name, value_num) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>%
          dplyr::select(datetime_start, display_name, value_num) %>%
          dplyr::arrange(datetime_start)
        
        data_temp_labs_vitals <- data_temp_labs_vitals %>% tidyr::pivot_wider(names_from = display_name, values_from = value_num)
        
        data_temp_labs_vitals <- xts::xts(x = data_temp_labs_vitals %>% dplyr::select(-datetime_start), order.by = data_temp_labs_vitals %>% dplyr::pull(datetime_start))
        
        if (input$data_choice_%group_id%_%study_id% == "all_stays"){
          datetime_start <- min(r$data_patient$stays$admission_datetime)
          datetime_stop <- max(r$data_patient$stays$discharge_datetime)
        }
        
        if (length(r$chosen_stay) > 0){ # For the tests in plugins creation zone
          if (input$data_choice_%group_id%_%study_id% == "current_stay" & !is.na(r$chosen_stay)){
            datetime_start <- min(r$data_patient$stays %>% dplyr::filter(stay_id == r$chosen_stay) %>% dplyr::pull(admission_datetime))
            datetime_stop <- min(r$data_patient$stays %>% dplyr::filter(stay_id == r$chosen_stay) %>% dplyr::pull(discharge_datetime))
          }
        }
        
        # Create the dygraph
        data_temp_labs_vitals <-
          dygraphs::dygraph(data_temp_labs_vitals) %>%
          dygraphs::dyAxis("y", valueRange = c(0, max(data_temp_labs_vitals, na.rm = T) + max(data_temp_labs_vitals, na.rm = T) / 4)) %>%
          dygraphs::dyAxis("x", drawGrid = FALSE) %>%
          dygraphs::dyRangeSelector(dateWindow = c(datetime_start, datetime_stop)) %>%
          dygraphs::dyRoller(rollPeriod = 1) %>%
          # Use data timezone, don't convert to UTC
          dygraphs::dyOptions(rightGap = 60, drawPoints = input$draw_points_%group_id%_%study_id%, pointSize = 2, useDataTimezone = TRUE) %>%
          dygraphs::dyLegend(show = "always", hideOnMouseOut = TRUE, labelsSeparateLines = T) %>%
          dygraphs::dyHighlight(highlightCircleSize = 5)
        
        # Show stays with vertical lines ?
        if (input$show_stays_%group_id%_%study_id%){
          
          data_temp_labs_vitals <- data_temp_labs_vitals %>%
            dygraphs::dyEvent(r$data_patient$stays %>% dplyr::pull(admission_datetime),# color = "#5AAE61",
              label = paste0(r$data_patient$stays %>% dplyr::pull(unit_name), " - ", translate(language, "admission", new_words)), labelLoc = "bottom") %>%
            dygraphs::dyEvent(r$data_patient$stays %>% dplyr::pull(discharge_datetime),# color = "#5AAE61",
              label = paste0(r$data_patient$stays %>% dplyr::pull(unit_name), " - ", translate(language, "discharge", new_words)), labelLoc = "bottom")
        } 
        
        sapply(1:nrow(data_temp_items), function(i){
          row <- data_temp_items[i, ]
          data_temp_labs_vitals <<- 
            data_temp_labs_vitals %>%
            dygraphs::dySeries(
              name = row$display_name, 
              label = paste0(row$display_name, " (", row$unit, ")"),
              color = row$colour
            )
        })
        
        output$dygraph_%group_id%_%study_id% <- dygraphs::renderDygraph(data_temp_labs_vitals)
      }
      
      if (!requireNamespace("dygraphs", quietly = TRUE)) output$message_bar_1_%group_id%_%study_id% <- renderUI(
        div(shiny.fluent::MessageBar(translate(language, "dygraphs_required", new_words), messageBarType = 3), style = "margin-top:10px;"))
      
      if (!requireNamespace("xts", quietly = TRUE)) output$message_bar_2_%group_id%_%study_id% <- renderUI(
        div(shiny.fluent::MessageBar(translate(language, "xts_required", new_words), messageBarType = 3), style = "margin-top:10px;"))
      
      shinyjs::show("div_dygraph_%group_id%_%study_id%")    
    }
    
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})