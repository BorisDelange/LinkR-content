##########################################
# Translations                           #
##########################################

new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
  "EN", "count", "Count",
  "FR", "count", "Occurences",
  "EN", "value", "Value",
  "FR", "value", "Valeur"
)

##########################################
# Show / hide divs                       #
##########################################

divs_%group_id%_%study_id% <- c("div_choose_feature_%group_id%_%study_id%", "div_manage_features_%group_id%_%study_id%")

observeEvent(input$div_%group_id%_%study_id%, {
  
  sapply(divs_%group_id%_%study_id% %>% setdiff(., input$div_%group_id%_%study_id%), shinyjs::hide)
  shinyjs::show(input$div_%group_id%_%study_id%)
})

##########################################
# Display visualization                  #
##########################################

observeEvent(input$display_%group_id%_%study_id%, {
  
  tryCatch({
    
    if (length(r$data_subset$labs_vitals) == 0) show_message_bar(output, 1, translate(language, "select_a_subset", r$words), "severeWarning", language)
    else if (is.na(r$data_subset$labs_vitals)) show_message_bar(output, 1, translate(language, "select_a_subset", r$words), "severeWarning", language)
    
    req(length(r$data_subset$labs_vitals) > 0, !is.na(r$data_subset$labs_vitals))
    
    # Filter data
    
    sql <- glue::glue_sql("SELECT * FROM thesaurus_items WHERE id = {input$feature_%group_id%_%study_id%$key}", .con = r$db)
    item <- DBI::dbGetQuery(r$db, sql)
    
    data_viz <- r$data_subset$labs_vitals %>% dplyr::filter(item_id == item$item_id)
    
    output$figure_%group_id%_%study_id% <- plotly::renderPlotly({
      
      p <-
        data_viz %>%
        ggplot2::ggplot(ggplot2::aes(x = value_num)) +
        ggplot2::geom_histogram(color = "#5DADE2", fill = "#AED6F1") +
        ggplot2::labs(title = isolate(input$feature_%group_id%_%study_id%$text),
          x = paste0(translate(language, "value", new_words), " (", item$unit, ")"), y = translate(language, "count", new_words)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      
      plotly::ggplotly(p)
    })
    
    # Render summary
    
    captured_output <- capture.output(summary(data_viz$value_num))
    
    options('cli.num_colors' = 1)
    output$result_%group_id%_%study_id% <- renderText(paste(captured_output, collapse = "\n"))
    options('cli.num_colors' = NULL)
    
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Manage features                        #
##########################################

##########################################
# Update combobox with selected features #
##########################################

observeEvent(input$thesaurus_%group_id%_%study_id%, r$agg_reload_dropdown_%group_id%_%study_id% <- as.character(Sys.time()))

observeEvent(r$agg_reload_dropdown_%group_id%_%study_id% , {
  
  tryCatch({
    
    if (grepl("datatable", r$agg_reload_dropdown_%group_id%_%study_id%)) thesaurus <- input$thesaurus_datatable_%group_id%_%study_id%
        else thesaurus <- input$thesaurus_%group_id%_%study_id%
            
            sql <- glue::glue_sql("SELECT value_num FROM modules_elements_options WHERE deleted IS FALSE AND
                group_id = %group_id% AND study_id = %study_id% AND link_id = {thesaurus} AND category = 'aggregated'
                AND name = 'dataviz_selected_features'", .con = r$db)
            features <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
            
            sql <- glue::glue_sql("SELECT * FROM thesaurus_items WHERE thesaurus_id = {thesaurus}
                AND id IN ({features*}) AND deleted IS FALSE ORDER BY id", .con = r$db)
            items <- DBI::dbGetQuery(r$db, sql) %>%
              dplyr::mutate(display_name = dplyr::case_when((is.na(display_name) | display_name == "") ~ name, TRUE ~ display_name))
            
            options <- convert_tibble_to_list(items %>% dplyr::arrange(display_name), key_col = "id", text_col = "display_name", words = r$words)
            shiny.fluent::updateComboBox.shinyInput(session, "feature_%group_id%_%study_id%", options = options, value = NULL)
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Render datatable with thesaurus items  #
##########################################

observeEvent(input$thesaurus_datatable_%group_id%_%study_id%, {
  
  tryCatch({
    
    ##########################################
    # Update dropdown with selected features #
    ##########################################
    
    sql <- glue::glue_sql("SELECT value_num FROM modules_elements_options WHERE deleted IS FALSE AND
                    group_id = %group_id% AND study_id = %study_id% AND link_id = {input$thesaurus_datatable_%group_id%_%study_id%} AND category = 'aggregated'
                    AND name = 'dataviz_selected_features'", .con = r$db)
    features <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
    
    sql <- glue::glue_sql("SELECT * FROM thesaurus_items WHERE thesaurus_id = {input$thesaurus_datatable_%group_id%_%study_id%}
                    AND id IN ({features*}) AND deleted IS FALSE ORDER BY id", .con = r$db)
    items <- DBI::dbGetQuery(r$db, sql) %>%
      dplyr::mutate(display_name = dplyr::case_when((is.na(display_name) | display_name == "") ~ name, TRUE ~ display_name))
    
    options <- convert_tibble_to_list(items %>% dplyr::arrange(display_name), key_col = "id", text_col = "display_name", words = r$words)
    value <- items %>% dplyr::pull(id)
    shiny.fluent::updateDropdown.shinyInput(session, "features_datatable_%group_id%_%study_id%", options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
    
    r$agg_thesaurus_selected_items_%group_id%_%study_id% <- items %>% dplyr::transmute(id, thesaurus_id = input$thesaurus_datatable_%group_id%_%study_id%, thesaurus_item_display_name = display_name)
    
    ##########################################
    # Load datatable                         #
    ##########################################
    
    page_id <- id
    
    # Get thesaurus items with plus and minus buttons
    r$agg_thesaurus_items_%group_id%_%study_id% <- create_datatable_cache(output = output, r = r, language = language, module_id = id, thesaurus_id = input$thesaurus_datatable_%group_id%_%study_id%, 
      category = paste0(id, "_plus_data_explorer"))
    
    # Get datamart id
    sql <- glue::glue_sql("SELECT datamart_id FROM studies WHERE id = %study_id%", .con = r$db)
    datamart <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(datamart_id)
    
    # Get count patients rows col from cache
    count_patients_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = input$thesaurus_datatable_%group_id%_%study_id%, datamart_id = datamart, category = "count_patients_rows")
    
    # Get count items rows col from cache
    count_items_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = input$thesaurus_datatable_%group_id%_%study_id%, datamart_id = datamart, category = "count_items_rows")
    
    # Merge tibbles
    r$agg_thesaurus_items_%group_id%_%study_id% <- r$agg_thesaurus_items_%group_id%_%study_id% %>%
      dplyr::left_join(count_items_rows, by = "item_id") %>%
      dplyr::left_join(count_patients_rows, by = "item_id") %>%
      dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
      dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action") %>%
      dplyr::mutate(display_name = ifelse((display_name != "" & !is.na(display_name)), display_name, name))
    
    # Select only used items
    r$agg_thesaurus_items_%group_id%_%study_id% <- r$agg_thesaurus_items_%group_id%_%study_id% %>% dplyr::filter(count_items_rows > 0)
    
    # Replace module name in cache with our page id
    r$agg_thesaurus_items_%group_id%_%study_id% <- r$agg_thesaurus_items_%group_id%_%study_id% %>%
      dplyr::mutate_at("action", stringr::str_replace_all, "settings_modules_patient_lvl_modules_elements_creation", page_id)
    
    # Cols settings
    searchable_cols <- c("item_id", "name", "display_name", "category", "unit")
    factorize_cols <- c("category", "unit")
    column_widths <- c("id" = "80px", "action" = "80px", "display_name" = "300px", "unit" = "100px")
    
    sortable_cols <- c("id", "item_id", "name", "display_name", "category", "count_patients_rows", "count_items_rows")
    centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
    col_names <- c(translate(language, "id", r$words), translate(language, "thesaurus", r$words), translate(language, "item", r$words), translate(language, "name", r$words), 
      translate(language, "display_name", r$words), translate(language, "category", r$words), translate(language, "unit", r$words),
      translate(language, "datetime", r$words), translate(language, "deleted", r$words),
      translate(language, "num_patients", r$words), translate(language, "num_rows", r$words), translate(language, "action", r$words))
    
    hidden_cols <- c("id", "name", "thesaurus_id", "item_id", "datetime", "deleted", "modified")
    
    # Render datatable
    render_datatable(output = output, r = r, ns = ns, language = language, data = r$agg_thesaurus_items_%group_id%_%study_id%,
      output_name = "datatable_%group_id%_%study_id%", col_names =  col_names,
      sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
      searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
  
})

##########################################
# Add or remove items                    #
##########################################

# When add button is clicked

observeEvent(input$data_explorer_item_selected, {
  
  tryCatch({
    
    req(length(input$thesaurus_datatable_%group_id%_%study_id%) > 0)
    
    # Initiate r variable if doesn't exist
    if (length(r$agg_thesaurus_selected_items_%group_id%_%study_id%) == 0){
      r$agg_thesaurus_selected_items_%group_id%_%study_id% <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_item_display_name = character()) 
    }
    
    # Get ID of chosen thesaurus item
    link_id <- as.integer(substr(input$data_explorer_item_selected, nchar("select_") + 1, nchar(input$data_explorer_item_selected)))
    
    thesaurus <- input$thesaurus_datatable_%group_id%_%study_id%
      
      # If this thesaurus item is not already chosen, add it to the "thesaurus selected items" dropdown
      
      value <- integer(1)
    if (nrow(r$agg_thesaurus_selected_items_%group_id%_%study_id%) > 0) value <- r$agg_thesaurus_selected_items_%group_id%_%study_id% %>% 
      dplyr::filter(thesaurus_id == thesaurus) %>% dplyr::pull(id)
    
    if (link_id %not_in% value){
      
      # Get thesaurus name
      thesaurus_name <- r$thesaurus %>% dplyr::filter(id == thesaurus) %>% dplyr::pull(name)
      
      # Get item informations from datatable / 
      
      item <- r$agg_thesaurus_items_%group_id%_%study_id% %>% dplyr::filter(id == link_id)
      
      display_name <- ifelse((item$display_name == "" | is.na(item$display_name)), item$name, item$display_name)
      
      # Add item to selected items
      r$agg_thesaurus_selected_items_%group_id%_%study_id% <- 
        tibble::tribble(~id, ~thesaurus_id, ~thesaurus_item_display_name,
          as.integer(link_id), as.integer(thesaurus), as.character(display_name)) %>%
        dplyr::bind_rows(r$agg_thesaurus_selected_items_%group_id%_%study_id%)
      
      # Update dropdown of selected items
      options <- convert_tibble_to_list(r$agg_thesaurus_selected_items_%group_id%_%study_id% %>% dplyr::arrange(thesaurus_item_display_name), key_col = "id", text_col = "thesaurus_item_display_name", words = r$words)
      value <- r$agg_thesaurus_selected_items_%group_id%_%study_id% %>% dplyr::pull(id)
      shiny.fluent::updateDropdown.shinyInput(session, "features_datatable_%group_id%_%study_id%",
        options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
    }
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

# When dropdown is modified
observeEvent(input$features_datatable_%group_id%_%study_id%, {
  
  r$agg_thesaurus_selected_items_%group_id%_%study_id% <- r$agg_thesaurus_selected_items_%group_id%_%study_id% %>%
    dplyr::filter(id %in% input$features_datatable_%group_id%_%study_id%)
  
  options <- convert_tibble_to_list(r$agg_thesaurus_selected_items_%group_id%_%study_id% %>% dplyr::arrange(thesaurus_item_display_name), key_col = "id", text_col = "thesaurus_item_display_name", words = r$words)
  value <- r$agg_thesaurus_selected_items_%group_id%_%study_id% %>% dplyr::pull(id)
  
  shiny.fluent::updateDropdown.shinyInput(session, "features_datatable_%group_id%_%study_id%",
    options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
})

##########################################
# Save selected items                    #
##########################################

observeEvent(input$save_%group_id%_%study_id%, {
  
  tryCatch({
    
    # Delete old selected items
    
    sql <- glue::glue_sql("DELETE FROM modules_elements_options WHERE group_id = %group_id% AND study_id = %study_id% AND category = 'aggregated'
                AND name = 'dataviz_selected_features' AND link_id = {input$thesaurus_datatable_%group_id%_%study_id%}", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    # Add selected items
    
    selected_items <- r$agg_thesaurus_selected_items_%group_id%_%study_id% %>% dplyr::filter(id %in% input$features_datatable_%group_id%_%study_id%)
    
    sapply(1:nrow(selected_items), function(i){
      
      row <- selected_items[i, ]
      
      sql <- glue::glue_sql("INSERT INTO modules_elements_options(id, group_id, study_id, link_id, category, name, value_num, creator_id, datetime, deleted)
                    SELECT {get_last_row(r$db, 'modules_elements_options') + 1}, %group_id%, %study_id%, {input$thesaurus_datatable_%group_id%_%study_id%}, 'aggregated', 'dataviz_selected_features',
                    {row$id}, {r$user_id}, {as.character(Sys.time())}, FALSE", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
    })
    
    # Notify user
    show_message_bar(output, 1, translate(language, "modif_saved", r$words), "success", language)
    
    # Update dropdown & combobox
    
    shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_%group_id%_%study_id%", value = input$thesaurus_datatable_%group_id%_%study_id%)
    r$agg_reload_dropdown_%group_id%_%study_id% <- paste0("datatable_", Sys.time())
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})