##########################################
# Translations                           #
##########################################

new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
  "EN", "new_bib_added", "New bibliography added",
  "FR", "new_bib_added", "Nouvelle bibliographie ajoutée",
  "EN", "delete_bib", "Delete a bibliography",
  "FR", "delete_bib", "Supprimer une bibliographie",
  "EN", "delete_bib_subtext", "Are you sure you want to delete this bibliography ?",
  "FR", "delete_bib_subtext", "Etes-vous sûr de vouloir supprimer cette bibliographie ?",
  "EN", "bib_deleted", "Bibliography deleted",
  "FR", "bib_deleted", "Bibliographie supprimée",
  "EN", "bib_imported", "Bibliography imported",
  "FR", "bib_imported", "Bibliographie importée",
  "EN", "bib_import_choose_bib", "Choose a bibliography to import BibTeX file",
  "FR", "bib_import_choose_bib", "Choisir une bibliographie pour importer le fichier BibTeX",
  "EN", "title", "Title",
  "FR", "title", "Titre",
  "EN", "authors", "Authors",
  "FR", "authors", "Auteurs",
  "EN", "journal", "Journal",
  "FR", "journal", "Revue",
  "EN", "doi", "DOI",
  "FR", "doi", "DOI"
)

##########################################
# Show / hide divs                       #
##########################################

divs_%group_id%_%study_id% <- c("div_open_bib_%group_id%_%study_id%", "div_add_bib_%group_id%_%study_id%",
  "div_add_bibtex_%group_id%_%study_id%", "div_delete_bib_%group_id%_%study_id%")

observeEvent(input$div_%group_id%_%study_id%, {
  
  sapply(divs_%group_id%_%study_id% %>% setdiff(., input$div_%group_id%_%study_id%), shinyjs::hide)
  shinyjs::show(input$div_%group_id%_%study_id%)
})


##########################################
# Display a BibTeX file with DT          #
##########################################

if (requireNamespace("bib2df", quietly = TRUE) & requireNamespace("clipr", quietly = TRUE)){
  
  observeEvent(input$bib_select_%group_id%_%study_id%, {
    
    tryCatch({
      
      # Get page_id for delete button
      page_id <- id
      
      # Use same dir as datamarts
      
      if (length(r$datamarts_folder) > 0) folder <- paste0(r$datamarts_folder, "/bib/")
      else folder <- paste0(path.expand('~'), "/data/bib/")
      path <- paste0(folder, "bib_%study_id%_", input$bib_select_%group_id%_%study_id%, ".bib")
      
      # If file exists, transform bib file to dataframe & render as a datatable
      
      if (file.exists(path)){
        
        # Select & rename cols
        # Make a copy button in action column
        
        bib <- bib2df::bib2df(path) %>% 
          dplyr::select(title = TITLE, authors = AUTHOR, journal = JOURNAL, doi = DOI, key = BIBTEXKEY) %>%
          dplyr::mutate(id = 1:dplyr::n()) %>%
          dplyr::mutate(authors = sapply(authors, toString)) %>% 
          dplyr::mutate(title = stringr::str_replace_all(title, c("\\{" = "", "\\}" = "")))
        
        r$agg_bib_%group_id%_%study_id% <- bib
        
        bib <- bib %>% dplyr::rowwise() %>%
          dplyr::mutate(key = as.character(
            div(
              shiny::actionButton(ns(paste0("bib_copy_%group_id%_%study_id%_", id)), "", icon = icon("copy"),
                onclick = paste0("Shiny.setInputValue('", page_id, "-bib_copy_%group_id%_%study_id%', ", id, ", {priority: 'event'})"))
            )
          ))
        
        col_names <- c(translate(language, "title", new_words), translate(language, "authors", new_words), translate(language, "journal", new_words), 
          translate(language, "doi", new_words), translate(language, "action", r$words))
        
        sortable_cols <- c("title", "journal")
        centered_cols <- c("doi", "key")
        column_widths = c("key" = "80px")
        searchable_cols <- c("title", "authors", "journal")
        factorize_cols <- c("journal")
        truncated_cols <- c("title", "authors", "journal")
        
        render_datatable(
          data = bib %>% dplyr::select(-id),
          output = output,
          r = r,
          ns = ns,
          language = language,
          output_name = "bib_datatable_%group_id%_%study_id%",
          col_names = col_names,
          page_length = 10,
          sortable_cols = sortable_cols,
          centered_cols = centered_cols,
          column_widths = column_widths,
          filter = TRUE,
          searchable_cols = searchable_cols,
          factorize_cols = factorize_cols,
          truncated_cols = truncated_cols
        )
      }
    },
      error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
  })
  
  observeEvent(input$bib_copy_%group_id%_%study_id%,{
    clipr::write_clip(paste0("[@", r$agg_bib_%group_id%_%study_id% %>% dplyr::filter(id == input$bib_copy_%group_id%_%study_id%) %>% dplyr::pull(key), "]"))
  })
  
  output$test <- renderText(input$bib_copy_%group_id%_%study_id%)
}

if (!requireNamespace("bib2df", quietly = TRUE) | !requireNamespace("clipr", quietly = TRUE)){
  
}

##########################################
# Add a new bibliography                 #
##########################################

observeEvent(input$new_bib_add_%group_id%_%study_id%, {
  
  tryCatch({
    
    new_name <- input$new_bib_name_%group_id%_%study_id%
      
      if (length(new_name) == 0) shiny.fluent::updateTextField.shinyInput(session, "new_bib_name_%group_id%_%study_id%", errorMessage = translate(language, paste0("provide_valid_name"), r$words))
    else if (new_name == "") shiny.fluent::updateTextField.shinyInput(session, "new_bib_name_%group_id%_%study_id%", errorMessage = translate(language, paste0("provide_valid_name"), r$words))
    else shiny.fluent::updateTextField.shinyInput(session, "new_bib_name_%group_id%_%study_id%", errorMessage = NULL)
    
    req(length(new_name) > 0, new_name != "")
    
    sql <- "SELECT DISTINCT(value) FROM modules_elements_options WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id% 
            AND category = 'aggregated' AND name = 'bib_name'" 
    distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() %>% tolower()
    
    if (tolower(new_name) %in% distinct_values) show_message_bar(output, 2, "name_already_used", "severeWarning", language)
    req(tolower(new_name) %not_in% distinct_values)
    
    last_row <- get_last_row(r$db, "modules_elements_options")
    
    # Insert new bib in database
    # One row for name, one row for bib file path
    
    sql <- glue::glue_sql("INSERT INTO modules_elements_options(id, group_id, study_id, link_id, category, name, value, creator_id, datetime, deleted)
            SELECT {last_row + 1}, %group_id%, %study_id%, {NA_integer_}, 'aggregated', 'bib_name', {new_name}, {r$user_id}, {as.character(Sys.time())}, FALSE
      UNION SELECT {last_row + 2}, %group_id%, %study_id%, {last_row + 1}, 'aggregated', 'bib_file_path', '', {r$user_id}, {as.character(Sys.time())}, FALSE", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    # Reset textfield
    shiny.fluent::updateTextField.shinyInput(session, "new_bib_name_%group_id%_%study_id%", value = NULL)
    
    # Update dropdowns
    r$agg_reload_%group_id%_%study_id% <- Sys.time()
    
    show_message_bar(output, 1, translate(language, "new_bib_added", new_words), "success", language)
    
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Import a BibTeX file                   #
##########################################

observeEvent(input$import_bib_browse_%group_id%_%study_id%, shinyjs::click("import_bib_file_%group_id%_%study_id%"))

output$import_bib_status_%group_id%_%study_id% <- renderUI(tagList(div(
  span(translate(language, "loaded_file", r$words), " : ", style = "padding-top:5px;"), 
  span(input$import_bib_file_%group_id%_%study_id%$name, style = "font-weight:bold; color:#0078D4;"), style = "padding-top:10px;")))

observeEvent(input$import_bib_validate_%group_id%_%study_id%, {
  
  tryCatch({
    
    if (length(input$import_bib_name_%group_id%_%study_id%) == 0) show_message_bar(output, 3, "bib_import_choose_bib", "severeWarning", language, words = new_words)
    
    req(input$import_bib_file_%group_id%_%study_id%, input$import_bib_name_%group_id%_%study_id%)
    
    # Use same dir as datamarts
    
    if (length(r$datamarts_folder) > 0) folder <- paste0(r$datamarts_folder, "/bib/")
    else folder <- paste0(path.expand('~'), "/data/bib/")
    dir.create(folder, showWarnings = FALSE)
    path <- paste0(folder, "bib_%study_id%_", input$import_bib_name_%group_id%_%study_id%, ".bib")
    
    file.copy(input$import_bib_file_%group_id%_%study_id%$datapath, path)
    
    show_message_bar(output, 3, "bib_imported", "success", language, words = new_words)
    
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
  
})

##########################################
# Delete a bibliography                  #
##########################################

# Delete button is pressed
observeEvent(input$delete_bib_validate_%group_id%_%study_id%, r$agg_delete_dialog_%group_id%_%study_id% <- TRUE)

# Rendering react output
observeEvent(r$agg_delete_dialog_%group_id%_%study_id% , {
  output$delete_confirm_%group_id%_%study_id% <- shiny.fluent::renderReact({
    dialogContentProps <- list(
      type = 0,
      title = translate(language, "delete_bib", new_words),
      closeButtonAriaLabel = "Close",
      subText = translate(language, "delete_bib_subtext", new_words)
    )
    shiny.fluent::Dialog(
      hidden = !r$agg_delete_dialog_%group_id%_%study_id%,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('hide_dialog_%group_id%_%study_id%', Math.random()); }")),
      dialogContentProps = dialogContentProps,
      modalProps = list(),
      shiny.fluent::DialogFooter(
        shiny.fluent::PrimaryButton.shinyInput(ns("delete_confirmed_%group_id%_%study_id%"), text = translate(language, "delete", r$words)),
        shiny.fluent::DefaultButton.shinyInput(ns("delete_canceled_%group_id%_%study_id%"), text = translate(language, "dont_delete", r$words))
      )
    )
  })
})

# Whether to close or not delete dialog box
observeEvent(input$hide_dialog_%group_id%_%study_id%, r$agg_delete_dialog_%group_id%_%study_id% <- FALSE)
observeEvent(input$delete_canceled_%group_id%_%study_id%, r$agg_delete_dialog_%group_id%_%study_id% <- FALSE)

# When the delete is confirmed
observeEvent(input$delete_confirmed_%group_id%_%study_id%, {
  
  tryCatch({
    
    # Get value of deleted row
    row_deleted <- as.integer(input$delete_bib_%group_id%_%study_id%)
    
    # Delete row in database
    sql <- glue::glue_sql("UPDATE modules_elements_options SET deleted = TRUE WHERE id = {row_deleted} OR link_id = {row_deleted}", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    # Close dialog box
    r$agg_delete_dialog_%group_id%_%study_id% <- FALSE
    
    # Notification to user
    show_message_bar(output = output, id = 3, translate(language, "bib_deleted", new_words), type = "severeWarning", language = language)
    
    r$agg_reload_%group_id%_%study_id% <- Sys.time()
    
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Reload data                            #
##########################################

observeEvent(r$agg_reload_%group_id%_%study_id%, {
  
  # Update dropdowns
  
  options <- convert_tibble_to_list(DBI::dbGetQuery(r$db, "SELECT * FROM modules_elements_options WHERE deleted IS FALSE AND study_id = %study_id% 
        AND category = 'aggregated' AND name = 'bib_name'"), key_col = "id", text_col = "value")
  shiny.fluent::updateDropdown.shinyInput(session, "bib_select_%group_id%_%study_id%", options = options)
  shiny.fluent::updateDropdown.shinyInput(session, "import_bib_name_%group_id%_%study_id%", options = options)
  shiny.fluent::updateDropdown.shinyInput(session, "delete_bib_%group_id%_%study_id%", options = options)
})