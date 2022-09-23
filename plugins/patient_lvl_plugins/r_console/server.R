##########################################
# Translations                           #
##########################################

new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
  "EN", "new_script_added", "New script added",
  "FR", "new_script_added", "Nouveau script ajouté",
  "EN", "delete_script", "Delete a script",
  "FR", "delete_script", "Supprimer un script",
  "EN", "delete_script_subtext", "Are you sure you want to delete this script ?",
  "FR", "delete_script_subtext", "Etes-vous sûr de vouloir supprimer ce script ?",
  "EN", "script_deleted", "Script deleted",
  "FR", "script_deleted", "Script supprimé"
)

##########################################
# Show / hide divs                       #
##########################################

divs_%group_id%_%study_id% <- c("div_my_scripts_%group_id%_%study_id%", "div_new_script_%group_id%_%study_id%", "div_delete_script_%group_id%_%study_id%")

observeEvent(input$div_%group_id%_%study_id%, {
  
  sapply(divs_%group_id%_%study_id% %>% setdiff(., input$div_%group_id%_%study_id%), shinyjs::hide)
  shinyjs::show(input$div_%group_id%_%study_id%)
})

##########################################
# Add a new script                       #
##########################################

observeEvent(input$new_script_add_%group_id%_%study_id%, {
  
  tryCatch({
    
    new_name <- input$new_script_name_%group_id%_%study_id%
      
      if (length(new_name) == 0) shiny.fluent::updateTextField.shinyInput(session, "new_script_name_%group_id%_%study_id%", errorMessage = translate(language, paste0("provide_valid_name"), r$words))
    else shiny.fluent::updateTextField.shinyInput(session, "new_script_name_%group_id%_%study_id%", errorMessage = NULL)
    
    req(length(new_name) > 0)
    
    sql <- "SELECT DISTINCT(value) FROM modules_elements_options WHERE deleted IS FALSE AND
            category = 'patient_lvl' AND group_id = %group_id% AND study_id = %study_id% AND name = 'r_script_name'" 
    distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() %>% tolower()
    
    if (tolower(new_name) %in% distinct_values) show_message_bar(output, 2, "name_already_used", "severeWarning", language)
    req(tolower(new_name) %not_in% distinct_values)
    
    last_row <- get_last_row(r$db, "modules_elements_options")
    
    # Insert new script in database
    # One row for name, one row for code
    
    sql <- glue::glue_sql("INSERT INTO modules_elements_options(id, group_id, study_id, link_id, category, name, value, creator_id, datetime, deleted)
            SELECT {last_row + 1}, %group_id%, %study_id%, {NA_integer_}, 'patient_lvl', 'r_script_name', {new_name}, {r$user_id}, {as.character(Sys.time())}, FALSE
      UNION SELECT {last_row + 2}, %group_id%, %study_id%, {last_row + 1}, 'patient_lvl', 'r_script_code', ', {r$user_id}, { as.character(Sys.time())}, FALSE", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    # Reset textfield
    shiny.fluent::updateTextField.shinyInput(session, "new_script_name_%group_id%_%study_id%", value = NULL)
    
    # Update dropdowns
    r$pl_reload_%group_id%_%study_id% <- Sys.time()
    
    show_message_bar(output, 1, translate(language, "new_script_added", new_words), "success", language)
    
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Edit a script                          #
##########################################


##########################################
# Load script code                       #
##########################################

observeEvent(input$Rcode_select_%group_id%_%study_id%, {
  
  tryCatch({
    sql <- glue::glue_sql("SELECT value FROM modules_elements_options WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id% AND
                category = 'patient_lvl' AND name = 'r_script_code' AND link_id = {as.integer(input$Rcode_select_%group_id%_%study_id%)}", .con = r$db)
    code <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(value)
    
    shinyAce::updateAceEditor(session, "Rcode_%group_id%_%study_id%", value = code)
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Save updates                           #
##########################################

observeEvent(input$Rcode_save_%group_id%_%study_id%, {
  
  tryCatch({
    req(!is.na(input$Rcode_select_%group_id%_%study_id%))
    
    code <- input$Rcode_%group_id%_%study_id% %>% stringr::str_replace_all("\r", "\n")
    
    sql <- glue::glue_sql("UPDATE modules_elements_options SET value = {code} WHERE name = 'r_script_code' 
            AND category = 'patient_lvl' AND link_id = {as.integer(input$Rcode_select_%group_id%_%study_id%)}", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    show_message_bar(output = output, id = 3, message = "modif_saved", type = "success", language = language)
    
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Execute script code                    #
##########################################

observeEvent(input$Rcode_execute_%group_id%_%study_id%, {
  
  # If this is a console output
  
  if (input$Rcode_output_type_%group_id%_%study_id% == "console"){
    
    # Show & hide divs
    shinyjs::show("Rcode_console_output_%group_id%_%study_id%")
    sapply(c("Rcode_plot_output_%group_id%_%study_id%", "Rcode_table_output_%group_id%_%study_id%", "Rcode_markdown_output_%group_id%_%study_id%"), shinyjs::hide)
    
    tryCatch({
      
      # Change this option to display correctly tibble in textbox
      options('cli.num_colors' = 1)
      
      # Capture console output of our code
      captured_output <- capture.output(
        tryCatch(eval(parse(text = as.character(input$Rcode_%group_id%_%study_id%) %>% stringr::str_replace_all("\r", "\n"))), error = function(e) print(e), warning = function(w) print(w)))
      
      # Display result
      output$Rcode_console_result_%group_id%_%study_id% <- renderText(paste(captured_output, collapse = "\n"))
      
      # Restore normal value
      options('cli.num_colors' = NULL)
    },
      error = function(e) "")
  }
  
  # If this is a table output
  
  if (input$Rcode_output_type_%group_id%_%study_id% == "table"){
    
    # Show & hide divs
    shinyjs::show("Rcode_table_output_%group_id%_%study_id%")
    sapply(c("Rcode_console_output_%group_id%_%study_id%", "Rcode_plot_output_%group_id%_%study_id%", "Rcode_markdown_output_%group_id%_%study_id%"), shinyjs::hide)
    
    tryCatch({
      
      data <- eval(parse(text = as.character(isolate(input$Rcode_%group_id%_%study_id%)) %>% stringr::str_replace_all("\r", "\n")))
      
      render_datatable(
        data = data,
        output = output,
        r = r,
        ns = ns,
        language = language,
        output_name = "Rcode_table_result_%group_id%_%study_id%",
        filter = TRUE,
        searchable_cols = names(data),
        sortable_cols = names(data)
      )
    },
      error = function(e) "")
  }
  
  # If this is a plot output
  
  if (input$Rcode_output_type_%group_id%_%study_id% == "plot"){
    
    # Show & hide divs
    shinyjs::show("Rcode_plot_output_%group_id%_%study_id%")
    sapply(c("Rcode_console_output_%group_id%_%study_id%", "Rcode_table_output_%group_id%_%study_id%", "Rcode_markdown_output_%group_id%_%study_id%"), shinyjs::hide)
    
    tryCatch({
      output$Rcode_plot_ui_result_%group_id%_%study_id% <- renderUI(plotOutput(ns("Rcode_plot_result_%group_id%_%study_id%"), width = paste0(input$Rcode_plot_width_%group_id%_%study_id%, "%")))
      output$Rcode_plot_result_%group_id%_%study_id% <- renderPlot(eval(parse(text = as.character(isolate(input$Rcode_%group_id%_%study_id%)) %>% stringr::str_replace_all("\r", "\n"))))
    },
      error = function(e) "")
  }
  
  # If this is a markdown output
  
  if (input$Rcode_output_type_%group_id%_%study_id% == "markdown"){
    
    # Show & hide divs
    shinyjs::show("Rcode_markdown_output_%group_id%_%study_id%")
    sapply(c("Rcode_console_output_%group_id%_%study_id%", "Rcode_table_output_%group_id%_%study_id%", "Rcode_plot_output_%group_id%_%study_id%"), shinyjs::hide)
    
    tryCatch({
      
      # Clear temp dir
      unlink(paste0(path.expand("~"), "/cdwtools_temp_files"), recursive = TRUE, force = TRUE)
      
      markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", 
        path.expand("~"), "/cdwtools_temp_files')\n",
        "knitr::opts_chunk$set(root.dir = '", path.expand("~"), "/cdwtools_temp_files', fig.path = '", path.expand("~"), "/cdwtools_temp_files')\n```\n")
      
      markdown_file <- paste0(markdown_settings,
        isolate(input$Rcode_%group_id%_%study_id%) %>% stringr::str_replace_all("\r", "\n"))
      
      print(markdown_file)
      
      # Create temp dir
      dir <- paste0(path.expand("~"), "/cdwtools_temp_files")
      file <- paste0(dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
      if (!dir.exists(dir)) dir.create(dir)
      
      # Create the markdown file
      knitr::knit(text = markdown_file, output = file, quiet = TRUE)
      
      output$Rcode_markdown_result_%group_id%_%study_id% <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(file))))
    },
      error = function(e) "")
  }
  
})

##########################################
# Delete a script                        #
##########################################

r$pl_delete_dialog_%group_id%_%study_id% <- FALSE

# Delete button is pressed
observeEvent(input$delete_script_validate_%group_id%_%study_id%, r$pl_delete_dialog_%group_id%_%study_id% <- TRUE)

# Rendering react output
output$delete_confirm_%group_id%_%study_id% <- shiny.fluent::renderReact({
  dialogContentProps <- list(
    type = 0,
    title = translate(language, "delete_script", new_words),
    closeButtonAriaLabel = "Close",
    subText = translate(language, "delete_script_subtext", new_words)
  )
  shiny.fluent::Dialog(
    hidden = !r$pl_delete_dialog_%group_id%_%study_id%,
    onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('hide_dialog_%group_id%_%study_id%', Math.random()); }")),
    dialogContentProps = dialogContentProps,
    modalProps = list(),
    shiny.fluent::DialogFooter(
      shiny.fluent::PrimaryButton.shinyInput(ns("delete_confirmed_%group_id%_%study_id%"), text = translate(language, "delete", r$words)),
      shiny.fluent::DefaultButton.shinyInput(ns("delete_canceled_%group_id%_%study_id%"), text = translate(language, "dont_delete", r$words))
    )
  )
})

# Whether to close or not delete dialog box
observeEvent(input$hide_dialog_%group_id%_%study_id%, r$pl_delete_dialog_%group_id%_%study_id% <- FALSE)
observeEvent(input$delete_canceled_%group_id%_%study_id%, r$pl_delete_dialog_%group_id%_%study_id% <- FALSE)

# When the delete is confirmed
observeEvent(input$delete_confirmed_%group_id%_%study_id%, {
  
  tryCatch({
    
    # Get value of deleted row
    row_deleted <- as.integer(input$delete_script_%group_id%_%study_id%)
    
    # Delete row in database
    sql <- glue::glue_sql("UPDATE modules_elements_options SET deleted = TRUE WHERE id = {row_deleted} OR link_id = {row_deleted}", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    # Close dialog box
    r$pl_delete_dialog_%group_id%_%study_id% <- FALSE
    
    # Notification to user
    show_message_bar(output = output, id = 3, translate(language, "script_deleted", new_words), type = "severeWarning", language = language)
    
    r$pl_reload_%group_id%_%study_id% <- Sys.time()
    
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Reload data                            #
##########################################

observeEvent(r$pl_reload_%group_id%_%study_id%, {
  
  # Update dropdowns
  
  options <- convert_tibble_to_list(DBI::dbGetQuery(r$db, "SELECT * FROM modules_elements_options WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id% AND name = 'r_script_name'"), key_col = "id", text_col = "value")
  shiny.fluent::updateDropdown.shinyInput(session, "Rcode_select_%group_id%_%study_id%", options = options)
  shiny.fluent::updateDropdown.shinyInput(session, "delete_script_%group_id%_%study_id%", options = options)
  
})