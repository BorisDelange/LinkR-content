# Careful : hidden dependency : "webshot" and phantomJS with install.packages("phantomJS")

##########################################
# Translations                           #
##########################################

new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
  "EN", "report", "report",
  "FR", "report", "article",
  "EN", "tinytex_required", "Library tinytex required to create a PDF report, please install it with install.packages('tinytex') & tinytex::install_tinytex()",
  "FR", "tinytex_required", "Le package tinytex est nécessaire pour afficher créer un PDF, merci de l'installer avec install.packages('tinytex') & tinytex::install_tinytex()"
)

##########################################
# Save editors                           #
##########################################

observeEvent(input$report_save_%group_id%_%study_id%, {
  
  pages <- c("abstract", "intro", "methods", "results", "discussion", "conclusion")
  
  tryCatch({
    sapply(pages, function(page){
      
      sql <- glue::glue_sql("UPDATE modules_elements_options SET value = {input[[paste0('ace_editor_', page, '_%group_id%_%study_id%')]]}, creator_id = {r$user_id}, datetime = {as.character(Sys.time())}
                WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id% AND category = 'aggregated' AND name = {paste0('report_', page)}" ,.con = r$db)
      
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
    })
    
    show_message_bar(output, 1, translate(language, "modif_saved", r$words), "success", language)
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language),
    warning = function(w) if (nchar(w[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Warning", error_report = toString(w), language = language))
  
})

##########################################
# Preview                                #
##########################################

observeEvent(input$report_preview_%group_id%_%study_id%, {
  
  output$report_preview_ui_%group_id%_%study_id% <- renderUI({
    
    tryCatch({
      # Clear temp dir
      unlink(paste0(path.expand("~"), "/cdwtools_temp_files"), recursive = TRUE, force = TRUE)
      
      markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", 
        path.expand("~"), "/cdwtools_temp_files')\n",
        "knitr::opts_chunk$set(root.dir = '", path.expand("~"), "/cdwtools_temp_files', fig.path = '", path.expand("~"), "/cdwtools_temp_files')\n```\n")
      
      markdown_file <- paste0(markdown_settings,
        isolate(input[[paste0("ace_editor_", input$report_page_%group_id%_%study_id%, "_%group_id%_%study_id%")]]) %>%
          stringr::str_replace_all("\r", ""))
      
      print(markdown_file)
      
      # Create temp dir
      dir <- paste0(path.expand("~"), "/cdwtools_temp_files")
      file <- paste0(dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
      if (!dir.exists(dir)) dir.create(dir)
      
      # Create the markdown file
      knitr::knit(text = markdown_file, output = file, quiet = TRUE)
      
      div(class = "markdown", withMathJax(includeMarkdown(file)))
    },
      error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
        error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language),
      warning = function(w) if (nchar(w[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
        error_name = "%group_id% - run server code", category = "Warning", error_report = toString(w), language = language))
  })
})

##########################################
# Bibliography                           #
##########################################

observeEvent(r$reload_bibliography, {
  
  bib_options <- convert_tibble_to_list(DBI::dbGetQuery(r$db, "SELECT * FROM modules_elements_options WHERE DELETED IS FALSE AND study_id = %study_id% AND name = 'bib_name'"), 
    key_col = "id", text_col = "value", null_value = TRUE)
  
  shiny.fluent::updateDropdown.shinyInput(session, "choose_bib_%group_id%_%study_id%", options = bib_options)
})

##########################################
# Export                                 #
##########################################

observeEvent(input$report_export_button_%group_id%_%study_id%, {
  
  tryCatch({
    
    pdf_lib_available <- TRUE
    
    if (input$report_export_format_%group_id%_%study_id% == "pdf" & !requireNamespace("tinytex", quietly = TRUE)){
      pdf_lib_available <- FALSE
      output$message_bar_1_%group_id%_%study_id% <- renderUI(
        div(shiny.fluent::MessageBar(translate(language, "tinytex_required", new_words), messageBarType = 3), style = "margin-top:10px;"))
    }
    
    req(pdf_lib_available)
    
    pages <- c("abstract", "intro", "methods", "results", "discussion", "conclusion")
    
    report_pages <- ""
    sapply(pages, function(page){
      report_pages <<- paste0(report_pages, "\n", paste0(input[[paste0("ace_editor_", page, "_%group_id%_%study_id%")]], " \\pagebreak\n"))
    })
    
    bibliography <- ""
    bibliography_page <- ""
    
    if (length(input$choose_bib_%group_id%_%study_id%) > 0){
      
      # Get bib directory
      if (length(r$datamarts_folder) > 0) folder <- paste0(r$datamarts_folder, "/bib/")
      else folder <- paste0(path.expand('~'), "/data/bib/")
      path <- paste0(folder, "bib_%study_id%_", input$choose_bib_%group_id%_%study_id%, ".bib")
      
      if (file.exists(path)) bibliography <- paste0("\nbibliography: ", path)
      
      bibliography_page <- "\n# References\n\n<div id='refs'></div>"
    }
    
    report <- paste0(
      "---\n",
      "title: 'Test'\n",
      "output: ", input$report_export_format_%group_id%_%study_id%, "_document\n",
      bibliography,
      "\ncsl: https://www.zotero.org/styles/american-medical-association?source=1",
      "\n---\n\n",
      report_pages,
      bibliography_page
    )
    
    params <- list()
    
    study_name <- r$studies %>% dplyr::filter(id == input$study) %>% dplyr::pull(name)
    
    unlink(paste0(path.expand("~"), "/cdwtools_temp_files"), recursive = TRUE, force = TRUE)
    dir.create(paste0(path.expand("~"), "/cdwtools_temp_files"), showWarnings = FALSE)
    
    filename <- paste0(path.expand("~"), "/cdwtools_temp_files/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"),
      " - " , study_name, " - ", translate(language, "report", new_words))
    output_filename <- paste0(filename, ".", input$report_export_format_%group_id%_%study_id%)
    r$filename_%group_id%_%study_id% <- output_filename
    
    writeLines(report, paste0(filename, ".Rmd"))
    
    rmarkdown::render(
      input = paste0(filename, ".Rmd"),
      output_format = paste0(input$report_export_format_%group_id%_%study_id%, "_document"),
      output_file = output_filename)
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language),
    warning = function(w) if (nchar(w[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Warning", error_report = toString(w), language = language))
  
  output$report_export_%group_id%_%study_id% <- renderUI(a("test", href = output_filename, download=NA, target = "_blank"))
  
  shinyjs::click("report_export_%group_id%_%study_id%")
  
})

output$report_export_%group_id%_%study_id% <- downloadHandler(
  filename = function() paste(r$filename_%group_id%_%study_id%),
  content = function(file) {
    file.copy(r$filename_%group_id%_%study_id%, file)
  }
)

