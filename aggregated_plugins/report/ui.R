temp <- function(){
  
  ##########################################
  # Translations                           #
  ##########################################
  
  new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
    "EN", "abstract", "Abstract",
    "FR", "abstract", "Abstract",
    "EN", "intro", "Intro",
    "FR", "intro", "Intro",
    "EN", "methods", "Methods",
    "FR", "methods", "Matériels & méthodes",
    "EN", "results", "Results",
    "FR", "results", "Résultats",
    "EN", "discussion", "Discussion",
    "FR", "discussion", "Discussion",
    "EN", "conclusion", "Conclusion",
    "FR", "conclusion", "Conclusion",
    "EN", "preview", "Preview",
    "FR", "preview", "Aperçu",
    "EN", "format", "Format",
    "FR", "format", "Format",
    "EN", "export", "Export",
    "FR", "export", "Exporter",
    "EN", "bibliography", "Bibliography",
    "FR", "bibliography", "Bibliographie"
  )
  
  ##########################################
  # Get current values                     #
  ##########################################
  
  pages <- c("abstract", "intro", "methods", "results", "discussion", "conclusion")
  
  report_data <- list()
  
  tryCatch({
    sapply(pages, function(page){
      
      sql <- glue::glue_sql("SELECT id, value, creator_id, datetime FROM modules_elements_options WHERE deleted IS FALSE
                AND group_id = %group_id% AND study_id = %study_id% AND category = 'aggregated' AND name = {paste0('report_', page)}", .con = r$db)
      report_data[[page]] <<- DBI::dbGetQuery(r$db, sql)
      
      # If there's noy any value yet
      if (nrow(report_data[[page]]) == 0){
        
        sql <- glue::glue_sql("INSERT INTO modules_elements_options(id, group_id, study_id, category, name, value, creator_id, datetime, deleted)
                    SELECT {get_last_row(r$db, 'modules_elements_options') + 1}, %group_id%, %study_id%, 'aggregated', {paste0('report_', page)}, '', {r$user_id}, 
                    {as.character(Sys.time())}, FALSE", .con = r$db)
        
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
      }
    })
    
    bib_options <- convert_tibble_to_list(DBI::dbGetQuery(r$db, "SELECT * FROM modules_elements_options WHERE DELETED IS FALSE AND study_id = %study_id% AND name = 'bib_name'"), 
      key_col = "id", text_col = "value", null_value = TRUE)
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_ui_code", 
      error_name = "%group_id% - run ui code", category = "Error", error_report = toString(e), language = language),
    warning = function(w) if (nchar(w[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_ui_code", 
      error_name = "%group_id% - run ui code", category = "Warning", error_report = toString(w), language = language))
  
  ##########################################
  # Render UI                              #
  ##########################################
  
  pages <- c("abstract", "intro", "methods", "results", "discussion", "conclusion")
  
  ace_editor <- function(page){
    conditionalPanel(condition = paste0("input.report_page_%group_id%_%study_id% == '", page, "'"), ns = ns,
      div(shinyAce::aceEditor(ns(paste0("ace_editor_", page, "_%group_id%_%study_id%")), value = report_data[[page]]$value,
        mode = "markdown", autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 2000), style = "width: 100%;"))
  }
  
  ace_editors <- tagList()
  options <- list()
  
  sapply(pages, function(page){
    ace_editors <<- tagList(ace_editors, ace_editor(page))
    options <<- rlist::list.append(options, list(key = page, text = translate(language, page, new_words)))    
  })
  
  tagList(
    make_dropdown(language = language, ns = ns, label = "bibliography", id = "choose_bib_%group_id%_%study_id%", options = bib_options, width = "300px", words = new_words),
    shiny.fluent::ChoiceGroup.shinyInput(ns("report_page_%group_id%_%study_id%"), value = "abstract", options = options, className = "inline_choicegroup"),
    ace_editors,
    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 3),
      shiny.fluent::PrimaryButton.shinyInput(ns("report_save_%group_id%_%study_id%"), translate(language, "save", r$words)),
      shiny.fluent::DefaultButton.shinyInput(ns("report_preview_%group_id%_%study_id%"), translate(language, "preview", new_words)),
      div(shiny.fluent::Dropdown.shinyInput(ns("report_export_format_%group_id%_%study_id%"), translate(language, "format", new_words),
        options = list(list(key = "pdf", text = "PDF"), list(key = "html", text = "HTML")), value = "pdf"), style = "width:80px"),
      shiny.fluent::DefaultButton.shinyInput(ns("report_export_button_%group_id%_%study_id%"), translate(language, "export", new_words))),
    br(),
    #div(uiOutput(ns("report_export_%group_id%_%study_id%"))),#, style = "display:none;"),
    div(uiOutput(ns("report_preview_ui_%group_id%_%study_id%")), style = "border:dashed 1px; min-height:40px; padding-left:10px; padding-right:10px;"),
  )
}

temp()