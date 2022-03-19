##########################################
# Translations                           #
##########################################

new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
  "EN", "screened_patients", "Screened patients",
  "FR", "screened_patients", "Patients screenés",
  "EN", "patients_not_excluded", "Patients not excluded",
  "FR", "patients_not_excluded", "Patients non exclus",
  "EN", "clipr_required", "Library clipr required to copy a figure, please install it with install.packages('clipr')",
  "FR", "clipr_required", "Le package clipr est nécessaire pour copier une figure, merci de l'installer avec install.packages('clipr')",
  "EN", "diagrammeR_required", "Library diagrammeR required to display a flowchart, please install it with install.packages('DiagrammeR')",
  "FR", "diagrammeR_required", "Le package diagrammeR est nécessaire pour afficher le flowchart, merci de l'installer avec install.packages('DiagrammeR')",
  "EN", "diagrammeRsvg_required", "Library diagrammeRsvg required to copy a flowchart, please install it with install.packages('DiagrammeRsvg')",
  "FR", "diagrammeRsvg_required", "Le package diagrammeRsvg est nécessaire pour copier le flowchart, merci de l'installer avec install.packages('DiagrammeRsvg')",
  "EN", "rsvg_required", "Library rsvg required to copy a flowchart, please install it with install.packages('rsvg')",
  "FR", "rsvg_required", "Le package rsvg est nécessaire pour copier le flowchart, merci de l'installer avec install.packages('rsvg')",
  "EN", "no_exclusion_criteria", "No exclusion criteria available for this study",
  "FR", "no_exclusion_criteria", "Pas de critère d'exclusion disponible pour cette étude"
)

##########################################
# The flowchart is a reactive            #
##########################################

flowchart_%group_id%_%study_id% <- reactive({ 
  
  tryCatch({
    
    ##########################################
    # Get exclusion reasons                  #
    ##########################################
    
    # Existing exclusion reasons
    
    # A message sent by other plugins to reload flowchart
    # No need to send a message from Patient-level data, cause it changes r$patients_options variable, with makes this reactives reload
    if (length(r$reload_flowchart_%study_id%) > 0) r$reload_flowchart_%study_id% <- FALSE
    
    sql <- glue::glue_sql(paste0("SELECT meo1.id, meo1.value AS exclusion_reason, meo2.value_num AS display_order
            FROM modules_elements_options meo1
            LEFT JOIN modules_elements_options meo2 ON meo1.id = meo2.link_id
            WHERE meo1.deleted IS FALSE AND meo2.deleted IS FALSE AND meo1.category = 'aggregated' AND meo1.name = 'exclusion_reason_name'
            AND meo1.study_id = %study_id%"), .con = r$db)
    
    exclusion_reasons <<- DBI::dbGetQuery(r$db, sql) %>% dplyr::arrange(display_order) %>% dplyr::mutate_at("display_order", as.integer)
    
    # Excluded patients with exclusions reasons
    
    excluded_patients <- r$patients_options %>% dplyr::filter(category == "exclusion_reason" & study_id == %study_id%) %>% dplyr::select(patient_id, value_num)
    
    # Merge the two tables
    
    excluded_patients <- 
      excluded_patients %>%
      dplyr::left_join(exclusion_reasons %>% dplyr::select(value_num = id, exclusion_reason, display_order), by = "value_num") %>%
      dplyr::select(-value_num)
    
    # Get list of all patients
    subsets <- DBI::dbGetQuery(r$db, "SELECT id FROM subsets WHERE deleted IS FALSE AND study_id = %study_id%") %>% dplyr::pull()
    sql <- glue::glue_sql("SELECT patient_id FROM subset_patients WHERE deleted IS FALSE AND subset_id IN ({subsets*}) GROUP BY patient_id", .con = r$db)
    all_patients <<- DBI::dbGetQuery(r$db, sql)
    
    # Merge all_patients table with excluded_patients table
    # To have all exclusion reasons (if an exclusion reason is not used), merge with exclusion_reasons
    all_patients <<-
      all_patients %>%
      dplyr::left_join(excluded_patients, by = "patient_id") %>%
      dplyr::count(exclusion_reason)
    
    exclusion_reasons <<-
      exclusion_reasons %>%
      dplyr::left_join(all_patients, by = "exclusion_reason") %>%
      dplyr::mutate(n = ifelse(!is.na(n), n, 0L))
    
    if (nrow(exclusion_reasons) == 0) show_message_bar(output = output, id = 4, message = "no_exclusion_criteria", language = language, words = new_words)
    
    ##########################################
    # Create flowchart                       #
    ##########################################
    
    patients_count <<- sum(all_patients$n)
    
    flowchart_text <<- paste0("[1]: '", translate(language, "screened_patients", new_words), " | n = ", patients_count, "'")
    
    nodes_text <<- ""
    vertical_arrows_1 <<- ""
    vertical_arrows_2 <<- ""
    horizontal_arrows <<- ""
    
    i <- 2
    j <- 1
    
    if (nrow(exclusion_reasons) > 0){
      sapply(1:nrow(exclusion_reasons), function(k) {
        
        row <- exclusion_reasons[k, ]
        
        patients_count <<- patients_count - row$n
        
        flowchart_text <<- paste0(flowchart_text, 
          "\n[", i, "]: '", row$exclusion_reason, " | n = ", row$n, "'",
          "\n[", i + 1, "]: '", translate(language, "patients_not_excluded", new_words), " | n = ", patients_count, "'")
        
        nodes_text <<- paste0(nodes_text, 
          "i", j, " [label = '@@", i - 1, "']\n",
          "e", j, " [label = '@@", i, "']\n")
        
        vertical_arrows_1 <<- paste0(vertical_arrows_1, "i", j, " -> d", j, "; ")
        vertical_arrows_2 <<- paste0(vertical_arrows_2, "d", j, " -> i", j + 1, "; ")
        horizontal_arrows <<- paste0(horizontal_arrows, "{rank = same; d", j, " -> e", j, "}\n")
        
        i <<- i + 2
        j <<- j + 1
      })
    }
    
    nodes_text <<- paste0(nodes_text,
      "i", j, " [label = '@@", i - 1, "']\n")
    
    graph <- paste0(
      "digraph flowchart {\n",
      "node [fontname = Helvetica, shape = rectangle, width = 3]\n\n",
      nodes_text, "\n",
      "node [shape = none, width = 0, height = 0, label = '']\n\n",
      "edge [dir = none]\n",
      vertical_arrows_1, "\n\n",
      "edge [len = 0.5, style = solid]\n", 
      vertical_arrows_2, "\n",
      horizontal_arrows, "\n\n",
      "}\n",
      flowchart_text)
    
    graph
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language),
    warning = function(w) if (nchar(w[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Warning", error_report = toString(w), language = language))
})

# Render flowchart
if (requireNamespace("DiagrammeR", quietly = TRUE)){
  
  if (length(r$flowchart_width_%group_id%_%study_id%) == 0) r$flowchart_width_%group_id%_%study_id% <- 400
  if (length(r$flowchart_height_%group_id%_%study_id%) == 0) r$flowchart_height_%group_id%_%study_id% <- 400
  
  output$flowchart_%group_id%_%study_id% <- renderUI(div(DiagrammeR::grVizOutput(ns("flowchart_grviz_%group_id%_%study_id%"), 
    width = r$flowchart_width_%group_id%_%study_id%, height = r$flowchart_height_%group_id%_%study_id%), style = "float:left; margin-right: 100px;"))
  output$flowchart_grviz_%group_id%_%study_id% <- DiagrammeR::renderGrViz(DiagrammeR::grViz(flowchart_%group_id%_%study_id%()))
}
if (!requireNamespace("DiagrammeR", quietly = TRUE)) output$message_bar_1_%group_id%_%study_id% <- renderUI(
  div(shiny.fluent::MessageBar(translate(language, "diagrammeR_required", new_words), messageBarType = 3), style = "margin-top:10px;"))


##########################################
# Change width or height                 #
##########################################

observeEvent(input$flowchart_render_%group_id%_%study_id%, {
  r$flowchart_width_%group_id%_%study_id% <- input$flowchart_width_%group_id%_%study_id%
    r$flowchart_height_%group_id%_%study_id% <- input$flowchart_height_%group_id%_%study_id%
})

##########################################
# Copy figure                            #
##########################################

observeEvent(input$flowchart_copy_%group_id%_%study_id%, {
  if (requireNamespace("clipr", quietly = TRUE) & requireNamespace("DiagrammeR", quietly = TRUE) & requireNamespace("DiagrammeRsvg", quietly = TRUE) & requireNamespace("rsvg", quietly = TRUE)){
    
    # Save figure in data/figures dir
    
    dir <-  paste0(find.package("cdwtools"), "/data/figures")
    if (!dir.exists(dir)) dir.create(dir)
    file <- paste0(dir, "/flowchart_%group_id%_%study_id%.png")
    
    DiagrammeR::grViz(flowchart_%group_id%_%study_id%()) %>% DiagrammeRsvg::export_svg() %>% charToRaw() %>% 
      rsvg::rsvg_png(file, height = r$flowchart_height_%group_id%_%study_id%)
    
    # Copy the code to user clipboard
    clipr::write_clip(paste0("`r knitr::include_graphics('", file, "')`"))
    
  }
  else {
    if (!requireNamespace("clipr", quietly = TRUE)) output$message_bar_2_%group_id%_%study_id% <- renderUI(
      div(shiny.fluent::MessageBar(translate(language, "clipr_required", new_words), messageBarType = 3), style = "margin-top:10px;"))
    if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) output$message_bar_3_%group_id%_%study_id% <- renderUI(
      div(shiny.fluent::MessageBar(translate(language, "diagrammeRsvg_required", new_words), messageBarType = 3), style = "margin-top:10px;"))
    if (!requireNamespace("rsvg", quietly = TRUE)) output$message_bar_4_%group_id%_%study_id% <- renderUI(
      div(shiny.fluent::MessageBar(translate(language, "rsvg_required", new_words), messageBarType = 3), style = "margin-top:10px;"))
  }
})

##########################################
# Save settings                          #
##########################################

observeEvent(input$flowchart_save_%group_id%_%study_id%, {
  
  tryCatch({
    
    sapply(c("width", "height"), function(attr){
      
      sql <- glue::glue_sql("UPDATE modules_elements_options SET value_num = {input[[paste0('flowchart_', attr, '_%group_id%_%study_id%')]]}
                WHERE category = 'aggregated' AND name = {paste0('flowchart_', attr)} AND group_id = %group_id% AND study_id = %study_id% AND deleted IS FALSE", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
    })
    
    show_message_bar(output, 1, "modif_saved", "success", language, r$words)
  },
    
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language),
    warning = function(w) if (nchar(w[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Warning", error_report = toString(w), language = language))
})