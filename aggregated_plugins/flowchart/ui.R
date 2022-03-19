temp <- function(){
  
  # Translations
  new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
    "EN", "copy_figure", "Copy figure",
    "FR", "copy_figure", "Copier la figure",
    "EN", "display_figure", "Display figure",
    "FR", "display_figure", "Afficher la figure"
  )
  
  # Get width & height
  
  flowchart_attr <- list()
  
  sapply(c("width", "height"), function(attr){
    
    flowchart_attr[[attr]] <- DBI::dbGetQuery(r$db, paste0("SELECT value_num FROM modules_elements_options WHERE category = 'aggregated' AND name = 'flowchart_", attr, "' 
            AND group_id = %group_id% AND study_id = %study_id% AND deleted IS FALSE"))
    
    # If no entry in database yet, create one
    if (nrow(flowchart_attr[[attr]]) == 0){
      
      last_row <- get_last_row(r$db, "modules_elements_options")
      sql <- glue::glue_sql("INSERT INTO modules_elements_options(id, group_id, study_id, category, name, value_num, creator_id, datetime, deleted)
                SELECT {last_row + 1}, %group_id%, %study_id%, 'aggregated', {paste0('flowchart_', attr)}, 300, {r$user_id}, {as.character(Sys.time())}, FALSE", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      flowchart_attr[[attr]] <<- 400
    }
    
    # If already an entry, get this entry and update sliders
    if (nrow(flowchart_attr[[attr]]) > 0){
      
      value <- as.integer(flowchart_attr[[attr]] %>% dplyr::pull(value_num))
      flowchart_attr[[attr]] <<- value
      r[[paste0("flowchart_", attr, "_%group_id%_%study_id%")]] <- value
    }  
  })
  
  div(
    uiOutput(ns("message_bar_1_%group_id%_%study_id%")),
    uiOutput(ns("message_bar_2_%group_id%_%study_id%")),
    uiOutput(ns("message_bar_3_%group_id%_%study_id%")),
    uiOutput(ns("message_bar_4_%group_id%_%study_id%")), br(),
    uiOutput(ns("flowchart_%group_id%_%study_id%")),
    div(
      span("Width", style = "font-weight:bold; margin-left: 5px;"), br(),
      shiny.fluent::Slider.shinyInput(ns("flowchart_width_%group_id%_%study_id%"), value = flowchart_attr$width, min = 10, max = 2000), br(),
      span("Height", style = "font-weight:bold; margin-left: 5px;"), br(),
      shiny.fluent::Slider.shinyInput(ns("flowchart_height_%group_id%_%study_id%"), value = flowchart_attr$height, min = 10, max = 2000), br(),
      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
        shiny.fluent::DefaultButton.shinyInput(ns("flowchart_render_%group_id%_%study_id%"), translate(language, "display_figure", new_words)),
        shiny.fluent::DefaultButton.shinyInput(ns("flowchart_copy_%group_id%_%study_id%"), translate(language, "copy_figure", new_words)),
        shiny.fluent::PrimaryButton.shinyInput(ns("flowchart_save_%group_id%_%study_id%"), translate(language, "save", r$words))
      ),
      style = "float:center;")
  )
  
}

temp()