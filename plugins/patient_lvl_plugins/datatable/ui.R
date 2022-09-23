temp <- function(){
  
  ##########################################
  # Translations                           #
  ##########################################
  
  #new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
  #    
  #)
  
  ##########################################
  # Render UI                              #
  ##########################################
  
  options <- list(
    list(key = "all_stays", text = translate(language, "all_stays", r$words)),
    list(key = "current_stay", text = translate(language, "current_stay", r$words))
  )
  
  div(
    shiny.fluent::Pivot(
      shiny.fluent::PivotItem(
        item_key = "pivot_figure_%group_id%_%study_id%", headerText = translate(language, "figure", r$words), br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
          shiny.fluent::ChoiceGroup.shinyInput(ns("data_choice_%group_id%_%study_id%"), value = "all_stays", options = options, className = "inline_choicegroup")
        ),
        uiOutput(ns("message_bar_1_%group_id%_%study_id%")),
        shinyjs::hidden(DT::DTOutput(ns("datatable_labs_vitals_%group_id%_%study_id%"))), br(),
        shinyjs::hidden(DT::DTOutput(ns("datatable_orders_%group_id%_%study_id%")))
      ),
      #shiny.fluent::PivotItem(
      #    item_key = "pivot_settings_%group_id%_%study_id%", headerText = translate(language, "settings", r$words), br(),
      #    ""
      #)
    )
  )
  
}

temp()