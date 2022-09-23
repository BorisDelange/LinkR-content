temp <- function(){
  
  ##########################################
  # Translations                           #
  ##########################################
  
  new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
    "EN", "no_data", "No features added for this study yet",
    "FR", "no_data", "Pas de paramètres ajoutés pour cette étude",
  )
  
  ##########################################
  # Render UI                              #
  ##########################################
  
  features <- DBI::dbGetQuery(r$db, "SELECT f1.id, f1.value AS feature_name, f2.value AS feature_type, f3.value_num AS display_order
        FROM modules_elements_options f1
        LEFT JOIN modules_elements_options f2 ON f1.id = f2.link_id AND f2.category = 'aggregated' AND f2.name = 'feature_type'
        LEFT JOIN modules_elements_options f3 ON f1.id = f3.link_id AND f3.category = 'aggregated' AND f3.name = 'feature_display_order'
        WHERE f1.deleted IS FALSE AND f1.study_id = %study_id% AND f1.category = 'aggregated' AND f1.name = 'feature_name'") %>%
    dplyr::arrange(display_order)
  
  print(features)
  
  features_options <- DBI::dbGetQuery(r$db, "SELECT f1.id, f1.link_id, f1.value AS feature_option_name
        FROM modules_elements_options f1
        WHERE f1.deleted IS FALSE AND f1.study_id = %study_id% AND f1.category = 'aggregated' AND f1.name = 'feature_option'")
  
  features_inputs <- tagList()
  
  if (nrow(features) > 0){
    
    r$features_%group_id%_%study_id% <- features %>% dplyr::mutate(input_id = paste0("feature_", id, "_%group_id%_%study_id%")) %>% dplyr::pull(input_id)
    
    sapply(1:nrow(features), function(i) {
      
      row <- features[i, ]
      
      if (row$feature_type == "dropdown"){
        options <- convert_tibble_to_list(features_options %>% dplyr::filter(link_id == row$id), key_col = "id", text_col = "feature_option_name")
        
        features_inputs <<- tagList(features_inputs, 
          div(
            div(id = ns(paste0(id, "_title")), class = "input_title", row$feature_name),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 5), 
              div(shiny.fluent::Dropdown.shinyInput(ns(paste0("feature_", row$id, "_%group_id%_%study_id%")), options = options), style = "width:300px;"),
              div(style = "width:30px;"),
              div(shiny.fluent::DatePicker.shinyInput(ns(paste0("date_feature_", row$id, "_%group_id%_%study_id%"))), style = "width:200px;"),
              div(shiny.fluent::TextField.shinyInput(ns(paste0("time_feature_", row$id, "_%group_id%_%study_id%")), value = "00:00:00"), style = "width:70px;")
            )
          )
        )
      }
      
      if (row$feature_type == "textfield"){
        features_inputs <<- tagList(features_inputs, 
          div(
            div(id = ns(paste0(id, "_title")), class = "input_title", row$feature_name),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 5),
              div(shiny.fluent::TextField.shinyInput(ns(paste0("feature_", row$id, "_%group_id%_%study_id%"))), style = "width:300px;"),
              div(style = "width:30px;"),
              div(shiny.fluent::DatePicker.shinyInput(ns(paste0("date_feature_", row$id, "_%group_id%_%study_id%"))), style = "width:200px;"),
              div(shiny.fluent::TextField.shinyInput(ns(paste0("time_feature_", row$id, "_%group_id%_%study_id%")), value = "00:00:00"), style = "width:70px;")
            )
          )
        )
      }
      
      if (row$feature_type == "toggle"){
        features_inputs <<- tagList(features_inputs, 
          div(
            div(class = "input_title", row$feature_name),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 5),
              div(shiny.fluent::Toggle.shinyInput(ns(paste0("feature_", row$id, "_%group_id%_%study_id%"))), style = "width:300px;"),
              div(style = "width:30px;"),
              div(shiny.fluent::DatePicker.shinyInput(ns(paste0("date_feature_", row$id, "_%group_id%_%study_id%"))), style = "width:200px;"),
              div(shiny.fluent::TextField.shinyInput(ns(paste0("time_feature_", row$id, "_%group_id%_%study_id%")), value = "00:00:00"), style = "width:70px;")
            )
          )
        )
      }
    })
  }
  
  if (length(features_inputs) == 0) div(
    shiny.fluent::MessageBar(translate(language, "no_data", new_words), messageBarType = 0), style = "margin-top:10px;") -> result
  
  else div(
    features_inputs, br(),
    shiny.fluent::PrimaryButton.shinyInput(ns("save_%group_id%_%study_id%"), translate(language, "save", r$words))
  ) -> result
  
  result
}

temp()