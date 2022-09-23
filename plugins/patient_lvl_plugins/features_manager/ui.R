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
  
  features <- DBI::dbGetQuery(r$db, "SELECT f1.id, f1.value AS feature_name, f2.value AS feature_type
        FROM modules_elements_options f1
        LEFT JOIN modules_elements_options f2 ON f1.id = f2.link_id AND f2.category = 'aggregated' AND f2.name = 'feature_type'
        WHERE f1.deleted IS FALSE AND f1.study_id = %study_id% AND f1.category = 'aggregated' AND f1.name = 'feature_name'")
  
  features_options <- DBI::dbGetQuery(r$db, "SELECT f1.id, f1.link_id, f1.value AS feature_option_name
        FROM modules_elements_options f1
        WHERE f1.deleted IS FALSE AND f1.study_id = %study_id% AND f1.category = 'aggregated' AND f1.name = 'feature_option'")
  
  features_values <- DBI::dbGetQuery(r$db, "SELECT f1.id, f1.link_id, f1.value AS feature_value
        FROM modules_elements_options f1
        WHERE f1.deleted IS FALSE AND f1.study_id = %study_id% AND f1.patient_id = %study_id% AND f1.category = 'patient_lvl' AND f1.name = 'feature_value'")
  
  dropdowns <- tagList()
  textfields <- tagList()
  toggles <- tagList()
  
  if (nrow(features) > 0){
    
    r$features_%group_id%_%study_id% <- features %>% dplyr::mutate(input_id = paste0("feature_", id, "_%group_id%_%study_id%")) %>% dplyr::pull(input_id)
    
    sapply(1:nrow(features), function(i) {
      
      row <- features[i, ]
      
      value <- NULL
      
      if (nrow(features_values) > 0) value <- features_values %>% dplyr::filter(link_id == row$id) %>% dplyr::pull(feature_value)
      
      if (row$feature_type == "dropdown"){
        options <- convert_tibble_to_list(features_options %>% dplyr::filter(link_id == row$id), key_col = "id", text_col = "feature_option_name")
        
        dropdowns <<- tagList(dropdowns, 
          div(
            div(id = ns(paste0(id, "_title")), class = "input_title", row$feature_name),
            div(shiny.fluent::Dropdown.shinyInput(ns(paste0("feature_", row$id, "_%group_id%_%study_id%")), options = options, value = as.integer(value)), style = "width:300px;")
          )
        )
      }
      
      if (row$feature_type == "textfield"){
        textfields <<- tagList(textfields, 
          div(
            div(id = ns(paste0(id, "_title")), class = "input_title", row$feature_name),
            div(shiny.fluent::TextField.shinyInput(ns(paste0("feature_", row$id, "_%group_id%_%study_id%")), value = value), style = "width:300px;")
          )
        )
      }
      
      if (row$feature_type == "toggle"){
        if (length(value) == 0) value <- FALSE
        toggles <<- tagList(toggles, 
          div(
            div(class = "input_title", row$feature_name),
            shiny.fluent::Toggle.shinyInput(ns(paste0("feature_", row$id, "_%group_id%_%study_id%")), value = as.logical(value))
          )
        )
      }
    })
  }
  
  if (length(dropdowns) == 0 & length(textfields) == 0 & length(toggles) == 0) div(
    shiny.fluent::MessageBar(translate(language, "no_data", new_words), messageBarType = 0), style = "margin-top:10px;") -> result
  
  else div(
    dropdowns, textfields, toggles,
    br(),
    shiny.fluent::PrimaryButton.shinyInput(ns("save_%group_id%_%study_id%"), translate(language, "save", r$words))
  ) -> result
  
  result
}

temp()