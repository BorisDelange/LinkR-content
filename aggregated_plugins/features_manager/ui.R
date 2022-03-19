temp <- function(){
  
  # Translations
  new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
    "EN", "all_features", "All features",
    "FR", "all_features", "Tous les paramètres",
    "EN", "new_features", "New features",
    "FR", "new_features", "Créer des paramètres",
    "EN", "new_feature", "New feature",
    "FR", "new_feature", "Nouveau paramètre",
    "EN", "feature", "Feature",
    "FR", "feature", "Paramètre",
    "EN", "feature_type", "Feature type",
    "FR", "feature_type", "Type de paramètre",
    "EN", "new_feature_option", "New feature choice",
    "FR", "new_feature_option", "Ajouter des choix aux paramètres",
    "EN", "dropdown", "Dropdown",
    "FR", "dropdown", "Menu déroulant",
    "EN", "textfield", "Textfield",
    "FR", "textfield", "Texte libre",
    "EN", "toggle", "Toggle",
    "FR", "toggle", "Binaire",
    "EN", "features_management", "Features management",
    "FR", "features_management", "Gestion des paramètres",
    "EN", "features_options", "Features values",
    "FR", "features_options", "Valeurs des paramètres"
  )
  
  feature_type_options <- list(
    list(key = "dropdown", text = translate(language, "dropdown", new_words)),
    list(key = "textfield", text = translate(language, "textfield", new_words)),
    list(key = "toggle", text = translate(language, "toggle", new_words)))
  
  ##########################################
  # Update dropdowns                       #
  ##########################################
  
  features <- convert_tibble_to_list(DBI::dbGetQuery(r$db, "SELECT o1.id, o1.value FROM modules_elements_options o1 
        INNER JOIN modules_elements_options o2 ON o1.id = o2.link_id AND o2.name = 'feature_type'
        WHERE o1.deleted IS FALSE AND o1.study_id = %study_id% AND o1.name = 'feature_name' AND o2.value = 'dropdown'"), key_col = "id", text_col = "value")
  
  
  div(
    shiny.fluent::reactOutput(ns("delete_confirm_%group_id%_%study_id%")),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-div_%group_id%_%study_id%', item.props.id)")),
      shiny.fluent::PivotItem(id = "div_all_features_%group_id%_%study_id%", item_key = "div_all_features_%group_id%_%study_id%", headerText = translate(language, "all_features", new_words)),
      shiny.fluent::PivotItem(id = "div_features_options_%group_id%_%study_id%", item_key = "div_features_options_%group_id%_%study_id%", headerText = translate(language, "features_options", new_words)),
      shiny.fluent::PivotItem(id = "div_new_feature_%group_id%_%study_id%", item_key = "div_new_feature_%group_id%_%study_id%", headerText = translate(language, "new_feature", new_words)),
      shiny.fluent::PivotItem(id = "div_new_feature_option_%group_id%_%study_id%", item_key = "div_new_feature_option_%group_id%_%study_id%", headerText = translate(language, "new_feature_option", new_words))
    ),
    
    div(id = ns("div_all_features_%group_id%_%study_id%"), br(),
      DT::DTOutput(ns("features_datatable_%group_id%_%study_id%"))
    ),
    shinyjs::hidden(
      div(id = ns("div_features_options_%group_id%_%study_id%"), br(),
        make_dropdown(label = "feature", id = "features_options_feature_%group_id%_%study_id%", ns = ns, options = features, width = "300px", words = new_words),
        DT::DTOutput(ns("features_options_datatable_%group_id%_%study_id%")),
      )
    ),
    shinyjs::hidden(
      div(id = ns("div_new_feature_%group_id%_%study_id%"), br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          make_textfield(label = "name", id = "new_feature_name_%group_id%_%study_id%", ns = ns, width = "300px"),
          make_dropdown(label = "feature_type", id = "new_feature_type_%group_id%_%study_id%", ns = ns, width = "300px", words = new_words, options = feature_type_options),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("new_feature_add_%group_id%_%study_id%"), translate(language, "add", r$words)), style = "padding-top:38px;")
        )
      )
    ),
    shinyjs::hidden(
      div(id = ns("div_new_feature_option_%group_id%_%study_id%"), br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          make_dropdown(label = "feature", id = "feature_%group_id%_%study_id%", ns = ns, width = "300px", options = features, words = new_words),
          make_textfield(label = "name", id = "new_feature_option_name_%group_id%_%study_id%", ns = ns, width = "300px"),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("new_feature_option_add_%group_id%_%study_id%"), translate(language, "add", r$words)), style = "padding-top:38px;")
        )
      )
    ),
  )
}

temp()