temp <- function(){
  
  ##########################################
  # Translations                           #
  ##########################################
  
  new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
    "EN", "all_stays", "All stays",
    "FR", "all_stays", "Tous les séjours",
    "EN", "current_stay", "Current stay",
    "FR", "current_stay", "Du séjour sélectionné",
    "EN", "show_data_of", "Show data of",
    "FR", "show_data_of", "Afficher les données de",
    "EN", "manage_words_sets", "Manage words sets",
    "FR", "manage_words_sets", "Gérer les sets de mots",
    "EN", "words_set", "Words set",
    "FR", "words_set", "Set de mots",
    "EN", "new_words_set", "New words set",
    "FR", "new_words_set", "Nouveau set de mots",
    "EN", "delete_words_set", "Delete a words set",
    "FR", "delete_words_set", "Supprimer un set de mots",
    "EN", "manage_words_set", "Manage a words set",
    "FR", "manage_words_set", "Gérer un set de mots",
    "EN", "add_remove_words", "Add or remove words",
    "FR", "add_remove_words", "Ajouter ou supprimer des mots",
    "EN", "words", "Words",
    "FR", "words", "Mots",
    "EN", "show_last_version", "Show only last versions",
    "FR", "show_last_version", "N'afficher que les dernières versions",
    "EN", "features", "Features",
    "FR", "features", "Paramètres",
    "EN", "search_words", "Search these words",
    "FR", "search_words", "Rechercher ces mots",
    "EN", "show_all_data", "Show all data",
    "FR", "show_all_data", "Tout afficher",
    "EN", "more_recent_up", "Order in descending order (more recent first)",
    "FR", "more_recent_up", "Classer les notes du plus récent au plus ancien"
  )
  
  ##########################################
  # Render UI                              #
  ##########################################
  
  options <- list(
    list(key = "all_stays", text = translate(language, "all_stays", new_words)),
    list(key = "current_stay", text = translate(language, "current_stay", new_words)))
  
  options_words_set <- convert_tibble_to_list(DBI::dbGetQuery(r$db, "SELECT * FROM modules_elements_options WHERE deleted IS FALSE 
        AND group_id = %group_id% AND study_id = %study_id% AND name = 'words_set_name'"), key_col = "id", text_col = "value")
  
  div(
    shiny.fluent::reactOutput(ns("delete_confirm_%group_id%_%study_id%")),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-div_%group_id%_%study_id%', item.props.id)")),
      shiny.fluent::PivotItem(
        id = "div_figure_%group_id%_%study_id%",
        item_key = "div_figure_%group_id%_%study_id%", headerText = translate(language, "figure", r$words)
      ),
      shiny.fluent::PivotItem(
        id = "div_settings_%group_id%_%study_id%",
        item_key = "div_settings_%group_id%_%study_id%", headerText = translate(language, "settings", r$words)
      ),
      shiny.fluent::PivotItem(
        id = "div_manage_words_set_%group_id%_%study_id%",
        item_key = "div_manage_words_set_%group_id%_%study_id%", headerText = translate(language, "manage_words_set", new_words)
      ),
      shiny.fluent::PivotItem(
        id = "div_new_words_set_%group_id%_%study_id%",
        item_key = "div_new_words_set_%group_id%_%study_id%", headerText = translate(language, "new_words_set", new_words)
      ),
      shiny.fluent::PivotItem(
        id = "div_delete_words_set_%group_id%_%study_id%",
        item_key = "div_delete_words_set_%group_id%_%study_id%", headerText = translate(language, "delete_words_set", new_words)
      )
    ),
    br(),
    div(id = ns("div_figure_%group_id%_%study_id%"),
      shiny.fluent::ChoiceGroup.shinyInput(ns("data_choice_%group_id%_%study_id%"), value = "all_stays", options = options, className = "inline_choicegroup"),
      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
        make_dropdown(label = "words", id = "select_words_%group_id%_%study_id%", ns = ns, width = "300px", language = language, words = new_words),
        make_dropdown(label = "words_set", id = "select_words_set_%group_id%_%study_id%", ns = ns, options = options_words_set, width = "300px", language = language, words = new_words),
        div(shiny.fluent::PrimaryButton.shinyInput(ns("search_words_%group_id%_%study_id%"), translate(language, "search_words", new_words)), style = "padding-top:38px;"),
        div(shiny.fluent::DefaultButton.shinyInput(ns("show_all_data_%group_id%_%study_id%"), translate(language, "show_all_data", new_words)), style = "padding-top:38px;")
      ),
      uiOutput(ns("text_%group_id%_%study_id%"))
    ),
    
    shinyjs::hidden(
      div(id = ns("div_settings_%group_id%_%study_id%"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), div(),
          shiny.fluent::Checkbox.shinyInput(ns("more_recent_up_%group_id%_%study_id%"), value = TRUE), div(style = "width:8px"),
          paste0(translate(language, "more_recent_up", new_words))
        )#, br(),
        #shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), div(),
        #    shiny.fluent::Checkbox.shinyInput(ns("show_last_version_%group_id%_%study_id%"), value = TRUE), div(style = "width:8px"),
        #    paste0(translate(language, "show_last_version", new_words))
        #)
      )
    ),
    
    shinyjs::hidden(
      div(id = ns("div_manage_words_set_%group_id%_%study_id%"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          make_dropdown(label = "words_set", id = "manage_words_set_%group_id%_%study_id%", ns = ns, options = options_words_set, width = "300px", language = language, words = new_words),
          make_dropdown(label = "words", id = "manage_words_%group_id%_%study_id%", ns = ns, width = "300px", language = language, words = new_words, multiSelect = TRUE),
          div(shiny.fluent::DefaultButton.shinyInput(ns("manage_words_save_%group_id%_%study_id%"), translate(language, "save", r$words)), style = "padding-top:38px;")
        ),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          make_textfield(label = "name", id = "manage_words_add_name_%group_id%_%study_id%", language = language, ns = ns, width = "300px"),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("manage_words_add_%group_id%_%study_id%"), translate(language, "add", r$words)), style = "padding-top:38px;")
        )
      )
    ),
    
    shinyjs::hidden(
      div(id = ns("div_new_words_set_%group_id%_%study_id%"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          make_textfield(label = "name", id = "new_words_set_name_%group_id%_%study_id%", ns = ns, language = language, width = "300px"),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("new_words_set_add_%group_id%_%study_id%"), translate(language, "add", r$words)), style = "padding-top:38px;")
        )
      )
    ),
    
    shinyjs::hidden(
      div(id = ns("div_delete_words_set_%group_id%_%study_id%"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          make_dropdown(label = "words_set", id = "delete_words_set_%group_id%_%study_id%", ns = ns, options = options_words_set, width = "300px", language = language, words = new_words),
          div(shiny.fluent::DefaultButton.shinyInput(ns("delete_words_set_validate_%group_id%_%study_id%"), translate(language, "delete", r$words)), style = "padding-top:38px;")
        )
      )
    )
  )
}

temp()