temp <- function(){
  
  # Create our dictionnary
  new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
    "EN", "add_exclusion_criterion", "Add an exclusion criterion",
    "FR", "add_exclusion_criterion", "Ajouter un critère d'exclusion",
    "EN", "exclusion_criteria_management", "Exclusion criteria management",
    "FR", "exclusion_criteria_management", "Gestion des critères d'exclusion"
  )
  
  # Render UI
  div(
    shiny.fluent::reactOutput(ns("delete_confirm_%group_id%_%study_id%")),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-div_%group_id%_%study_id%', item.props.id)")),
      shiny.fluent::PivotItem(id = "div_manage_exclusion_criteria_%group_id%_%study_id%", item_key = "div_manage_exclusion_criteria_%group_id%_%study_id%", headerText = translate(language, "exclusion_criteria_management", new_words)),
      shiny.fluent::PivotItem(id = "div_add_exclusion_criterion_%group_id%_%study_id%", item_key = "div_add_exclusion_criterion_%group_id%_%study_id%", headerText = translate(language, "add_exclusion_criterion", new_words))
    ),
    
    div(id = ns("div_manage_exclusion_criteria_%group_id%_%study_id%"), br(),
      DT::DTOutput(ns("datatable_%group_id%_%study_id%")),
      shiny.fluent::PrimaryButton.shinyInput(ns("save_%group_id%_%study_id%"), translate(language, "save", r$words))
    ),
    shinyjs::hidden(
      div(id = ns("div_add_exclusion_criterion_%group_id%_%study_id%"), br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          make_textfield(language = language, ns = ns, label = "name", id = "name_%group_id%_%study_id%", width = "300px"),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("add_%group_id%_%study_id%"), translate(language, "add", r$words)), style = "margin-top:38px;")
        )
      )
    )
  ) -> result
  
  result
}

temp()