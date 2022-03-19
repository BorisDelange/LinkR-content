temp <- function(){
  
  ##########################################
  # Translations                           #
  ##########################################
  
  new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
    "EN", "show_stays", "Show stays",
    "FR", "show_stays", "Afficher les séjours",
    "EN", "draw_points", "Draw points",
    "FR", "draw_points", "Afficher les points",
    "EN", "sync_timeline", "Synchronize timeline",
    "FR", "sync_timeline", "Synchroniser la timeline entre les figures"
  )
  
  ##########################################
  # Render UI                              #
  ##########################################
  
  options <- list(
    list(key = "all_stays", text = translate(language, "all_stays", r$words)),
    list(key = "current_stay", text = translate(language, "current_stay", r$words))
  )
  
  dygraph_output <- ""
  if (requireNamespace("dygraphs", quietly = TRUE)) dygraph_output <- div(id = ns("div_dygraph_%group_id%_%study_id%"),
    dygraphs::dygraphOutput(ns("dygraph_%group_id%_%study_id%"), height = "300px"), style = "margin-left:40px;")
  
  
  div(
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-div_%group_id%_%study_id%', item.props.id)")),
      shiny.fluent::PivotItem(
        id = "div_figure_%group_id%_%study_id%",
        item_key = "div_figure_%group_id%_%study_id%", headerText = translate(language, "figure", r$words)
      ),
      shiny.fluent::PivotItem(
        id = "div_settings_%group_id%_%study_id%",
        item_key = "div_settings_%group_id%_%study_id%", headerText = translate(language, "settings", r$words)
      )
    ),
    br(),
    div(id = ns("div_figure_%group_id%_%study_id%"),
      shiny.fluent::ChoiceGroup.shinyInput(ns("data_choice_%group_id%_%study_id%"), value = "all_stays", options = options, className = "inline_choicegroup"), br(),
      uiOutput(ns("message_bar_1_%group_id%_%study_id%")),
      uiOutput(ns("message_bar_2_%group_id%_%study_id%")),
      uiOutput(ns("message_bar_3_%group_id%_%study_id%")),
      uiOutput(ns("message_bar_4_%group_id%_%study_id%")), br(),
      dygraph_output
    ),
    shinyjs::hidden(
      div(id = ns("div_settings_%group_id%_%study_id%"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), div(),
          shiny.fluent::Checkbox.shinyInput(ns("show_stays_%group_id%_%study_id%"), value = FALSE), div(style = "width:8px"),
          paste0(translate(language, "show_stays", new_words))
        ), br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), div(),
          shiny.fluent::Checkbox.shinyInput(ns("draw_points_%group_id%_%study_id%"), value = TRUE), div(style = "width:8px"),
          paste0(translate(language, "draw_points", new_words))
        ), br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), div(),
          shiny.fluent::Checkbox.shinyInput(ns("sync_timeline_%group_id%_%study_id%"), value = TRUE), div(style = "width:8px"),
          paste0(translate(language, "sync_timeline", new_words))
        )   
      )
    )
  )
  
}

temp()