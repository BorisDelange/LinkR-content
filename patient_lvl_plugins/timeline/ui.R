temp <- function(){
  
  ##########################################
  # Translations                           #
  ##########################################
  
  new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
    "EN", "sync_timeline", "Synchronize timeline",
    "FR", "sync_timeline", "Synchroniser la timeline entre les figures",
    "EN", "show_data", "Show data",
    "FR", "show_data", "Afficher les données",
    "EN", "num_after_comma", "Digits after the comma",
    "FR", "num_after_comma", "Chiffres après la virgule"
  )
  
  ##########################################
  # Render UI                              #
  ##########################################
  
  options <- list(
    list(key = "all_stays", text = translate(language, "all_stays", r$words)),
    list(key = "current_stay", text = translate(language, "current_stay", r$words))
  )
  
  plotly_output <- ""
  if (requireNamespace("plotly", quietly = TRUE)) plotly_output <- div(id = ns("div_vistime_%group_id%_%study_id%"),
    plotly::plotlyOutput(ns("plotly_%group_id%_%study_id%"), height = "20%"))
  
  
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
      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
        shiny.fluent::ChoiceGroup.shinyInput(ns("data_choice_%group_id%_%study_id%"), value = "all_stays", options = options, className = "inline_choicegroup"),
        div(shiny.fluent::DefaultButton.shinyInput(ns("show_data_%group_id%_%study_id%"), translate(language, "show_data", words = new_words)), style = "margin-top:3px;")
      ), br(),
      uiOutput(ns("message_bar_1_%group_id%_%study_id%")),
      uiOutput(ns("message_bar_2_%group_id%_%study_id%")),
      uiOutput(ns("message_bar_3_%group_id%_%study_id%")),
      uiOutput(ns("message_bar_4_%group_id%_%study_id%")), br(),
      plotly_output
    ),
    shinyjs::hidden(
      div(id = ns("div_settings_%group_id%_%study_id%"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), div(),
          shiny.fluent::Checkbox.shinyInput(ns("sync_timeline_%group_id%_%study_id%"), value = TRUE), div(style = "width:8px"),
          paste0(translate(language, "sync_timeline", new_words))
        ), br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), div(),
          div(paste0(translate(language, "num_after_comma", new_words), " :"), style = "margin-top:5px;"),
          div(shiny.fluent::SpinButton.shinyInput(ns("num_after_comma_%group_id%_%study_id%"), value = 2, min = 0, max = 9, step = 1), style = "width:30px;"),
        )
      )
    )
  )
  
}

temp()