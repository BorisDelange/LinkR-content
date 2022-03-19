temp <- function(){
  
  # Create our dictionnary
  new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
    "EN", "choose_feature", "Choose a feature",
    "FR", "choose_feature", "Choisir un paramètre",
    "EN", "manage_features", "Manage features",
    "FR", "manage_features", "Gérer les paramètres",
    "EN", "feature", "Feature",
    "FR", "feature", "Paramètre",
    "EN", "features", "Features",
    "FR", "features", "Paramètres",
    "EN", "display", "Display",
    "FR", "display", "Afficher",
    "EN", "percent", "Percent",
    "FR", "percent", "Pourcentages",
    "EN", "occurences", "Occurences",
    "FR", "occurences", "Occurences",
    "EN", "x_axis_min", "X-axis min",
    "FR", "x_axis_min", "Axe X - minimum",
    "EN", "x_axis_max", "X-axis max",
    "FR", "x_axis_max", "Axe X - maximum",
    "EN", "y_axis_min", "Y-axis min",
    "FR", "y_axis_min", "Axe Y - minimum",
    "EN", "y_axis_max", "Y-axis max",
    "FR", "y_axis_max", "Axe Y - maximum"
  )
  
  sql <- glue::glue_sql("SELECT datamart_id FROM studies WHERE id = %study_id%", .con = r$db)
  datamart <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(datamart_id)
  sql <- glue::glue_sql("SELECT data_source_id FROM datamarts WHERE id = {datamart}", .con = r$db)
  data_source <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(data_source_id)
  
  thesaurus_ids <- integer()
  
  sapply(1:nrow(r$thesaurus), function(i){
    row <- r$thesaurus[i, ]
    data_sources <- stringr::str_split(row$data_source_id, ", ") %>% unlist()
    if (data_source %in% data_sources) thesaurus_ids <<- c(thesaurus_ids, row$id)
  })
  
  thesaurus <- r$thesaurus %>% dplyr::filter(id %in% thesaurus_ids)
  
  thesaurus_options <- convert_tibble_to_list(thesaurus, key_col = "id", text_col = "name")
  
  display_type_options <- list(
    list(key = "percent", text = translate(language, "percent", new_words)),
    list(key = "occurences", text = translate(language, "occurences", new_words))
  )
  
  div(
    shiny.fluent::reactOutput(ns("delete_confirm_%group_id%_%study_id%")),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-div_%group_id%_%study_id%', item.props.id)")),
      shiny.fluent::PivotItem(id = "div_choose_feature_%group_id%_%study_id%", item_key = "div_choose_feature_%group_id%_%study_id%", headerText = translate(language, "choose_feature", new_words)),
      shiny.fluent::PivotItem(id = "div_manage_features_%group_id%_%study_id%", item_key = "div_manage_features_%group_id%_%study_id%", headerText = translate(language, "manage_features", new_words))
    ),
    
    div(id = ns("div_choose_feature_%group_id%_%study_id%"), br(),
      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
        make_dropdown(label = "thesaurus", id = "thesaurus_%group_id%_%study_id%", words = r$words, ns = ns, options = thesaurus_options, width = "300px"),
        make_combobox(language = "EN", ns = ns, label = "feature", id = "feature_%group_id%_%study_id%", multiSelect = FALSE, allowFreeform = TRUE, 
          autoComplete = "on", width = "300px", words = new_words),
        div(shiny.fluent::PrimaryButton.shinyInput(ns("display_%group_id%_%study_id%"), translate(language, "display", new_words)), style = "padding-top:38px;")
      ), br(),
      #shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
      div(plotly::plotlyOutput(ns("figure_%group_id%_%study_id%")), style = "float:left; width:calc(100% - 350px)"),
      div(
        shiny.fluent::ChoiceGroup.shinyInput(ns("display_type_%group_id%_%study_id%"), value = "percent", options = display_type_options, className = "inline_choicegroup"), br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          div(shiny.fluent::SpinButton.shinyInput(ns("x_axis_min"), step = 1), style = "width:100px;"),
          div(translate(language, "x_axis_min", new_words)), style = "margin-bottom:10px;"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          div(shiny.fluent::SpinButton.shinyInput(ns("x_axis_max"), step = 1), style = "width:100px;"),
          div(translate(language, "x_axis_max", new_words)), style = "margin-bottom:10px;"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          div(shiny.fluent::SpinButton.shinyInput(ns("y_axis_min"), step = 1), style = "width:100px;"),
          div(translate(language, "y_axis_min", new_words)), style = "margin-bottom:10px;"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          div(shiny.fluent::SpinButton.shinyInput(ns("y_axis_max"), step = 1), style = "width:100px;"),
          div(translate(language, "y_axis_max", new_words)), style = "margin-bottom:10px;"),
        style = "width:300px; float:right;"
      ),
      div(verbatimTextOutput(ns("result_%group_id%_%study_id%")), style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; 
                    margin: 20px 5px 0px 0px; float:left;")
      #)
    ),
    
    shinyjs::hidden(
      div(id = ns("div_manage_features_%group_id%_%study_id%"), br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
          make_dropdown(label = "thesaurus", id = "thesaurus_datatable_%group_id%_%study_id%", words = r$words, ns = ns, options = thesaurus_options, width = "300px"),
          make_dropdown(language = "EN", ns = ns, label = "features", id = "features_datatable_%group_id%_%study_id%", multiSelect = TRUE,
            width = "600px", words = new_words),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("save_%group_id%_%study_id%"), translate(language, "save", r$words)), style = "padding-top:38px;")
        ),
        DT::DTOutput(ns("datatable_%group_id%_%study_id%"))
      )
    )
  ) -> result
  
  result
}

temp()