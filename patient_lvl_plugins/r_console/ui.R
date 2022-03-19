temp <- function(){
  
  # Translations
  new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
    "EN", "my_scripts", "My scripts",
    "FR", "my_scripts", "Mes scripts",
    "EN", "manage_scripts", "Manage scripts",
    "FR", "manage_scripts", "Gérer les scripts",
    "EN", "r_script", "Script",
    "FR", "r_script", "Script",
    "EN", "new_script", "New script",
    "FR", "new_script", "Nouveau script",
    "EN", "delete_script", "Delete a script",
    "FR", "delete_script", "Supprimer un script",
    "EN", "console", "Console",
    "FR", "console", "Console",
    "EN", "table", "Table",
    "FR", "table", "Tableau",
    "EN", "plot", "Plot",
    "FR", "plot", "Figure",
    "EN", "output", "Output",
    "FR", "output", "Output",
    "EN", "width", "Width (%)",
    "FR", "width", "Largeur (%)",
    "EN", "markdown", "Markdown",
    "FR", "markdown", "Markdown"
  )
  
  scripts_options <- convert_tibble_to_list(DBI::dbGetQuery(r$db, "SELECT * FROM modules_elements_options WHERE DELETED IS FALSE AND group_id = %group_id% AND study_id = %study_id% AND name = 'r_script_name'"), key_col = "id", text_col = "value")
  
  output_options <- list(
    list(key = "console", text = translate(language, "console", new_words)),
    list(key = "table", text = translate(language, "table", new_words)),
    list(key = "markdown", text = translate(language, "markdown", new_words)),
    list(key = "plot", text = translate(language, "plot", new_words))
  )
  
  div(
    shiny.fluent::reactOutput(ns("delete_confirm_%group_id%_%study_id%")),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-div_%group_id%_%study_id%', item.props.id)")),
      shiny.fluent::PivotItem(id = "div_my_scripts_%group_id%_%study_id%", item_key = "div_my_scripts_%group_id%_%study_id%", headerText = translate(language, "my_scripts", new_words)),
      shiny.fluent::PivotItem(id = "div_new_script_%group_id%_%study_id%", item_key = "div_new_script_%group_id%_%study_id%", headerText = translate(language, "new_script", new_words)),
      shiny.fluent::PivotItem(id = "div_delete_script_%group_id%_%study_id%", item_key = "div_delete_script_%group_id%_%study_id%", headerText = translate(language, "delete_script", new_words))
    ),
    
    div(id = ns("div_my_scripts_%group_id%_%study_id%"), br(),
      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), div(),
        make_dropdown(label = "r_script", id = "Rcode_select_%group_id%_%study_id%", words = new_words, ns = ns, options = scripts_options, width = "300px"),
        div(translate(language, "output", new_words), " : ", style = "margin:43px 0px 0px 40px; font-weight:bold;"),
        div(shiny.fluent::ChoiceGroup.shinyInput(ns("Rcode_output_type_%group_id%_%study_id%"), value = "console", options = output_options, className = "inline_choicegroup"), 
            style = "margin:35px 0px 0px 20px;"),
          conditionalPanel(condition = "input.Rcode_output_type_%group_id%_%study_id% == 'plot'", ns = ns,
            div(translate(language, "width", new_words), " : ", style = "margin:43px 0px 0px 20px; font-weight:bold;")),
          conditionalPanel(condition = "input.Rcode_output_type_%group_id%_%study_id% == 'plot'", ns = ns,
            div(shiny.fluent::Slider.shinyInput(ns("Rcode_plot_width_%group_id%_%study_id%"), value = 100, min = 1, max = 100), style = "margin:40px 0px 0px 0px; width:200px;"))
      ), br(),
      div(shinyAce::aceEditor(ns("Rcode_%group_id%_%study_id%"), "", mode = "r", autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),
      shiny.fluent::PrimaryButton.shinyInput(ns("Rcode_save_%group_id%_%study_id%"), translate(language, "save", r$words)), " ",
      shiny.fluent::DefaultButton.shinyInput(ns("Rcode_execute_%group_id%_%study_id%"), translate(language, "execute_code", r$words)), br(), br(),
      div(id = ns("Rcode_console_output_%group_id%_%study_id%"),
        verbatimTextOutput(ns("Rcode_console_result_%group_id%_%study_id%")), style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"),
      shinyjs::hidden(div(id = ns("Rcode_plot_output_%group_id%_%study_id%"),
        uiOutput(ns("Rcode_plot_ui_result_%group_id%_%study_id%")), style = "width: 99%; margin-right: 5px;")),
      shinyjs::hidden(div(id = ns("Rcode_table_output_%group_id%_%study_id%"),
        DT::DTOutput(ns("Rcode_table_result_%group_id%_%study_id%")), style = "width: 99%; margin-right: 5px;")),
      shinyjs::hidden(div(id = ns("Rcode_markdown_output_%group_id%_%study_id%"),
        uiOutput(ns("Rcode_markdown_result_%group_id%_%study_id%")), style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px; padding-top: 10px;"))
    ),
    
    shinyjs::hidden(
      div(id = ns("div_new_script_%group_id%_%study_id%"), br(),
        shiny.fluent::Text(variant = "large", translate(language, "new_script", new_words), block = TRUE),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          make_textfield(label = "name", id = "new_script_name_%group_id%_%study_id%", ns = ns, width = "300px"),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("new_script_add_%group_id%_%study_id%"), translate(language, "add", r$words)), style = "padding-top:38px;")
        )
      )
    ),
    shinyjs::hidden(
      div(id = ns("div_delete_script_%group_id%_%study_id%"), br(),
        shiny.fluent::Text(variant = "large", translate(language, "delete_script", new_words), block = TRUE),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          make_dropdown(label = "r_script", id = "delete_script_%group_id%_%study_id%", ns = ns, options = scripts_options, width = "300px", words = new_words),
          div(shiny.fluent::DefaultButton.shinyInput(ns("delete_script_validate_%group_id%_%study_id%"), translate(language, "delete", r$words)), style = "padding-top:38px;")
        )
      )
    )
  )
}

temp()