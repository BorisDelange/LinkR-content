temp <- function(){
  
  # Translations
  new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
    "EN", "open_bib", "Open a bibliography",
    "FR", "open_bib", "Ouvrir une bibliographie",
    "EN", "bibliography", "Bibliography",
    "FR", "bibliography", "Bibliographie",
    "EN", "new_bib", "New bibliography",
    "FR", "new_bib", "Nouvelle bibliographie",
    "EN", "browse_bib", "Choose BibTeX file",
    "FR", "browse_bib", "Choisir un fichier BibTeX",
    "EN", "upload_bib", "Upload BibTeX file",
    "FR", "upload_bib", "Charger un fichier BibTeX",
    "EN", "import_bib_file", "Import a bibliography file",
    "FR", "import_bib_file", "Importer un fichier de bibliographie",
    "EN", "file", "File",
    "FR", "file", "Fichier",
    "EN"," add_bib", "Add a biblio",
    "FR", "add_bib", "Ajouter une biblio",
    "EN", "add_bibtex", "Add a BibTeX file",
    "FR", "add_bibtex", "Ajouter un fichier BibTeX",
    "EN", "delete_bib", "Delete a biblio",
    "FR", "delete_bib", "Supprimer une biblio"
  )
  
  bib_options <- convert_tibble_to_list(DBI::dbGetQuery(r$db, "SELECT * FROM modules_elements_options WHERE DELETED IS FALSE AND study_id = %study_id% AND name = 'bib_name'"), key_col = "id", text_col = "value")
  
  div(
    shiny.fluent::reactOutput(ns("delete_confirm_%group_id%_%study_id%")),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-div_%group_id%_%study_id%', item.props.id)")),
      shiny.fluent::PivotItem(id = "div_open_bib_%group_id%_%study_id%", item_key = "div_open_bib_%group_id%_%study_id%", headerText = translate(language, "open_bib", new_words)),
      shiny.fluent::PivotItem(id = "div_add_bib_%group_id%_%study_id%", item_key = "div_add_bib_%group_id%_%study_id%", headerText = translate(language, "add_bib", new_words)),
      shiny.fluent::PivotItem(id = "div_add_bibtex_%group_id%_%study_id%", item_key = "div_add_bibtex_%group_id%_%study_id%", headerText = translate(language, "add_bibtex", new_words)),
      shiny.fluent::PivotItem(id = "div_delete_bib_%group_id%_%study_id%", item_key = "div_delete_bib_%group_id%_%study_id%", headerText = translate(language, "delete_bib", new_words))
    ),
    
    div(id = ns("div_open_bib_%group_id%_%study_id%"), br(),
      make_dropdown(label = "bibliography", id = "bib_select_%group_id%_%study_id%", words = new_words, ns = ns, options = bib_options, width = "300px"),
      DT::DTOutput(ns("bib_datatable_%group_id%_%study_id%"))
    ),
    
    shinyjs::hidden(
      div(id = ns("div_add_bib_%group_id%_%study_id%"), br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          make_textfield(label = "name", id = "new_bib_name_%group_id%_%study_id%", ns = ns, width = "300px"),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("new_bib_add_%group_id%_%study_id%"), translate(language, "add", r$words)), style = "padding-top:38px;")
        )
      )
    ),
    shinyjs::hidden(
      div(id = ns("div_add_bibtex_%group_id%_%study_id%"), br(),
        div(style = "display:none;", fileInput(ns("import_bib_file_%group_id%_%study_id%"), label = "", multiple = FALSE, accept = ".bib")),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          make_dropdown(label = "bibliography", id = "import_bib_name_%group_id%_%study_id%", ns = ns, options = bib_options, width = "300px", words = new_words),
          div(shiny.fluent::DefaultButton.shinyInput(ns("import_bib_browse_%group_id%_%study_id%"), translate(language, "browse_bib", new_words), iconProps = list(iconName = "Upload")), style = "padding-top:38px;"),
          div(uiOutput(ns("import_bib_status_%group_id%_%study_id%")), style = "padding-top:38px;")
        ), br(),
        shiny.fluent::PrimaryButton.shinyInput(ns("import_bib_validate_%group_id%_%study_id%"), translate(language, "upload_bib", new_words)), br(),
      )
    ),
    shinyjs::hidden(
      div(id = ns("div_delete_bib_%group_id%_%study_id%"), br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          make_dropdown(label = "bibliography", id = "delete_bib_%group_id%_%study_id%", ns = ns, options = bib_options, width = "300px", words = new_words),
          div(shiny.fluent::DefaultButton.shinyInput(ns("delete_bib_validate_%group_id%_%study_id%"), translate(language, "delete", r$words)), style = "padding-top:38px;")
        )
      )
    ),
  )
}

temp()