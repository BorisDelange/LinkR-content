div(
    div(
        shiny.fluent::Dropdown.shinyInput(ns("variable_%widget_id%"), options = convert_tibble_to_list(selected_concepts, key_col = "concept_id", text_col = "concept_name")),
        style = "width: 200px;"
    ),
    style = "height: calc(100% - 10px); padding: 5px;"  
)
