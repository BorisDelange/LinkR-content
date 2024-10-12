# UI - Figure settings
#
# Insert the UI components for configuring the figure settings in this section.

div(
    div(shiny.fluent::Dropdown.shinyInput(
        ns("features_%widget_id%"), label = i18np$t("features"), multiSelect = TRUE,
        options = convert_tibble_to_list(selected_concepts, key_col = "concept_id", text_col = "concept_name")
    ), style = "width: 200px;")
)
