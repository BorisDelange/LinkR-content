# UI - Figure settings
#
# Insert the UI components for configuring the figure settings in this section.

div(
    div(
        shiny.fluent::Dropdown.shinyInput(
            ns("data_source_%widget_id%"), options = list(
                list(key = "person", text = i18np$t("patient_data")),
                list(key = "visit_detail", text = i18np$t("stay_data"))
            ), value = "visit_detail", label = i18np$t("data_to_display")
        ),
        style = "width: 200px;"
    ),
    div(
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("features_choice_%widget_id%"), options = list(
                    list(key = "all_features", text = i18np$t("all_drugs")),
                    list(key = "selected_features", text = i18np$t("selected_drugs"))
                ), value = "all_features", label = i18np$t("drugs_to_display")
            ),
            style = "width: 200px;"
        ),
        shinyjs::hidden(
            div(
                id = ns("features_div_%widget_id%"),
                div(
                    shiny.fluent::Dropdown.shinyInput(
                        ns("features_%widget_id%"), label = i18np$t("drugs"),
                        options = convert_tibble_to_list(
                            selected_concepts %>% dplyr::filter(domain_id == "Drug"),
                            key_col = "concept_id", text_col = "concept_name"
                        ),
                        multiSelect = TRUE
                    ),
                    style = "width: 200px;"
                )
            )
        ),
        style = "display: flex; gap: 10px;"
    )
)
