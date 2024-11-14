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
    )
)
