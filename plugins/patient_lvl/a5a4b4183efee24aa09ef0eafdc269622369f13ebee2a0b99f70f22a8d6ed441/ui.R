tagList(
  shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
    shiny.fluent::TextField.shinyInput(ns("text_input_%widget_id%")),
    shiny.fluent::PrimaryButton.shinyInput(ns("submit_%widget_id%"), i18np$t("show"))
  ),
  div(verbatimTextOutput(ns("text_output_%widget_id%")), style = "border:dashed 1px; margin-top:10px;")
)
