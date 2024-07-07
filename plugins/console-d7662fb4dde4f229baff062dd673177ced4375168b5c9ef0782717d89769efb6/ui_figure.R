tagList(
    shinyjs::hidden(verbatimTextOutput(ns("console_output_%widget_id%"))),
    shinyjs::hidden(plotOutput(ns("figure_output_%widget_id%"), height = "calc(100% - 5px)"))
)
