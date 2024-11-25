div(
    shinyjs::hidden(verbatimTextOutput(ns("console_output_%widget_id%"))),
    shinyjs::hidden(uiOutput(ns("ui_output_%widget_id%"))),
    shinyjs::hidden(plotOutput(ns("figure_output_%widget_id%"), height = "calc(100% - 5px)")),
    shinyjs::hidden(tableOutput(ns("table_output_%widget_id%"))),
    shinyjs::hidden(DT::DTOutput(ns("datatable_output_%widget_id%"))),
    shinyjs::hidden(uiOutput(ns("rmarkdown_output_%widget_id%"))),
    style = "height: calc(100% - 5px); width: 100%; margin-top: 5px; "
)
