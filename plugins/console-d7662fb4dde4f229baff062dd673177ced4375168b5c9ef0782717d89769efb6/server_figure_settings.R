# Change programmation language

output_dropdown_options <- list()
output_dropdown_options$r <- list(
    list(key = "console", text = i18np$t("console")),
    list(key = "ui", text = i18np$t("ui_html")),
    list(key = "figure", text = i18np$t("figure")),
    list(key = "table", text = i18np$t("table")),
    list(key = "datatable", text = i18np$t("datatable")),
    list(key = "rmarkdown", text = i18np$t("rmarkdown"))
)
output_dropdown_options$python <- list(
    list(key = "console", text = i18n$t("console")),
    list(key = "matplotlib", text = i18n$t("matplotlib"))
)

observeEvent(input$prog_language_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$prog_language"))
    
    tryCatch({
    
        # Update ace editor
        shinyAce::updateAceEditor(session, "code_editor_%widget_id%", mode = input$prog_language_%widget_id%)
        
        # Update output dropdown
        shiny.fluent::updateDropdown.shinyInput(session, "output_%widget_id%", options = output_dropdown_options[[input$prog_language_%widget_id%]], value = "console")
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - input$prog_language - error = ", toString(e))))
})

observeEvent(input$output_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$output"))
    
    tryCatch({
    
        # Update ace editor
        if (input$output_%widget_id% == "rmarkdown") mode <- "markdown"
        else mode <- input$prog_language_%widget_id%
        shinyAce::updateAceEditor(session, "code_editor_%widget_id%", mode = mode)
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - input$prog_language - error = ", toString(e))))
})
