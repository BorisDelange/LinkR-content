# Change language
observeEvent(input$language_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$language"))
    
    tryCatch({
    
        # Update ace editor
        shinyAce::updateAceEditor(session, "code_editor_%widget_id%", mode = input$language_%widget_id%)
        
        # Update output dropdown
        if (input$language_%widget_id% == "r") dropdown_options <- list(
            list(key = "console", text = i18np$t("console")),
            list(key = "figure", text = i18np$t("figure")),
            list(key = "table", text = i18np$t("table")),
            list(key = "datatable", text = i18np$t("datatable")),
            list(key = "rmarkdown", text = i18np$t("rmarkdown"))
        )
        else if (input$language_%widget_id% == "python") dropdown_options <- list(
            list(key = "console", text = i18n$t("console")),
            list(key = "matplotlib", text = i18n$t("matplotlib"))
        )
        
        shiny.fluent::updateDropdown.shinyInput(session, "output_%widget_id%", options = dropdown_options, value = "console")
        
    }, error = function(e) cat(paste0("\n", now(), " - widget %widget_id% - input$language - error = ", toString(e))))
})
