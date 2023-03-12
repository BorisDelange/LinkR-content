observeEvent(input$submit_%widget_id%, {
    req(o[[session_code]] == session_num)
    output$text_output_%widget_id% <- renderText(paste0(i18np$t("input_text_is"), " : ", isolate(input$text_input_%widget_id%)))
})
