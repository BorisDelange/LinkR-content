# Run code
observeEvent(input$display_figure_%widget_id%, {
    %req%
    
    tryCatch({
        # Language = R
        if (input$language_%widget_id% == "r"){
            
            # Output = figure
            if (input$output_%widget_id% == "figure"){
                
                output$figure_output_%widget_id% <- renderPlot(eval(parse(text = input$code_%widget_id%)))
            }
        }
        
        # Language = Python
        else if (input$language_%widget_id% == "python"){
            
        }
        
        # Go to figure tab
        shinyjs::click("figure_%widget_id%")
        
    }, error = function(e){
        show_message_bar(output, "error_displaying_figure", "severeWarning", i18n = i18np, ns = ns)
        cat(paste0("\n", now(), " - widget %widget_id% - input$display_figure - error = ", toString(e)))
    })
})
