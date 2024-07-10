# Outputs
outputs <- list()
outputs$r <- c("console", "figure", "table", "datatable", "rmarkdown")
outputs$python <- c("console", "matplotlib")

# Run code at patient update
observeEvent(m$selected_person, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$selected_person"))
    
    req(length(m$selected_person) > 0)
    req(!is.na(m$selected_person))
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Run code when button is clicked
observeEvent(input$display_figure_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$display_figure"))
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Run code
observeEvent(input$run_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_code"))
    
    req(length(input$prog_language_%widget_id%) > 0)
    
    tryCatch({
        
        language <- input$prog_language_%widget_id%
        code <- input$code_%widget_id%
        
        # Hide all outputs
        sapply(paste0(outputs[[language]], "_output_%widget_id%"), shinyjs::hide)
        shinyjs::show(paste0(input$output_%widget_id%, "_output_%widget_id%"))
    
        # Language = R
        if (language == "r"){
            
            # Output = console
            if (input$output_%widget_id% == "console"){
                captured_output <- capture.output(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w))) %>% paste(collapse = "\\n")
                output$console_output_%widget_id% <- renderText(captured_output)
            }
            
            # Output = figure
            else if (input$output_%widget_id% == "figure"){
                output$figure_output_%widget_id% <- renderPlot(eval(parse(text = code)))
            }
        }
        
        # Language = Python
        else if (language == "python"){
            
        }
        
        # Go to figure tab
        if (!input$figure_and_settings_side_by_side_%widget_id%) shinyjs::click("figure_button_%widget_id%")
        
    }, error = function(e){
        show_message_bar(output, "error_displaying_figure", "severeWarning", i18n = i18np, ns = ns)
        cat(paste0("\\n", now(), " - widget %widget_id% - input$display_figure - error = ", toString(e)))
    })
})
