# When script code is executed

observeEvent(input$script_code_%widget_id%_run_selection, {
    %req%
    if(!shinyAce::is.empty(input$script_code_%widget_id%_run_selection$selection)) m$script_final_code_%widget_id% <- input$script_code_%widget_id%_run_selection$selection
    else m$script_final_code_%widget_id% <- input$script_code_%widget_id%_run_selection$line
    
    m$script_code_trigger_%widget_id% <- Sys.time()
})

observeEvent(input$script_code_%widget_id%_run_all, {
    %req%
    m$script_final_code_%widget_id% <- input$script_code_%widget_id%
    m$script_code_trigger_%widget_id% <- Sys.time()
})

observeEvent(input$run_code_%widget_id%, {
    %req%
    m$script_final_code_%widget_id% <- input$script_code_%widget_id%
    m$script_code_trigger_%widget_id% <- Sys.time()
})

observeEvent(m$script_code_trigger_%widget_id%, {
    %req%
    
    divs <- c("r_script_result_div_%widget_id%", "rmarkdown_script_result_div_%widget_id%", "plot_script_result_div_%widget_id%")
    
    code <- m$script_final_code_%widget_id%
    
    # R code
    if (input$script_type_%widget_id% == "r"){
        captured_output <- capture.output(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
        
        shinyjs::show("r_script_result_div_%widget_id%")
        sapply(c("rmarkdown_script_result_div_%widget_id%", "plot_script_result_div_%widget_id%"), shinyjs::hide)
        
        output$r_script_result_%widget_id% <- renderText(paste(captured_output, collapse = "\n"))
    }
    
    # RMarkdown code
    if (input$script_type_%widget_id% == "rmarkdown"){
    
        shinyjs::show("rmarkdown_script_result_div_%widget_id%")
        sapply(c("r_script_result_div_%widget_id%", "plot_script_result_div_%widget_id%"), shinyjs::hide)
    }
    
    # Plot code
    if (input$script_type_%widget_id% == "plot"){
        
        shinyjs::show("plot_script_result_div_%widget_id%")
        sapply(c("r_script_result_div_%widget_id%", "rmarkdown_script_result_div_%widget_id%")
        
        output$plot_script_result_%widget_id% <- renderPlot(eval(parse(text = paste(code, collapse = "\n"))))
    }
})
