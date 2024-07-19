# Init code var
m$code_%widget_id% <- ""

# Outputs
outputs <- list()
outputs$r <- c("console", "figure", "table", "datatable", "rmarkdown")
outputs$python <- c("console", "matplotlib")

# Prevent a bug with scroll into ace editor
shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))

# Run code at patient update
observeEvent(m$selected_person, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$selected_person"))
    
    req(length(m$selected_person) > 0)
    req(!is.na(m$selected_person))
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Comment code
observeEvent(input$code_%widget_id%_comment, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$code_comment"))

    tryCatch({
        lines <- strsplit(input$code_%widget_id%, "\\n")[[1]]
        req(length(lines) > 0)
        
        start_row <- input$code_%widget_id%_comment$range$start$row + 1
        end_row <- input$code_%widget_id%_comment$range$end$row + 1
        
        for (i in start_row:end_row) if (startsWith(lines[i], "# ")) lines[i] <- substr(lines[i], 3, nchar(lines[i])) else lines[i] <- paste0("# ", lines[i])
        
        shinyAce::updateAceEditor(session, "code_%widget_id%", value = paste0(lines, collapse = "\\n"))
        
        shinyjs::runjs(sprintf("
            var editor = ace.edit('%s-rode');
            editor.moveCursorTo(%d, %d);
            editor.focus();
        ", id, input$code_%widget_id%_comment$range$end$row, input$code_%widget_id%_comment$range$end$column))
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# Run all code with shortcut
observeEvent(input$code_%widget_id%_run_all, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$code_run_all"))

    m$code_%widget_id% <- input$code_%widget_id%
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Run code when button is clicked
observeEvent(input$display_figure_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$display_figure"))
    
    m$code_%widget_id% <- input$code_%widget_id%
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Run code
observeEvent(input$run_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_code"))
    
    req(length(input$prog_language_%widget_id%) > 0)
    
    tryCatch({
        
        language <- input$prog_language_%widget_id%
        code_output <- input$output_%widget_id%
        code <- m$code_%widget_id%
        
        # Hide all outputs
        sapply(paste0(outputs[[language]], "_output_%widget_id%"), shinyjs::hide)
        shinyjs::show(paste0(code_output, "_output_%widget_id%"))
    
        # Language = R
        if (language == "r"){
            
            # Output = console
            if (code_output == "console"){
                captured_output <- capture.output(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w))) %>% paste(collapse = "\\n")
                output$console_output_%widget_id% <- renderText(captured_output)
            }
            
            # Output = UI
            else if (code_output == "ui") output$ui_output_%widget_id% <- renderUI(eval(parse(text = code)))
            
            # Output = figure
            else if (code_output == "figure") output$figure_output_%widget_id% <- renderPlot(eval(parse(text = code)))
            
            # Output = table
            else if (code_output == "table") output$table_output_%widget_id% <- renderTable(eval(parse(text = code)))
            
            # Output = DataTable
            else if (code_output == "datatable") output$datatable_output_%widget_id% <- DT::renderDT(
                DT::datatable(
                    eval(parse(text = code)),
                    
                    rownames = FALSE,
                    options = list(
                        dom = "<'datatable_length'l><'top't><'bottom'p>",
                        compact = TRUE, hover = TRUE
                    ),
                    
                    # CSS for datatable
                    callback = htmlwidgets::JS(
                      "table.on('draw.dt', function() {",
                      "  $('.dataTable tbody tr td').css({",
                      "    'height': '12px',",
                      "    'padding': '2px 5px'",
                      "  });",
                      "  $('.dataTable thead tr td div .form-control').css('font-size', '12px');",
                      "  $('.dataTable thead tr td').css('padding', '5px');",
                      "});"
                    )
                )
            )
            
            # Output = RMarkdown
            else if (code_output == "rmarkdown"){
                
                # Create temp dir
                dir <- paste0(m$app_folder, "/temp_files/", m$user_id, "/markdowns")
                output_file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_", now() %>% stringr::str_replace_all(":", "_") %>% stringr::str_replace_all(" ", "_"), ".Md")
                if (!dir.exists(dir)) dir.create(dir)
                  
                # Create the markdown file
                knitr::knit(text = code, output = output_file, quiet = TRUE)
          
                output$rmarkdown_output_%widget_id% <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
            }
        }
        
        # Language = Python
        else if (language == "python"){
            
            if (code_output == "console") output$console_output_%widget_id% <- renderText(capture_python_output(code))
            
            # else if (code_output == "matplotlib")
        }
        
        # Go to figure tab
        if (!input$figure_and_settings_side_by_side_%widget_id%) shinyjs::click("figure_button_%widget_id%")
        
    }, error = function(e){
        show_message_bar(output, "error_displaying_figure", "severeWarning", i18n = i18np, ns = ns)
        cat(paste0("\\n", now(), " - widget %widget_id% - input$display_figure - error = ", toString(e)))
    })
})

# Save code with shortcut
observeEvent(input$code_%widget_id%_save, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$code_save"))
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_%widget_id%', Math.random());"))
})
