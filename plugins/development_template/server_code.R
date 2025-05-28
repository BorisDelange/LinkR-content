# Server - Code

# Init code var
m$code_%widget_id% <- ""

# Prevent a bug with scroll into ace editor
shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))

# Comment code
observeEvent(input$code_%widget_id%_comment, try_catch("input$code_%widget_id%_comment", {
    
    toggle_comments(
        id = id, input_id = "code_%widget_id%", code = input$code_%widget_id%,
        selection = input$code_%widget_id%_comment$range, session = session
    )
}))

# Run all code with shortcut
observeEvent(input$code_%widget_id%_run_all, try_catch("input$code_%widget_id%_run_all", {
    
    if ("projects_widgets_console" %in% user_accesses){
        m$code_%widget_id% <- input$code_%widget_id%
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
}))

# Run code when button is clicked
observeEvent(input$display_figure_%widget_id%, try_catch("input$display_figure_%widget_id%", {
    
    # If current selected tab is figure settings when run code button is clicked, generate code from these settings
    if (length(input$current_tab_%widget_id%) == 0) current_tab <- "figure_settings"
    else current_tab <- input$current_tab_%widget_id%
    
    if (current_tab == "figure_settings"){
        
        # Code to generate code from figure settings
        
        # ...
        code <- ""
        
        # Update ace editor with generated code
        shinyAce::updateAceEditor(session, "code_%widget_id%", value = code)
        
        m$code_%widget_id% <- code
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
    
    # Check if user has access
    else if ("projects_widgets_console" %in% user_accesses){
        m$code_%widget_id% <- input$code_%widget_id%
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
}))

# Run code at patient update
# observeEvent(m$selected_person, try_catch("m$selected_person", {
    # if (isTRUE(input$run_code_on_data_update_%widget_id%)) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
# }))

# Run code at visit_detail update
# observeEvent(m$selected_visit_detail, try_catch("m$selected_visit_detail", {
    # if (isTRUE(input$run_code_on_data_update_%widget_id%)) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
# }))

# Run code
observeEvent(input$run_code_%widget_id%, try_catch("(input$run_code_%widget_id%", {
        
    # Put here the code to execute when the "Run code" button is clicked
    
    # ...
    
    # Go to figure tab
    if (!input$figure_and_settings_side_by_side_%widget_id%) shinyjs::click("figure_button_%widget_id%")
}))

# Save code with shortcut
observeEvent(input$code_%widget_id%_save, try_catch("input$code_%widget_id%_save", {
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_%widget_id%', Math.random());"))
}))
