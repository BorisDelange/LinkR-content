# Update x & y variables dropdowns
x_variables <- convert_tibble_to_list(selected_concepts, key_col = "concept_id", text_col = "concept_name")
shiny.fluent::updateDropdown.shinyInput(session, "x_variable_%widget_id%", options = x_variables)
y_variables <- convert_tibble_to_list(selected_concepts, key_col = "concept_id", text_col = "concept_name")
shiny.fluent::updateDropdown.shinyInput(session, "y_variable_%widget_id%", options = y_variables)

# Render plot
observeEvent(input$show_%widget_id%, {
    %req%
    
    # A x variable must be selected
    req(length(input$x_variable_%widget_id%) > 0)
    
    x_data <- tibble::tibble()
    
    # Get x variable in data
    x_variable <- d$dataset_all_concepts %>% dplyr::filter(concept_id_1 == input$x_variable_%widget_id%)
    req(nrow(x_variable) > 0)
    
    x_variable$domain_id <- tolower(x_variable$domain_id)
    if (x_variable$domain_id %in% c("observation", "measurement")) x_data <- d[[x_variable$domain_id]] %>% 
    dplyr::filter(get(paste0(x_variable$domain_id, "_concept_id")) == x_variable$concept_id_1)
    
    req(nrow(x_data) > 0)
    
    # Render plot
    output$plot_output_%widget_id% <- renderPlot(
        x_data %>% 
            ggplot2::ggplot(ggplot2::aes(x = value_as_number)) +
            ggplot2::geom_histogram() +
            do.call(getFromNamespace(isolate(input$plot_theme_%widget_id%), "ggplot2"), list()) +
            ggplot2::labs(x = isolate(input$x_label_%widget_id%), y = isolate(input$y_label_%widget_id%))
    )
})
