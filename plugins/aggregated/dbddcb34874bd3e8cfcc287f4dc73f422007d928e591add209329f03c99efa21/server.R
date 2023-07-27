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
    output$plot_output_%widget_id% <- renderPlot({
    
        figure <- ggplot2::ggplot()
    
        if (isolate(input$plot_function_%widget_id%) == "geom_histogram"){
            figure <- x_data %>% ggplot2::ggplot(ggplot2::aes(x = value_as_number))
            
            if (isolate(input$bins_type_%widget_id%) == "num_of_bins") figure <- figure + 
                ggplot2::geom_histogram(bins = isolate(input$num_of_bins_%widget_id%), fill = isolate(input$x_colour_%widget_id%), color = "#FFFFFF")
            else if (isolate(input$bins_type_%widget_id%) == "bin_width") figure <- figure + 
                ggplot2::geom_histogram(binwidth = isolate(input$bin_width_%widget_id%), fill = isolate(input$x_colour_%widget_id%), color = "#FFFFFF")
                
            figure <- figure +
                do.call(getFromNamespace(isolate(input$plot_theme_%widget_id%), "ggplot2"), list()) +
                ggplot2::labs(x = isolate(input$x_label_%widget_id%), y = isolate(input$y_label_%widget_id%))
        }
        
        figure
    })
})

# Change SwatchColorPicker

observeEvent(input$x_colour_pal_%widget_id%, {

    pal <- RColorBrewer::brewer.pal(n = 8, name = input$x_colour_pal_%widget_id%)
    pal_tibble <- tibble::tibble(name = pal)
    colorCells <- list()
    for (i in 1:nrow(pal_tibble)) colorCells <- rlist::list.append(colorCells, list(id = pal_tibble[[i, "name"]], color = pal_tibble[[i, "name"]]))
    
    output$x_colour_ui_%widget_id% <- renderUI(
        shiny.fluent::SwatchColorPicker.shinyInput(ns("x_colour_%widget_id%"), colorCells = colorCells, columnCount = length(colorCells), value = pal[1])
    )
})

observeEvent(input$y_colour_pal_%widget_id%, {

    pal <- RColorBrewer::brewer.pal(n = 8, name = input$y_colour_pal_%widget_id%)
    pal_tibble <- tibble::tibble(name = pal)
    colorCells <- list()
    for (i in 1:nrow(pal_tibble)) colorCells <- rlist::list.append(colorCells, list(id = pal_tibble[[i, "name"]], color = pal_tibble[[i, "name"]]))
    
    output$y_colour_ui_%widget_id% <- renderUI(
        shiny.fluent::SwatchColorPicker.shinyInput(ns("y_colour_%widget_id%"), colorCells = colorCells, columnCount = length(colorCells), value = pal[1])
    )
})
