observeEvent(input$display_figure_%widget_id%, {
    %req%
    
    tryCatch({
        
        concept <- selected_concepts %>% dplyr::filter(concept_id == input$variable_%widget_id%)
        data <- tibble::tibble()
        print(concept)
        
        if (concept$domain_id == "Measurement"){
            concept_id <- concept$concept_id
            concept_name <- concept$concept_name
            print(concept_name)
        
            data <-
                d$data_person$measurement %>%
                dplyr::filter(measurement_concept_id == concept_id) %>%
                dplyr::transmute(concept_name = concept_name, value = value_as_number, datetime = measurement_datetime) %>%
                dplyr::collect()
        }
        
        if (nrow(data) > 0){
            pivoted_data <- data %>% tidyr::pivot_wider(names_from = "concept_name", values_from = "value", id_cols = "datetime", values_fn = mean)
            
            pivoted_data_xts <- xts::xts(pivoted_data[,-1], order.by = as.POSIXct(pivoted_data$datetime))
            
            output$dygraph_%widget_id% <- dygraphs::renderDygraph({
                dygraphs::dygraph(pivoted_data_xts) %>%
                  dygraphs::dySeries("Heart rate", label = "Heart Rate")
              })
              
             shinyjs::click("figure_%widget_id%")
        }
    }, error = function(e){
        show_message_bar(output, "error_displaying_figure", "severeWarning", i18n = i18np, ns = ns)
        cat(paste0("\n", now(), " - widget %widget_id% - input$display_figure - error = ", toString(e)))
    })
})
