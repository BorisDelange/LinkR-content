# Style

selected_element_style <- paste0("
    display: inline-block;
    color: white;
    
    max-width: 320px;
    border-radius: 8px;
    padding: 1px 5px;
    align-items: center;
    height: 18px;
    font-weight: 600;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
")

selected_word_style <- paste0(selected_element_style, "background-color: #FF8C00;")
selected_words_set_filter_style <- paste0(selected_element_style, "background-color: #FF8C00;")
selected_title_filter_style <- paste0(selected_element_style, "background-color: #17A589;")

##############
# TABS       #
##############

sub_tabs <- c("select_notes", "filters", "words_sets", "layout")

observeEvent(input$current_figure_settings_tab_trigger_%widget_id%, {
    %req%
     if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$current_figure_settings_tab_trigger_%widget_id%"))
    
    tryCatch({
        current_sub_tab <- 
            input$current_figure_settings_tab_%widget_id% %>%
            gsub(paste0(id, "-"), "", .) %>%
            gsub("_%widget_id%", "", .)
        
        sapply(sub_tabs, function(sub_tab) {
            if (current_sub_tab == sub_tab){
                shinyjs::addClass(class = "selected_widget_pivot_item", selector = paste0("#", id, "-", sub_tab, "_%widget_id%"))
                shinyjs::delay(50, shinyjs::show(paste0(sub_tab, "_div_%widget_id%")))
            }
            else {
                shinyjs::removeClass(class = "selected_widget_pivot_item", selector = paste0("#", id, "-", sub_tab, "_%widget_id%"))
                shinyjs::hide(paste0(sub_tab, "_div_%widget_id%"))
            }
        })
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})
