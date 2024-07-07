# -------
# Tabs --
# -------

## All tabs
tabs <- c("figure", "figure_settings", "code", "general_settings")

## Create an observer by tab, show selected tab, hide all others
sapply(tabs, function(tab){

    observeEvent(input[[paste0(tab, "_%widget_id%")]], {
        %req%
        if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$", tab))
        
        tryCatch({
            if (tab == "general_settings") shinyjs::hide("figure_settings_code_div_%widget_id%")
            else shinyjs::show("figure_settings_code_div_%widget_id%")
            
            sapply(paste0(setdiff(tabs, tab), "_div_%widget_id%"), shinyjs::hide)
            shinyjs::show(paste0(tab, "_div_%widget_id%"))
            
            if (tab %in% c("figure_settings", "code")){
                if (length(input$figure_and_settings_side_by_side_%widget_id%) > 0) if (input$figure_and_settings_side_by_side_%widget_id%) shinyjs::show("figure_div_%widget_id%")
                shinyjs::show("figure_settings_code_sidenav_%widget_id%")
            }
            else shinyjs::hide("figure_settings_code_sidenav_%widget_id%")
        }, error = function(e) cat(paste0("\n", now(), " - widget %widget_id% - error = ", toString(e))))
    })
})

# ---------
# Figure --
# ---------

# ------------------
# Figure settings --
# ------------------

%import_script('server_figure_settings.R')%

# -------
# Code --
# -------

%import_script('server_code.R')%

# -----------
# Settings --
# -----------

# Settings / editor side-by-side with figure

observeEvent(input$figure_and_settings_side_by_side_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$figure_and_settings_side_by_side"))
    
    tryCatch({
        if (input$figure_and_settings_side_by_side_%widget_id%){
            shinyjs::runjs(paste0(
                "$('#", id, "-figure_div_%widget_id%').css('width', '50%');",
                "$('#", id, "-figure_settings_div_%widget_id%').css('width', '50%');",
                "$('#", id, "-code_div_%widget_id%').css('width', '50%');"
            ))
            shinyjs::hide("figure_%widget_id%")
        }
        else {
            shinyjs::runjs(paste0(
                "$('#", id, "-figure_div_%widget_id%').css('width', '100%');",
                "$('#", id, "-figure_settings_div_%widget_id%').css('width', '100%');",
                "$('#", id, "-code_div_%widget_id%').css('width', '100%');"
            ))
            shinyjs::show("figure_%widget_id%")
        }
    }, error = function(e) cat(paste0("\n", now(), " - widget %widget_id% - error = ", toString(e))))
})
