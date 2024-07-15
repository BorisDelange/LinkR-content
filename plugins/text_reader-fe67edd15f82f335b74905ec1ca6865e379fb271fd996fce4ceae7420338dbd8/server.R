# -------
# Tabs --
# -------

## All tabs
tabs <- c("figure", "figure_settings", "code", "general_settings")

## Create an observer by tab, show selected tab, hide all others
sapply(tabs, function(tab){

    observeEvent(input[[paste0(tab, "_button_%widget_id%")]], {
        %req%
        if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$", tab))
        
        tryCatch({
            if (tab == "general_settings") shinyjs::hide("figure_settings_code_div_%widget_id%")
            else shinyjs::show("figure_settings_code_div_%widget_id%")
            
            sapply(paste0(setdiff(c("figure_settings", "code", "general_settings"), tab), "_div_%widget_id%"), shinyjs::hide)
            shinyjs::hide("saved_settings_div_%widget_id%")
            shinyjs::show(paste0(tab, "_div_%widget_id%"))
            
            if (tab %in% c("figure_settings", "code")){
                if (length(input$figure_and_settings_side_by_side_%widget_id%) > 0) if (input$figure_and_settings_side_by_side_%widget_id%) shinyjs::show("figure_div_%widget_id%")
                else shinyjs::hide("figure_div_%widget_id%")
                
                shinyjs::show("figure_settings_code_sidenav_%widget_id%")
            }
            else {
                shinyjs::hide("figure_settings_code_sidenav_%widget_id%")
                if (tab != "figure") shinyjs::hide("figure_div_%widget_id%")
            }
            
            # Prevent a bug with scroll into ace editor
            shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
            
        }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
    })
})

# ---------
# Figure --
# ---------

%import_script('server_figure.R')%

# ------------------
# Figure settings --
# ------------------

%import_script('server_figure_settings.R')%

# -------
# Code --
# -------

# -------------------
# General settings --
# -------------------

%import_script('server_general_settings.R')%

# -----------------
# Saved settings --
# -----------------

%import_script('server_saved_settings.R')%
