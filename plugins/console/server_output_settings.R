# ==========================================
# server_output_settings.R - Output Configuration Server Logic
# ==========================================
# 
# Manages output settings including loading/saving from user configurations,
# timeline synchronization, chart type management, and settings persistence
# Smart defaults based on concept selection and saved configurations
#
# ======================================
# CENTRALIZED INPUT DEFINITIONS
# ======================================

all_inputs_%widget_id% <- list(
    list(id = "prog_language", type = "dropdown", default = "r"),
    list(id = "output", type = "dropdown", default = "console"),
    list(id = "auto_update", type = "toggle", default = TRUE)
)

# ======================================
# CONDITIONAL UI DISPLAY LOGIC
# ======================================

# Show/hide plot title input based on output type selection
observe_event(input$output_type_%widget_id%, {
    output_type <- input$output_type_%widget_id%
    
    if (!is.null(output_type)) {
        if (output_type == "histogram") {
            shinyjs::show("plot_title_div_%widget_id%")
        } else {
            shinyjs::hide("plot_title_div_%widget_id%")
        }
    }
})

# ======================================
# VARIABLES CHECK/UNCHECK BUTTONS
# ======================================

observe_event(input$variables_check_all_%widget_id%, {
    all_variables <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
    shiny.fluent::updateDropdown.shinyInput(session, "variables_%widget_id%", value = all_variables)
})

observe_event(input$variables_uncheck_all_%widget_id%, {
    shiny.fluent::updateDropdown.shinyInput(session, "variables_%widget_id%", value = character(0))
})
