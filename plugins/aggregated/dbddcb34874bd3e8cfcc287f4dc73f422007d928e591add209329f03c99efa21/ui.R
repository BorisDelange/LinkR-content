concepts <- tibble::tibble(concept_id = 0L, concept_name = i18np$t("none")) %>% dplyr::bind_rows(selected_concepts %>% dplyr::select(concept_id, concept_name))
x_variables <- convert_tibble_to_list(concepts, key_col = "concept_id", text_col = "concept_name")
y_variables <- convert_tibble_to_list(concepts, key_col = "concept_id", text_col = "concept_name")

palettes <- convert_tibble_to_list(data = tibble::tibble(pal = c("Set1", "Set2", "Set3", "Reds", "Purples", "Oranges", "Greens", "Blues", "Greys")), key_col = "pal", text_col = "pal")

# List of inputs (to save & get saved params)

dropdowns <- c("plot_function", "plot_theme", "stat", "bins_type", "x_variable", "y_variable", "colour_pal", "group_by", "summarize_fct")
textfields <- c("x_label", "y_label")
spin_buttons <- c("num_of_bins", "bin_width", "group_by_num")
toggle_inputs <- "group_data"
colour_inputs <- "colour"
inputs <- c(dropdowns, textfields, spin_buttons, toggle_inputs, colour_inputs)

inputs_values <- list()

# Get saved params for this widget
sql <- glue::glue_sql("SELECT * FROM aggregated_widgets_options WHERE widget_id = %widget_id%", .con = r$db)
widget_options <- DBI::dbGetQuery(m$db, sql)

for (input_name in inputs){
    widget_option <- widget_options %>% dplyr::filter(name == input_name)
    
    if (nrow(widget_option) > 0){
        if (input_name %in% spin_buttons) inputs_values[[input_name]] <- widget_option$value_num
        else inputs_values[[input_name]] <- widget_option$value
    }
    else {
        if (input_name %in% spin_buttons) inputs_values[[input_name]] <- NA_integer_
        else inputs_values[[input_name]] <- ""
    }
}

splitLayout(
    cellWidths = c("50%", "50%"),
    div(style = "padding-right:10px;",
        br(), br(),
        plotOutput(ns("plot_output_%widget_id%")), style = "border:dashed 1px; margin-top:10px;"
    ),
    div(style = "padding-left:10px;",
        conditionalPanel(
            condition = "input.hide_params_%widget_id% == false || input.hide_params_%widget_id% == null", ns = ns,
            shiny.fluent::Pivot(
                id = ns("pivot_%widget_id%"),
                onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab_%widget_id%', item.props.id)")),
                shiny.fluent::PivotItem(id = "plot_parameters_%widget_id%", itemKey = "plot_parameters", headerText = i18np$t("plot_parameters")),
                shiny.fluent::PivotItem(id = "variables_%widget_id%", itemKey = "variables", headerText = i18np$t("variables"))
            ),
            conditionalPanel(
                condition = "input.current_tab_%widget_id% == 'plot_parameters_%widget_id%' || input.current_tab_%widget_id% == null", ns = ns, br(),
                shiny.fluent::Dropdown.shinyInput(ns("plot_function_%widget_id%"), label = i18np$t("plot_choice"),
                    options = list(
                        list(key = "geom_histogram", text = paste0(i18np$t("histogram"), " (geom_histogram)")),
                        list(key = "geom_point", text = paste0(i18np$t("scatter_plot"), " (geom_point)"))
                    ),
                    value = ifelse(inputs_values$plot_function == "", "geom_histogram", inputs_values$plot_function)),
                shiny.fluent::Dropdown.shinyInput(ns("plot_theme_%widget_id%"), label = i18np$t("theme"),
                    options = list(
                        list(key = "theme_grey", text = "Grey"),
                        list(key = "theme_gray", text = "Gray"),
                        list(key = "theme_bw", text = "Black & white"),
                        list(key = "theme_linedraw", text = "Linedraw"),
                        list(key = "theme_light", text = "Light"),
                        list(key = "theme_dark", text = "Dark"),
                        list(key = "theme_minimal", text = "Minimal"),
                        list(key = "theme_classic", text = "Classic"),
                        list(key = "theme_void", text = "Void"),
                        list(key = "theme_test", text = "Test")
                    ),
                    value = ifelse(inputs_values$plot_theme == "", "theme_minimal", inputs_values$plot_theme)),
                shiny.fluent::TextField.shinyInput(ns("x_label_%widget_id%"), label = i18np$t("x_label"), value = inputs_values$x_label),
                shiny.fluent::TextField.shinyInput(ns("y_label_%widget_id%"), label = i18np$t("y_label"), value = inputs_values$y_label),
                conditionalPanel(
                    condition = "input.plot_function_%widget_id% == 'geom_histogram'", ns = ns,
                    shiny.fluent::Dropdown.shinyInput(ns("stat_%widget_id%"), label = i18np$t("stat"),
                        options = list(
                            list(key = "bin", text = "bin"),
                            list(key = "count", text = "count")
                        ),
                        value = ifelse(inputs_values$stat == "", "bin", inputs_values$stat)),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        div(
                            shiny.fluent::Dropdown.shinyInput(ns("bins_type_%widget_id%"), label = i18np$t("bins"),
                                options = list(
                                    list(key = "num_of_bins", text = i18np$t("num_of_bins")),
                                    list(key = "bin_width", text = i18np$t("bin_width"))
                                ),
                                value = ifelse(inputs_values$bins_type == "", "num_of_bins", inputs_values$bins_type)),
                            style = "width:50%"
                        ),
                        div(
                            conditionalPanel(
                                condition = "input.bins_type_%widget_id% == 'num_of_bins'", ns = ns,
                                shiny.fluent::SpinButton.shinyInput(ns("num_of_bins_%widget_id%"), label = i18np$t("value"), 
                                    value = ifelse(inputs_values$num_of_bins == NA_integer_, 10, inputs_values$num_of_bins), step = 5, min = 0, max = 2000)
                            ),
                            conditionalPanel(
                                condition = "input.bins_type_%widget_id% == 'bin_width'", ns = ns,
                                shiny.fluent::SpinButton.shinyInput(ns("bin_width_%widget_id%"), label = i18np$t("value"), 
                                value = ifelse(inputs_values$bin_width == NA_integer_, 1, inputs_values$bin_width), step = 1, min = 0, max = 100)
                            ),
                            style = "width:50%; margin-top:28px;"
                        )
                    )
                )
            ),
            conditionalPanel(
                condition = "input.current_tab_%widget_id% == 'variables_%widget_id%'", ns = ns, br(),
                shiny.fluent::Dropdown.shinyInput(ns("x_variable_%widget_id%"), label = i18np$t("x_variable"),
                    options = x_variables, value = ifelse(inputs_values$x_variable %in% c("0", ""), 0L, as.integer(inputs_values$x_variable))),
                conditionalPanel(
                    condition = "['geom_point'].includes(input.plot_function_%widget_id%)", ns = ns,
                    shiny.fluent::Dropdown.shinyInput(ns("y_variable_%widget_id%"), label = i18np$t("y_variable"),
                     options = y_variables, value = ifelse(inputs_values$y_variable %in% c("0", ""), 0L, as.integer(inputs_values$y_variable)))
                ),
                shiny.fluent::Dropdown.shinyInput(ns("colour_pal_%widget_id%"), options = palettes, 
                    value = ifelse(inputs_values$colour_pal == "", "Set1", inputs_values$colour_pal), label = i18np$t("palette")),
                uiOutput(ns("colour_ui_%widget_id%")),
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    shiny.fluent::Toggle.shinyInput(ns("group_data_%widget_id%"), 
                        value = ifelse(inputs_values$group_data == "", FALSE, as.logical(inputs_values$group_data)), style = "margin-top:5px;"),
                    div(class = "toggle_title", i18np$t("group_data"), style = "padding-top:5px;")
                ),
                conditionalPanel(
                    condition = "input.group_data_%widget_id% == true", ns = ns,
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        div(
                            shiny.fluent::Dropdown.shinyInput(ns("group_by_%widget_id%"), 
                                options = list(
                                    list(key = "person_id", text = i18np$t("person")),
                                    list(key = "datetime", text = i18np$t("duration"))
                                ), 
                                value = ifelse(inputs_values$group_by == "", "datetime", inputs_values$group_by), label = i18np$t("group_by")),
                            style = "width:33%"
                        ),
                        div(
                            conditionalPanel(
                                condition = "input.group_by_%widget_id% == 'datetime'", ns = ns,
                                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                                    div(
                                        shiny.fluent::SpinButton.shinyInput(ns("group_by_num_%widget_id%"), label = "", 
                                            value = ifelse(inputs_values$group_by_num == NA_integer_, 4, inputs_values$group_by_num), step = 1, min = 0, max = 2000), 
                                        style = "width:50%; margin-top:29px;"
                                    ),
                                    div(
                                        shiny.fluent::Dropdown.shinyInput(ns("group_by_type_%widget_id%"), 
                                            options = list(
                                                list(key = "minutes", text = i18np$t("minutes")),
                                                list(key = "hours", text = i18np$t("hours")),
                                                list(key = "days", text = i18np$t("days"))
                                            ), 
                                            value = ifelse(inputs_values$group_by_type == "", "hours", inputs_values$group_by_type), label = i18np$t("duration")),
                                        style = "width:50%"
                                    )
                                )
                            ),
                            style = "width:67%"
                        )
                    ),
                    div(
                        shiny.fluent::Dropdown.shinyInput(ns("summarize_fct_%widget_id%"), 
                            options = list(
                                list(key = "min", text = i18np$t("min")),
                                list(key = "max", text = i18np$t("max")),
                                list(key = "mean", text = i18np$t("mean"))
                            ), 
                            value = ifelse(inputs_values$summarize_fct == "", "mean", inputs_values$summarize_fct), label = i18np$t("summarize_fct")
                        ),
                        style = "width:33%"
                    )
                )
            ),
        ),
        br(), br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            shiny.fluent::PrimaryButton.shinyInput(ns("show_%widget_id%"), i18np$t("show")),
            shiny.fluent::DefaultButton.shinyInput(ns("save_%widget_id%"), i18np$t("save")), 
            shiny.fluent::Toggle.shinyInput(ns("hide_params_%widget_id%"), value = FALSE, style = "margin-top:5px;"),
            div(class = "toggle_title", i18np$t("hide_params"), style = "padding-top:5px;")
        )
    )
)
