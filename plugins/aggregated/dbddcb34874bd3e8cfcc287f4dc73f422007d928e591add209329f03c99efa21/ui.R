# Get widget options
sql <- glue::glue_sql("SELECT * FROM aggregated_widgets_options WHERE widget_id = %widget_id%", .con = r$db)
widget_options <- DBI::dbGetQuery(m$db, sql)
plots <- widget_options %>% dplyr::filter(name == "script") %>% dplyr::select(id = value_num, name = value)
selected_script <- NULL
selected_script_result <- widget_options %>% dplyr::filter(name == "selected_script")
if (nrow(selected_script_result) > 0) if ((selected_script_result %>% dplyr::pull(value_num)) %in% plots$id) selected_script <- selected_script_result %>% dplyr::pull(value_num)

# Get concepts associated with this widget
concepts <- tibble::tibble(concept_id = 0L, concept_name = i18np$t("none")) %>% dplyr::bind_rows(selected_concepts %>% dplyr::select(concept_id, concept_name))
x_variables <- convert_tibble_to_list(concepts, key_col = "concept_id", text_col = "concept_name")
y_variables <- convert_tibble_to_list(concepts, key_col = "concept_id", text_col = "concept_name")

# Get palettes from RColorBrewer
palettes <- convert_tibble_to_list(data = tibble::tibble(pal = c("Set1", "Set2", "Set3", "Reds", "Purples", "Oranges", "Greens", "Blues", "Greys")), key_col = "pal", text_col = "pal")

# List of inputs (to save & get saved params)

dropdowns <- c("plot_function", "plot_theme", "bins_type", "x_variable", "y_variable", "colour_pal", "group_by", "group_by_type", "summarize_fct")
textfields <- c("x_label", "y_label")
spin_buttons <- c("num_of_bins", "bin_width", "group_by_num")
toggle_inputs <- c("group_data", "run_code_at_script_launch", "run_plot_at_script_launch")
colour_inputs <- "colour"
ace_inputs <- "code"
inputs <- c(dropdowns, textfields, spin_buttons, toggle_inputs, colour_inputs, ace_inputs)

default_values <- list()
default_values$plot_function <- "geom_histogram"
default_values$plot_theme <- "theme_minimal"
default_values$bins_type <- "num_of_bins"
default_values$x_variable <- 0L
default_values$y_variable <- 0L
default_values$colour_pal <- "Set1"
default_values$group_by <- "datetime"
default_values$group_by_type <- "hours"
default_values$summarize_fct <- "mean"
default_values$x_label <- ""
default_values$y_label <- ""
default_values$num_of_bins <- 50L
default_values$bin_width <- 10L
default_values$group_by_num <- 4L
default_values$group_data <- FALSE
default_values$colour <- "#E41A1C"
default_values$run_code_at_script_launch <- FALSE
default_values$run_plot_at_script_launch <- FALSE
default_values$code <- ""

inputs_values <- list()

# Get saved params for this widget
sql <- glue::glue_sql("SELECT * FROM aggregated_widgets_options WHERE widget_id = %widget_id% AND link_id = {selected_script}", .con = r$db)
widget_options <- DBI::dbGetQuery(m$db, sql)

for (input_name in inputs){
    widget_option <- widget_options %>% dplyr::filter(name == input_name)
    
    if (nrow(widget_option) > 0){
        if (input_name %in% spin_buttons || input_name %in% c("x_variable", "y_variable")) inputs_values[[input_name]] <- widget_option$value_num
        else inputs_values[[input_name]] <- widget_option$value
    }
    else inputs_values[[input_name]] <- default_values[[input_name]]
}

# aceEditor div : show editor if user has access to the console
ace_editor_div <- div(br(), shiny.fluent::MessageBar(i18np$t("unauthorized_access_to_console"), messageBarType = 5), br())
if (length(m$user_accesses) > 0) if ("data_console" %in% m$user_accesses) ace_editor_div <- div(
    div(
        shinyAce::aceEditor(
            ns("code_%widget_id%"), "", mode = "r", value = inputs_values$code,
                code_hotkeys = list(
                    "r", list(
                      run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                      run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                      save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S")
                    )
                ),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
        ), 
    style = "width: 100%;"),
    shiny.fluent::PrimaryButton.shinyInput(ns("run_code_%widget_id%"), i18n$t("run_code")), br()
)

tagList(
    shiny.fluent::reactOutput(ns("delete_confirm_%widget_id%")),
    shiny.fluent::Pivot(
        id = ns("pivot_%widget_id%"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab_%widget_id%', item.props.id)")),
        shiny.fluent::PivotItem(id = "plot_%widget_id%", itemKey = "plot", headerText = i18np$t("plot")),
        shiny.fluent::PivotItem(id = "code_%widget_id%", itemKey = "code", headerText = i18np$t("code")),
        shiny.fluent::PivotItem(id = "scripts_management_%widget_id%", itemKey = "scripts_management", headerText = i18np$t("scripts_management"))
    ),
    conditionalPanel(
        condition = "input.current_tab_%widget_id% == 'plot_%widget_id%' || input.current_tab_%widget_id% == 'code_%widget_id%' || input.current_tab_%widget_id% == null", ns = ns, br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            div(shiny.fluent::Dropdown.shinyInput(ns("script_choice_%widget_id%"),
                options = convert_tibble_to_list(plots, key_col = "id", text_col = "name"), value = selected_script), style = "width:300px"),
            shiny.fluent::DefaultButton.shinyInput(ns("save_%widget_id%"), i18np$t("save")),
            conditionalPanel(
                condition = "input.current_tab_%widget_id% == 'plot_%widget_id%' || input.current_tab_%widget_id% == null", ns = ns,
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    shiny.fluent::Toggle.shinyInput(ns("run_plot_at_script_launch_%widget_id%"), value = FALSE, style = "margin-top:5px;"),
                    div(class = "toggle_title", i18np$t("run_plot_at_script_launch"), style = "padding-top:5px;")
                )
            ),
            conditionalPanel(
                condition = "input.current_tab_%widget_id% == 'code_%widget_id%'", ns = ns,
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    shiny.fluent::Toggle.shinyInput(ns("run_code_at_script_launch_%widget_id%"), value = FALSE, style = "margin-top:5px;"),
                    div(class = "toggle_title", i18np$t("run_code_at_script_launch"), style = "padding-top:5px;")
                )
            )
        )
    ),
    conditionalPanel(
        condition = "input.current_tab_%widget_id% == 'plot_%widget_id%' || input.current_tab_%widget_id% == null", ns = ns,
        div(
            id = ns("plot_tab_%widget_id%"),
            div(
                style = "display:flex;",
                div(id = ns("split_layout_left_%widget_id%"),
                    style = "padding-right:10px; width:50%;",
                    div(
                        id = ns("plot_div_%widget_id%"), br(),
                        plotOutput(ns("plot_output_%widget_id%"))
                    ), br(),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        shiny.fluent::Toggle.shinyInput(ns("hide_params_%widget_id%"), value = FALSE, style = "margin-top:5px;"),
                        div(class = "toggle_title", i18np$t("hide_params"), style = "padding-top:5px;"),
                        div(strong(i18np$t("plot_width")), style = "margin-top:6px;"),
                        div(shiny.fluent::Slider.shinyInput(ns("plot_width_%widget_id%"), value = 100, min = 1, max = 100), style = "width:300px; margin-left:0px; padding-top:4px;")
                    )
                ),
                div(id = ns("split_layout_right_%widget_id%"),
                    style = "padding-left:10px; width:50%;",
                    shiny.fluent::Pivot(
                        id = ns("plot_pivot_%widget_id%"),
                        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-plot_current_tab_%widget_id%', item.props.id)")),
                        shiny.fluent::PivotItem(id = "plot_parameters_%widget_id%", itemKey = "plot_parameters", headerText = i18np$t("plot_parameters")),
                        shiny.fluent::PivotItem(id = "variables_%widget_id%", itemKey = "variables", headerText = i18np$t("variables"))
                    ),
                    conditionalPanel(
                        condition = "input.plot_current_tab_%widget_id% == 'plot_parameters_%widget_id%' || input.plot_current_tab_%widget_id% == null", ns = ns, br(),
                        shiny.fluent::Dropdown.shinyInput(ns("plot_function_%widget_id%"), label = i18np$t("plot_choice"),
                            options = list(
                                list(key = "geom_histogram", text = paste0(i18np$t("histogram"), " (geom_histogram)")),
                                list(key = "geom_point", text = paste0(i18np$t("scatter_plot"), " (geom_point)"))
                            ),
                            value = inputs_values$plot_function),
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
                            value = inputs_values$plot_theme),
                        shiny.fluent::TextField.shinyInput(ns("x_label_%widget_id%"), label = i18np$t("x_label"), value = inputs_values$x_label),
                        shiny.fluent::TextField.shinyInput(ns("y_label_%widget_id%"), label = i18np$t("y_label"), value = inputs_values$y_label),
                        conditionalPanel(
                            condition = "input.plot_function_%widget_id% == 'geom_histogram'", ns = ns,
                            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                                div(
                                    shiny.fluent::Dropdown.shinyInput(ns("bins_type_%widget_id%"), label = i18np$t("bins"),
                                        options = list(
                                            list(key = "num_of_bins", text = i18np$t("num_of_bins")),
                                            list(key = "bin_width", text = i18np$t("bin_width"))
                                        ),
                                        value = inputs_values$bins_type),
                                    style = "width:50%"
                                ),
                                div(
                                    conditionalPanel(
                                        condition = "input.bins_type_%widget_id% == 'num_of_bins'", ns = ns,
                                        shiny.fluent::SpinButton.shinyInput(ns("num_of_bins_%widget_id%"), label = i18np$t("value"), 
                                            value = inputs_values$num_of_bins, step = 5, min = 0, max = 2000)
                                    ),
                                    conditionalPanel(
                                        condition = "input.bins_type_%widget_id% == 'bin_width'", ns = ns,
                                        shiny.fluent::SpinButton.shinyInput(ns("bin_width_%widget_id%"), label = i18np$t("value"), 
                                        value = inputs_values$bin_width, step = 1, min = 0)
                                    ),
                                    style = "width:50%; margin-top:28px;"
                                )
                            )
                        )
                    ),
                    shinyjs::hidden(
                        div(
                            id = ns("variables_div_%widget_id%"), br(),
                            shiny.fluent::Dropdown.shinyInput(ns("x_variable_%widget_id%"), label = i18np$t("x_variable"),
                                options = x_variables, value = inputs_values$x_variable),
                            conditionalPanel(
                                condition = "['geom_point'].includes(input.plot_function_%widget_id%)", ns = ns,
                                shiny.fluent::Dropdown.shinyInput(ns("y_variable_%widget_id%"), label = i18np$t("y_variable"),
                                 options = y_variables, value = inputs_values$y_variable)
                            ),
                            shiny.fluent::Dropdown.shinyInput(ns("colour_pal_%widget_id%"), options = palettes, 
                                value = inputs_values$colour_pal, label = i18np$t("palette")),
                            uiOutput(ns("colour_ui_%widget_id%")),
                            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                                shiny.fluent::Toggle.shinyInput(ns("group_data_%widget_id%"), 
                                    value = inputs_values$group_data, style = "margin-top:5px;"),
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
                                            value = inputs_values$group_by, label = i18np$t("group_by")),
                                        style = "width:33%"
                                    ),
                                    div(
                                        conditionalPanel(
                                            condition = "input.group_by_%widget_id% == 'datetime'", ns = ns,
                                            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                                                div(
                                                    shiny.fluent::SpinButton.shinyInput(ns("group_by_num_%widget_id%"), label = "", 
                                                        value = inputs_values$group_by_num, step = 1, min = 0, max = 2000), 
                                                    style = "width:50%; margin-top:29px;"
                                                ),
                                                div(
                                                    shiny.fluent::Dropdown.shinyInput(ns("group_by_type_%widget_id%"), 
                                                        options = list(
                                                            list(key = "minutes", text = i18np$t("minutes")),
                                                            list(key = "hours", text = i18np$t("hours")),
                                                            list(key = "days", text = i18np$t("days"))
                                                        ), 
                                                        value = inputs_values$group_by_type, label = i18np$t("duration")),
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
                                        value = inputs_values$summarize_fct, label = i18np$t("summarize_fct")
                                    ),
                                    style = "width:33%"
                                )
                            )
                        )
                    ), br(), br(),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        shiny.fluent::PrimaryButton.shinyInput(ns("show_%widget_id%"), i18np$t("show")),
                        shiny.fluent::DefaultButton.shinyInput(ns("generate_code_%widget_id%"), i18np$t("generate_code"))
                    )
                )
            )
        )
    ),
    conditionalPanel(
        condition = "input.current_tab_%widget_id% == 'code_%widget_id%'", ns = ns,
        div(
            id = ns("code_tab_%widget_id%"),
            ace_editor_div
        )
    ),
    conditionalPanel(
        condition = "input.current_tab_%widget_id% == 'scripts_management_%widget_id%'", ns = ns,
        div(
            id = ns("scripts_management_tab_%widget_id%"),
            shiny.fluent::Stack(
                horizontal = TRUE, tokens = list(childrenGap = 10),
                make_textfield(i18n = i18n, ns = ns, label = "name", id = "script_name_%widget_id%", width = "300px"),
                div(shiny.fluent::PrimaryButton.shinyInput(ns("add_script_%widget_id%"), i18n$t("add")), style = "margin-top:38px;")
            ),
            DT::DTOutput(ns("scripts_management_datatable_%widget_id%")),
            shiny.fluent::Stack(
                horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("save_scripts_%widget_id%"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("delete_scripts_%widget_id%"), i18n$t("delete_selection"))
            )
        )
    )
)
