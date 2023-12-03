# Get widget options
sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id%", .con = m$db)
widget_options <- DBI::dbGetQuery(m$db, sql)
plots <- widget_options %>% dplyr::filter(name == "script") %>% dplyr::select(id = value_num, name = value)
selected_script <- NULL
selected_script_result <- widget_options %>% dplyr::filter(name == "selected_script")
if (nrow(selected_script_result) > 0) if ((selected_script_result %>% dplyr::pull(value_num)) %in% plots$id) selected_script <- selected_script_result %>% dplyr::pull(value_num)

# Get concepts associated with this widget
concepts <- tibble::tibble(concept_id = 0L, concept_name = i18np$t("none")) %>% dplyr::bind_rows(selected_concepts %>% dplyr::select(concept_id, concept_name))
variables <- convert_tibble_to_list(concepts, key_col = "concept_id", text_col = "concept_name")

# Get palettes from RColorBrewer
palettes <- convert_tibble_to_list(data = tibble::tibble(pal = c("Set1", "Set2", "Set3", "Reds", "Purples", "Oranges", "Greens", "Blues", "Greys")), key_col = "pal", text_col = "pal")

# List of inputs (to save & get saved params)

dropdowns <- paste(unlist(sapply(c("variable", "colour_pal"), function(x) paste0(x, "_", 1:10))))
textfields <- paste(unlist(sapply("variable_name", function(x) paste0(x, "_", 1:10))))
spin_buttons <- c("y_min", "y_max")
toggle_inputs <- c("show_stays", "stay_data_only", "show_range_selector", "synchronize_timelines", "smooth_curves", "draw_points", "change_y_values")
colour_inputs <- paste(unlist(sapply("colour", function(x) paste0(x, "_", 1:10))))
ace_inputs <- "code"
inputs <- c(dropdowns, textfields, spin_buttons, toggle_inputs, colour_inputs, ace_inputs)

default_values <- list()
default_values$run_code_at_script_launch <- FALSE
default_values$run_plot_at_script_launch <- FALSE
default_values$y_min <- NULL
default_values$y_max <- NULL
default_values$show_stays <- FALSE
default_values$stay_data_only <- FALSE
default_values$show_range_selector <- TRUE
default_values$synchronize_timelines <- FALSE
default_values$smooth_curves <- FALSE
default_values$draw_points <- TRUE
default_values$change_y_values <- FALSE
default_values$code <- ""

for (i in 1:10){
    default_values[[paste0("variable_", i)]] <- 0L
    default_values[[paste0("variable_name_", i)]] <- ""
    default_values[[paste0("colour_", i)]] <- "#E41A1C"
    default_values[[paste0("colour_pal_", i)]] <- "Set1"
}

inputs_values <- list()

# Get saved params for this widget
sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id% AND link_id = {selected_script}", .con = m$db)
widget_options <- DBI::dbGetQuery(m$db, sql)

for (input_name in inputs){
    widget_option <- widget_options %>% dplyr::filter(name == input_name)
    
    if (nrow(widget_option) > 0){
        if (input_name %in% spin_buttons || grepl("variable_[0-9]", input_name)) inputs_values[[input_name]] <- widget_option$value_num
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

# Variables div

for (i in 1:10){

    variables_div_temp <- div(
        id = ns(paste0("variable_div_", i)),
        shiny.fluent::Dropdown.shinyInput(ns(paste0("variable_", i, "_%widget_id%")), label = i18np$t("variable"), options = variables, value = inputs_values[[paste0("variable_", i)]]),
        shiny.fluent::TextField.shinyInput(ns(paste0("variable_name_", i, "_%widget_id%")), label = i18np$t("display_name"), value = inputs_values[[paste0("variable_name_", i)]]),
        shiny.fluent::Dropdown.shinyInput(ns(paste0("colour_pal_", i, "_%widget_id%")), options = palettes,  value = inputs_values[[paste0("colour_pal_", i)]], label = i18np$t("palette")),
        uiOutput(ns(paste0("colour_ui_", i, "_%widget_id%")))
    )
    
    if (i == 1) variables_div <- variables_div_temp
    else variables_div <- tagList(variables_div, shinyjs::hidden(variables_div_temp))
}

variables_div <- div(
    shiny.fluent::Dropdown.shinyInput(ns("variable_num_%widget_id%"), label = i18np$t("variable_num"), 
    options = convert_tibble_to_list(tibble::tibble(num = 1:10), key_col = "num", text_col = "num"), value = 1L),
    variables_div
)

result <- tagList(
    shiny.fluent::reactOutput(ns("delete_confirm_%widget_id%")),
    shiny.fluent::Pivot(
        id = ns("pivot_%widget_id%"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab_%widget_id%', item.props.id)")),
        shiny.fluent::PivotItem(id = "plot_tab_%widget_id%", itemKey = "plot", headerText = i18np$t("plot")),
        shiny.fluent::PivotItem(id = "code_tab_%widget_id%", itemKey = "code", headerText = i18np$t("code")),
        shiny.fluent::PivotItem(id = "scripts_management_tab_%widget_id%", itemKey = "scripts_management", headerText = i18np$t("scripts_management"))
    ),
    div(
        id = ns("plot_and_code_tab_header_%widget_id%"),
        br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            div(shiny.fluent::Dropdown.shinyInput(ns("script_choice_%widget_id%"),
                options = convert_tibble_to_list(plots, key_col = "id", text_col = "name"), value = selected_script), style = "width:300px"),
            shiny.fluent::DefaultButton.shinyInput(ns("save_%widget_id%"), i18np$t("save")),
            div(
                id = ns("plot_tab_header_%widget_id%"),
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    shiny.fluent::Toggle.shinyInput(ns("run_plot_at_script_launch_%widget_id%"), value = as.logical(inputs_values$run_plot_at_script_launch), style = "margin-top:5px;"),
                    div(class = "toggle_title", i18np$t("run_plot_at_script_launch"), style = "padding-top:5px;")
                )
            ),
            shinyjs::hidden(
                div(
                    id = ns("code_tab_header_%widget_id%"),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        shiny.fluent::Toggle.shinyInput(ns("run_code_at_script_launch_%widget_id%"), value = as.logical(inputs_values$run_code_at_script_launch), style = "margin-top:5px;"),
                        div(class = "toggle_title", i18np$t("run_code_at_script_launch"), style = "padding-top:5px;")
                    )
                )
            )
        )
    ),
    div(
        id = ns("plot_tab_%widget_id%"),
        div(
            style = "display:flex;",
            div(id = ns("split_layout_left_%widget_id%"),
                style = "padding-right:10px; width:50%;",
                div(
                    id = ns("dygraph_div_%widget_id%"), br(),
                    dygraphs::dygraphOutput(ns("dygraph_%widget_id%")),
                    shinyjs::hidden(div(id = ns("empty_dygraph_%widget_id%"), style = "height:400px; background-color:#EBEBEB;"))
                ), br(),
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    shiny.fluent::Toggle.shinyInput(ns("hide_params_%widget_id%"), value = FALSE, style = "margin-top:5px;"),
                    div(class = "toggle_title", i18np$t("hide_params"), style = "padding-top:5px;"),
                    div(strong(i18np$t("dygraph_width")), style = "margin-top:6px;"),
                    div(shiny.fluent::Slider.shinyInput(ns("dygraph_width_%widget_id%"), value = 100, min = 1, max = 100), style = "width:300px; margin-left:0px; padding-top:4px;")
                )
            ),
            div(id = ns("split_layout_right_%widget_id%"),
                style = "padding-left:10px; width:50%;",
                shiny.fluent::Pivot(
                    id = ns("plot_pivot_%widget_id%"),
                    onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-plot_current_tab_%widget_id%', item.props.id)")),
                    shiny.fluent::PivotItem(id = "plot_parameters_tab_%widget_id%", itemKey = "plot_parameters", headerText = i18np$t("plot_parameters")),
                    shiny.fluent::PivotItem(id = "variables_tab_%widget_id%", itemKey = "variables", headerText = i18np$t("variables"))
                ),
                div(
                    id = ns("plot_parameters_tab_%widget_id%"), br(),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        shiny.fluent::Toggle.shinyInput(ns("show_stays_%widget_id%"), value = as.logical(inputs_values$show_stays), style = "margin-top:5px;"),
                        div(class = "toggle_title", i18np$t("show_stays"), style = "padding-top:5px;")
                    ),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        shiny.fluent::Toggle.shinyInput(ns("stay_data_only_%widget_id%"), value = as.logical(inputs_values$stay_data_only), style = "margin-top:5px;"),
                        div(class = "toggle_title", i18np$t("stay_data_only"), style = "padding-top:5px;")
                    ), br(),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        shiny.fluent::Toggle.shinyInput(ns("show_range_selector_%widget_id%"), value = as.logical(inputs_values$show_range_selector), style = "margin-top:5px;"),
                        div(class = "toggle_title", i18np$t("show_range_selector"), style = "padding-top:5px;")
                    ), 
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        shiny.fluent::Toggle.shinyInput(ns("synchronize_timelines_%widget_id%"), value = as.logical(inputs_values$synchronize_timelines), style = "margin-top:5px;"),
                        div(class = "toggle_title", i18np$t("synchronize_timelines"), style = "padding-top:5px;")
                    ), br(),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        shiny.fluent::Toggle.shinyInput(ns("smooth_curves_%widget_id%"), value = as.logical(inputs_values$smooth_curves), style = "margin-top:5px;"),
                        div(class = "toggle_title", i18np$t("smooth_curves"), style = "padding-top:5px;")
                    ),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        shiny.fluent::Toggle.shinyInput(ns("draw_points_%widget_id%"), value = as.logical(inputs_values$draw_points), style = "margin-top:5px;"),
                        div(class = "toggle_title", i18np$t("draw_points"), style = "padding-top:5px;")
                    ), br(),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        shiny.fluent::Toggle.shinyInput(ns("change_y_values_%widget_id%"), value = as.logical(inputs_values$change_y_values), style = "margin-top:5px;"),
                        div(class = "toggle_title", i18np$t("change_y_values"), style = "padding-top:5px;")
                    ),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        div(
                            div(class = "input_title", i18np$t("y_min")),
                            shiny.fluent::SpinButton.shinyInput(ns("y_min_%widget_id%"), value = inputs_values$y_min, step = 1),
                            style = "width:250px"
                        ),
                        div(
                            div(class = "input_title", i18np$t("y_max")),
                            shiny.fluent::SpinButton.shinyInput(ns("y_max_%widget_id%"), value = inputs_values$y_max, step = 1),
                            style = "width:250px"
                        )
                    )
                ),
                shinyjs::hidden(
                    div(
                        id = ns("variables_tab_%widget_id%"),
                        variables_div
                    )
                ), br(), br(),
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    shiny.fluent::PrimaryButton.shinyInput(ns("show_%widget_id%"), i18np$t("show")),
                    shiny.fluent::DefaultButton.shinyInput(ns("generate_code_%widget_id%"), i18np$t("generate_code"))
                )
            )
        )
    ),
    shinyjs::hidden(
        div(
            id = ns("code_tab_%widget_id%"),
            ace_editor_div
        )
    ),
    shinyjs::hidden(
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

if (!requireNamespace("dygraphs", quietly = TRUE) | !requireNamespace("xts", quietly = TRUE)) result <- shiny.fluent::MessageBar(i18np$t("dygraphs_and_xts_not_installed"), messageBarType = 5)

result
