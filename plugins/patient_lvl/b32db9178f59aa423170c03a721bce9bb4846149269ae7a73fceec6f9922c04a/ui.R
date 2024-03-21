# -----------------------------
# --- Get saved parameters ----
# -----------------------------

# Get widget options
sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id%", .con = m$db)
widget_options <- DBI::dbGetQuery(m$db, sql)

# Get scripts and selected script in the dropdown
scripts <- widget_options %>% dplyr::filter(name == "script") %>% dplyr::select(id = value_num, name = value)
selected_script <- NA_integer_
selected_script_result <- widget_options %>% dplyr::filter(name == "selected_script")
if (nrow(selected_script_result) > 0) if ((selected_script_result %>% dplyr::pull(value_num)) %in% scripts$id) selected_script <- selected_script_result %>% dplyr::pull(value_num)

# Get concepts associated with this widget
variables <- selected_concepts %>% dplyr::select(concept_id, concept_name) %>% convert_tibble_to_list(key_col = "concept_id", text_col = "concept_name")

# List of inputs (to save & get saved params)
# Add each input name (without the suffix "_widget_id%") in the corresponding list
# Example : dropdowns <- c("variable") for the dropdown with id "variable_%widget_id%"

dropdowns <- c("")
textfields <- c("")
spin_buttons <- c("")
toggle_inputs <- c("")
colour_inputs <- c("")
ace_inputs <- c("")
inputs <- c(dropdowns, textfields, spin_buttons, toggle_inputs, colour_inputs, ace_inputs)

# List of default values
# Add a default value for each input, with the corresponding name without the suffix "_%widget_id%"
# Example : default_values$num_bins <- 50L

default_values <- list()
###
###
### Default values here
###
###

# Get saved params for the selected script
# If there is no saved value for an input, get the default value
inputs_values <- list()

sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id% AND link_id = {selected_script}", .con = m$db)
widget_options <- DBI::dbGetQuery(m$db, sql)

for (input_name in inputs){
    widget_option <- widget_options %>% dplyr::filter(name == input_name)
    
    if (nrow(widget_option) > 0){
        # Get the col value_num for spin buttons, otherwise get the col value 
        if (input_name %in% spin_buttons) inputs_values[[input_name]] <- widget_option$value_num
        else inputs_values[[input_name]] <- widget_option$value
    }
    else inputs_values[[input_name]] <- default_values[[input_name]]
}

if (d$data_person$measurement %>% dplyr::count() %>% dplyr::pull() > 0){
    min_visit_detail_start_datetime <- d$data_person$measurement %>% dplyr::summarize(minimum = min(measurement_datetime, na.rm = TRUE)) %>% dplyr::pull(minimum)
    max_visit_detail_end_datetime <- d$data_person$measurement %>% dplyr::summarize(maximum = max(measurement_datetime, na.rm = TRUE)) %>% dplyr::pull(maximum)
    
    # Get various hospital units
    hospital_units_options <-
        d$visit_detail %>%
        dplyr::count(visit_detail_concept_id, sort = TRUE) %>%
        dplyr::collect() %>%
        dplyr::left_join(
            d$dataset_all_concepts %>% dplyr::select(visit_detail_concept_id = concept_id_1, unit_name = concept_name_1),
            by = "visit_detail_concept_id"
        )
} else {
    min_visit_detail_start_datetime <- ""
    max_visit_detail_end_datetime <- ""
}

# -------------------
# --- Ace editor ----
# -------------------

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

# -----------
# --- UI ----
# -----------

result <- tagList(
    shiny.fluent::reactOutput(ns("delete_confirm_%widget_id%")),
    div(
        id = ns("no_code_and_code_tab_header_%widget_id%"),
        br(),
        div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                div(shiny.fluent::Dropdown.shinyInput(ns("script_choice_%widget_id%"),
                    options = convert_tibble_to_list(scripts, key_col = "id", text_col = "name"), value = selected_script), style = "width:300px"),
                shiny.fluent::DefaultButton.shinyInput(ns("save_%widget_id%"), i18np$t("save"))
            ),
            style = "margin-top:-20px;"
        )
    ), br(),
    div(
        id = ns("no_code_tab_%widget_id%"),
        div(
            style = "display:flex;",
            div(id = ns("split_layout_left_%widget_id%"),
                style = "padding-right:10px; width:50%;",
                div(DT::DTOutput(ns("datatable_%widget_id%")), style = "overflow-x:auto;")
            ),
            div(id = ns("split_layout_right_%widget_id%"),
                style = "margin-left:20px; padding-left:10px; width:50%;",
                shiny.fluent::Pivot(
                    id = ns("no_code_right_pivot_%widget_id%"),
                    onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-no_code_right_pivot_current_tab_%widget_id%', item.props.id)")),
                    shiny.fluent::PivotItem(id = "parameters_tab_%widget_id%", itemKey = "parameters", headerText = i18np$t("parameters")),
                    shiny.fluent::PivotItem(id = "variables_tab_%widget_id%", itemKey = "variables", headerText = i18np$t("variables"))
                ), br(),
                shinyjs::hidden(
                    div(
                        id = ns("parameters_tab_%widget_id%"),
                        shiny.fluent::Stack(
                            horizontal = TRUE, tokens = list(childrenGap = 10),
                            div(
                                div(strong(i18np$t("start_datetime")), style = "margin-bottom:10px;"),
                                shiny.fluent::DatePicker.shinyInput(ns("start_datetime_%widget_id%"), value = min_visit_detail_start_datetime),
                                style = "width:50%;"
                            ),
                            div(
                                div(strong(i18np$t("end_datetime")), style = "margin-bottom:10px;"),
                                shiny.fluent::DatePicker.shinyInput(ns("end_datetime_%widget_id%"), value = max_visit_detail_end_datetime),
                                style = "width:50%;"
                            )
                        ), br(),
                        shiny.fluent::Stack(
                            horizontal = TRUE, tokens = list(childrenGap = 10),
                            div(
                                div(strong(i18np$t("aggregate_function")), style = "margin-bottom:10px;"),
                                shiny.fluent::Dropdown.shinyInput(ns("aggregate_function_%widget_id%"), value = "mean", options = list(
                                    list(key = "mean", text = i18np$t("mean")),
                                    list(key = "min", text = i18np$t("min")),
                                    list(key = "max", text = i18np$t("max"))
                                )),
                                style = "width:50%;"
                            ),
                            div(
                                style = "width:50%;"
                            )
                        ), br(),
                        shiny.fluent::Stack(
                            horizontal = TRUE, tokens = list(childrenGap = 10),
                            div(
                                div(strong(i18np$t("intervals")), style = "margin-bottom:10px;"),
                                shiny.fluent::SpinButton.shinyInput(ns("num_of_intervals_%widget_id%"), value = 5, step = 1, min = 1, max = 30),
                                style = "width:50%;"
                            ),
                            div(
                                style = "width:50%;"
                            )
                        )
                    )
                ),
                div(
                    id = ns("variables_tab_%widget_id%"),
                    div(shiny.fluent::Dropdown.shinyInput(ns("variables_%widget_id%"), options = variables, multiSelect = TRUE), style = "width:300px;")
                ), br(),
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    shiny.fluent::PrimaryButton.shinyInput(ns("execute_%widget_id%"), i18np$t("execute")),
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
            ),
            style = "margin-top:-30px;"
        )
    )
)

result
