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
concepts <- tibble::tibble(concept_id = 0L, concept_name = i18np$t("none")) %>% dplyr::bind_rows(selected_concepts %>% dplyr::select(concept_id, concept_name))
variables <- convert_tibble_to_list(concepts, key_col = "concept_id", text_col = "concept_name")

# List of inputs (to save & get saved params)
# Add each input name (without the suffix "_widget_id%") in the corresponding list
# Example : dropdowns <- c("variable") for the dropdown with id "variable_%widget_id%"

dropdowns <- c("variable")
textfields <- c("")
spin_buttons <- c("")
toggle_inputs <- c("run_plot_at_script_load", "run_code_at_script_load")
colour_inputs <- c("")
ace_inputs <- c("code")
inputs <- c(dropdowns, textfields, spin_buttons, toggle_inputs, colour_inputs, ace_inputs)

# List of default values
# Add a default value for each input, with the corresponding name without the suffix "_%widget_id%"
# Example : default_values$num_bins <- 50L

default_values <- list()
default_values$variable <- 0L
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
                options = convert_tibble_to_list(scripts, key_col = "id", text_col = "name"), value = selected_script), style = "width:300px"),
            shiny.fluent::DefaultButton.shinyInput(ns("save_%widget_id%"), i18np$t("save")),
            div(
                id = ns("plot_tab_header_%widget_id%"),
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    shiny.fluent::Toggle.shinyInput(ns("run_plot_at_script_load_%widget_id%"), value = as.logical(inputs_values$run_plot_at_patient_changeover), style = "margin-top:5px;"),
                    div(class = "toggle_title", i18np$t("run_plot_at_script_load"), style = "padding-top:5px;")
                )
            ),
            shinyjs::hidden(
                div(
                    id = ns("code_tab_header_%widget_id%"),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        shiny.fluent::Toggle.shinyInput(ns("run_code_at_script_load_%widget_id%"), value = as.logical(inputs_values$run_code_at_patient_changeover), style = "margin-top:5px;"),
                        div(class = "toggle_title", i18np$t("run_code_at_script_load"), style = "padding-top:5px;")
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
                    id = ns("plot_div_%widget_id%"), br(),
                    ###
                    ###
                    ### Output here (replace div with id "plot_%widget_id%")
                    ###
                    ###
                    div(
                        id = ns("plot_%widget_id%"),
                        style = "width:100%; height:400px; border:solid 2px #EFEEEE;"
                    )
                ), br(),
                div(
                    id = ns("plot_size_div_%widget_id%"),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        div(
                            div(strong(i18np$t("plot_width")), style = "margin-top:6px;"),
                            div(shiny.fluent::Slider.shinyInput(ns("plot_width_%widget_id%"), value = 100, min = 1, max = 100), style = "width:200px; margin-left:-8px; padding-top:4px;"),
                            style = "width:200px"
                        ),
                        div(
                            div(strong(i18np$t("plot_height")), style = "margin-top:6px;"),
                            div(shiny.fluent::Slider.shinyInput(ns("plot_height_%widget_id%"), value = 100, min = 1, max = 100), style = "width:200px; margin-left:-8px; padding-top:4px;"),
                            style = "width:200px"
                        ),
                        div(
                            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                                shiny.fluent::Toggle.shinyInput(ns("hide_params_%widget_id%"), value = FALSE, style = "margin-top:5px;"),
                                div(class = "toggle_title", i18np$t("hide_params"), style = "padding-top:5px;")
                            ),
                            style = "width:300px; margin-top:15px;"
                        )
                    )
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
                    ###
                    ###
                    ### Plot parameters here
                    ###
                    ###
                ),
                div(
                    id = ns("variables_tab_%widget_id%"),
                    shiny.fluent::Dropdown.shinyInput(ns("variable_%widget_id%"), label = i18np$t("variable"), options = variables, value = inputs_values$variable),
                ), br(),
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

result
