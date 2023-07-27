x_variables <- convert_tibble_to_list(selected_concepts, key_col = "concept_id", text_col = "concept_name")
y_variables <- convert_tibble_to_list(selected_concepts, key_col = "concept_id", text_col = "concept_name")

palettes <- convert_tibble_to_list(data = tibble::tibble(pal = c("Set1", "Set2", "Set3", "Reds", "Purples", "Oranges", "Greens", "Blues", "Greys")), key_col = "pal", text_col = "pal")

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
                onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab_%widget_id%', item.props.id)")),
                shiny.fluent::PivotItem(id = "plot_parameters_%widget_id%", itemKey = "plot_parameters", headerText = i18np$t("plot_parameters")),
                shiny.fluent::PivotItem(id = "variables_%widget_id%", itemKey = "variables", headerText = i18np$t("variables"))
            ),
            conditionalPanel(
                condition = "input.current_tab_%widget_id% == 'plot_parameters_%widget_id%'", ns = ns, br(),
                shiny.fluent::Dropdown.shinyInput(ns("plot_function_%widget_id%"), label = i18np$t("plot_choice"),
                    options = list(
                        list(key = "geom_histogram", text = "geom_histogram"),
                        list(key = "geom_bar", text = "geom_bar")
                    ),
                    value = "geom_histogram"),
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
                    value = "theme_minimal"),
                conditionalPanel(
                    condition = "input.plot_function_%widget_id% == 'geom_histogram'", ns = ns,
                    shiny.fluent::TextField.shinyInput(ns("x_label_%widget_id%"), label = i18np$t("x_label")),
                    shiny.fluent::TextField.shinyInput(ns("y_label_%widget_id%"), label = i18np$t("y_label")),
                    shiny.fluent::Dropdown.shinyInput(ns("stat_%widget_id%"), label = i18np$t("stat"),
                        options = list(
                            list(key = "bin", text = "bin"),
                            list(key = "count", text = "count")
                        ),
                        value = "bin"),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        div(
                            shiny.fluent::Dropdown.shinyInput(ns("bins_type_%widget_id%"), label = i18np$t("bins"),
                                options = list(
                                    list(key = "num_of_bins", text = i18np$t("num_of_bins")),
                                    list(key = "bin_width", text = i18np$t("bin_width"))
                                ),
                                value = "num_of_bins"),
                            style = "width:50%"
                        ),
                        div(
                            conditionalPanel(
                                condition = "input.bins_type_%widget_id% == 'num_of_bins'", ns = ns,
                                shiny.fluent::SpinButton.shinyInput(ns("num_of_bins_%widget_id%"), label = i18np$t("bins_value"), value = 10, step = 5, min = 0, max = 2000)
                            ),
                            conditionalPanel(
                                condition = "input.bins_type_%widget_id% == 'bin_width'", ns = ns,
                                shiny.fluent::SpinButton.shinyInput(ns("bin_width_%widget_id%"), label = i18np$t("bins_value"), value = 1, step = 1, min = 0, max = 100)
                            ),
                            style = "width:50%; margin-top:28px;"
                        )
                    )
                )
            ),
            conditionalPanel(
                condition = "input.current_tab_%widget_id% == 'variables_%widget_id%'", ns = ns, br(),
                shiny.fluent::Dropdown.shinyInput(ns("x_variable_%widget_id%"), options = x_variables, label = i18np$t("x_variable")),
                shiny.fluent::Dropdown.shinyInput(ns("x_colour_pal_%widget_id%"), options = palettes, value = "Set1", label = i18np$t("palette")),
                uiOutput(ns("x_colour_ui_%widget_id%")),
                shiny.fluent::Dropdown.shinyInput(ns("y_variable_%widget_id%"), options = y_variables, label = i18np$t("y_variable")),
                shiny.fluent::Dropdown.shinyInput(ns("y_colour_pal_%widget_id%"), options = palettes, value = "Set1", label = i18np$t("palette")),
                uiOutput(ns("y_colour_ui_%widget_id%")),
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

