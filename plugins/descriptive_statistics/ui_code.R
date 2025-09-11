# ==========================================
# ui_code.R - Code Editor Interface (No Tabs)
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 REQUIRES CUSTOMIZATION - PLUGIN IMPLEMENTATION  🔧                     ██
# ██                                                                            ██
# ██  ACE editors only - tabs are shared with output_settings                  ██
# ██  Each tab has its own editor controlled by output_settings navigation     ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

tagList(
    # ====================
    # IMPORT DATA ACE EDITOR (VISIBLE BY DEFAULT)
    # ====================
    div(
        id = ns("code_import_data_div_%widget_id%"),
        shinyAce::aceEditor(
            ns("code_import_data_%widget_id%"), 
            value = "",
            mode = "r",
            hotkeys = list(
                save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
            ),
            autoScrollEditorIntoView = TRUE,
            height = "100%",
            debounce = 100,
            fontSize = 11,
            showPrintMargin = FALSE
        ),
        style = "height: 100%; display: block;"
    ),
    
    # ====================
    # VISUALIZATION ACE EDITOR
    # ====================
    shinyjs::hidden(
        div(
            id = ns("code_visualization_div_%widget_id%"),
            shinyAce::aceEditor(
                ns("code_visualization_%widget_id%"), 
                value = "",
                mode = "r",
                hotkeys = list(
                    save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                    run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                    comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
                ),
                autoScrollEditorIntoView = TRUE,
                height = "100%",
                debounce = 100,
                fontSize = 11,
                showPrintMargin = FALSE
            ),
            style = "height: 100%; display: block;"
        )
    ),
    
    # ====================
    # STATISTICS ACE EDITOR
    # ====================
    shinyjs::hidden(
        div(
            id = ns("code_statistics_div_%widget_id%"),
            shinyAce::aceEditor(
                ns("code_statistics_%widget_id%"), 
                value = "",
                mode = "r",
                hotkeys = list(
                    save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                    run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                    comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
                ),
                autoScrollEditorIntoView = TRUE,
                height = "100%",
                debounce = 100,
                fontSize = 11,
                showPrintMargin = FALSE
            ),
            style = "height: 100%; display: block;"
        )
    ),
    
    # ====================
    # REPORT ACE EDITOR
    # ====================
    shinyjs::hidden(
        div(
            id = ns("code_report_div_%widget_id%"),
            shinyAce::aceEditor(
                ns("code_report_%widget_id%"), 
                value = "",
                mode = "r",
                hotkeys = list(
                    save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                    run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                    comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
                ),
                autoScrollEditorIntoView = TRUE,
                height = "100%",
                debounce = 100,
                fontSize = 11,
                showPrintMargin = FALSE
            ),
            style = "height: 100%; display: block;"
        )
    )
)
