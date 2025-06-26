# ==========================================
# ui_code.R - Code Editor Interface
# ==========================================

# R Code Editor with syntax highlighting and keyboard shortcuts
shinyAce::aceEditor(
    ns("code_%widget_id%"), 
    value = "",                    # Initial empty content
    mode = "r",                    # R syntax highlighting
    
    # Keyboard shortcuts configuration
    hotkeys = list(
        save = list(
            win = "CTRL-S", 
            mac = "CTRL-S|CMD-S"
        ),
        run_all = list(
            win = "CTRL-SHIFT-ENTER", 
            mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"
        ),
        comment = list(
            win = "CTRL-SHIFT-C", 
            mac = "CTRL-SHIFT-C|CMD-SHIFT-C"
        )
    ),
    
    # Editor configuration
    autoScrollEditorIntoView = TRUE,  # Auto-scroll to cursor position
    height = "100%",                  # Full height of container
    debounce = 100,                   # 100ms delay before triggering events
    fontSize = 11,                    # Font size for code
    showPrintMargin = FALSE           # Hide print margin line
)

