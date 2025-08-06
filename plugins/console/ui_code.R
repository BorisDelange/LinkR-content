# ==========================================
# ui_code.R - Code Editor Interface
# ==========================================

# R Code Editor with syntax highlighting and keyboard shortcuts
shinyAce::aceEditor(
    ns("code_%widget_id%"), 
    value = "",                    # Initial empty content - will be populated by server logic
    mode = "r",                    # R syntax highlighting (change if using different language)
    
    # Keyboard shortcuts configuration
    # These shortcuts integrate with the main widget's save/run functionality
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
    
    # Editor configuration optimized for widget development
    autoScrollEditorIntoView = TRUE,  # Auto-scroll to cursor position for better UX
    height = "100%",                  # Full height to integrate with resizable panels
    debounce = 100,                   # 100ms delay before triggering events (prevents excessive updates)
    fontSize = 11,                    # Compact font size suitable for side panels
    showPrintMargin = FALSE           # Hide print margin line for cleaner interface
)
