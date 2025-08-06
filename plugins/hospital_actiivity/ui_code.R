# ==========================================
# ui_code.R - Code Editor Interface
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 OPTIONAL CUSTOMIZATION - PLUGIN ENHANCEMENT  🔧                        ██
# ██                                                                            ██
# ██  This file provides default functionality that works out-of-the-box.       ██
# ██  Customize only if you need specific features or modifications.            ██
# ██  Safe to use as-is for standard plugin requirements.                       ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# PLUGIN TEMPLATE - CODE EDITOR UI FILE
# 
# This file defines the code editor interface for the widget plugin template.
# It provides a full-featured R code editor with syntax highlighting, keyboard shortcuts,
# and integration with the main widget interface.
# 
# WHEN CREATING A NEW PLUGIN WITH THIS TEMPLATE:
# - The default configuration should work for most R-based widgets
# - Customize keyboard shortcuts if needed for specific plugin requirements
# - Adjust editor settings (font size, theme, etc.) based on plugin needs
# - Consider changing the mode if your plugin uses a different language
# 
# FEATURES:
# - R syntax highlighting and auto-completion
# - Keyboard shortcuts for common operations (save, run, comment)
# - Auto-scrolling and responsive layout
# - Integration with the main UI's resizable panel system
# 
# CUSTOMIZATION OPTIONS:
# - mode: Change from "r" to other languages (javascript, sql, python, etc.)
# - fontSize: Adjust for better readability
# - theme: Can be customized via aceEditor themes
# - hotkeys: Add or modify keyboard shortcuts for plugin-specific actions

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
