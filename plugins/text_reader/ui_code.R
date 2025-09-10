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

# TEXT READER PLUGIN - CODE EDITOR UI FILE
# 
# This file defines the code editor interface for the text reader plugin.
# It provides an optional R code editor for advanced clinical note processing,
# custom text analysis, and medical data manipulation beyond the no-code interface.
# 
# TEXT READER ADVANCED FEATURES:
# - Custom R code for complex clinical note queries
# - Advanced text processing and medical concept extraction
# - Integration with OMOP CDM for custom note analysis
# - Medical text mining and keyword extraction algorithms
# - Clinical data preprocessing and transformation
# 
# MEDICAL DATA CODING CAPABILITIES:
# - OMOP CDM note table custom queries
# - Clinical concept vocabulary integration
# - Patient cohort filtering and analysis
# - Medical text preprocessing and cleaning
# - Healthcare data transformation workflows
# 
# EDITOR FEATURES:
# - R syntax highlighting for medical data analysis
# - Keyboard shortcuts for clinical coding workflows
# - Auto-scrolling and responsive layout for long queries
# - Integration with clinical data processing pipeline

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
