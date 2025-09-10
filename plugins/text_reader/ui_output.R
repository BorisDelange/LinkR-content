# ==========================================
# Text Reader Plugin - Output Display UI
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

# TEXT READER PLUGIN - OUTPUT DISPLAY UI
# 
# This file defines the output display area for the text reader plugin.
# It provides the main viewing area for clinical notes with highlighting
# and formatting capabilities for medical text analysis.
# 
# DISPLAY FEATURES:
# - Clinical note content display with keyword highlighting
# - Raw text vs formatted viewing modes
# - Scrollable interface for long clinical documents
# - Note selection and detailed viewing capabilities
# - Medical text formatting and presentation

div(
    div(
        id = ns("figure_div_%widget_id%"),
        uiOutput(ns("notes_%widget_id%"), style = "height: calc(100% - 15px);"),
        style = "height: 100%;"
    ),
    style = "width: 100%; padding: 5px; box-sizing: border-box; height: 100%; overflow-y: auto;"
)