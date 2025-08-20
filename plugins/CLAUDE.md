# CLAUDE.md - LinkR Plugins

This file provides guidance for working with LinkR plugins in this directory.

## Plugin Architecture

LinkR plugins are modular widget components that extend the application's functionality for healthcare data visualization and analysis using the OMOP Common Data Model. Each plugin follows a standardized structure with unique SHA-256 hash identifiers.

### Plugin Structure
```
plugin_unique_id/          # SHA-256 hash directory name (e.g., b205c8c15d31cdbb8e160bea4b2a938c5e3157079efc813965212b31b9cc82e2/)
├── plugin.xml            # Plugin metadata and configuration (required)
├── server.R              # Main server logic controller (required)
├── ui.R                  # Main user interface definition (required)
├── translations.csv      # Internationalization translations (required)
├── README.md             # Plugin documentation (optional)
├── server_*.R            # Additional server modules (optional)
├── ui_*.R                # Additional UI modules (optional)
└── other files           # Additional resources (optional)
```

### Required Files

#### plugin.xml
XML configuration defining:
- Plugin metadata (`unique_id`, `version`, `name_en`, `name_fr`, `type`)
- Author information and creation/update timestamps
- Application version compatibility (`app_version`)
- Short descriptions in English and French

#### server.R
Main server logic controller managing:
- Tab navigation system (output, output_settings, code, user_configurations)
- Layout control (side-by-side vs full-width modes)
- User access permissions validation
- Module imports via `%import_script('filename.R')%` syntax

#### ui.R  
Main user interface providing:
- Auto-hiding navigation bar (appears on hover, hides after 5 seconds)
- Resizable split-panel layout with drag-to-resize functionality
- Access-controlled components based on user permissions
- User configuration dropdown management
- Module imports via `%import_script('filename.R')%` syntax

#### translations.csv
CSV file with three columns:
- `base`: Translation key identifier
- `en`: English text
- `fr`: French text
All user-facing text must use `i18np$t("key_name")` referencing these keys.

### Standard Plugin Components

#### Tab System Architecture
Every plugin implements four standard tabs:

- **output**: Displays execution results (charts, plots, tables, visualizations)
- **output_settings**: No-code interface with graphical controls for configuration
- **code**: R/Python code editor (requires `"projects_widgets_console"` access)
- **user_configurations**: Full-screen configuration management interface

#### Server Module Files
Standard server modules (imported via `%import_script()`):

- **server_code.R**: Code editor logic, execution engine, auto-generation from UI settings
- **server_output_settings.R**: No-code interface logic, input validation, conditional UI display
- **server_user_configurations.R**: Configuration save/load/rename/delete functionality
- **server_layout_manager.R**: Dynamic panel resizing and layout management

#### UI Module Files  
Standard UI modules (imported via `%import_script()`):

- **ui_output.R**: Output display containers (plots, tables, dynamic content)
- **ui_output_settings.R**: No-code configuration interface with dropdowns, toggles, text inputs
- **ui_code.R**: Code editor with syntax highlighting and keyboard shortcuts
- **ui_user_configurations.R**: Configuration management interface with modals

### Widget ID System

Plugins use `%widget_id%` placeholders replaced at runtime:
- Input IDs: `input$element_%widget_id%`
- UI namespacing: `ns("element_%widget_id%")`
- Database queries: `WHERE widget_id = %widget_id%`
- JavaScript functions: `window.function_%widget_id%`

### Programming Languages

Plugins support:
- **R**: Native Shiny integration with reactive programming
- **Python**: Data science capabilities (no shell/SQL execution)

### Data Access

Plugins access healthcare data through LinkR's reactive system:
- `d`: Project OMOP CDM data tables  
- `m`: Plugin and widget-specific data
- Database: `m$db` for widget options and configurations

### Development Patterns

#### Code Generation System
The template demonstrates automatic R code generation from UI settings:
- `generate_output_code_%widget_id%()` function creates R code from UI inputs
- Output types: histogram, table, summary statistics
- Variable selection and plot customization
- Automatic execution when settings change

#### Configuration Management
Built-in user configuration system:
- Save/load different analysis scenarios
- Automatic database persistence via `widgets_options` table
- Validation and duplicate name checking
- Real-time UI updates

#### Access Control
User permission validation:
- Code editor: requires `"projects_widgets_console"` access
- Save functions: require `"projects_widgets_settings"` access
- Conditional UI element display based on permissions

## Development Guidelines

### Template Plugin Reference

The development template (`b205c8c15d31cdbb8e160bea4b2a938c5e3157079efc813965212b31b9cc82e2/`) provides:
- Complete working implementation using iris dataset
- Detailed inline documentation with color-coded sections:
  - 🔧 **REQUIRES CUSTOMIZATION**: Files that must be modified for specific plugins
  - 🔧 **OPTIONAL CUSTOMIZATION**: Files that work out-of-the-box but can be enhanced
  - ⚠️ **DO NOT MODIFY**: Core framework files that work automatically
- Example implementations for all common patterns
- Best practices demonstrations

### Customization Workflow

When creating a new plugin:

1. **Copy template directory** and rename with new unique_id
2. **Customize required files**:
   - `plugin.xml`: Update metadata, names, descriptions
   - `server_code.R`: Implement `generate_output_code_%widget_id%()` function
   - `server_output_settings.R`: Define `all_inputs_%widget_id%` configuration
   - `ui_output.R`: Choose appropriate output containers
   - `ui_output_settings.R`: Create no-code interface controls
   - `translations.csv`: Add plugin-specific translation keys

3. **Optional enhancements**:
   - Additional server/UI modules for complex functionality
   - Custom validation logic
   - Specialized output types

### Reactive Programming Best Practices

**Use `observe_event()` instead of `observeEvent()`**:
- `observe_event()` includes built-in error handling with integrated `tryCatch()`
- No need to manually wrap code in `tryCatch()` blocks
- Provides consistent error management across the plugin framework

### Internationalization

**All user-facing text must use `i18np$t()` system**:
- Use `i18np$t("key_name")` for all text in plugins (note: `i18np$t`, not `i18n$t`)
- Add translation keys to `translations.csv` with English and French translations
- Keep translation files sorted alphabetically by base column
- Maintain consistency with LinkR's language support

### OMOP Integration

Leverage OMOP Common Data Model:
- Access standardized healthcare vocabularies and concept mappings
- Use LinkR's OMOP-specific functions for data queries
- Follow healthcare data privacy and security practices
- Integrate with patient cohort and data subset systems

## OMOP Version Compatibility

LinkR supports both OMOP CDM versions 5.3 and 5.4. The current dataset version is available via `m$omop_version`. Plugins must handle version differences appropriately.

### Version Detection and Column Validation

Use the built-in function to check column availability:

```r
# Check if a column exists in the current OMOP version
if ("birth_datetime" %in% get_omop_col_names(m$omop_version)$person) {
    # Use birth_datetime column
} else {
    # Fallback to year_of_birth
}
```

### Key Differences Between OMOP 5.3 and 5.4

**Tables that differ significantly:**

- **`visit_occurrence`**: Column names changed (e.g., `admitting_source_concept_id` vs `admitted_from_concept_id`)
- **`visit_detail`**: Similar admission/discharge column naming differences (`admitting_source_value` vs `admitted_from_source_value`, `discharge_to_source_value` vs `discharged_to_source_value`)
- **`device_exposure`**: Additional columns in 5.4 (`production_id`, `unit_concept_id`, `unit_source_value`, `unit_source_concept_id`)
- **`location`**: Extended with geography columns in 5.4 (`country_concept_id`, `country_source_value`, `latitude`, `longitude`)
- **`measurement`**, **`observation`**, **`procedure_occurrence`**: Event linking columns added in 5.4 (`measurement_event_id`, `observation_event_id`, etc.)
- **`metadata`**: Additional `metadata_id` and `value_as_number` columns in 5.4
- **`note`**: Event linking columns added in 5.4 (`note_event_id`, `note_event_field_concept_id`)

**Missing tables in some versions:**
- **`attribute_definition`**: Only available in OMOP 5.3

### Best Practices for Version Compatibility

1. **Always validate column existence before using** for the tables and columns mentioned above:
   ```r
   omop_cols <- get_omop_col_names(m$omop_version)
   if ("birth_datetime" %in% omop_cols$person) {
       # Safe to use birth_datetime
   }
   ```

2. **Implement fallback logic** for optional columns:
   ```r
   # Age calculation with version compatibility
   age_calc <- if ("birth_datetime" %in% get_omop_col_names(m$omop_version)$person) {
       "as.numeric(difftime(visit_start_date, birth_datetime, units = 'days')) / 365.25"
   } else {
       "lubridate::year(visit_start_date) - year_of_birth"
   }
   ```

3. **Handle admission/discharge columns** appropriately:
   ```r
   # Visit occurrence admission source column
   admission_col <- if ("admitted_from_concept_id" %in% get_omop_col_names(m$omop_version)$visit_occurrence) {
       "admitted_from_concept_id"  # OMOP 5.4
   } else {
       "admitting_source_concept_id"  # OMOP 5.3
   }
   ```

4. **Document version requirements** in plugin metadata if specific columns are essential.

### Testing Across Versions

When developing plugins, test with both OMOP versions if possible. The `get_omop_col_names()` function provides the authoritative column list for each version, ensuring compatibility without hardcoding version-specific logic.

### Code Quality Standards

- Follow LinkR's reactive programming patterns with `observe_event()`
- Implement proper input validation and user feedback
- Document functions with healthcare context
- Test with sample OMOP datasets
- Maintain accessibility and internationalization

### Database Integration

Plugin data persistence:
- Widget settings stored in `widgets_options` table
- Automatic configuration save/load via template framework
- User-specific configurations linked by `widget_id`
- No additional database setup required

This template-based architecture ensures consistent user experience across all LinkR plugins while providing flexibility for healthcare-specific data analysis needs.