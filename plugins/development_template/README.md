# LinkR Plugin Development Template
A comprehensive template for creating LinkR healthcare data visualization and analysis plugins.

## Plugin Types
Before starting, determine your plugin type:
- **Individual data plugins**: Work patient by patient
- **Aggregated data plugins**: Work on groups of patients  
- **Mixed plugins**: Can handle both types

Most visualization plugins work with aggregated data, while detailed patient analysis typically uses individual data.

## Quick Start
1. **Navigate to Plugins > Create Plugin > From Existing Plugin** and select this template
2. **Choose your plugin type** (individual, aggregated, or both)
3. **Modify the 5 required files** (marked with REQUIRES CUSTOMIZATION)
4. **Test your plugin** using the Test tab with sample concepts
5. **Document and share** your plugin with the community

## File Structure & Customization Guide

### 🔧 MUST CUSTOMIZE (Required for all plugins)
| File | Purpose | What to Modify |
|------|---------|----------------|
| `server_code.R` | Code generation logic | `generate_output_code_%widget_id%()` function |
| `server_output_settings.R` | Configuration logic | `all_inputs_%widget_id%` list matching your UI |
| `ui_output.R` | Output display | Choose appropriate output type (plot, table, etc.) |
| `ui_output_settings.R` | Settings interface | Add your plugin's configuration controls |
| `translations.csv` | Internationalization | Add your plugin-specific text translations |

### 🔧 OPTIONAL CUSTOMIZATION (Enhance if needed)
| File | Purpose | When to Modify |
|------|---------|----------------|
| `server.R` | Main controller | Add custom tab logic or navigation |
| `ui.R` | Main interface | Modify layout or add custom components |
| `ui_code.R` | Code editor | Change language mode or shortcuts |

### ⚠️ DO NOT MODIFY (Core framework files)
| File | Purpose |
|------|---------|
| `server_layout_manager.R` | Panel resizing system |
| `server_user_configurations.R` | Save/load configurations |
| `ui_user_configurations.R` | Configuration management UI |

## Development Workflow

### Step 1: Define Your UI Controls
Use Shiny Fluent components for consistency with LinkR's interface:
```r
# ui_output_settings.R
div(
    strong(i18np$t("my_parameter")), 
    style="margin-bottom:10px;"
),
div(
    shiny.fluent::Dropdown.shinyInput(
        ns("my_parameter_%widget_id%"),
        options = list(...)
    ),
    style="width:300px;"
)
```

### Step 2: Register Inputs for Persistence
```r
# server_output_settings.R
all_inputs_%widget_id% <- list(
    list(id = "my_parameter", type = "dropdown", default = "value")
)
```

### Step 3: Implement Code Generation
```r
# server_code.R
generate_output_code_%widget_id% <- function(...) {
    # Generate R/Python code based on UI settings
    # This code will be executed when the widget runs
}
```

## Working with OMOP Data

### Understanding selected_concepts
When users create a widget, they select concepts that become available in `selected_concepts`:
- `concept_id`: Standard (from Athena) or non-standard (>2B) concept ID
- `concept_name`: Display name of the concept  
- `domain_id`: OMOP domain (often corresponds to table name)
- `vocabulary_id`: Terminology source (LOINC, SNOMED, etc.)

### Accessing OMOP Tables
Data is available through the `d$` object:
- `d$person` - Demographics
- `d$measurement` - Lab results, vital signs
- `d$condition_occurrence` - Diagnoses
- `d$drug_exposure` - Medications
- `d$procedure_occurrence` - Procedures
- `d$observation` - Other clinical facts

### Example: Filtering by Selected Concepts
```r
# Filter measurement data for selected concepts
data <- d$measurement %>%
    dplyr::filter(measurement_concept_id %in% selected_concepts$concept_id)
```

## Common Plugin Patterns

### Data Visualization Plugin
```r
# ui_output.R - Use plot output
plotOutput(ns("plot_%widget_id%"))

# server_code.R - Generate ggplot2 code
plot <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = value_as_number)) +
    ggplot2::geom_histogram(bins = input$num_bins_%widget_id%) +
    ggplot2::theme_minimal()
```

### Statistical Analysis Plugin
```r
# ui_output.R - Use table output
DT::DTOutput(ns("results_%widget_id%"))

# server_code.R - Generate analysis results
results <- data %>%
    dplyr::group_by(concept_name) %>%
    dplyr::summarise(
        mean_value = mean(value_as_number, na.rm = TRUE),
        std_dev = sd(value_as_number, na.rm = TRUE)
    )
```

### OMOP Query Plugin
```r
# Generate complex OMOP queries
query_code <- paste0(
    "data <- d$measurement %>%\n",
    "  dplyr::filter(measurement_concept_id == ", selected_concept_id, ") %>%\n",
    "  dplyr::inner_join(d$person, by = 'person_id') %>%\n",
    "  dplyr::filter(gender_concept_id == ", gender_filter, ")"
)
```

## Internationalization
All user-facing text must be translatable:

### In Code
```r
# Use i18np$t() for all text (not i18n$t())
strong(i18np$t("concept_to_show"))
```

### In translations.csv
```csv
base,en,fr
concept_to_show,Concept to show,Concept à afficher
num_bins,Number of bins,Nombre de barres
no_data_available,No data available,Pas de données disponibles
```

## Testing Your Plugin

### Using the Test Tab
1. Click "Select concepts" to choose test data
2. Select relevant concepts from OMOP terminologies
3. Click "Execute plugin" to run with test data
4. Check the console output for debugging information

### Testing with Different OMOP Versions
```r
# Handle version differences
omop_cols <- get_omop_col_names("measurement")
if ("measurement_datetime" %in% omop_cols) {
    # OMOP 5.4 logic
} else {
    # OMOP 5.3 logic  
}
```

## Best Practices

### Code Structure
1. **Always use `%widget_id%` suffix** for all element IDs to ensure uniqueness
2. **Wrap IDs in `ns()`** for proper Shiny module scoping
3. **Use `observe_event()` instead of `observeEvent()`** for better error handling
4. **Follow the reactive pattern** with clear separation of UI and server logic

### Error Handling
```r
observe_event(input$show_plot_%widget_id%, {
    tryCatch({
        # Your plot generation code
        if (nrow(data) == 0) {
            # Handle empty data case
            plot <- create_empty_plot()
        }
    }, error = function(e) {
        # Error will be logged automatically
        plot <- create_error_plot(e$message)
    })
})
```

### Performance
- Filter data early to reduce processing time
- Use `dplyr` for efficient data manipulation  
- Consider data size when designing visualizations
- Implement loading indicators for long-running operations

## Advanced Features

### Custom Validation
```r
# Validate domain compatibility
valid_domains <- c("Measurement", "Observation")
selected_concept <- selected_concepts %>%
    dplyr::filter(concept_id == input$concept_%widget_id%)

if (!selected_concept$domain_id %in% valid_domains) {
    # Show error message
    return()
}
```

### Dynamic UI Updates
```r
# Update dropdown options based on other selections
shinyjs::delay(500, {
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "concept_%widget_id%",
        options = concept_options,
        value = default_value
    )
})
```

## Common Pitfalls to Avoid

### Technical Issues
- Forgetting the `%widget_id%` suffix on element IDs
- Using `conditionalPanel()` instead of `shinyjs::show/hide`
- Modifying core framework files (layout manager, etc.)
- Hardcoding text instead of using translations
- Assuming specific OMOP version without checking

### Design Issues  
- Creating overly complex interfaces
- Not handling empty data gracefully
- Ignoring mobile/responsive design
- Poor error messaging for users

### Data Issues
- Not validating concept domains before processing
- Assuming data types without checking
- Not handling NULL/missing values
- Processing too much data without pagination

## Sharing Your Plugin

### Documentation Requirements
1. **Plugin Summary**: Brief description of functionality
2. **Detailed Description**: Markdown documentation with:
   - Purpose and use cases
   - Required concept types/domains  
   - Configuration options
   - Example outputs
   - Known limitations

### Publishing Process
1. Complete plugin information in the Summary tab
2. Test thoroughly with different data scenarios
3. Add comprehensive documentation
4. Use the Share tab to publish to the community repository
5. Follow LinkR's contribution guidelines

## Additional Resources
- <a href="https://linkr.interhop.org/en/docs/create_plugin/" target="_blank">LinkR Official Documentation</a>
- <a href="https://ohdsi.github.io/CommonDataModel/" target="_blank">OMOP CDM Reference</a>
- <a href="https://appsilon.github.io/shiny.fluent/" target="_blank">Shiny Fluent Components</a>
- <a href="https://mastering-shiny.org/" target="_blank">Mastering Shiny Book</a> - Essential for understanding reactive programming

## Getting Help
- <a href="https://linkr.interhop.org/en/" target="_blank">LinkR Website</a>
- Plugin development examples in the repository
- OMOP/OHDSI community resources
- Shiny community documentation and examples

