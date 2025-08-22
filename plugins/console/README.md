## Introduction

The Console plugin is an advanced code execution environment designed for users with programming experience. **This plugin requires solid knowledge of R programming language and data manipulation techniques**. It provides a powerful interface for executing custom R code directly within the LinkR platform, offering complete flexibility for data analysis and visualization.

*Screenshot of general plugin interface*

## Prerequisites

**Important**: This plugin is intended for data scientists, statisticians, and researchers with:
- Strong R programming skills
- Understanding of data manipulation and visualization libraries
- Experience with healthcare data analysis (preferred)
- Knowledge of the OMOP Common Data Model (recommended)

Users without programming experience should consider using other specialized LinkR plugins that provide no-code interfaces for specific analyses.

## Key Features

### 1. Multi-Language Code Execution

The Console plugin supports R programming language with plans for Python integration:

- **R Language**: Full support with access to all R packages and libraries
- **Comprehensive Output Types**: Execute code and display results in multiple formats
- **Syntax Highlighting**: Advanced code editor with R syntax highlighting and auto-completion
- **Error Handling**: Detailed error messages and debugging information

### 2. Flexible Output Types

Choose from eight different output formats to display your analysis results:

#### Console Output
Execute R code and display text-based results, ideal for data exploration, statistical summaries, and debugging.

*Screenshot of console output with data exploration code*

#### Interactive UI Elements
Create custom HTML interfaces with interactive elements, metrics displays, and formatted content.

*Screenshot of custom UI output with metrics display*

#### Static Plots
Generate publication-ready static visualizations using ggplot2 and other R plotting libraries.

*Screenshot of ggplot2 histogram with custom styling*

#### Data Tables
Display data in clean, formatted tables perfect for presenting summary statistics and results.

*Screenshot of formatted data table output*

#### Interactive Data Tables
Create searchable, sortable, and filterable data tables for exploring large datasets.

*Screenshot of interactive datatable with filtering options*

#### Time Series Visualizations
Build interactive time series charts with zoom, pan, and range selection capabilities using dygraphs.

*Screenshot of dygraphs time series with range selector*

#### Interactive Plotly Charts
Create highly interactive visualizations with hover effects, zooming, and dynamic filtering using plotly.

*Screenshot of plotly scatter plot with hover information*

#### R Markdown Reports
Generate comprehensive reports combining code, text, and visualizations in a single document.

*Screenshot of rendered R Markdown report with mixed content*

### 3. Smart Code Templates

When you select an output type, the plugin automatically provides intelligent code templates:

- **Context-Aware Examples**: Templates tailored to each output format
- **Best Practices**: Code examples following R programming best practices
- **OMOP Integration**: Templates that work with LinkR's OMOP CDM data structure
- **Customizable Starting Points**: Modify templates to suit your specific analysis needs

*Screenshot of code editor with template switching between output types*

## Configuration and Settings

### Programming Language Selection

Currently supports R with comprehensive package support. Python integration is planned for future releases.

### Output Type Configuration

Select from eight output formats based on your analysis requirements:

1. **Console**: Text-based output for data exploration and debugging
2. **UI**: Custom HTML interfaces and interactive elements
3. **Figure**: Static plots and visualizations
4. **Table**: Formatted data tables
5. **DataTable**: Interactive, searchable data tables
6. **Dygraphs**: Interactive time series visualizations
7. **Plotly**: Advanced interactive charts and plots
8. **R Markdown**: Comprehensive reports with mixed content

### Automatic Execution

The "Automatic Updates" toggle controls code execution behavior:

- **Enabled**: Code runs automatically when settings change or data updates
- **Disabled**: Manual execution required via "Display Result" button

*Screenshot of configuration panel with language and output type selection*

## Advanced Features

### Data Access

The Console plugin provides access to your project's OMOP CDM data through LinkR's reactive system:

- **d**: Project OMOP CDM data tables (person, visit_occurrence, condition_occurrence, etc.)
- **Database Access**: Direct database queries through d$con connection

### Code Isolation

When automatic updates are disabled, code execution is wrapped in `isolate()` to prevent unwanted reactive updates, giving you full control over when your analysis runs.

## Configuration Management

### Save and Load Configurations

The plugin supports saving your analysis configurations for reuse:

1. **Create Configuration**: Save current settings including code, language, and output type
2. **Load Configuration**: Quickly switch between different analysis scenarios
3. **Rename and Organize**: Manage your configurations with descriptive names
4. **Team Sharing**: Configurations can be shared between team members with project access

*Screenshot of user configuration management interface*

### Code Persistence

Your code is automatically saved with each configuration, ensuring:
- No loss of work when switching between analyses
- Version control for different analysis approaches
- Easy reproduction of previous results
- Seamless collaboration with team members

## Integration with LinkR Ecosystem

### OMOP CDM Compatibility

The Console plugin is designed to work seamlessly with OMOP Common Data Model:
- **Version Support**: Compatible with both OMOP CDM 5.3 and 5.4
- **Standard Vocabularies**: Access to OMOP concepts and standardized terminologies
- **Healthcare Contexts**: Pre-built examples for common healthcare analyses

## Example Use Cases

### Data Exploration
```r
# Explore patient demographics
d$person %>%
  dplyr::count(gender_concept_id) %>%
  dplyr::arrange(desc(n))
```

### Statistical Analysis
```r
# Calculate length of stay statistics
visit_stats <- d$visit_occurrence %>%
  dplyr::mutate(los = as.numeric(visit_end_date - visit_start_date)) %>%
  dplyr::summarise(
    mean_los = mean(los, na.rm = TRUE),
    median_los = median(los, na.rm = TRUE),
    sd_los = sd(los, na.rm = TRUE)
  )
```

### Custom Visualizations
```r
# Create condition prevalence chart
d$condition_occurrence %>%
  dplyr::inner_join(d$concept, by = c("condition_concept_id" = "concept_id")) %>%
  dplyr::count(concept_name, sort = TRUE) %>%
  dplyr::slice_head(n = 10) %>%
  ggplot2::ggplot(ggplot2::aes(x = reorder(concept_name, n), y = n)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(title = "Top 10 Conditions", x = "Condition", y = "Count")
```

The Console plugin provides unlimited flexibility for healthcare data analysis.

