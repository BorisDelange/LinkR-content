# URL where CSV files are
base_url <- "https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/"

# Files to download
tables <- c(
    "care_site", "condition_era", "condition_occurrence",
    "cost", "death", "device_exposure", "dose_era",
    "drug_era", "drug_exposure", "fact_relationship", "location",
    "measurement", "note", "note_nlp", "observation",
    "observation_period", "payer_plan_period", "person", 
    "procedure_occurrence", "provider", "specimen", 
    "visit_detail", "visit_occurrence"
)

# Local database files
output_dir <- "%dataset_folder%"

# Download files
for (table in tables) {
    
    file_url <- paste0(base_url, table, ".csv")
    parquet_file <- file.path(output_dir, paste0(table, ".parquet"))

    if (!file.exists(parquet_file)) {
    
        # Download and read the CSV file
        csv_file <- file.path(output_dir, paste0(table, ".csv"))
        data <- suppressWarnings(vroom::vroom(file_url, progress = FALSE))
        
        # Correct _id cols, with large positive and negative numeric values
        correct_id_columns <- function(data) {
            id_columns <- colnames(data)[grepl("_id$", colnames(data)) & !grepl("_concept_id$", colnames(data))]
            if (nrow(data) > 0) data %>% dplyr::mutate(dplyr::across(dplyr::all_of(id_columns), ~ if (is.numeric(.)) as.integer(. / 10^12 + 10^4) else .))
            else data
        }
        data <- correct_id_columns(data)
        
        # Write the data to a Parquet file
        arrow::write_parquet(data, parquet_file)
        
        # Erase csv file
        unlink(csv_file)
    }
}

# Import data
import_dataset(
    r, d, dataset_id = %dataset_id%, omop_version = "5.3",
    data_source = "disk", data_folder = "%dataset_folder%",
    read_with = "duckdb", save_as = "parquet", rewrite = FALSE
)
