# URL where CSV files are
base_url <- "https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/"

# Files to download
tables <- c(
    "person", "death", "visit_detail", "visit_occurrence", "observation_period",
    "measurement", "observation", "device_exposure", "procedure_occurrence", "condition_occurrence", "drug_exposure",
    "care_site", "location", "provider", "payer_plan_period", "cost",
    "condition_era", "dose_era",  "drug_era",
    "fact_relationship", "specimen",
    "concept", "concept_relationship", "vocabulary"
)

# Local database files

# Download files
for (table in tables) {
    
    if (table %in% c("concept", "concept_relationship", "vocabulary")) file_url <- paste0(base_url, "2b_", table, ".csv")
    else file_url <- paste0(base_url, table, ".csv")
    local_file <- file.path("%dataset_folder%", paste0(table, ".csv"))

    if (!file.exists(local_file)) {
    
        # Download and read the CSV file
        data <- suppressWarnings(vroom::vroom(file_url, progress = FALSE, show_col_types = FALSE))
        
        # Correct _id cols, with large positive and negative numeric values
        correct_id_columns <- function(data) {
            id_columns <- colnames(data)[grepl("_id$", colnames(data)) & !grepl("_concept_id$", colnames(data))]
            if (nrow(data) > 0) data <- data %>% dplyr::mutate(dplyr::across(dplyr::all_of(id_columns), ~ if (is.numeric(.)) as.integer(. / 10^12 + 10^7) else .))
            
            colnames(data) <- tolower(colnames(data))
            return(data)
        }
        data <- correct_id_columns(data)
        
        # Specific transformation for 'person' table
        if (table == "person") {
            data <- data %>%
                dplyr::mutate(
                    birth_datetime = as.POSIXct(paste0(year_of_birth, "-01-01"), format = "%Y-%m-%d", tz = "UTC")
                )
        }
        
        # Relocate some cols
        if (table == "condition_occurrence") data <- data %>% dplyr::relocate(condition_status_concept_id, .after = "condition_type_concept_id")
        else if (table == "visit_detail") data <- data %>%
            dplyr::relocate(visit_detail_source_value, .after = "care_site_id") %>%
            dplyr::relocate(visit_detail_source_concept_id, .after = "visit_detail_source_value") %>%
            dplyr::relocate(admitting_source_value, .after = "visit_detail_source_concept_id") %>%
            dplyr::relocate(discharge_to_source_value, .after = "admitting_source_concept_id")
        
        # Write the data to a CSV file
        readr::write_csv(data, local_file)
    }
}

# Import data
import_dataset(
    r, m, d, dataset_id = %dataset_id%, omop_version = "5.3",
    data_source = "disk", data_folder = "%dataset_folder%",
    save_as_duckdb_file = TRUE, rewrite = FALSE
)

# Fill empty visit_detail_id cols

update_visit_detail_id <- function(con, table_name, id_column, datetime_column, limit) {
    query <- sprintf("SELECT COUNT() FROM %s WHERE visit_detail_id IS NULL", table_name)
    count <- DBI::dbGetQuery(con, query) %>% dplyr::pull()
    
    if (count > limit) {
        create_temp_table_query <- sprintf("
            CREATE TABLE temp_ranked_%s AS
            WITH ranked_%s AS (
                SELECT
                    d.%s,
                    d.%s,
                    v.visit_detail_id,
                    ROW_NUMBER() OVER (PARTITION BY d.%s ORDER BY d.%s, v.visit_detail_id) AS row_num
                FROM %s d
                LEFT JOIN visit_detail v ON
                    d.person_id = v.person_id AND
                    d.%s >= v.visit_detail_start_datetime AND
                    d.%s <= v.visit_detail_end_datetime
            )
            SELECT %s, visit_detail_id
            FROM ranked_%s
            WHERE row_num = 1
        ", table_name, table_name, id_column, datetime_column, id_column, datetime_column, table_name, datetime_column, datetime_column, id_column, table_name)
        
        DBI::dbExecute(con, create_temp_table_query)

        update_query <- sprintf("
            UPDATE %s
            SET visit_detail_id = temp_ranked_%s.visit_detail_id
            FROM temp_ranked_%s
            WHERE %s.%s = temp_ranked_%s.%s
        ", table_name, table_name, table_name, table_name, id_column, table_name, id_column)
        
        DBI::dbExecute(con, update_query)
        
        drop_temp_table_query <- sprintf("DROP TABLE temp_ranked_%s", table_name)
        DBI::dbExecute(con, drop_temp_table_query)
    }
}

tables_info <- list(
    measurement = c("measurement_id", "measurement_datetime", 20000),
    observation = c("observation_id", "observation_datetime", 1000),
    condition_occurrence = c("condition_occurrence_id", "condition_start_datetime", 1000),
    device_exposure = c("device_exposure_id", "device_exposure_start_datetime", 1),
    drug_exposure = c("drug_exposure_id", "drug_exposure_start_datetime", 300),
    procedure_occurrence = c("procedure_occurrence_id", "procedure_datetime", 3000)
)

for (table in names(tables_info)) {
    id_column <- tables_info[[table]][1]
    datetime_column <- tables_info[[table]][2]
    limit <- tables_info[[table]][3] %>% as.numeric()
    update_visit_detail_id(d$con, table, id_column, datetime_column, limit)
}
