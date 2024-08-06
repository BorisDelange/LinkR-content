import_dataset(
    r, d, dataset_id = %dataset_id%, omop_version = "5.3",
    data_source = "disk", data_folder = "/Users/borisdelange/Documents/Datasets/mimic-iv-demo",
    read_with = "duckdb", save_as = "parquet", rewrite = FALSE
)
