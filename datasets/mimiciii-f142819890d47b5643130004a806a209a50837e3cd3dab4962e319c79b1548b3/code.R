con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = "localhost",
    port = 5432,
    dbname = "mimic",
    user = "postgres",
    password = "postgres"
)

import_dataset(
    r, d, dataset_id = %dataset_id%, omop_version = "5.3",
    data_source = "db", con = con, load_tables = c("person", "visit_detail", "visit_occurrence", "note", "measurement"),
    read_with = "duckdb", save_as = "parquet", rewrite = FALSE
)
