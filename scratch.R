#clear memory
rm(list = ls())
gc()
.rs.restartR()

params$database_path <- stringr::str_c(getwd(),"/database/test.db")
database_path <- stringr::str_c("/Users/gregwaitt/Data/project_101224.db")

create_db(params$database_path)

create_db <- function(db_name) {
  conn <- dbConnect(RSQLite::SQLite(), db_name) 
  RSQLite::dbDisconnect(conn)
}

read_table <- function(table_name, params){
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  df <- dbReadTable(conn, table_name)
  RSQLite::dbDisconnect(conn)
  return(df)
}

write_table <- function(table_name, df, params){
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  RSQLite::dbWriteTable(conn, table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
}

list_tables <- function(table_name, params){
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  table_list <- dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  return(table_list)
}

filter_db <- function(table_name, column_name, key_word, params) {
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  query <- stringr::str_c("SELECT * FROM ", table_name, " WHERE ", column_name, " LIKE '", accession,"'") 
  df <- dbGetQuery(conn, query)
  RSQLite::dbDisconnect(conn)
  return(df)
}

get_max_rowid <- function(table_name, params) {
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  query <- stringr::str_c("SELECT max(RowId) FROM ", table_name) 
  df <- dbGetQuery(conn, query)
  RSQLite::dbDisconnect(conn)
  return(df)
}

get_max_rowid('Analytes', params)

list_tables(params)

df_params <- read_table('params', params)
df_analytes <- read_table('Analytes', params)
df_qc <- read_table('QC', params)
df_qc_report <- read_table('QC_Report', params)
df_report <- read_table('Report', params)
df_data_raw <- read_table('data_raw', params)
df_data_status <- read_table('data_status', params)
df_info <- read_table('data_info', params)
df_data_start <- read_table('data_start', params)
df_no_ind <- read_table_try("data_no_indicators", params)
df_plasma <- read_table('plasma', params)



#-------------------------------------------------------------------------------------------
#subset df_plasma where Sample.description contains SPQC
df_report <- read_table('QC_Report', params)
df_plasma_spqc <- df_plasma[grep("SPQC", df_plasma$Sample.description),]

