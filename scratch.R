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

list_tables(params)

df_raw <- read_table('precursor_raw', params)
