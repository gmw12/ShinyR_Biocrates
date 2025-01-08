cat(file = stderr(), "Shiny_File.R", "\n")


#----------------------------------------------------------
Simple_fread <- function(file) {
  data.table::fread(file = file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
} 

#----------------------------------------------
Simple_Excel <- function(df, sheetname, filename) {
  cat(file=stderr(), stringr::str_c("Simple_Excel -> ", filename), "\n")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetname)
  openxlsx::writeData(wb, sheet=1, df)  
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}

#----------------------------------------------
db_table_to_Excel <- function(table_name, sheetname, filename, params) {
  cat(file=stderr(), stringr::str_c("Function db_table_to_Excel -> ", table_name, " to", filename), "\n")
  
  arg_list <- list(table_name, sheetname, filename, params)
  bg_db_table_to_Excel <- callr::r_bg(func = db_table_to_Excel_bg , args = arg_list, stderr = stringr::str_c(params$error_path, "//error_db_table_to_Excel.txt"), supervise = TRUE)
  bg_db_table_to_Excel$wait()
  print_stderr("error_db_table_to_Excel.txt")
  cat(file=stderr(), stringr::str_c("db_table_to_Excel...end"), "\n")
}
#----------------------------------------------
db_table_to_Excel_bg <- function(table_name, sheetname, filename, params) {
  cat(file=stderr(), stringr::str_c("db_table_to_Excel_bg..."), "\n")
  
  source("Shiny_File.R")
  df <- read_table_try(table_name, params)
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetname)
  openxlsx::writeData(wb, sheet=1, df)  
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  cat(file=stderr(), stringr::str_c("db_table_to_Excel_bg...end"), "\n")
}

#----------------------------------------------------------------------------------------
file_set <- function(){
  cat(file = stderr(), "Function file_set", "\n")
  
  #set paths
  params$backup_path <<- create_dir(str_c(params$data_path, "Backup"))
  params$extra_path <<- create_dir(str_c(params$data_path, "Extra"))
  params$qc_path <<- create_dir(str_c(params$data_path, "QC"))
  params$string_path <<- create_dir(str_c(params$data_path, "String"))
  params$phos_path <<- create_dir(str_c(params$data_path, "Phos"))
  params$app_path <<- create_dir(str_c(params$data_path, "Backup/App"))

  param_save_to_database()
  
  #archive code with data
  cat(file = stderr(), "Function file_set... archive R files", "\n")
  r_files <- list.files()
  for (i in 1:length(r_files)) {
    if (grepl(".R$", r_files[i])) {
      file.copy(r_files[i], str_c(params$app_path, r_files[i]))
    }
  }
  
  cat(file = stderr(), "Function file_set...end", "\n\n")
}


#----------------------------------------------------------------------------------------
create_dir <- function(name){
  cat(file = stderr(), "Function create_dir...", "\n")
  if (fs::is_dir(name)) {
    #added file delete, dir delete not working on customer shiny server
    cat(file = stderr(), "dir exists, deleting...", "\n")
    do.call(file.remove, list(list.files(name, full.names = TRUE)))
    fs::dir_delete(name)
    cat(file = stderr(), "dir deleted, re-creating...", "\n")
    fs::dir_create(name, mode="u=rwx,go=rwx", recurse=TRUE)
  }else{
    cat(file = stderr(), stringr::str_c("dir does NOT exist, creating...", name), "\n")
    fs::dir_create(name, mode="u=rwx,go=rwx", recurse=TRUE)
  }
  
  if (fs::is_dir(name)) {
    name <- stringr::str_replace_all(name, "/", "//")
    name <- stringr::str_c(name, "//")
    cat(file = stderr(), stringr::str_c(name, " confirmed created...", "\n"))
  }else{
    cat(file = stderr(), stringr::str_c(name, " NOT created...", "\n"))
  }  
  
  cat(file = stderr(), "Function create_dir...end", "\n\n")
  return(name)
}
#----------------------------------------------------------------------------------------
create_dir_only <- function(name){
  cat(file = stderr(), "Function create_dir_only...", "\n")
  if (fs::is_dir(name)) {
    #added file delete, dir delete not working on customer shiny server
    cat(file = stderr(), "dir exists...", "\n")
  }else{
    dir_create(name)
    name <- str_replace_all(name, "/", "//")
    name <- str_c(name, "//")
    cat(file = stderr(), str_c(name, " created...", "\n"))
  }

  cat(file = stderr(), "Function create_dir_only...end", "\n\n")
  return(name)
}

#--------------------------------------------------------------
excel_to_db <- function(excel_path, table_name, database_path){
  df <- readxl::read_excel(excel_path)
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
  RSQLite::dbWriteTable(conn, table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
}

#----------------------------------------------------------------------------------------
save_data <- function(data_file){
  backup_path <- param_query("backup_path")
  file.copy(data_file, str_c(backup_path, basename(data_file)))
}

#----------------------------------------------------------------------------------------
save_data_bg <- function(file1, dir1){
  file2 <- paste(dir1, basename(file1))
  file.copy(file1, file2)
}

#----------------------------------------------------------------------------------------
param_query <- function(param){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  query_str <- str_c("SELECT ", param, " FROM params")
  df <- RSQLite::dbGetQuery(conn, query_str)
  RSQLite::dbDisconnect(conn)
  return(df[1,1])
}

#----------------------------------------------------------------------------------------
param_update <- function(param, value){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  query_str <<- stringr::str_c("UPDATE params SET ", param, "=", value)
  RSQLite::dbExecute(conn, query_str)
  RSQLite::dbDisconnect(conn)
}

#----------------------------------------------------------------------------------------
param_load_from_database <- function(){
  cat(file = stderr(), "Function - param_load_from_database", "\n")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  params <- RSQLite::dbReadTable(conn, "params")
  RSQLite::dbDisconnect(conn)
  return(params)
}

#----------------------------------------------------------------------------------------
param_save_to_database <- function(){
  cat(file = stderr(), "Function - param_save_to_database", "\n")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  RSQLite::dbWriteTable(conn, "params", params, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
}

#----------------------------------------------------------------------------------------
table_exists <- function(table_name){
  cat(file = stderr(), "Function - table exists...", "\n")
  conn <- dbConnect(RSQLite::SQLite(), params$database_path)
  tables_list <- dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  cat(file = stderr(), stringr::str_c("Function - table exists...end, ", table_name, "  ", table_name %in% tables_list), "\n")
  return(table_name %in% tables_list)
}
#-----------------------------------------------------------------------------------------
filter_db <- function(table_name, column_name, key_word, params) {
  require('RSQLite')
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  query <- stringr::str_c("SELECT * FROM ", table_name, " WHERE ", column_name, " LIKE '", key_word,"'") 
  df <- dbGetQuery(conn, query)
  RSQLite::dbDisconnect(conn)
  return(df)
}
#----------------------------------------------------------------------------------------
up <- function(packed_string){
  return(strsplit(packed_string, ","))
}


#----------------------------------------------------------------------------------------
print_stderr <- function(file_name){
  error_list = readLines(stringr::str_c(params$error_path, "//", file_name))
  for (i in 1:length(error_list)) {
    cat(file = stderr(), error_list[i], "\n")
  }
}

#----------------------------------------------------------------------------------------
print_stderr2 <- function(file_name, params){
  error_list = readLines(stringr::str_c(params$error_path, "//", file_name))
  for (i in 1:length(error_list)) {
    cat(file = stderr(), error_list[i], "\n")
  }
}

#----------------------------------------------------------------------------------------
read_table <- function(table_name, params){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path) 
  df <- RSQLite::dbReadTable(conn, table_name)
  RSQLite::dbDisconnect(conn)
  return(df)
}

#----------------------------------------------------------------------------------------
read_table_try <- function(table_name, params){
  for (i in 1:10) {
    df <- try(read_table(table_name, params))
    if (class(df) != "try-error"){
      i <- 10
      return(df)
    }else{
      cat(file = stderr(), "Read table error...", "\n")
      Sys.sleep(0.5)
    }
  }
}

#----------------------------------------------------------------------------------------
write_table <- function(table_name, df, params){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path) 
  RSQLite::dbWriteTable(conn, table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
}

#----------------------------------------------------------------------------------------
write_table_try <- function(table_name, df, params){
  for (i in 1:10) {
    df <- try(write_table(table_name, df, params))
    if (class(df) != "write-error"){
      i <- 10
      return(df)
    }else{
      cat(file = stderr(), "Write table error...", "\n")
      Sys.sleep(0.5)
    }
  }
}


#----------------------------------------------------------------------------------------
list_tables <- function(params){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path) 
  table_list <- RSQLite::dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  return(table_list)
}

#----------------------------------------------------------------------------------------
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#----------------------------------------------------------------------------------------
zip_data_save <- function(session, input, output, params){
  cat(file = stderr(), "Function - zip_data_save...", "\n")
  source('Shiny_File.R')
  showModal(modalDialog("Saving data to zip file...", footer = NULL))
  
  arg_list <- list(input$archive_data_filename, params)
  archive_data <- callr::r_bg(func = zip_data_save_bg, args = arg_list, stderr = str_c(params$error_path, "//error_archive_data.txt"), supervise = TRUE)
  archive_data$wait()
  print_stderr("error_archive_data.txt")
  
  removeModal()
  cat(file = stderr(), "Function - zip_data_save...end", "\n")
}

#----------------------------------------------------------------------------------------
zip_data_save_bg <- function(input_archive_data_filename, params){
  cat(file = stderr(), "Function - zip_data_save_bg...", "\n")

  filename <- stringr::str_c(params$database_dir, "/params")
  #save(params, file=filename)
  
  filename <- stringr::str_c(params$data_path, input_archive_data_filename)
  files2zip <- dir(stringr::str_c(getwd(),"/database"), full.names = TRUE)
  
  utils::zip(zipfile = filename, files = files2zip, extras = '-j')
  
  cat(file = stderr(), "Function - zip_data_save_bg...end", "\n")
}
