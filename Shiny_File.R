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
    unlink(name, recursive = TRUE)
    if (fs::is_dir(name)) {
      fs::dir_delete(name)
      }
    cat(file = stderr(), "dir deleted, re-creating...", "\n")
  }else{
    cat(file = stderr(), stringr::str_c("dir does NOT exist, creating...", name), "\n")
  }
  
  fs::dir_create(name, mode="u=rwx,go=rwx", recurse=TRUE)
  
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
excel_to_db <- function(excel_path, table_name, database_path, sheet_names){
  cat(file = stderr(), "Function - excel_to_db", "\n")
 
  for (name in sheet_names) {
    cat(file = stderr(), stringr::str_c("Reading excel: ", name), "\n\n")
    df <- readxl::read_excel(excel_path, sheet = name)
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
    RSQLite::dbWriteTable(conn, name, df, overwrite = TRUE)
    RSQLite::dbDisconnect(conn)
  }
  
  cat(file = stderr(), "Function - excel_to_db...end", "\n")
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
read_table <- function(table_name, params, check_names = TRUE){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path) 
  df <- RSQLite::dbReadTable(conn, table_name, check.names = check_names)
  RSQLite::dbDisconnect(conn)
  return(df)
}

#----------------------------------------------------------------------------------------
read_table_try <- function(table_name, params, check_names = TRUE){
  for (i in 1:10) {
    df <- try(read_table(table_name, params, check_names))
    if (!inherits(df, "try-error")){
      return(df)
    }else{
      cat(file = stderr(), "Read table error...", "\n")
      Sys.sleep(0.5)
    }
  }
  #stop instead of returning NULL so later steps never run on missing data
  stop(stringr::str_c("Could not read table ", table_name, ": ", conditionMessage(attr(df, "condition"))))
}

#----------------------------------------------------------------------------------------
write_table <- function(table_name, df, params){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path) 
  RSQLite::dbWriteTable(conn, table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
}

#----------------------------------------------------------------------------------------
write_table_try <- function(table_name, df, params){
  #SQLite tables hold at most 2000 columns, wider tables (e.g. Q1000 exports with Status columns) use write_wide_table
  if (ncol(df) > 2000) {
    stop(stringr::str_c("Table ", table_name, " has ", ncol(df), " columns, SQLite allows 2000"))
  }
  for (i in 1:10) {
    result <- try(write_table(table_name, df, params))
    if (!inherits(result, "try-error")){
      return(result)
    }else{
      cat(file = stderr(), "Write table error...", "\n")
      Sys.sleep(0.5)
    }
  }
  #stop instead of carrying on, a failed write would leave an old (or no) table for the next step
  stop(stringr::str_c("Could not write table ", table_name, ": ", conditionMessage(attr(result, "condition"))))
}

#----------------------------------------------------------------------------------------
#tables that can be wider than SQLite's 2000 column limit (raw WebIDQ export) are saved as .rds next to the database
#the file is in database/ so it is included in the archive zip
wide_table_path <- function(table_name, params){
  stringr::str_c(sub("\\.db$", "", params$database_path), "_", table_name, ".rds")
}

write_wide_table <- function(table_name, df, params){
  saveRDS(as.data.frame(df), wide_table_path(table_name, params))
}

#check_names = TRUE gives R-mangled column names, the same as read_table_try
read_wide_table <- function(table_name, params, check_names = TRUE){
  file_name <- wide_table_path(table_name, params)
  if (file.exists(file_name)) {
    df <- readRDS(file_name)
    if (check_names) { colnames(df) <- make.names(colnames(df), unique = TRUE) }
    return(df)
  }
  #archives made before the .rds files keep these tables in the database
  return(read_table_try(table_name, params, check_names))
}

#column names of a wide table without loading the data into the caller
wide_table_fields <- function(table_name, params){
  file_name <- wide_table_path(table_name, params)
  if (file.exists(file_name)) { return(colnames(readRDS(file_name))) }
  return(list_fields(table_name, params))
}


#----------------------------------------------------------------------------------------
list_fields <- function(table_name, params){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path) 
  field_list <- RSQLite::dbListFields(conn, table_name)
  RSQLite::dbDisconnect(conn)
  return(field_list)
}

#----------------------------------------------------------------------------------------
#store columns that only contain numbers (or blanks) as numbers so Excel does not show numbers as text
excel_numeric <- function(df, cols = seq_along(df)){
  for (i in cols) {
    x <- df[[i]]
    if (is.character(x)) {
      x_num <- suppressWarnings(as.numeric(x))
      if (all(is.na(x) | trimws(x) == "" | !is.na(x_num))) { df[[i]] <- x_num }
    }
  }
  return(df)
}

#----------------------------------------------------------------------------------------
#restore export header names on sample info columns (R mangles them, e.g. Plate.bar.code) and make analyte columns numeric
excel_format <- function(df, df_analytes, raw_names){
  lookup <- stats::setNames(raw_names, make.names(raw_names))
  info_cols <- !(colnames(df) %in% df_analytes$Name) & colnames(df) %in% names(lookup)
  colnames(df)[info_cols] <- lookup[colnames(df)[info_cols]]
  df <- excel_numeric(df, which(colnames(df) %in% df_analytes$Name))
  return(df)
}

#----------------------------------------------------------------------------------------
#add worksheet with an Excel safe name (31 characters max, unique in the workbook), returns the sheet name
excel_add_sheet <- function(wb, sheetname){
  sheetname <- substr(gsub("[\\[\\]:*?/\\\\]", "_", sheetname, perl = TRUE), 1, 31)
  i <- 1
  base_name <- sheetname
  while (tolower(sheetname) %in% tolower(names(wb))) {
    i <- i + 1
    sheetname <- stringr::str_c(substr(base_name, 1, 31 - nchar(i) - 1), "_", i)
  }
  openxlsx::addWorksheet(wb, sheetname)
  return(sheetname)
}

#----------------------------------------------------------------------------------------
#logical matrix of cells that are numerically 0 ("0", "0.000" or 0)
is_zero <- function(df){
  as.matrix(as.data.frame(lapply(df, function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    !is.na(x_num) & x_num == 0
  })))
}

#----------------------------------------------------------------------------------------
#round halves away from zero like Excel (round() gives 13.7 for 100 * 0.055 / 0.4 because it is stored as 13.7499999...)
round_half_up <- function(x, digits = 0){
  p <- 10^digits
  sign(x) * floor(abs(x) * p + 0.5 + 1e-9) / p
}

#----------------------------------------------------------------------------------------
#TRUE for sample descriptions of the reference type ("SPQC", "NIST" or "Golden West")
#Golden West is matched in any spelling, e.g. "Golden West lot 1", "GoldenWest-LotB1", "golden_west"
is_ref_sample <- function(description, ref_type){
  if (ref_type == "Golden West") {
    grepl("golden[^a-z0-9]*west", description, ignore.case = TRUE)
  } else {
    grepl(ref_type, description, fixed = TRUE)
  }
}

#----------------------------------------------------------------------------------------
#logical matrix of cells that are below LOD before imputation ("< LOD" or 0)
is_below_lod <- function(df){
  as.matrix(as.data.frame(lapply(df, function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    (!is.na(x) & grepl("LOD", x)) | (!is.na(x_num) & x_num == 0)
  })))
}

#----------------------------------------------------------------------------------------
#error message of a failed callr::r_bg step, NULL if it worked
#(the exit status is 0 even when the function stops, the error only shows up in get_result())
bg_error <- function(bg){
  tryCatch({ bg$get_result(); NULL },
           error = function(e) if (!is.null(e$parent)) conditionMessage(e$parent) else conditionMessage(e))
}

#----------------------------------------------------------------------------------------
#barcodes of a plate group, e.g. "A | B | C" -> c("A", "B", "C")
split_barcodes <- function(x){
  trimws(unlist(stringr::str_split(x, stringr::fixed("|"))))
}

#plate group (from params$plates) each Plate.bar.code value belongs to, NA if none or more than one
#Q500 standards and Q1000 kit QC/standards are on single barcodes of a "FIA | LC" or "A | B | C" plate group
plate_group <- function(plate_barcodes, plates){
  groups <- lapply(plates, split_barcodes)
  sapply(plate_barcodes, function(x) {
    hit <- plates[sapply(groups, function(g) all(split_barcodes(x) %in% g))]
    if (length(hit) == 1) hit else NA_character_
  }, USE.NAMES = FALSE)
}

#----------------------------------------------------------------------------------------
#number of analyte description columns at the start of the report tables (config Analytes columns + R_colnames)
#bile acids: FullName, Name, R_colnames; Q500/Q1000: Name, Class, Sub_platform, PCA_Class, R_colnames
report_info_cols <- function(df_report){
  which(colnames(df_report) == "R_colnames")
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
  save(params, file=filename)
  
  filename <- stringr::str_c(params$data_path, input_archive_data_filename)
  files2zip <- dir(stringr::str_c(getwd(),"/database"), full.names = TRUE)
  
  utils::zip(zipfile = filename, files = files2zip, extras = '-j')
  
  cat(file = stderr(), "Function - zip_data_save_bg...end", "\n")
}

#----------------------------------------------------------------------------------------
get_max_rowid <- function(table_name, params) {
  cat(file = stderr(), "Function - get_max_rowid...", "\n")
  
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  query <- stringr::str_c("SELECT max(RowId) FROM ", table_name) 
  max_row <- dbGetQuery(conn, query)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "Function - get_max_rowid...end", "\n")
  return(max_row)
}

#----------------------------------------------------------------------------------------
fix_header <- function(df, df_analytes) {
  cat(file = stderr(), "Function - fix_header...", "\n")
  
  #analytes are the last nrow(df_analytes) columns, in config order (checked by name so headers can never be shifted)
  n_info <- ncol(df) - nrow(df_analytes)
  df_info <- df[, 1:n_info, drop = FALSE]
  df_data <- df[, (n_info + 1):ncol(df), drop = FALSE]

  if (!identical(colnames(df_data), make.names(df_analytes$Name))) {
    stop("fix_header: analyte columns are not the config analytes in config order")
  }
  colnames(df_data) <- df_analytes$Name

  cat(file = stderr(), "Function - fix_header...end", "\n\n")
  return(cbind(df_info, df_data))
}

#----------------------------------------------------------------------------------------
fix_header_filter <- function(df, df_filter, df_analytes) {
  cat(file = stderr(), "Function - fix_header_filter...", "\n")
  
  #the filter only removes analyte columns, so the sample info columns are the same as in the full table
  n_info <- ncol(df) - nrow(df_analytes)
  #sample info from the filtered table itself (rows can differ if the material was processed again)
  df_info <- df_filter[, 1:n_info, drop = FALSE]
  df_filter <- df_filter[, (n_info + 1):ncol(df_filter), drop = FALSE]

  #rename the remaining analyte columns by name (R-mangled -> config name)
  lookup <- stats::setNames(df_analytes$Name, make.names(df_analytes$Name))
  if (!all(colnames(df_filter) %in% names(lookup))) {
    stop("fix_header_filter: filtered table has columns that are not config analytes")
  }
  colnames(df_filter) <- lookup[colnames(df_filter)]

  cat(file = stderr(), "Function - fix_header_filter...end", "\n\n")
  return(cbind(df_info, df_filter))
}
