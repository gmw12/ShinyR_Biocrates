cat(file = stderr(), "Shiny_Setup.R", "\n")

#---------------------------------------------------------------------
create_parameter_table <- function(session, input, output){
  cat(file = stderr(), "\n",  "Function create_parameter_table", "\n")

  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  RSQLite::dbWriteTable(conn, "params", df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)

  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), "Function create_parameter_table...end", "\n")
  
  return(df)
}


#---------------------------------------------------------------------
load_parameters <- function(session, input, output){
  
  cat(file = stderr(), "Function load_parameter_table...", "\n")
  
  updateCheckboxInput(session, "primary_group", value = params$primary_group)
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), "Function load_parameter_table...end", "\n")
  
}


#---------------------------------------------------------------------
load_config_file <- function(session, input, output){
  cat(file = stderr(), "Function load_config_file...", "\n")
  showModal(modalDialog("Loading config file...", footer = NULL))
  
  params$file_prefix <<- input$file_prefix
  
  config_sbf <- parseFilePaths(volumes, input$sfb_config_file)
  params$config_path <<- str_extract(config_sbf$datapath, "^/.*/")
  params$config_file <<- config_sbf$datapath
  
  # set root data dir
  volumes <<- c(dd = params$config_path, volumes)
  cat(file = stderr(), stringr::str_c("Adding default data directory --> "), "\n")
  cat(file = stderr(), stringr::str_c(volumes), "\n")
  
  #Global set of data and database paths
  params$data_source <<- "unkown"
  params$data_path <<- create_dir(str_c(params$config_path, input$file_prefix, "/"))
  database_dir <<- create_dir(str_c(getwd(), "/database/"))
  params$database_path <<- str_c(database_dir, input$file_prefix, ".db")

  #create working directory for 
 #create_dir(params$data_path)
  #create_dir(database_dir)
  params$error_path <<- create_dir(str_c(params$data_path, "Error"))
  params$plot_path <<- create_dir(str_c(params$data_path, "Plots"))
  #create_dir(params$error_path)
  
  cat(file = stderr(), str_c("loading config file from ", params$config_path), "\n")
  
  bg_design <- callr::r_bg(excel_to_db, args = list(config_sbf$datapath, "config", params$database_path, list('Analytes', 'QC')), stderr = str_c(params$error_path, "//error_config.txt"), supervise = TRUE)
  bg_design$wait()
  print_stderr("error_config.txt")
  
  
  #set type of Biocrates file
  analyte_count <- get_max_rowid('Analytes', params)[[1]]
  if (analyte_count == 20) {
    params$data_source <<- "BileAcid"
  } else {
    params$data_source <<- "Q500"
  }
  params$analyte_count <<- analyte_count
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  
  #save paramater table to database
  write_table_try("params", params, params)
  
  cat(file = stderr(), "Function load_config_file...end", "\n")
  removeModal()
}


#------------------------

check_comp_name_length <- function(){
  cat(file = stderr(), "Function check_comp_name_length...", "\n")
  names <- unique(dpmsr_set$design$Group)
  names_len <- sort(nchar(names), decreasing = TRUE)
  if (length(names) > 1) {
    longest_comp <- names_len[1] + names_len[2]
  }else{
    longest_comp <- names_len[1] + names_len[1]
  }
  if (longest_comp > 27) {
    return(TRUE)
  }else {
    return(FALSE)
  }
}


