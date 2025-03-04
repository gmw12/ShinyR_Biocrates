cat(file = stderr(), "Shiny_Data.R", "\n")


#---------------------------------------------------------------------
load_config_file <- function(session, input, output){
  cat(file = stderr(), "Function load_config_file...", "\n")
  
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
  params$backup_path <<- create_dir(str_c(params$data_path, "Backup"))
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
  
  cat(file = stderr(), "Function load_config_file...end", "\n\n")
}



#---------------------------------------------------------------------
load_data_file <- function(session, input, output, params){
  cat(file = stderr(), "Function load_data_file", "\n")
  showModal(modalDialog("Loading data...", footer = NULL))
  
  
  data_sfb <- parseFilePaths(volumes, input$sfb_data_file)
  data_path <- str_extract(data_sfb$datapath, "^/.*/")
  params$data_file <- basename(data_sfb$datapath)
  params$simple_plate <- input$simple_plate
  
  save_data(data_sfb$datapath)
  
  cat(file = stderr(), str_c("loading data file(s) from ", data_path[1]), "\n")
  
  bg_load_data <- callr::r_bg(func = load_data_bg, args = list(data_sfb, params), stderr = str_c(params$error_path, "//error_load_data.txt"), supervise = TRUE)
  bg_load_data$wait()
  print_stderr("error_load_data.txt")
  
  infinity_error <- bg_load_data$get_result()
  
  if (infinity_error) {
    shinyalert("Oops!", "Data contains infinity values", type = "error")
  }
  
  #parameters are written to db during r_bg (process cannot write to params directly)
  params <<- read_table_try("params", params)
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), "Function load_data_file...end", "\n\n")
  removeModal()
}


#----------------------------------------------------------------------------------------
load_data_bg <- function(data_sfb, params){
  cat(file = stderr(), "Function load_data_bg...", "\n")
  source('Shiny_File.R')
  source('Shiny_Misc_Functions.R')
  
  df <- data.table::fread(file = data_sfb$datapath, header = TRUE, skip=1, stringsAsFactors = FALSE, sep = "\t", fill=TRUE)
  #save(df, file="z1")    #  load(file="z1") 
  
  #search for "infinity" in df report infinity_error = TRUE if found
  if (any(grepl("infinity", df, ignore.case = TRUE))){
    infinity_error <- TRUE
  }else{
    infinity_error <- FALSE  
  }
  
  #remove trailing and leading whitespace from df$Material
  df$Material <- trimws(df$Material)
  
  #clean Material column
  df$Material <- gsub(" ", "_", df$Material)
  
  #clean special characters
  df <- clean_dataframe(df)
  
  write_table_try("data_raw", df, params)
  
  write_table_try("params", params, params)
  #save(params, file="params")
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), "function load_unkown_data_bg...end", "\n\n")
  
  return(infinity_error)
}
#---------------------------------------------------------------------

remove_status_cols <- function(session, input, output, params){
  cat(file = stderr(), "Function remove_status_cols...", "\n")
  
  bg_status_col <- callr::r_bg(remove_status_cols_bg, args = list(params), stderr = str_c(params$error_path, "//error_removestatuscols.txt"), supervise = TRUE)
  bg_status_col$wait()
  print_stderr("error_removestatuscols.txt")

  cat(file = stderr(), "Function remove_status_cols...end", "\n\n")
  
}


#---------------------------------------------------------------------

remove_status_cols_bg <- function(params){
  cat(file = stderr(), "Function remove_status_cols_bg...", "\n")
  source('Shiny_File.R')
  
  df <- read_table_try("data_raw", params)
  
  #remove columns that contain the word Status from df
  df <- df |> dplyr::select(-contains("Status"))
  
  #from df column Plate.note keep only characters to the first .
  df$Plate.note <- gsub("\\..*", "", df$Plate.note)

  write_table_try("data_status", df, params)
  
  cat(file = stderr(), "Function remove_status_cols_bg...end", "\n\n")
  
}

#---------------------------------------------------------------------

remove_indicators <- function(session, input, output, params){
  cat(file = stderr(), "Function remove_indicators...", "\n")
  
  bg_indicators <- callr::r_bg(remove_indicators_bg, args = list(params), stderr = str_c(params$error_path, "//error_removeindicators.txt"), supervise = TRUE)
  bg_indicators$wait()
  print_stderr("error_removeindicators.txt")
  
  cat(file = stderr(), "Function remove_indicators...end", "\n\n")
  
}


#---------------------------------------------------------------------

remove_indicators_bg <- function(params){
  cat(file = stderr(), "Function remove_indicators_bg...", "\n")
  source('Shiny_File.R')
  
  df <- read_table_try("data_status", params)
  df_analytes <- read_table_try("analytes", params)  

  #find colnumber for df_analytes$Abbreviation[1]
  col_num <- which(colnames(df) == df_analytes$Name[1])
  last_col <- col_num + nrow(df_analytes) - 1
  
  df_indicators <- df[,(last_col + 1):ncol(df)]
  df <- df[,(1:last_col)]
  
  #find first row with a number 
  data_rows <- which(grepl("^[0-9]", df_indicators[[1]]))
  df_indicators <- df_indicators[data_rows,]
  
  write_table_try("data_indicators", df_indicators, params)
  write_table_try("data_no_indicators", df, params)
  
  cat(file = stderr(), "Function remove_indicators_bg...end", "\n\n")
  
}

#---------------------------------------------------------------------

separate_data <- function(session, input, output, params){
  cat(file = stderr(), "Function separate_data...", "\n")
  
  bg_separate_data <- callr::r_bg(separate_data_bg, args = list(params), stderr = str_c(params$error_path, "//error_separate_data.txt"), supervise = TRUE)
  bg_separate_data$wait()
  print_stderr("error_separate_data.txt")

  analyte_match <- bg_separate_data$get_result()[[1]]
  params <<- bg_separate_data$get_result()[[2]]
  
  if (analyte_match == FALSE) {
    shinyalert("Oops!", "Data and Configuration do not match", type = "error")
  }
  
  cat(file = stderr(), "Function separate_data...end", "\n\n")
}


#---------------------------------------------------------------------

separate_data_bg <- function(params){
  cat(file = stderr(), "Function separate_data_bg...", "\n")
  source('Shiny_File.R')
  source('Shiny_Misc_Functions.R')
  
  df <- read_table_try("data_no_indicators", params)

  if(params$simple_plate) {
    df <- simple_plate_name(df)
  }
  
  #find row number for first row with number or letters
  data_rows <- which(grepl("^[0-9]", df[[1]]))
  df_data <- df[data_rows,]
  df_info <- df[-data_rows,]
  
  df_analytes <- read_table_try("analytes", params)  
  
  #find colnumber for df_analytes$Abbreviation[1]
  col_num <- which(colnames(df) == df_analytes$Name[1])
  df_info <- df_info[,(col_num - 1):ncol(df_info)]
  
  #check if "Standard" in df_data$Sample.type

  if (any(grepl("Standard", df_data$Sample.type))) {
    plates <- unique(df_data[-grep("Standard", df_data$Sample.type),]$Plate.bar.code)
  }else{
    plates <- unique(df_data$Plate.bar.code)
  }

  params$plate_number <- length(plates)
  params$plates <- stringr::str_c(plates, collapse = ",")
  
  materials <- unique(df_data$Material)
  params$material_number <- length(materials)
  params$materials <- stringr::str_c(materials, collapse = ",")
  
  write_table_try("data_start", df_data, params)
  write_table_try("data_info", df_info, params)
  
  #check that analytes and order match
  test_data <- colnames(df[(ncol(df)-nrow(df_analytes)+1):ncol(df)])
  #replace . ( ) + in test with ""
  test_data <- gsub("[.()\\+]", "", test_data)
  test_config <- gsub("[.()\\+]", "", df_analytes$Abbreviation)
  
  if (all(test_data == test_config)) {
    analyte_match <- TRUE
  }else{
    analyte_match <- FALSE
  }
  
  cat(file = stderr(), stringr::str_c("Analytes match = ", analyte_match), "\n")
  
  cat(file = stderr(), "Function separate_data_bg...end", "\n\n")

  return(list(analyte_match, params))
    
}

#---------------------------------------------------------------------

report_template <- function(session, input, output, params){
  cat(file = stderr(), "Function report_template...", "\n")
  
  bg_report_template <- callr::r_bg(report_template_bg, args = list(params), stderr = str_c(params$error_path, "//error_report_template.txt"), supervise = TRUE)
  bg_report_template$wait()
  print_stderr("error_report_template.txt")
    
  cat(file = stderr(), "Function report_template...end", "\n")
}

#--------------------------------------------------------------
report_template_bg <- function(params){
  cat(file = stderr(), "Function report_template_bg...", "\n")
  
  source('Shiny_File.R')
  
  #create list from params$plates
  plates <- unlist(stringr::str_split(params$plates, ","))
  df_info <- read_table_try("data_info", params)
  df_report <- read_table_try("Analytes", params)

  df_report$R_colnames <- colnames(df_info[2:ncol(df_info)])
  
  df_report_colnames <- colnames(df_report)
  
  # plate = plates[1]
  for (plate in plates) {
    
    #works for bileacids and q500, cases where multiple plates listed
    #????????df_plate <- df_info[grep(paste(plate, collapse="|"), df_info[[1]]),]
    df_plate <- df_info[grep(plate, df_info[[1]]),]
    
    df_calc_lod <- df_plate[grep("calc", df_plate[[1]]),]
    df_op_lod <- df_plate[grep("from OP", df_plate[[1]]),]
    df_calc_lod <- as.data.frame(lapply(df_calc_lod, as.numeric))
    df_op_lod <- as.data.frame(lapply(df_op_lod, as.numeric))
    df_calc_lod[is.na(df_calc_lod)] <- 0
    df_op_lod[is.na(df_op_lod)] <- 0
    #consolidate values from flow inject and lcms (does nothing for bileacid)
    calc_lod <- apply(df_calc_lod[2:ncol(df_calc_lod)], 2, max)
    op_lod <- apply(df_op_lod[2:ncol(df_op_lod)], 2, max)
    
    #if value in calc_lod is 0 then replace with value from op_lod
    for (i in 1:length(calc_lod)) {
      if (calc_lod[i] == 0) { calc_lod[i] <- op_lod[i] }
    }
    
    col_name <- stringr::str_c("LOD_", plate, ", uM")
    df_report_colnames <- c(df_report_colnames, col_name)
    
    #create new column called from col_name in df_report
    df_report[[col_name]] <- calc_lod
      
  }
  
  df_lloq <- df_info[grep("LLOQ", df_info[[1]]),]
  df_uloq <- df_info[grep("ULOQ", df_info[[1]]),]
  
  #drop first column and set dataframe to numeric
  df_lloq <- as.data.frame(lapply(df_lloq[2:ncol(df_lloq)], as.numeric))
  df_uloq <- as.data.frame(lapply(df_uloq[2:ncol(df_uloq)], as.numeric))
  
  #get column maxs from df_uloq and mins from df_lloq
  uloq_max <- apply(df_uloq, 2, max, na.rm = TRUE)
  lloq_min <- apply(df_lloq, 2, min, na.rm = TRUE)

  #replace Inf and -Inf with empty string
  uloq_max[is.infinite(uloq_max)] <- ""
  lloq_min[is.infinite(lloq_min)] <- ""
  
  col_name <- stringr::str_c("Lowest CS, uM")
  df_report_colnames <- c(df_report_colnames, col_name)
  df_report[[col_name]] <- lloq_min
    
  col_name <- stringr::str_c("Highest CS, uM")
  df_report_colnames <- c(df_report_colnames, col_name)
  df_report[[col_name]] <- uloq_max
    
  colnames(df_report) <- df_report_colnames
  
  write_table_try("Report_template", df_report, params)
  
  cat(file = stderr(), "Function report_template_bg...end", "\n\n")
}



#---------------------------------------------------------------------

replace_lod <- function(session, input, output, params){
  cat(file = stderr(), "Function replace_lod...", "\n")
  
  params$fixed_lod <- input$fixed_lod
  
  bg_replace_lod <- callr::r_bg(replace_lod_bg, args = list(params), stderr = str_c(params$error_path, "//error_replace_lod.txt"), supervise = TRUE)
  bg_replace_lod$wait()
  print_stderr("error_replace_lod.txt")
  
  params <<- params
  
  cat(file = stderr(), "Function replace_lod...end", "\n\n")
}

#---------------------------------------------------------------------

replace_lod_bg <- function(params){
  cat(file = stderr(), "Function replace_lod_bg...", "\n")
  
  source('Shiny_File.R')
  
  df <- read_table_try("data_start", params)
  df_report <- read_table_try("Report_template", params)
  
  #sort df by plate.bar.code and reindex
  df <- df[order(df$Plate.bar.code),]
  rownames(df) <- NULL
  
  df_info <- df[,1:(ncol(df)-nrow(df_report))]
  df_data <- df[,(ncol(df)-nrow(df_report)+1):ncol(df) ]
  
  #replace 0 value with "< LOD" in df_data
  df_data[df_data == 0] <- "< LOD"
  
  #fill df_random with random numbers between 0 and 1
  set.seed(1234)
  df_random <- df_data
  for (i in 1:ncol(df_random)) {
    df_random[[i]] <- runif(nrow(df_random), min = 0.1, max = 1)
  }
  
  
  for (r in 1:nrow(df_data)) {
    #find columns in df_data that contain 'LOD'
    find_lod_cols <- which(grepl("LOD", df_data[r,]), arr.ind = TRUE)
    plate <- gsub("-", ".", df_info$Plate.bar.code[r])
    plate <- gsub(" ", ".", plate)
    plate <- gsub("\\|", ".", plate)
    #find column in df_report that contains 'LOD' and plate
    find_plate_lod_col <- which(grepl(plate, colnames(df_report)), arr.ind = TRUE )
    #loop through columns and replace values with values from df_report
    if (length(find_lod_cols) >=1 ) { 
      for (c in find_lod_cols) {
        if (params$fixed_lod){
          df_data[r,c] <- as.numeric(df_report[c,find_plate_lod_col], digits = 5)
        }else {
          df_data[r,c] <- round(df_random[r,c] * as.numeric(df_report[c,find_plate_lod_col]), digits = 5)
      }
    }
   }
  }
  
  df_final <- cbind(df_info, df_data)
  
  write_table_try("data_impute", df_final, params)
  
  cat(file = stderr(), "Function replace_lod_bg...end", "\n\n")
}



#---------------------------------------------------------------------

qc_calc <- function(session, input, output, params){
  cat(file = stderr(), "Function qc_calc...", "\n")
  
  params$qc_acc <- input$qc_acc
  params$fixed_lod <- input$fixed_lod
  params <<- params
  
  bg_qc_calc <- callr::r_bg(qc_calc_bg, args = list(params), stderr = str_c(params$error_path, "//error_qc_calc.txt"), supervise = TRUE)
  bg_qc_calc$wait()
  print_stderr("error_qc_calc.txt")
  
  cat(file = stderr(), "Function qc_calc...end", "\n\n")
}


#---------------------------------------------------------------------

qc_calc_bg <- function(params){
  cat(file = stderr(), "Function qc_calc_bg...", "\n")
  source('Shiny_File.R')
  
  df <- read_table_try("data_impute", params)
  df_report_template <- read_table_try("Report_template", params)
  df_qc_info <- read_table_try("QC", params)
  
  df_qc_report <- df_report_template[,1:2]
  
  qc_list <- c("QC1", "QC2", "QC3")
 
  #subset of data with "QC" in Sample.description
  df_qc <- df[grep("QC", df$Sample.identification),]
  plates <- unique(df_qc$Plate.bar.code)
  
  for (qc in qc_list) {
    qc_level <- df_qc_info[grep(qc, df_qc_info$Expected.values),]
    qc_std_levels <- unlist(qc_level[3:ncol(qc_level)]) 
    df_qc_report[stringr::str_c(qc, " Level (uM)")] <- qc_std_levels
    for (plate in plates) {
      df_plate <- df_qc[grep(plate, df_qc$Plate.bar.code),]
      df_plate <- df_plate[grep(qc, df_plate$Sample.identification),]
      df_plate <- df_plate[,(ncol(df_plate)-nrow(df_report_template)+1):ncol(df_plate)]
      #set df_plate to numeric
      df_plate <- as.data.frame(lapply(df_plate, as.numeric))
      df_mean <- round(colMeans(df_plate, na.rm = TRUE), digits = 5)
      df_qc_report[stringr::str_c(qc, " ", plate, " Mean (uM)")] <- df_mean
      #calc the %CV from columns of df_plate
      df_acc <- round(100 * (df_mean/qc_std_levels), digits = 1)
      df_qc_report[stringr::str_c(qc, " ", plate, " Accuracy")] <- df_acc
    }
  }
  
  write_table_try("QC_Report", df_qc_report, params)
  
  cat(file = stderr(), "Function qc_calc_bg...end", "\n\n")
}



#---------------------------------------------------------------------

process_data <- function(session, input, output, params){
  cat(file = stderr(), "Function process_data...", "\n")
  
  params$material_select <- input$material_select
  params$norm_select <- input$norm_select
  params$spqc_filter <- input$spqc_filter
  params$spqc_filter_value <- input$spqc_filter_value
  params$missing_filter <- input$missing_filter
  params$missing_filter_value <- input$missing_filter_value
  params$spqc_replace <- input$spqc_replace
  
  #check if params$material_list exists
  if (length(params$material_list) == 0) {
    params$material_list <- input$material_select
  }else{
    params$material_list <- stringr::str_c(params$material_list, ",", input$material_select)
  }
  
  process_data_calc <- callr::r_bg(process_data_bg, args = list(params), stderr = str_c(params$error_path, "//process_data_calc.txt"), supervise = TRUE)
  process_data_calc$wait()
  print_stderr("process_data_calc.txt")

  updatePickerInput(session, "pca_plate_select_list", choices = unlist(strsplit(params$plates, ",")))

  params <<- params
  
  cat(file = stderr(), "Function process_data...end", "\n\n")
}

#---------------------------------------------------------------------

process_data_bg <- function(params){
  cat(file = stderr(), "Function process_data_bg...", "\n")
  source('Shiny_File.R')
  source('Shiny_Data.R')
  
  material <- params$material_select
  
  df <- read_table_try("data_impute", params)
  df_report <- read_table_try("Report_template", params)

  df_material <- df[grep(material, df$Material),]
  
  if(params$spqc_replace != "No"){
    df_to_add <- df[grep(params$spqc_replace, df$Sample.description),]
    if(nrow(df_to_add) == 0){
      cat(file = stderr(), "No replacement SPQC data available...", "\n")
    }
    df_material <- rbind(df_material, df_to_add)
  }
  
  write_table_try(material, df_material, params)
  
  normalize_data(df, df_report, material, params)
  
  #create table with material SPQC data for report
  spqc_report(df_report, params)

  cat(file = stderr(), "Function process_data_bg...end", "\n\n")
}

#---------------------------------------------------------------------
normalize_data <- function(df, df_report, material, params){
  cat(file = stderr(), "Function normalize_data...", "\n")
  # material = params$material_select
  
  if (params$norm_select != "None" & params$plate_number > 1) {
    df_norm <- df[grep(params$norm_select, df$Sample.description),]
    #if using anything but SPQC, the material could be different
    if(params$norm_select == "SPQC") {
      df_norm_material <- df_norm[grep(material, df_norm$Material),]
    }else {
      df_norm_material <- df_norm
    }
    
    norm_material <- unique(df_norm_material$Material)
    
    df_norm_plate_mean <- df_report[,1:3]
    df_norm_mean <- df_report[,1:3]
    
    #incase normalizing on group that is not the same material
    if(nrow(df_norm_material) > 0 ) {
      df_norm <- df_norm_material
    }

    #isolate data only
    df_norm_data <- df_norm[,(ncol(df_norm)-nrow(df_report)+1):ncol(df_norm)]
    df_norm_data <- as.data.frame(lapply(df_norm_data, as.numeric))
    mean_df_norm_data <- round(colMeans(df_norm_data, na.rm = TRUE), digits = 5)
    df_norm_mean[stringr::str_c("Mean ", params$norm_select, " ", material)] <- mean_df_norm_data
    df_norm_mean[stringr::str_c("CV ", params$norm_select, " ", material)] <- round(100 * (apply(df_norm_data, 2, sd, na.rm = TRUE) / mean_df_norm_data), digits = 2)

    #list of plates
    norm_plates <- unique(df_norm$Plate.bar.code)    
    
    #calc the mean intensity from each SPQC by plate
    for (plate in norm_plates) {
      df_plate <- df_norm[grep(plate, df_norm$Plate.bar.code),]
      df_plate <- df_plate[,(ncol(df_plate)-nrow(df_report)+1):ncol(df_plate)]
      df_plate <- as.data.frame(lapply(df_plate, as.numeric))
      df_norm_plate_mean[stringr::str_c(params$norm_select, " ", plate, " ", material)] <- round(colMeans(df_plate, na.rm = TRUE), digits = 5)
    } 
    
    df_norm_plate_mean <- df_norm_plate_mean[,4:ncol(df_norm_plate_mean)]
    total_mean <- rowMeans(df_norm_plate_mean, na.rm = TRUE)
    
    #divide each column in dfs_spqc_sum by total_sum
    df_norm_factor <- df_norm_plate_mean/total_mean
    
    #normalize data on each plate
    for(plate in norm_plates){
      df_plate <- df[grep(plate, df$Plate.bar.code),]
      df_plate <- df_plate[grep(material, df_plate$Material),]
      #add NIST and Golden West data since could be different material
      if(material != norm_material) {
        df_plate <- rbind(df_plate, df_norm_material)
        df_plate <- df_plate[grep(plate, df_plate$Plate.bar.code),]
      }
      df_info_plate <- df_plate[,1:(ncol(df_plate)-nrow(df_report))]
      df_plate <- df_plate[,(ncol(df_plate)-nrow(df_report)+1):ncol(df_plate)]
      df_plate <- as.data.frame(lapply(df_plate, as.numeric))
      norm_factor <- df_norm_factor[[stringr::str_c(params$norm_select, " ", plate, " ", material)]]
      #divide each row of df_plate by norm_factor
      df_plate_norm <- sweep(df_plate, 2,  norm_factor, "/")
      df_plate_norm <- round(df_plate_norm, digits = 5)
      df_plate_norm <- cbind(df_info_plate, df_plate_norm)
      
      #if first pass through loop
      if (plate == norm_plates[1]) {
        df_final <- df_plate_norm
      }else{
        df_final <- rbind(df_final, df_plate_norm)
      }
      
    }
    
    write_table_try(stringr::str_c(params$norm_select, "_Norm_", material), df_final, params)
    write_table_try("Report", df_report, params)
    write_table_try(stringr::str_c(params$norm_select, "_Norm_Factor_", material), df_norm_factor, params)
    write_table_try(stringr::str_c("Norm_Mean_", material), df_norm_mean, params)
    
  }
   
  cat(file = stderr(), "Function normalize_data...end", "\n\n")
}

#---------------------------------------------------------------------
spqc_report <- function(df_report, params){
  cat(file = stderr(), "Function spqc_report...", "\n")
  
  if(params$data_source == "BileAcid"){
    df_spqc_report <- df_report[,1:3]
  }else {
    df_spqc_report <- df_report[,1:5]
  }
  
  
  #subset of data with "SPQC" in Sample.description
  if (params$norm_select != "None"){
    df <- read_table_try(stringr::str_c(params$norm_select, "_Norm_", params$material_select), params)
  }else {
    df <- read_table_try(params$material_select, params)
  }
  
  if(params$spqc_replace == "No"){
    df <- df[grep("SPQC", df$Sample.description),]
  }else {
    df <- df[grep(params$spqc_replace, df$Sample.description),]
  }
  
  
  if(nrow(df) != 0){
    #list of plates
    spqc_plates <- unique(df$Plate.bar.code)
    
    #isolate data only
    df_all <- df[,(ncol(df)-nrow(df_report)+1):ncol(df)]
    
    #set df_material to numeric
    df_all <- as.data.frame(lapply(df_all, as.numeric))
    mean_df_spqc_material <- round(colMeans(df_all, na.rm = TRUE), digits = 5)
    
    #calc the %CV from columns of df_spqc_material
    cv_df_spqc_material <- round(100 * (apply(df_all, 2, sd, na.rm = TRUE) / mean_df_spqc_material), digits = 2)
    df_spqc_report[stringr::str_c("Mean ", params$material_select, " SPQC(uM)")] <- mean_df_spqc_material
    df_spqc_report[stringr::str_c("%CV ", params$material_select, " SPQC(uM)")] <- cv_df_spqc_material
    
    for (plate in spqc_plates) {
      df_plate <- df[grep(plate, df$Plate.bar.code),]
      #isolate data only
      df_plate <- df_plate[,(ncol(df_plate)-nrow(df_report)+1):ncol(df_plate)]
      df_plate <- as.data.frame(lapply(df_plate, as.numeric))
      mean_plate_df_spqc_material <- round(colMeans(df_plate, na.rm = TRUE), digits = 5)
      cv_plate_df_spqc_material <- round(100 * (apply(df_plate, 2, sd, na.rm = TRUE) / mean_df_spqc_material), digits = 2)
      df_spqc_report[stringr::str_c("Mean ", plate, " ", params$material_select, " SPQC(uM)")] <- mean_plate_df_spqc_material
      df_spqc_report[stringr::str_c("%CV ", plate, " ", params$material_select, " SPQC(uM)")] <- cv_plate_df_spqc_material
    }
    
    
    write_table_try(stringr::str_c("SPQC_Report_", params$material_select), df_spqc_report, params)
    
  }
  
  cat(file = stderr(), "Function spqc_report...end", "\n\n")
}
#---------------------------------------------------------------------
#---------------------------------------------------------------------

spqc_missing_filter <- function(session, input, output, params){
  cat(file = stderr(), "Function spqc_filter...", "\n")

  bg_spqc_missing_filter <- callr::r_bg(spqc_missing_filter_bg, args = list(params, use_norm=FALSE), stderr = str_c(params$error_path, "//error_spqc_missing_filter.txt"), supervise = TRUE)
  bg_spqc_missing_filter$wait()
  print_stderr("error_spqc_missing_filter.txt")
  
  if (params$norm_select != "None"){
    bg_spqc_missing_filter_norm <- callr::r_bg(spqc_missing_filter_bg, args = list(params, use_norm=TRUE), stderr = str_c(params$error_path, "//error_spqc_missing_filter_norm.txt"), supervise = TRUE)
    bg_spqc_missing_filter_norm$wait()
    print_stderr("error_spqc_missing_filter_norm.txt")
}
  
  
  cat(file = stderr(), "Function spqc_missing_filter...end", "\n\n")
}

#---------------------------------------------------------------------

spqc_missing_filter_bg <- function(params, use_norm){
  cat(file = stderr(), "Function spqc_missing_filter_bg...", "\n")
  source('Shiny_File.R')  

  df_qc_report <- read_table_try("QC_Report", params)
  df_report <- read_table_try(stringr::str_c("SPQC_Report_", params$material_select), params)
  df_start <- read_table_try("data_start", params)
  
  high_cv_analytes <- c()
  high_LOD_analytes <- c()
  
  if (use_norm){
    df_material <- read_table_try(stringr::str_c(params$norm_select,"_Norm_", params$material_select), params)
  }else {
    df_material <- read_table_try(params$material_select, params)
  }
  
    
  if (params$spqc_filter) {
    cv_col <- grep("CV", colnames(df_report))[1]
    high_cv <- which(df_report[cv_col] > params$spqc_filter_value)
    high_cv_analytes <- df_report$R_colnames[high_cv]
  }
        
  if (params$missing_filter) {
    #filter df_start for material in Materials
    df_material_start <- df_start[grep(params$material_select, df_start$Material),]
    df_material_start <- df_material_start[,(ncol(df_material_start)-nrow(df_report)+1):ncol(df_material_start)]
    #replace 0 values with "< LOD"
    df_material_start[df_material_start == 0] <- "< LOD"
    
    #count the number of times "LOD" appears in each column of df_material_start
    lod_count <- apply(df_material_start, 2, function(x) sum(grepl("LOD", x)))
    lod_count <- round(lod_count/nrow(df_material_start)*100,2)
    lod_column <- which(lod_count > params$missing_filter_value)
    high_LOD_analytes <- df_report$R_colnames[lod_column]
  }
    
  #from df_material remove columns with column names in high_cv_analytes
  analytes_to_remove <- c(high_cv_analytes, high_LOD_analytes)
  analytes_to_remove <- unique(analytes_to_remove)  

  if (length(analytes_to_remove) > 0) {
    df_material <- df_material[,-which(colnames(df_material) %in% analytes_to_remove)]
  }
  
  if (use_norm){
    table_name <- stringr::str_c(params$norm_select, "_Filtered_Norm_", params$material_select)
  }else{
    table_name <- stringr::str_c("Filtered_", params$material_select)
  }  
  
  write_table_try(table_name, df_material, params)

  cat(file = stderr(), "Function spqc_missing_filter_bg...end", "\n\n")
}




#----------------------------------------------------------------------------------------
# create final excel documents
final_excel <- function(session, input, output, params) {
  cat(file = stderr(), "function Final_Excel...", "\n")
  showModal(modalDialog("Creating/Saving excel file...", footer = NULL))  
  
  require(openxlsx)
  
  input_excel_raw_data <- input$excel_raw_data
  input_excel_raw_data_no_indicator <- input$excel_raw_data_no_indicator
  input_excel_impute_data <- input$excel_impute_data
  input_excel_report <- input$excel_report
  input_excel_qc_report <- input$excel_qc_report
  input_excel_spqc_report <- input$excel_spqc_report
  input_excel_samples_raw <- input$excel_samples_raw
  input_excel_samples_norm <- input$excel_samples_norm
  input_excel_filename <- input$excel_filename
  
  arg_list <- list(params, input_excel_raw_data, input_excel_raw_data_no_indicator, input_excel_impute_data, 
                   input_excel_report, input_excel_qc_report, input_excel_spqc_report, input_excel_samples_raw, input_excel_samples_norm, input_excel_filename)
  
  bg_excel <- callr::r_bg(func = final_excel_bg, args = arg_list, stderr = stringr::str_c(params$error_path, "//error_finalexcel.txt"), supervise = TRUE)
  bg_excel$wait()
  print_stderr("error_finalexcel.txt")
  
  params <<- bg_excel$get_result()
  
  cat(file = stderr(), "function Final_Excel...end", "\n\n")
  removeModal()
}


#----------------------------------------------------------------------------------------
# create final excel documents
final_excel_bg <- function(params, input_excel_raw_data, input_excel_raw_data_no_indicator, input_excel_impute_data, 
                           input_excel_report, input_excel_qc_report, input_excel_spqc_report, input_excel_samples_raw, input_excel_samples_norm, input_excel_filename) {
  cat(file = stderr(), "Function Final_Excel_bg...", "\n")
  
  require(openxlsx)
  source('Shiny_File.R')

  filename <- stringr::str_c(params$data_path, "/", input_excel_filename)
  nextsheet <- 1
  wb <- createWorkbook()

  if (input_excel_raw_data) {
    addWorksheet(wb, "Raw Data")
    excel_df <- read_table_try("data_raw", params)
    writeData(wb, sheet = nextsheet, excel_df)
    nextsheet <- nextsheet + 1
  }
  
  if (input_excel_raw_data_no_indicator) {
    addWorksheet(wb, "Status Removed")
    excel_df <- read_table_try("data_no_indicators", params)
    writeData(wb, sheet = nextsheet, excel_df)
    nextsheet <- nextsheet + 1
  }
  
  if (input_excel_impute_data) {
    addWorksheet(wb, "Imputed Data")
    excel_df <- read_table_try("data_impute", params)
    writeData(wb, sheet = nextsheet, excel_df)
    nextsheet <- nextsheet + 1
  }
  
  if (input_excel_report) {
    addWorksheet(wb, "Sample Summary")
    excel_df <- read_table_try("Report_template", params)
    writeData(wb, sheet = nextsheet, excel_df)
    nextsheet <- nextsheet + 1
  }
  
  if (input_excel_qc_report) {
    addWorksheet(wb, "QC Summary")
    excel_df <- read_table_try("QC_Report", params)
    writeData(wb, sheet = nextsheet, excel_df)
    nextsheet <- nextsheet + 1
  }
  
  if (input_excel_spqc_report) {
    materials <- unique(unlist(strsplit(params$material_list, ",")))
    for (material in materials) {
      excel_df <- read_table_try(stringr::str_c("SPQC_Report_", material), params)
      if (material == materials[1]) {
        excel_df_all <- excel_df
      }else{
        if (params$data_source == "BileAcid") {
          excel_df_all <- rbind(excel_df_all, excel_df[,-c(1:3)])
        }else{
        excel_df_all <- rbind(excel_df_all, excel_df[,-c(1:5)])
        }
      }
    }
    addWorksheet(wb, "SPQC Summary")
    writeData(wb, sheet = nextsheet, excel_df_all)
    nextsheet <- nextsheet + 1
  }
  

  materials <- unique(unlist(strsplit(params$material_list, ",")))
  db_tables <- list_tables(params)
  for (material in materials) {
    
    if (input_excel_samples_raw) {
      if (material %in% db_tables) {
        addWorksheet(wb, material)
        excel_df <- read_table_try(material, params)
        writeData(wb, sheet = nextsheet, excel_df)
        nextsheet <- nextsheet + 1
      }
      if (stringr::str_c("Filtered_", material) %in% db_tables) {
        sheetname <- stringr::str_c("Filtered_", material)
        if (nchar(sheetname) > 31) {
          sheetname <- substr(sheetname, 1, 31)
        }
        addWorksheet(wb, sheetname)
        excel_df <- read_table_try(stringr::str_c("Filtered_", material), params)
        writeData(wb, sheet = nextsheet, excel_df)
        nextsheet <- nextsheet + 1
        }
    }
    
    if (input_excel_samples_norm) {
      if (stringr::str_c(params$norm_select, "_Norm_", material) %in% db_tables) {
        sheetname <- stringr::str_c(params$norm_select, "_Norm_", material)
        if (nchar(sheetname) > 31) {
          sheetname <- substr(sheetname, 1, 31)
        }
        addWorksheet(wb, sheetname)
        excel_df <- read_table_try(stringr::str_c(params$norm_select, "_Norm_", material), params)
        writeData(wb, sheet = nextsheet, excel_df)
        nextsheet <- nextsheet + 1
      }
      if (stringr::str_c(params$norm_select, "_Filtered_Norm_", material) %in% db_tables) {
        sheetname <- stringr::str_c(params$norm_select, "_Filtered_Norm_", material)
        if (nchar(sheetname) > 31) {
          sheetname <- substr(sheetname, 1, 31)
        }
        addWorksheet(wb, sheetname)
        excel_df <- read_table_try(stringr::str_c(params$norm_select, "_Filtered_Norm_", material), params)
        writeData(wb, sheet = nextsheet, excel_df)
        nextsheet <- nextsheet + 1
      }
    }
    
    }

  cat(file = stderr(), "writting excel to disk...", "\n")
  saveWorkbook(wb, filename, overwrite = TRUE)
  cat(file = stderr(), stringr::str_c("Creating Excel Output File...", filename), "\n")

  
  #archive code
  cat(file = stderr(), "Archiving Code...", "\n")
  archive_path <- stringr::str_c(params$data_path, "/archive")
  params$archive_path <- create_dir(archive_path)
  #
  #copy all *.R files into archive
  file_list <- list.files(path = getwd(), pattern = "*.R", full.names = TRUE)
  for (file in file_list) {
    file_copy <- stringr::str_c(params$archive_path, "/", basename(file))
    file.copy(file, file_copy)
  }
    
  cat(file = stderr(), "Function Final_Excel_bg...", "\n\n")
  
  return(params)
}



#---------------------------------------------------------------------

load_archive_file <- function(session, input, output){
  cat(file = stderr(), "Function load_archive_file...", "\n")
  #showModal(modalDialog("Loading archive file...", footer = NULL))

  archive_sfb <- parseFilePaths(volumes, input$sfb_archive_file)
  archive_path <- str_extract(archive_sfb$datapath, "^/.*/")
  cat(file = stderr(), stringr::str_c("archive_path --->", archive_path), "\n")
  archive_zip <- archive_sfb$datapath
  archive_name <- basename(archive_sfb$datapath)

  #save(archive_sfb, file = "testarchive_sfb")
  # load(file = "testarchive_sfb")

  database_dir <- stringr::str_c(getwd(), "/database")
  create_dir(database_dir)
  
  cat(file = stderr(), stringr::str_c("unzip file to database_dir:  ", database_dir), "\n")
  utils::unzip(zipfile = archive_zip, exdir = database_dir)
  load(file = stringr::str_c(database_dir, "/params"))
  
  temp_files <- list.files(database_dir)
  if (length(temp_files) == 0) {
    temp_files <- ""
  }
  
  cat(file = stderr(), stringr::str_c("files in database_dir:  ", temp_files), "\n")

  database_path <- stringr::str_c(database_dir, "/", file_name)

  cat(file = stderr(), "Function load_archive_file...end", "\n\n")
  return(archive_zip)
}





