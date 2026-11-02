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
  
  infinity_message <- bg_load_data$get_result()
  
  if (nchar(infinity_message) > 0) {
    shinyalert("Infinity values", infinity_message, type = "warning")
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
  
  #infinity in sample analyte values means a calibration curve problem, user needs to fix it in the Biocrates software
  df_analytes <- read_table_try("Analytes", params)
  df_check <- as.data.frame(df)
  df_check <- df_check[grepl("^[0-9]", df_check[[1]]),]
  sample_type <- df_check[["Sample type"]]
  df_check <- df_check[, colnames(df_check) %in% df_analytes$Name, drop = FALSE]
  infinity_cells <- as.matrix(as.data.frame(lapply(df_check, function(x) grepl("infinity", x, ignore.case = TRUE))))
  infinity_message <- ""
  if (any(infinity_cells)) {
    infinity_analytes <- colnames(df_check)[colSums(infinity_cells) > 0]
    infinity_rows <- rowSums(infinity_cells) > 0
    other_rows <- sum(infinity_rows & sample_type != "Sample")
    infinity_message <- stringr::str_c("Infinity values found in sample data: ", length(infinity_analytes), " analyte(s) (",
                                       stringr::str_c(head(infinity_analytes, 10), collapse = ", "), if (length(infinity_analytes) > 10) ", ..." else "",
                                       ") in ", sum(infinity_rows & sample_type == "Sample"), " sample(s)",
                                       if (other_rows > 0) stringr::str_c(" and ", other_rows, " QC/standard row(s)") else "", ". ",
                                       "Correct the calibration curves in the Biocrates software and re-export the data.")
    cat(file = stderr(), infinity_message, "\n")
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
  
  return(infinity_message)
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
  
  #from df column Plate.note keep only characters to the first . (some exports have no Plate note column)
  if ("Plate.note" %in% colnames(df)) {
    df$Plate.note <- gsub("\\..*", "", df$Plate.note)
  }

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

  #drop the metabolism indicator columns after the last analyte
  col_num <- which(colnames(df) == df_analytes$Name[1])
  last_col <- col_num + nrow(df_analytes) - 1
  df <- df[,(1:last_col)]

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
  export_message <- bg_separate_data$get_result()[[3]]

  if (analyte_match == FALSE) {
    shinyalert("Oops!", "Data and Configuration do not match", type = "error")
  } else if (nchar(export_message) > 0) {
    shinyalert("Check WebIDQ export", export_message, type = "warning")
  }
  
  cat(file = stderr(), "Function separate_data...end", "\n\n")
}


#---------------------------------------------------------------------

separate_data_bg <- function(params){
  cat(file = stderr(), "Function separate_data_bg...", "\n")
  library(dplyr)
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
  
  df_analytes <- read_table_try("Analytes", params)  
  
  #find colnumber for df_analytes$Abbreviation[1]
  col_num <- which(colnames(df) == df_analytes$Name[1])
  df_info <- df_info[,(col_num - 1):ncol(df_info)]
  
  #check if "Standard" in df_data$Sample.type

  if (any(grepl("Standard", df_data$Sample.type))) {
    plates <- unique(df_data[-grep("Standard", df_data$Sample.type),]$Plate.bar.code)
  }else{
    plates <- unique(df_data$Plate.bar.code)
  }

  #sample rows with only some of another plate's barcodes (e.g. FIA only re-injections in a Q500 "FIA | LC" export)
  #mean the WebIDQ export is not correct, the user needs to fix it in the Biocrates software
  split_barcodes <- function(x) { trimws(unlist(stringr::str_split(x, stringr::fixed("|")))) }
  partial_plates <- plates[sapply(plates, function(plate) {
    any(sapply(setdiff(plates, plate), function(other) all(split_barcodes(plate) %in% split_barcodes(other))))
  })]
  export_message <- ""
  if (length(partial_plates) > 0) {
    df_partial <- df_data[df_data$Plate.bar.code %in% partial_plates,]
    export_message <- stringr::str_c("The export has ", nrow(df_partial), " row(s) with only part of a plate's barcodes (plate ",
                                     stringr::str_c(partial_plates, collapse = ", "), "; sample(s) ",
                                     stringr::str_c(head(unique(df_partial$Sample.identification), 10), collapse = ", "),
                                     "). These are usually re-injections. Correct the export in the Biocrates software and re-export the data.")
    cat(file = stderr(), export_message, "\n")
  }

  params$plate_number <- length(plates)
  params$plates <- stringr::str_c(plates, collapse = ",")
  
  materials <- unique(df_data$Material)
  params$material_number <- length(materials)
  params$materials <- stringr::str_c(materials, collapse = ",")
  
  write_table_try("data_start", df_data, params)
  write_table_try("data_info", df_info, params)
  
  #create lookup table for table barcode and names
  plate_info <- df_data |>
    select(Plate.bar.code, Submission.name) |>
    distinct()
  write_table_try("plate_info", plate_info, params)
  
  #check that analytes and order match
  test_data <- colnames(df[(ncol(df)-nrow(df_analytes)+1):ncol(df)])
  #compare names without punctuation (data column names are R-mangled)
  test_data <- gsub("[^A-Za-z0-9]", "", test_data)
  test_config <- gsub("[^A-Za-z0-9]", "", make.names(df_analytes$Name))

  if (length(test_data) == length(test_config) && all(test_data == test_config)) {
    analyte_match <- TRUE
  }else{
    analyte_match <- FALSE
  }
  
  cat(file = stderr(), stringr::str_c("Analytes match = ", analyte_match), "\n")
  
  cat(file = stderr(), "Function separate_data_bg...end", "\n\n")

  return(list(analyte_match, params, export_message))
    
}

#---------------------------------------------------------------------

report_template <- function(session, input, output, params){
  cat(file = stderr(), "Function report_template...", "\n")
  
  bg_report_template <- callr::r_bg(report_template_bg, args = list(params), stderr = str_c(params$error_path, "//error_report_template.txt"), supervise = TRUE)
  bg_report_template$wait()
  print_stderr("error_report_template.txt")
  
  if (bg_report_template$get_exit_status() != 0) {
    shinyalert("Oops!", "LOD extraction failed, see error_report_template.txt", type = "error")
  } else {
    lod_problems <- bg_report_template$get_result()
    if (length(lod_problems) > 0) {
      shinyalert("LOD check", stringr::str_c(lod_problems, collapse = "\n"), type = "warning")
    }
  }
    
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

  if (ncol(df_info) - 1 != nrow(df_report)) {
    stop(stringr::str_c("data_info has ", ncol(df_info) - 1, " analyte columns but config has ", nrow(df_report), " analytes"))
  }
  df_report$R_colnames <- colnames(df_info[2:ncol(df_info)])
  
  df_report_colnames <- colnames(df_report)
  lod_problems <- c()
  
  #parse LOD row labels into barcodes, e.g. "LOD (calc.) 1053482453-1 [uM]" -> "1053482453-1"
  calc_barcode <- stringr::str_match(df_info[[1]], "^LOD \\(calc\\.\\) (.+?) \\[")[,2]
  op_barcode <- stringr::str_match(df_info[[1]], "^LOD \\(from OP\\) (.+?) \\[")[,2]
  
  #numeric matrix of LOD rows for the barcodes, max per analyte (NA if no values)
  lod_max <- function(barcode_col, barcodes) {
    rows <- which(barcode_col %in% barcodes)
    if (length(rows) == 0) { return(rep(NA_real_, ncol(df_info) - 1)) }
    lod_values <- suppressWarnings(sapply(df_info[rows, -1, drop = FALSE], as.numeric))
    lod_values <- matrix(lod_values, nrow = length(rows))
    apply(lod_values, 2, function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE))
  }
  
  # plate = plates[1]
  for (plate in plates) {
    
    #q500 plates pair FIA and LC barcodes ("A | B"), bile acid plates have one barcode
    barcodes <- trimws(unlist(stringr::str_split(plate, stringr::fixed("|"))))
    
    if (!any(c(calc_barcode, op_barcode) %in% barcodes)) {
      lod_problems <- c(lod_problems, stringr::str_c("Plate ", plate, ": no LOD rows found"))
    }
    
    #consolidate values from flow inject and lcms barcodes, use calc LOD and fall back to LOD from OP
    calc_lod <- lod_max(calc_barcode, barcodes)
    op_lod <- lod_max(op_barcode, barcodes)
    use_op <- is.na(calc_lod) | calc_lod == 0
    calc_lod[use_op] <- op_lod[use_op]
    
    missing_lod <- which(is.na(calc_lod) | calc_lod == 0)
    if (length(missing_lod) > 0) {
      lod_problems <- c(lod_problems, stringr::str_c("Plate ", plate, ": ", length(missing_lod), " analytes with no LOD (",
                                                     stringr::str_c(head(df_report$Name[missing_lod], 5), collapse = ", "),
                                                     if (length(missing_lod) > 5) ", ..." else "", ")"))
      calc_lod[is.na(calc_lod)] <- 0
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
  
  for (problem in lod_problems) { cat(file = stderr(), stringr::str_c("LOD problem: ", problem), "\n") }
  
  cat(file = stderr(), "Function report_template_bg...end", "\n\n")
  return(lod_problems)
}



#---------------------------------------------------------------------

replace_lod <- function(session, input, output, params){
  cat(file = stderr(), "Function replace_lod...", "\n")
  
  params$fixed_lod <- input$fixed_lod
  
  bg_replace_lod <- callr::r_bg(replace_lod_bg, args = list(params), stderr = str_c(params$error_path, "//error_replace_lod.txt"), supervise = TRUE)
  bg_replace_lod$wait()
  print_stderr("error_replace_lod.txt")
  
  if (bg_replace_lod$get_exit_status() != 0) {
    shinyalert("Oops!", "LOD replacement failed, see error_replace_lod.txt", type = "error")
  } else {
    lod_problems <- bg_replace_lod$get_result()
    if (length(lod_problems) > 0) {
      shinyalert("LOD check", stringr::str_c(lod_problems, collapse = "\n"), type = "warning")
    }
  }
  
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
  
  #replace 0 value with "< LOD" in df_data (compare as numbers, text columns can hold "0.000")
  df_data[is_zero(df_data)] <- "< LOD"
  
  #fill df_random with random numbers between 0 and 1
  set.seed(1234)
  df_random <- df_data
  for (i in 1:ncol(df_random)) {
    df_random[[i]] <- runif(nrow(df_random), min = 0.1, max = 1)
  }
  
  
  #map each Plate.bar.code value to its LOD column in df_report by exact barcode match
  #q500 plate groups pair FIA and LC barcodes ("A | B"); standards may list only one barcode of the group
  split_barcodes <- function(x) { trimws(unlist(stringr::str_split(x, stringr::fixed("|")))) }
  plates <- unlist(stringr::str_split(params$plates, ","))
  lod_problems <- c()
  plate_lod_col <- integer(0)
  
  for (row_plate in unique(df_info$Plate.bar.code)) {
    row_barcodes <- split_barcodes(row_plate)
    plate_match <- plates[sapply(plates, function(plate) all(row_barcodes %in% split_barcodes(plate)))]
    lod_col <- integer(0)
    if (length(plate_match) == 1) {
      lod_col_name <- stringr::str_c("LOD_", plate_match, ", uM")
      #column names may be mangled by make.names when read back from the database
      lod_col <- which(colnames(df_report) %in% c(lod_col_name, make.names(lod_col_name)))
    }
    if (length(lod_col) == 1) {
      plate_lod_col[row_plate] <- lod_col
    } else {
      lod_problems <- c(lod_problems, stringr::str_c("Plate ", row_plate, ": no unique LOD column, < LOD values not replaced"))
    }
  }
  
  for (r in 1:nrow(df_data)) {
    #find columns in df_data that contain 'LOD'
    find_lod_cols <- which(grepl("LOD", df_data[r,]), arr.ind = TRUE)
    find_plate_lod_col <- plate_lod_col[df_info$Plate.bar.code[r]]
    #loop through columns and replace values with values from df_report
    if (length(find_lod_cols) >= 1 && !is.na(find_plate_lod_col)) { 
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
  
  for (problem in lod_problems) { cat(file = stderr(), stringr::str_c("LOD problem: ", problem), "\n") }
  
  cat(file = stderr(), "Function replace_lod_bg...end", "\n\n")
  return(lod_problems)
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
  
  if (bg_qc_calc$get_exit_status() != 0) {
    shinyalert("Oops!", "QC calculation failed, see error_qc_calc.txt", type = "error")
  } else {
    qc_problems <- bg_qc_calc$get_result()
    if (length(qc_problems) > 0) {
      shinyalert("QC check", stringr::str_c(qc_problems, collapse = "\n"), type = "warning")
    }
  }
  
  cat(file = stderr(), "Function qc_calc...end", "\n\n")
}


#---------------------------------------------------------------------

qc_calc_bg <- function(params){
  cat(file = stderr(), "Function qc_calc_bg...", "\n")
  source('Shiny_File.R')
  
  #use data before LOD imputation, values < LOD are excluded from QC stats and counted
  df <- read_table_try("data_start", params)
  df_report_template <- read_table_try("Report_template", params)
  df_qc_info <- read_table_try("QC", params)
  
  df_qc_report <- df_report_template[,1:2]
  qc_problems <- c()
  
  #QC level name in config -> Sample.type in WebIDQ export
  qc_list <- c("QC1" = "QC Level 1", "QC2" = "QC Level 2", "QC3" = "QC Level 3")
  
  #expected values must line up with the analytes (compare names without punctuation)
  qc_names <- gsub("[^A-Za-z0-9]", "", colnames(df_qc_info)[3:ncol(df_qc_info)])
  analyte_names <- gsub("[^A-Za-z0-9]", "", make.names(df_report_template$Name))
  if (length(qc_names) != length(analyte_names) || any(qc_names != analyte_names)) {
    stop("QC expected values in config do not match the analyte list")
  }
  
  df_qc <- df[df$Sample.type %in% qc_list,]
  plates <- unique(df_qc$Plate.bar.code)
  
  for (qc in names(qc_list)) {
    qc_level <- df_qc_info[df_qc_info$Expected.values == qc,]
    if (nrow(qc_level) != 1) { stop(stringr::str_c("Config QC sheet needs exactly one ", qc, " row")) }
    qc_std_levels <- as.numeric(unlist(qc_level[3:ncol(qc_level)]))
    df_qc_report[stringr::str_c(qc, " Level (uM)")] <- qc_std_levels
    for (plate in plates) {
      df_plate <- df_qc[df_qc$Plate.bar.code == plate & df_qc$Sample.type == qc_list[[qc]],]
      if (nrow(df_plate) == 0) {
        qc_problems <- c(qc_problems, stringr::str_c(qc, " missing on plate ", plate))
      }
      df_plate <- df_plate[,(ncol(df_plate)-nrow(df_report_template)+1):ncol(df_plate), drop = FALSE]
      #0 or "< LOD" are below LOD, set to NA
      below_lod <- as.data.frame(lapply(df_plate, function(x) {
        x_num <- suppressWarnings(as.numeric(x))
        (!is.na(x) & grepl("LOD", x)) | (!is.na(x_num) & x_num == 0)
      }))
      df_plate <- suppressWarnings(as.data.frame(lapply(df_plate, as.numeric)))
      df_plate[as.matrix(below_lod)] <- NA
      df_mean <- round(colMeans(df_plate, na.rm = TRUE), digits = 5)
      df_mean[is.nan(df_mean)] <- NA
      df_qc_report[stringr::str_c(qc, " ", plate, " Mean (uM)")] <- df_mean
      #calc the %CV from columns of df_plate
      df_acc <- round(100 * (df_mean/qc_std_levels), digits = 1)
      df_qc_report[stringr::str_c(qc, " ", plate, " Accuracy")] <- df_acc
      
      df_cv <- round(100 * (apply(df_plate, 2, sd, na.rm = TRUE) / df_mean), digits = 2)
      df_qc_report[stringr::str_c(qc, " ", plate, " CV")] <- df_cv
      
      df_qc_report[stringr::str_c(qc, " ", plate, " n < LOD")] <- colSums(below_lod)
    }
  }
  
  write_table_try("QC_Report", df_qc_report, params)
  
  for (problem in qc_problems) { cat(file = stderr(), stringr::str_c("QC problem: ", problem), "\n") }
  
  cat(file = stderr(), "Function qc_calc_bg...end", "\n\n")
  return(qc_problems)
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
  
  #normalization needs 2+ plates (norm_select is hidden for single plate data but can keep an old value)
  if (params$plate_number <= 1) { params$norm_select <- "None" }
  
  #stop before processing if the material/settings cannot be processed (e.g. kit QC only materials)
  material_errors <- check_material(params)
  if (length(material_errors) > 0) {
    shinyalert("Cannot process material", stringr::str_c(material_errors, collapse = "\n\n"), type = "error")
    cat(file = stderr(), "Function process_data...end (not processed)", "\n\n")
    return(FALSE)
  }
  
  process_data_calc <- callr::r_bg(process_data_bg, args = list(params), stderr = str_c(params$error_path, "//process_data_calc.txt"), supervise = TRUE)
  process_data_calc$wait()
  print_stderr("process_data_calc.txt")
  
  if (process_data_calc$get_exit_status() != 0) {
    shinyalert("Oops!", stringr::str_c("Processing ", params$material_select, " failed, see process_data_calc.txt"), type = "error")
    cat(file = stderr(), "Function process_data...end (failed)", "\n\n")
    return(FALSE)
  }
  
  norm_problems <- process_data_calc$get_result()
  if (length(norm_problems) > 0) {
    shinyalert("Normalization", stringr::str_c(norm_problems, collapse = "\n"), type = "warning")
  }
  
  #check if params$material_list exists
  if (length(params$material_list) == 0) {
    params$material_list <- input$material_select
  }else{
    params$material_list <- stringr::str_c(params$material_list, ",", input$material_select)
  }
  
  #remember the normalization used for each processed material ("material=norm,...")
  material_norms <- material_norm_list(params)
  material_norms[input$material_select] <- params$norm_select
  params$material_norms <- stringr::str_c(names(material_norms), "=", material_norms, collapse = ",")

  updatePickerInput(session, "pca_plate_select_list", choices = unlist(strsplit(params$plates, ",")))
  updateSelectInput(session, "view_material_select", choices = names(material_norms), selected = input$material_select)

  params <<- params
  
  cat(file = stderr(), "Function process_data...end", "\n\n")
  return(TRUE)
}

#---------------------------------------------------------------------
#processed materials and the normalization used for each, as a named vector c(material = norm)
material_norm_list <- function(params){
  if (length(params$material_norms) == 0 || is.na(params$material_norms) || params$material_norms == "") {
    return(c())
  }
  pairs <- strsplit(unlist(strsplit(params$material_norms, ",")), "=")
  return(stats::setNames(sapply(pairs, `[`, 2), sapply(pairs, `[`, 1)))
}

#---------------------------------------------------------------------
#returns error messages (empty if the selected material and settings can be processed)
check_material <- function(params){
  cat(file = stderr(), "Function check_material...", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbGetQuery(conn, 'SELECT "Material", "Sample.type", "Sample.description", "Plate.bar.code" FROM data_impute')
  RSQLite::dbDisconnect(conn)
  
  material <- params$material_select
  errors <- c()
  
  #study samples = Sample type rows that are not SPQC or reference (NIST, Golden West) samples
  reference <- is_ref_sample(df$Sample.description, "SPQC") | is_ref_sample(df$Sample.description, "NIST") | is_ref_sample(df$Sample.description, "Golden West")
  study <- df$Sample.type == "Sample" & !reference
  study_materials <- unique(df$Material[study])
  
  if (!any(study & df$Material == material)) {
    errors <- c(errors, stringr::str_c("'", material, "' has no study samples, it only contains kit QC or reference samples. ",
                                       "Select a study material: ", stringr::str_c(study_materials, collapse = ", ")))
  } else {
    if (params$spqc_replace == "No") {
      if (!any(grepl("SPQC", df$Sample.description) & df$Material == material)) {
        errors <- c(errors, stringr::str_c("'", material, "' has no SPQC samples. Use 'Replace SPQC?' to use reference samples instead."))
      }
    } else if (!any(is_ref_sample(df$Sample.description, params$spqc_replace))) {
      errors <- c(errors, stringr::str_c("No ", params$spqc_replace, " samples found to replace SPQC."))
    }
    
    if (params$norm_select != "None" && params$plate_number > 1) {
      norm_rows <- is_ref_sample(df$Sample.description, params$norm_select)
      if (params$norm_select == "SPQC") { norm_rows <- norm_rows & df$Material == material }
      if (!any(norm_rows)) {
        errors <- c(errors, stringr::str_c("No ", params$norm_select, " samples found for normalization."))
      } else {
        missing_plates <- setdiff(unique(df$Plate.bar.code[df$Material == material]), unique(df$Plate.bar.code[norm_rows]))
        if (length(missing_plates) > 0) {
          errors <- c(errors, stringr::str_c("No ", params$norm_select, " samples for normalization on plate(s): ", stringr::str_c(missing_plates, collapse = ", ")))
        }
      }
    }
  }
  
  if (isTRUE(params$spqc_filter) && is.na(params$spqc_filter_value)) {
    errors <- c(errors, "SPQC %CV Filter is checked but Max %CV is empty.")
  }
  if (isTRUE(params$missing_filter) && is.na(params$missing_filter_value)) {
    errors <- c(errors, "Max% <LOD filter is checked but Max % <LOD is empty.")
  }
  
  cat(file = stderr(), "Function check_material...end", "\n\n")
  return(errors)
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

  df_material <- df[df$Material == material & df$Sample.type == "Sample",]
  
  if(params$spqc_replace != "No"){
    df_to_add <- df[is_ref_sample(df$Sample.description, params$spqc_replace),]
    if(nrow(df_to_add) == 0){
      cat(file = stderr(), "No replacement SPQC data available...", "\n")
    }
    df_material <- rbind(df_material, df_to_add)
  }
  
  write_table_try(material, df_material, params)
  
  norm_problems <- normalize_data(df, df_report, material, params)
  
  #create table with material SPQC data for report
  spqc_report(df_report, params)
  spqc_summary_report(material, params)
  
  cat(file = stderr(), "Function process_data_bg...end", "\n\n")
  return(norm_problems)
}

#---------------------------------------------------------------------
normalize_data <- function(df, df_report, material, params){
  cat(file = stderr(), "Function normalize_data...", "\n")
  # material = params$material_select
  norm_problems <- c()
  
  if (params$norm_select != "None" & params$plate_number > 1) {
    
    #reference rows (SPQC must be the same material, NIST/Golden West can be any material)
    is_ref <- function(x) {
      ref <- is_ref_sample(x$Sample.description, params$norm_select)
      if (params$norm_select == "SPQC") { ref <- ref & x$Material == material }
      ref
    }
    
    #norm factors use reference values before LOD imputation, values < LOD (or 0) are excluded
    df_start <- read_table_try("data_start", params)
    df_norm <- df_start[is_ref(df_start),]
    analyte_cols <- (ncol(df_norm)-nrow(df_report)+1):ncol(df_norm)
    df_norm_data <- df_norm[, analyte_cols, drop = FALSE]
    below_lod <- as.matrix(as.data.frame(lapply(df_norm_data, function(x) {
      x_num <- suppressWarnings(as.numeric(x))
      (!is.na(x) & grepl("LOD", x)) | (!is.na(x_num) & x_num == 0)
    })))
    df_norm_data <- suppressWarnings(as.data.frame(lapply(df_norm_data, as.numeric)))
    df_norm_data[below_lod] <- NA
    
    df_norm_mean <- df_report[,1:3]
    mean_df_norm_data <- colMeans(df_norm_data, na.rm = TRUE)
    mean_df_norm_data[is.nan(mean_df_norm_data)] <- NA
    df_norm_mean[stringr::str_c("Mean ", params$norm_select, " ", material)] <- round(mean_df_norm_data, digits = 5)
    df_norm_mean[stringr::str_c("CV ", params$norm_select, " ", material)] <- round(100 * (apply(df_norm_data, 2, sd, na.rm = TRUE) / mean_df_norm_data), digits = 2)
    
    #every plate with samples of this material needs reference samples (checked in check_material)
    norm_plates <- unique(df$Plate.bar.code[df$Material == material])
    missing_plates <- setdiff(norm_plates, df_norm$Plate.bar.code)
    if (length(missing_plates) > 0) {
      stop(stringr::str_c("No ", params$norm_select, " samples on plate(s): ", stringr::str_c(missing_plates, collapse = ", ")))
    }
    
    #calc the mean of the reference samples on each plate (analytes x plates)
    plate_means <- sapply(norm_plates, function(plate) {
      plate_mean <- colMeans(df_norm_data[df_norm$Plate.bar.code == plate, , drop = FALSE], na.rm = TRUE)
      plate_mean[is.nan(plate_mean)] <- NA
      plate_mean
    })
    plate_means <- matrix(plate_means, nrow = nrow(df_report))
    total_mean <- rowMeans(plate_means, na.rm = TRUE)
    
    #factor = plate mean / mean of plate means, analytes with no reference values >= LOD on a plate are not normalized (factor 1)
    df_norm_factor <- as.data.frame(plate_means / total_mean)
    colnames(df_norm_factor) <- stringr::str_c(params$norm_select, " ", norm_plates, " ", material)
    for (i in seq_along(norm_plates)) {
      no_factor <- is.na(df_norm_factor[[i]])
      if (any(no_factor)) {
        norm_problems <- c(norm_problems, stringr::str_c("Plate ", norm_plates[i], ": ", sum(no_factor), " analytes not normalized (all ", params$norm_select, " values < LOD)"))
      }
      df_norm_factor[[i]][no_factor] <- 1
    }
    
    #normalize data on each plate
    for(plate in norm_plates){
      #add NIST and Golden West rows since they could be a different material
      df_plate <- df[df$Plate.bar.code == plate & (df$Material == material | is_ref(df)),]
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
    
    for (problem in norm_problems) { cat(file = stderr(), stringr::str_c("Norm problem: ", problem), "\n") }
  }
   
  cat(file = stderr(), "Function normalize_data...end", "\n\n")
  return(norm_problems)
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
    df <- df[is_ref_sample(df$Sample.description, params$spqc_replace),]
  }
  
  
  if(nrow(df) != 0){
    #list of plates
    spqc_plates <- unique(df$Plate.bar.code)

    #isolate data only
    df_all <- df[,(ncol(df)-nrow(df_report)+1):ncol(df)]

    #set df_material to numeric
    df_all <- as.data.frame(lapply(df_all, as.numeric))

    #values < LOD (or 0) before imputation are left out of the SPQC mean and %CV, match rows to data_start by sample info
    df_start <- read_table_try("data_start", params)
    info_key <- function(x) { do.call(paste, c(x[, 1:(ncol(x)-nrow(df_report)), drop = FALSE], sep = "\r")) }
    start_rows <- match(info_key(df), info_key(df_start))
    if (any(is.na(start_rows))) { stop("SPQC rows not found in data_start, cannot find values < LOD") }
    df_all[is_below_lod(df_start[start_rows, (ncol(df_start)-nrow(df_report)+1):ncol(df_start), drop = FALSE])] <- NA

    mean_df_spqc_material <- colMeans(df_all, na.rm = TRUE)
    mean_df_spqc_material[is.nan(mean_df_spqc_material)] <- NA

    #calc the %CV from columns of df_spqc_material (unrounded mean)
    cv_df_spqc_material <- round(100 * (apply(df_all, 2, sd, na.rm = TRUE) / mean_df_spqc_material), digits = 2)
    df_spqc_report[stringr::str_c("Mean ", params$material_select, " SPQC(uM)")] <- round(mean_df_spqc_material, digits = 5)
    df_spqc_report[stringr::str_c("%CV ", params$material_select, " SPQC(uM)")] <- cv_df_spqc_material
    
    for (plate in spqc_plates) {
      df_plate <- df_all[df$Plate.bar.code == plate, , drop = FALSE]
      mean_plate_df_spqc_material <- colMeans(df_plate, na.rm = TRUE)
      mean_plate_df_spqc_material[is.nan(mean_plate_df_spqc_material)] <- NA
      #plate %CV uses the plate mean
      cv_plate_df_spqc_material <- round(100 * (apply(df_plate, 2, sd, na.rm = TRUE) / mean_plate_df_spqc_material), digits = 2)
      df_spqc_report[stringr::str_c("Mean ", plate, " ", params$material_select, " SPQC(uM)")] <- round(mean_plate_df_spqc_material, digits = 5)
      df_spqc_report[stringr::str_c("%CV ", plate, " ", params$material_select, " SPQC(uM)")] <- cv_plate_df_spqc_material
    }
    
    
    write_table_try(stringr::str_c("SPQC_Report_", params$material_select), df_spqc_report, params)
    
  }
  
  cat(file = stderr(), "Function spqc_report...end", "\n\n")
}


#-------------------------------------------------------------------
spqc_summary_report <- function(material, params) {
  cat(file = stderr(), "Function spqc_summary_report...", "\n")
  
  library(dplyr)
  
  df <- read_table_try(stringr::str_c("SPQC_Report_", material), params, check_names = FALSE)
  
  params$spqc_filter
  params$spqc_filter_value
  
  base_cv_cols <- grep("CV", colnames(df), value = TRUE)
  
  #bile acid config has no Class/Sub_platform, use the Class row of the export (one value per analyte)
  if (!("Class" %in% colnames(df))) {
    df_info <- read_table_try("data_info", params)
    class_row <- unlist(df_info[df_info[[1]] == "Class", -1][1,])
    df$Class <- if (length(class_row) == nrow(df)) class_row else ""
  }
  if (!("Sub_platform" %in% colnames(df))) { df$Sub_platform <- "" }
  
  qc_summary <- df[, c("Class", "Sub_platform", base_cv_cols)] |>
    group_by(Class, Sub_platform) |>
    summarise(
      across(everything(), list(
        n    = ~{
          valid <- !is.na(.) & !is.nan(.) & . != 0
          if (isTRUE(params$spqc_filter)) valid <- valid & . <= params$spqc_filter_value
          sum(valid)
        },
        mean = ~round(mean(.[!is.na(.) & !is.nan(.) & . != 0], na.rm = TRUE), 2)
      )),
      .groups = "drop"
    )
  
  # Interleave _n and _mean columns, preserving original CV column order
  paired <- as.vector(sapply(base_cv_cols, function(col) {
    c(paste0(col, "_n"), paste0(col, "_mean"))
  }))
  
  #number of analytes in each Class/Sub_platform (one report row per analyte)
  targeted_counts <- df |>
    select(Class, Sub_platform) |>
    group_by(Class, Sub_platform) |>
    summarise(Targeted = n(), .groups = "drop")
  
  qc_summary <- qc_summary |>
    select(Class, Sub_platform, all_of(paired)) |>
    left_join(targeted_counts, by = c("Class", "Sub_platform")) |>
    relocate(Targeted, .after = Sub_platform)
  
  
  #"%CV <plate> <material> SPQC(uM)" -> "<plate> <material>"
  descriptors <- sub(" SPQC\\(uM\\)$", "", sub("^%CV ", "", base_cv_cols))
  
  rename_map <- setNames(
    c(paste0(base_cv_cols, "_n"), paste0(base_cv_cols, "_mean")),
    c(paste0("n ", descriptors),
      paste0("CV_mean ", descriptors))
  )
  
  qc_summary <- qc_summary |>
    rename(all_of(rename_map))
  
  write_table_try(stringr::str_c("SPQC_Summary_Report_", material), qc_summary, params)
  
  cat(file = stderr(), "Function spqc_summary_report...end", "\n\n")
}



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
    df_material_start <- df_start[df_start$Material == params$material_select,]
    df_material_start <- df_material_start[,(ncol(df_material_start)-nrow(df_report)+1):ncol(df_material_start)]
    #replace 0 values with "< LOD" (compare as numbers, text columns can hold "0.000")
    df_material_start[is_zero(df_material_start)] <- "< LOD"
    
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
    df_material <- df_material[,!(colnames(df_material) %in% analytes_to_remove)]
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
  input_excel_spqc_summary_report <- input$excel_spqc_summary_report
  input_excel_samples_raw <- input$excel_samples_raw
  input_excel_samples_norm <- input$excel_samples_norm
  input_excel_filename <- input$excel_filename
  
  arg_list <- list(params, input_excel_raw_data, input_excel_raw_data_no_indicator, input_excel_impute_data, 
                   input_excel_report, input_excel_qc_report, input_excel_spqc_report, input_excel_spqc_summary_report, 
                   input_excel_samples_raw, input_excel_samples_norm, input_excel_filename)
  
  bg_excel <- callr::r_bg(func = final_excel_bg, args = arg_list, stderr = stringr::str_c(params$error_path, "//error_finalexcel.txt"), supervise = TRUE)
  bg_excel$wait()
  print_stderr("error_finalexcel.txt")
  
  if (bg_excel$get_exit_status() != 0) {
    shinyalert("Oops!", "Creating the Excel file failed, see error_finalexcel.txt", type = "error")
  } else {
    params <<- bg_excel$get_result()
  }
  
  cat(file = stderr(), "function Final_Excel...end", "\n\n")
  removeModal()
}


#----------------------------------------------------------------------------------------
# create final excel documents
final_excel_bg <- function(params, input_excel_raw_data, input_excel_raw_data_no_indicator, input_excel_impute_data, 
                           input_excel_report, input_excel_qc_report, input_excel_spqc_report, input_excel_spqc_summary_report, 
                           input_excel_samples_raw, input_excel_samples_norm, input_excel_filename) {
  cat(file = stderr(), "Function Final_Excel_bg...", "\n")
  
  require(openxlsx)
  source('Shiny_File.R')
  source('Shiny_Data.R')

  df_analytes <- read_table_try("Analytes", params)
  #original export header names, used to restore readable sample info headers
  raw_names <- list_fields("data_raw", params)

  filename <- stringr::str_c(params$data_path, "/", input_excel_filename)
  wb <- createWorkbook()
  
  #write a sample data table with export headers and numeric analyte columns
  write_data_sheet <- function(sheetname, excel_df) {
    sheetname <- excel_add_sheet(wb, sheetname)
    writeData(wb, sheet = sheetname, excel_format(excel_df, df_analytes, raw_names))
  }

  if (input_excel_raw_data) {
    sheetname <- excel_add_sheet(wb, "Raw Data")
    excel_df <- read_table_try("data_raw", params, check_names = FALSE)
    writeData(wb, sheet = sheetname, excel_df)
  }
  
  if (input_excel_raw_data_no_indicator) {
    excel_df <- read_table_try("data_no_indicators", params)
    write_data_sheet("Status Removed", fix_header(excel_df, df_analytes))
  }
  
  if (input_excel_impute_data) {
    excel_df <- read_table_try("data_impute", params)
    write_data_sheet("Imputed Data", fix_header(excel_df, df_analytes))
  }
  
  if (input_excel_report) {
    sheetname <- excel_add_sheet(wb, "Sample Summary")
    excel_df <- read_table_try("Report_template", params, check_names = FALSE)
    #remove 'R_colnames" from excel_df by name
    excel_df <- excel_df[,-which(colnames(excel_df) == "R_colnames")]
    writeData(wb, sheet = sheetname, excel_numeric(excel_df))
  }
  
  if (input_excel_qc_report) {
    sheetname <- excel_add_sheet(wb, "QC Summary")
    excel_df <- read_table_try("QC_Report", params, check_names = FALSE)
    writeData(wb, sheet = sheetname, excel_numeric(excel_df))
  }
  
  materials <- unique(unlist(strsplit(params$material_list, ",")))
  db_tables <- list_tables(params)
  
  if (input_excel_spqc_report) {
    for (material in materials) {
      excel_df <- read_table_try(stringr::str_c("SPQC_Report_", material), params, check_names = FALSE)
      if (material == materials[1]) {
        excel_df_all <- excel_df
      }else{
        if (params$data_source == "BileAcid") {
          excel_df_all <- cbind(excel_df_all, excel_df[,-c(1:3)])
        }else{
        excel_df_all <- cbind(excel_df_all, excel_df[,-c(1:5)])
        }
      }
    }
    sheetname <- excel_add_sheet(wb, "SPQC Summary")
    #remove 'R_colnames" from excel_df by name
    excel_df_all <- excel_df_all[,-which(colnames(excel_df_all) == "R_colnames")]
    writeData(wb, sheet = sheetname, excel_numeric(excel_df_all))
  }
  
  if (input_excel_spqc_summary_report) {
    for (material in materials) {
      excel_df <- read_table_try(stringr::str_c("SPQC_Summary_Report_", material), params, check_names = FALSE)
      if (material == materials[1]) {
        excel_df_all <- excel_df
      }else{
        #Class, Sub_platform and Targeted are the same for every material
        excel_df_all <- cbind(excel_df_all, excel_df[,-c(1:3)])
      }
    }
    sheetname <- excel_add_sheet(wb, "SPQC Summary Report")
    writeData(wb, sheet = sheetname, excel_numeric(excel_df_all))
  }
  
  material_norms <- material_norm_list(params)

  for (material in materials) {

    if (input_excel_samples_raw) {
      if (material %in% db_tables) {
        excel_df <- read_table_try(material, params)
        write_data_sheet(material, fix_header(excel_df, df_analytes))
      }
      if (stringr::str_c("Filtered_", material) %in% db_tables) {
        excel_filter_df <- read_table_try(stringr::str_c("Filtered_", material), params)
        write_data_sheet(stringr::str_c("Filtered_", material), fix_header_filter(excel_df, excel_filter_df, df_analytes))
      }
    }
    
    if (input_excel_samples_norm) {
      #each material uses the normalization it was processed with (params$norm_select is only the last one used)
      norm_select <- material_norms[material]
      if (is.na(norm_select)) { norm_select <- "None" }
      if (stringr::str_c(norm_select, "_Norm_", material) %in% db_tables) {
        excel_df <- read_table_try(stringr::str_c(norm_select, "_Norm_", material), params)
        write_data_sheet(stringr::str_c(norm_select, "_Norm_", material), fix_header(excel_df, df_analytes))
      }
      if (stringr::str_c(norm_select, "_Filtered_Norm_", material) %in% db_tables) {
        excel_filter_df <- read_table_try(stringr::str_c(norm_select, "_Filtered_Norm_", material), params)
        write_data_sheet(stringr::str_c(norm_select, "_Filtered_Norm_", material), fix_header_filter(excel_df, excel_filter_df, df_analytes))
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
  #load archived params into the global env so the rest of the app sees them
  params <<- loadRData(stringr::str_c(database_dir, "/params"))
  
  temp_files <- list.files(database_dir)
  if (length(temp_files) == 0) {
    temp_files <- ""
  }
  
  cat(file = stderr(), stringr::str_c("files in database_dir:  ", temp_files), "\n")

  #point params at the unzipped database (archive may come from another location)
  params$database_dir <<- database_dir
  params$database_path <<- stringr::str_c(database_dir, "/", basename(params$database_path))
  cat(file = stderr(), stringr::str_c("database_path --->", params$database_path), "\n")

  #re-point output paths to the archive's directory (archive is saved in data_path), keep existing contents
  params$data_path <<- archive_path
  params$error_path <<- create_dir_only(stringr::str_c(archive_path, "Error/"))
  params$plot_path <<- create_dir_only(stringr::str_c(archive_path, "Plots/"))
  params$backup_path <<- create_dir_only(stringr::str_c(archive_path, "Backup/"))
  cat(file = stderr(), stringr::str_c("data_path --->", params$data_path), "\n")

  cat(file = stderr(), "Function load_archive_file...end", "\n\n")
  return(archive_zip)
}





