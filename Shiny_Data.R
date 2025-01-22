cat(file = stderr(), "Shiny_Data.R", "\n")

#---------------------------------------------------------------------
load_data_file <- function(session, input, output, params){
  cat(file = stderr(), "Function load_data_file", "\n")
  showModal(modalDialog("Loading data...", footer = NULL))
  
  
  data_sfb <- parseFilePaths(volumes, input$sfb_data_file)
  data_path <- str_extract(data_sfb$datapath, "^/.*/")
  params$data_file <- basename(data_sfb$datapath)
  
  cat(file = stderr(), str_c("loading data file(s) from ", data_path[1]), "\n")
  
  bg_load_data <- callr::r_bg(func = load_data_bg, args = list(data_sfb, params), stderr = str_c(params$error_path, "//error_load_data.txt"), supervise = TRUE)
  bg_load_data$wait()
  print_stderr("error_load_data.txt")
  
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
  
  df <- data.table::fread(file = data_sfb$datapath, header = TRUE, skip=1, stringsAsFactors = FALSE, sep = "\t", fill=TRUE)
  #save(df, file="zdfloadunknowndata")    #  load(file="zdfloadunknowndata") 
  
  write_table_try("data_raw", df, params)
  
  write_table_try("params", params, params)
  #save(params, file="params")
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), "function load_unkown_data_bg...end", "\n\n")
}
#---------------------------------------------------------------------

remove_status_cols <- function(session, input, output, params){
  cat(file = stderr(), "Function remove_status_cols...", "\n")
  
  bg_status_col <- callr::r_bg(remove_status_cols_bg, args = list(params), stderr = str_c(params$error_path, "//error_removestatuscols.txt"), supervise = TRUE)
  bg_status_col$wait()
  print_stderr("error_removestatuscols.txt")

  cat(file = stderr(), "Function remove_status_cols...end", "\n")
  
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
  
  cat(file = stderr(), "Function remove_status_cols_bg...end", "\n")
  
}

#---------------------------------------------------------------------

remove_indicators <- function(session, input, output, params){
  cat(file = stderr(), "Function remove_indicators...", "\n")
  
  bg_indicators <- callr::r_bg(remove_indicators_bg, args = list(params), stderr = str_c(params$error_path, "//error_removeindicators.txt"), supervise = TRUE)
  bg_indicators$wait()
  print_stderr("error_removeindicators.txt")
  
  cat(file = stderr(), "Function remove_indicators...end", "\n")
  
}


#---------------------------------------------------------------------

remove_indicators_bg <- function(params){
  cat(file = stderr(), "Function remove_indicators_bg...", "\n")
  source('Shiny_File.R')
  
  df <- read_table_try("data_status", params)
  df_analytes <- read_table_try("analytes", params)  

  #find colnumber for df_analytes$Abbreviation[1]
  col_num <- which(colnames(df) == df_analytes$Abbreviation[1])
  last_col <- col_num + nrow(df_analytes) - 1
  
  df_indicators <- df[,(last_col + 1):ncol(df)]
  df <- df[,(1:last_col)]
  
  #find first row with a number 
  data_rows <- which(grepl("^[0-9]", df_indicators[[1]]))
  df_indicators <- df_indicators[data_rows,]
  
  write_table_try("data_indicators", df_indicators, params)
  write_table_try("data_no_indicators", df, params)
  
  cat(file = stderr(), "Function remove_indicators_bg...end", "\n")
  
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
  
  cat(file = stderr(), "Function separate_data...end", "\n")
}


#---------------------------------------------------------------------

separate_data_bg <- function(params){
  cat(file = stderr(), "Function separate_data_bg...", "\n")
  source('Shiny_File.R')
  
  df <- read_table_try("data_no_indicators", params)

  #find row number for first row with number or letters
  data_rows <- which(grepl("^[0-9]", df[[1]]))
  df_data <- df[data_rows,]
  df_info <- df[-data_rows,]
  
  df_analytes <- read_table_try("analytes", params)  
  
  #find colnumber for df_analytes$Abbreviation[1]
  col_num <- which(colnames(df) == df_analytes$Abbreviation[1])
  df_info <- df_info[,(col_num - 1):ncol(df_info)]

  plates <- unique(df_data$Plate.bar.code)
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
  
  cat(file = stderr(), "Function separate_data_bg...end", "\n")

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
  

  
  for (plate in plates) {
    
    #subset df_info for rows that the first column contains plate
    df_plate <- df_info[grep(plate, df_info[[1]]),]
    df_lod <- df_plate[grep("LOD", df_plate[[1]]),]
    
    #if any value in row 1 = 0 then the change the value to the value in row 2
    for (i in 2:ncol(df_lod)) {
      if (df_lod[1,i] == 0) { df_lod[1,i] <- df_lod[2,i] }
    }
  
    col_name <- stringr::str_c("LOD_", plate, ", uM")
    df_report_colnames <- c(df_report_colnames, col_name)
    
    #create new column called from col_name in df_report
    df_report[[col_name]] <- t(df_lod[1,2:ncol(df_lod)])
      
  }
  
  
  df_lloq <- df_info[grep("LLOQ", df_info[[1]]),]
  df_uloq <- df_info[grep("ULOQ", df_info[[1]]),]
    
  #get column maxs from df_uloq
  uloq_max <- apply(df_uloq[2:ncol(df_uloq)], 2, max)
  lloq_min <- apply(df_lloq[2:ncol(df_lloq)], 2, min)
  
  col_name <- stringr::str_c("Lowest CS, uM")
  df_report_colnames <- c(df_report_colnames, col_name)
  df_report[[col_name]] <- lloq_min
    
  col_name <- stringr::str_c("Highest CS, uM")
  df_report_colnames <- c(df_report_colnames, col_name)
  df_report[[col_name]] <- uloq_max
    
  colnames(df_report) <- df_report_colnames
  
  write_table_try("Report", df_report, params)
  
  cat(file = stderr(), "Function report_template_bg...end", "\n")
}



#---------------------------------------------------------------------

replace_lod <- function(session, input, output, params){
  cat(file = stderr(), "Function replace_lod...", "\n")
  
  params$fixed_lod <- input$fixed_lod
  
  bg_replace_lod <- callr::r_bg(replace_lod_bg, args = list(params), stderr = str_c(params$error_path, "//error_replace_lod.txt"), supervise = TRUE)
  bg_replace_lod$wait()
  print_stderr("error_replace_lod.txt")
  
  params <<- params
  
  cat(file = stderr(), "Function replace_lod...end", "\n")
}

#---------------------------------------------------------------------

replace_lod_bg <- function(params){
  cat(file = stderr(), "Function replace_lod_bg...", "\n")
  
  source('Shiny_File.R')
  
  df <- read_table_try("data_start", params)
  df_report <- read_table_try("Report", params)
  
  #sort df by plate.bar.code and reindex
  df <- df[order(df$Plate.bar.code),]
  rownames(df) <- NULL
  
  df_info <- df[,1:(ncol(df)-nrow(df_report))]
  df_data <- df[,(ncol(df)-nrow(df_report)+1):ncol(df) ]
  
  #find rows in df_data that contain 'LOD'
  
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
    #find column in df_report that contains 'LOD' and plate
    find_plate_lod_col <- which(grepl(plate, colnames(df_report)), arr.ind = TRUE )
    #loop through columns and replace values with values from df_report
    if (length(find_lod_cols) >=1 ) { 
      for (c in find_lod_cols) {
        if (params$fixed_lod){
          df_data[r,c] <- as.numeric(df_report[c,find_plate_lod_col], digits = 3)
        }else {
          df_data[r,c] <- round(df_random[r,c] * as.numeric(df_report[c,find_plate_lod_col]), digits = 3)
      }
    }
   }
  }
  
  df_final <- cbind(df_info, df_data)
  
  write_table_try("data_impute", df_final, params)
  
  cat(file = stderr(), "Function replace_lod_bg...end", "\n")
}



#---------------------------------------------------------------------

spqc_calc <- function(session, input, output, params){
  cat(file = stderr(), "Function spqc_calc...", "\n")
  
  bg_spqc_calc <- callr::r_bg(spqc_calc_bg, args = list(params), stderr = str_c(params$error_path, "//error_spqc_calc.txt"), supervise = TRUE)
  bg_spqc_calc$wait()
  print_stderr("error_spqc_calc.txt")
  
  cat(file = stderr(), "Function spqc_calc...end", "\n")
}

#---------------------------------------------------------------------

spqc_calc_bg <- function(params){
  cat(file = stderr(), "Function spqc_calc_bg...", "\n")
  source('Shiny_File.R')
  
  df <- read_table_try("data_impute", params)
  df_report <- read_table_try("Report", params)
  
  #subset of data with "SPQC" in Sample.description
  df_spqc <- df[grep("SPQC", df$Sample.description),]
  plates <- unique(df_spqc$Plate.bar.code)
  materials <- unique(df_spqc$Material)
  

  for (material in materials) {
    df_material <- df_spqc[grep(material, df_spqc$Material),]
    df_material <- df_material[,(ncol(df_material)-nrow(df_report)+1):ncol(df_material)]
    #set df_material to numeric
    df_material <- as.data.frame(lapply(df_material, as.numeric))
    df_mean <- round(colMeans(df_material, na.rm = TRUE), digits = 3)
    #calc the %CV from columns of df_material
    df_cv <- round(100 * (apply(df_material, 2, sd, na.rm = TRUE) / df_mean), digits = 2)
    df_report[stringr::str_c("Average ", material, " SPQC(uM)")] <- df_mean
    df_report[stringr::str_c("%CV ", material, " SPQC(uM)")] <- df_cv
  }

  write_table_try("Report", df_report, params)
  
  cat(file = stderr(), "Function spqc_calc_bg...end", "\n")
}


#---------------------------------------------------------------------

qc_calc <- function(session, input, output, params){
  cat(file = stderr(), "Function qc_calc...", "\n")
  
  bg_qc_calc <- callr::r_bg(qc_calc_bg, args = list(params), stderr = str_c(params$error_path, "//error_qc_calc.txt"), supervise = TRUE)
  bg_qc_calc$wait()
  print_stderr("error_qc_calc.txt")
  
  cat(file = stderr(), "Function qc_calc...end", "\n")
}


#---------------------------------------------------------------------

qc_calc_bg <- function(params){
  cat(file = stderr(), "Function qc_calc_bg...", "\n")
  source('Shiny_File.R')
  
  df <- read_table_try("data_impute", params)
  df_report <- read_table_try("Report", params)
  df_qc_info <- read_table_try("QC", params)
  
  df_qc_report <- df_report[,1:2]
  
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
      df_plate <- df_plate[,(ncol(df_plate)-nrow(df_report)+1):ncol(df_plate)]
      #set df_plate to numeric
      df_plate <- as.data.frame(lapply(df_plate, as.numeric))
      df_mean <- round(colMeans(df_plate, na.rm = TRUE), digits = 3)
      df_qc_report[stringr::str_c(qc, " ", plate, " Mean (uM)")] <- df_mean
      #calc the %CV from columns of df_plate
      df_acc <- round(100 * (df_mean/qc_std_levels), digits = 1)
      df_qc_report[stringr::str_c(qc, " ", plate, " Accuracy")] <- df_acc
    }
  }
  
  write_table_try("QC_Report", df_qc_report, params)
  
  cat(file = stderr(), "Function qc_calc_bg...end", "\n")
}




#---------------------------------------------------------------------

load_archive_file <- function(session, input, output){
  cat(file = stderr(), "Function load_archive_file...", "\n")
  #showModal(modalDialog("Loading archive file...", footer = NULL))
  
  if(site_user == "dpmsr") {
    archive_sfb <- parseFilePaths(volumes, input$sfb_archive_file)
    archive_path <- str_extract(archive_sfb$datapath, "^/.*/")
    cat(file = stderr(), stringr::str_c("archive_path --->", archive_path), "\n")
    archive_zip <- archive_sfb$datapath
    archive_name <- basename(archive_sfb$datapath)
  }else{
    archive_zip <-input$sfb_archive_customer_file$datapath
    cat(file = stderr(), stringr::str_c("archive_zip --->", archive_zip), "\n")
    archive_path <- str_extract(archive_zip, "^/.*/")
    cat(file = stderr(), stringr::str_c("archive_path --->", archive_path), "\n")
    database_dir <- stringr::str_c(database_dir, "/", format(Sys.time(), "%Y%m%d%H%M%S"))
  }
  
  #save(archive_sfb, file = "testarchive_sfb")
  # load(file = "testarchive_sfb")
  

  create_dir(database_dir)
  
  cat(file = stderr(), stringr::str_c("unzip file to database_dir:  ", database_dir), "\n")
  
  utils::unzip(zipfile = archive_zip, exdir = database_dir)
  
  temp_files <- list.files(database_dir)
  if (length(temp_files) == 0) {
    temp_files <- ""
  }
  
  cat(file = stderr(), stringr::str_c("files in database_dir:  ", temp_files), "\n")
  
  if(Sys.getenv("USER") == "erik") {
    system("chmod -R 777 /home/erik/Shiny_DPDP/database")
  }
  
  # section will load params file from db if not present
  zip_files <- list.files(path = database_dir, recursive = TRUE) 
  for (file_name in zip_files){
    if (tools::file_ext(file_name) == "db"){
      break
    }
  }
  
  database_path <- stringr::str_c(database_dir, "/", file_name)
  
  #load params file
  if (file.exists(stringr::str_c(database_dir, "/params"))){
    load(file=stringr::str_c(database_dir, "/params"))
  }else{
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path) 
    params <- RSQLite::dbReadTable(conn, "params")
    RSQLite::dbDisconnect(conn)
  }
  
  params$database_dir <- database_dir
  params$database_path <- stringr::str_c(database_dir, "/", file_name)
  
  #check params dir's and update if needed
  params <- archive_update_app(session, input, output, params, archive_path)
  
  params <<- params

  #removeModal()
  cat(file = stderr(), "Function load_archive_file...end", "\n")
  return(archive_zip)
}





#--------------------------------------------------------
meta_data <- function(table_string, params){
  cat(file = stderr(), "Function meta_data...", "\n")
  showModal(modalDialog("Gathering meta data...", footer = NULL))
  
  table_name <- str_c("precursor_", table_string)
  error_file <- str_c("error_", table_string, "_meta.txt")
  
  bg_meta <- callr::r_bg(func = meta_data_bg, args = list(table_name, table_string, params), stderr = str_c(params$error_path, "//", error_file), supervise = TRUE)
  bg_meta$wait()
  print_stderr(error_file)
  
  params <<- param_load_from_database()
  
  cat(file = stderr(), "Function meta_data...end", "\n\n")
  removeModal()
}

#--------------------------------------------------------
meta_data_bg <- function(table_name, data_format, params){
  cat(file = stderr(), "Function meta_data bg...", "\n")
  source('Shiny_File.R')
  
  #. table_name <- "precursor_start"; data_format <- "start"
  
  df <- read_table_try(table_name, params)
  
  precursor_name <- stringr::str_c("meta_precursor_", data_format)
  peptide_name <- stringr::str_c("meta_peptide_", data_format)
  protein_name <- stringr::str_c("meta_protein_", data_format)
  
  params[[precursor_name]] <- nrow(df)
  
  if (data_format == "raw") {
    params[[peptide_name]] <- length(unique(df$EG.ModifiedSequence))
    params[[protein_name]] <- length(unique(df$PG.ProteinAccessions))
  } else {
    params[[peptide_name]] <- length(unique(df$Sequence))
    params[[protein_name]] <- length(unique(df$Accession))
  }
  
  if (params$ptm) {
    phos_which <- which(grepl(params$ptm_grep, df$Sequence))
    df_other <- df[-phos_which,]
    df_phos <- df[phos_which,]
    df_phos_local <- df_phos[which(df_phos$Local2 == "Y"),]

    params$ptm_precursors <- nrow(df_phos)
    params$ptm_total <- length(unique(df_phos$Sequence))
    params$ptm_total_local <- length(unique(df_phos_local$Sequence))

    df_phos <- df_phos[,(ncol(df_phos)- params$sample_number+1):ncol(df_phos)]
    df_other <- df_other[,(ncol(df_other)- params$sample_number+1):ncol(df_other)]
    df_phos_local <- df_phos_local[,(ncol(df_phos_local)- params$sample_number+1):ncol(df_phos_local)]

    params$ptm_enrich <- round(sum(df_phos, na.rm = TRUE) / (sum(df_other, na.rm = TRUE) + sum(df_phos, na.rm = TRUE)), 2)
    params$ptm_enrich_local <- round(sum(df_phos_local, na.rm = TRUE) / (sum(df_other, na.rm = TRUE) + sum(df_phos, na.rm = TRUE)), 2)
    
    #--calc indiv phos sites
    
    # save(df, file = "zz999b")  #  load(file = "zz999b")
    
    test_df <- df[phos_which,] |> dplyr::select(contains(c('Accession', 'Genes', 'Local', 'Protein_PTM_Loc')))
    test_df$Local2 <- NULL
    test_df$Accession <- gsub(";.*$", "", test_df$Accession)
    test_df$Genes <- gsub(";.*$", "", test_df$Genes)
    test_df$Local <- gsub(";.*$", "", test_df$Local)
    test_df$Protein_PTM_Loc <- gsub(";.*$", "", test_df$Protein_PTM_Loc)
    
    find_mult <- which(grepl(",", df$Protein_PTM_Loc))
    new_df <- test_df[-find_mult,]
    new_df$Phos_ID <- paste(new_df$Accession, "_", new_df$Genes, "_", new_df$Protein_PTM_Loc, sep = "")
    
    for(r in find_mult) {
      sites <- unlist(stringr::str_split(test_df$Protein_PTM_Loc[r], ","))
      site_local <- unlist(stringr::str_split(test_df$Local[r], ","))
      for (i in (1:length(sites))){
        new_df <- rbind(new_df, c(test_df$Accession[r], test_df$Genes[r], site_local[i], test_df$Protein_PTM_Loc[r], 
                                  paste(test_df$Accession[r], "_", test_df$Genes[r], "_", sites[i], sep = "")))
      }
    }
    
    params$phos_site_unique_all <- length(unique(new_df$Phos_ID))
    params$phos_site_unique_local <- length(unique(new_df[new_df$Local > params$ptm_local,]$Phos_ID))
    
    write_table_try("phos_sites", new_df, params)
    
  }
  
  write_table_try("params", params, params)
  
  cat(file = stderr(), "Function meta_data bg...end", "\n\n")
  
}

#--------------------------------------------------------

#--------------------------------------------------------
impute_meta_data <- function(){
  cat(file = stderr(), "Function impute_meta_data...", "\n")
  showModal(modalDialog("Setting imputation parameters, calculating meta data, setting Duke impute intensity table...", footer = NULL))
  
  bg_meta <- callr::r_bg(func = impute_meta_data_bg, args = list("precursor_filter", params), stderr = str_c(params$error_path, "//impute_meta_data.txt"), supervise = TRUE)
  bg_meta$wait()
  print_stderr("impute_meta_data.txt")
  
  params <<- param_load_from_database()
  
  removeModal()
  cat(file = stderr(), "Function impute_meta_data...end", "\n\n")
}

#--------------------------------------------------------
impute_meta_data_bg <- function(table_name, params){
  cat(file = stderr(), "Function impute_meta_data bg...", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)
  df_groups <- RSQLite::dbReadTable(conn, "sample_groups")
  
  #filter data if imputation stats based on modification data only
  # if (params$impute_ptm) {
  #   df <- df[grep(params$impute_ptm_grep, df$Sequence, ignore.case = TRUE),]
  # }
  
  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
  df <- log(df,2)
  
  total_na <- list()
  for (i in 1:nrow(df_groups)) {
    temp_df <- df[df_groups$start[i]:df_groups$end[i]] 
    count_na <- sum(is.na(temp_df))
    total_na <- c(total_na, count_na)
    
    temp_df$average <- apply(temp_df, 1, FUN = function(x) {mean(x, na.rm = TRUE)})
    temp_df$sd <- apply(temp_df, 1, FUN = function(x) {sd(x, na.rm = TRUE)})
    temp_df$bin <- dplyr::ntile(temp_df$average, 20)  
    
    impute_bin_table <- subset(temp_df, !is.na(sd)) |> dplyr::group_by(bin) |> dplyr::summarize(min = min(average), max = max(average), sd = mean(sd))
    for (x in 1:(nrow(impute_bin_table) - 1)) {impute_bin_table$max[x] <- impute_bin_table$min[x + 1]}
    impute_bin_table$max[nrow(impute_bin_table)] <- 100
    impute_bin_table$min2 <- impute_bin_table$min
    impute_bin_table$min2[1] <- 0
    impute_bin_table <- impute_bin_table[-21,]
    
    impute_bin_table_name <- stringr::str_c("impute_bin_", df_groups$Group[i])
    RSQLite::dbWriteTable(conn, impute_bin_table_name, impute_bin_table, overwrite = TRUE)
  } 
  
  params$meta_impute_na <- toString(total_na)
  
  #create random table for imputation
  set.seed(123)
  rand_df = data.frame(matrix(runif(ncol(df) * nrow(df), min = -1, max = 1), ncol = ncol(df)))
  
  RSQLite::dbWriteTable(conn, "random", rand_df, overwrite = TRUE)
  RSQLite::dbWriteTable(conn, "params", params, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "Function impute_meta_data bg...end", "\n\n")
  
}

#--------------------------------------------------------








#----------------------------------------------------------------------------------------------------------
protein_to_peptide <- function(){
  cat(file = stderr(), "protein_to_peptide", "\n")
  protein <- dpmsr_set$data$data_raw_protein
  peptide_groups <- dpmsr_set$data$data_raw_peptide
  
  #add columns to preserve peptide to protein links
  peptide_groups$Proteins <- peptide_groups$Protein.Accessions
  peptide_groups$Unique <- peptide_groups$Quan.Info
  peptide_groups$Unique[peptide_groups$Unique == ""] <- "Unique"
  
  #protein raw has all confidence proteins - limit to high master
  protein_master <- subset(protein, Master %in% ("IsMasterProtein"))
  protein_high_master <- subset(protein_master, Protein.FDR.Confidence.Combined %in% ("High"))
  master_accessions <- protein_high_master$Accession 
  
  #PD will label which proteins get the razor peptides, 
  protein_razor <- subset(protein, Number.of.Razor.Peptides > 0)
  razor_accessions <- protein_razor$Accession
  
  #gather peptides that are shared
  peptide_shared <- subset(peptide_groups,  Quan.Info %in% ("NotUnique"))
  #gather peptides that have no quant values
  peptide_noquan <- subset(peptide_groups,  Quan.Info %in% ("NoQuanValues"))
  #gather unique peptides
  peptide_unique <- peptide_groups[peptide_groups$Quan.Info == "",]
  
  #expand shared peptides so that each protein has peptides listed separately
  peptide_shared_expand  <- peptide_shared %>% 
    mutate(Master.Protein.Accessions = strsplit(as.character(Master.Protein.Accessions), "; ", fixed = TRUE)) %>% 
    unnest(Master.Protein.Accessions)
  
  if (dpmsr_set$x$peptides_to_use == "Razor") {
    #reduce df to only peptides that have proteins that PD lists as having "razor" peptides
    peptide_shared_expand <- subset(peptide_shared_expand, Master.Protein.Accessions %in% razor_accessions )
    #gather df for razor proteins
    protein_razor_lookup <- protein_razor %>% dplyr::select(Accession, Description, Number.of.Peptides, 
                                                            Coverage.in.Percent, Number.of.Unique.Peptides, Number.of.Razor.Peptides)
    #add columns from protein to df
    peptide_shared_expand <- merge(peptide_shared_expand, protein_razor_lookup, by.x = "Master.Protein.Accessions", by.y = "Accession")
    #create column to check for duplicated peptides
    peptide_shared_expand$duplicated_test <- str_c(peptide_shared_expand$Annotated.Sequence, peptide_shared_expand$Modifications)
    peptide_shared_expand <- peptide_shared_expand[order(peptide_shared_expand$duplicated_test, -peptide_shared_expand$Number.of.Peptides, 
                                                         peptide_shared_expand$Coverage.in.Percent, 
                                                         -peptide_shared_expand$Number.of.Razor.Peptides),]
    #remove duplicated peptides
    peptide_final <- peptide_shared_expand[!duplicated(peptide_shared_expand$duplicated_test),]
    peptide_final$Master.Protein.Descriptions <- peptide_final$Description
    #remove extra columns
    peptide_final <- peptide_final[1:(ncol(peptide_groups))]
    #combine unique and razor/shared peptides
    peptide_final <- rbind(peptide_unique, peptide_final)
  }else if (dpmsr_set$x$peptides_to_use == "Shared") {
    peptide_final <- rbind(peptide_unique, peptide_shared_expand)
  }else{
    peptide_final <- peptide_unique
  }
  
  peptide_final <- peptide_final[order(peptide_final$Master.Protein.Accessions, peptide_final$Sequence),]
  peptide_out <- peptide_final |> dplyr::select(Confidence, Master.Protein.Accessions, Master.Protein.Descriptions, Proteins, 
                                                Sequence, Modifications, Unique,
                                                contains('RT.in.min.by.Search.Engine.'), 
                                                starts_with('mz.in.Da.by.Search.Engine.'), 
                                                contains('Charge.by.Search.Engine.'), 
                                                contains('Percolator.SVM'), 
                                                contains("Percolator.q.Value"), contains("Abundance.F"))
  
  
  if (ncol(peptide_out) != (12 + dpmsr_set$y$sample_number))
  {
    shinyalert("Oops!", str_c("Number of columns extracted is not as expected ", ncol(peptide_out), "/", (10 + dpmsr_set$y$sample_number)), type = "error")  
  }
  
  colnames(peptide_out)[1:12] <- c("Confidence", "Accession", "Description", "All.Proteins", "Sequence", "Modifications", "Unique", "Retention.Time","Da","mz", "Ion.Score", "q-Value")
  peptide_out <- subset(peptide_out, Accession %in% master_accessions )
  Simple_Excel(peptide_out, "Protein_Peptide_Raw", str_c(dpmsr_set$file$extra_prefix,"_ProteinPeptide_to_Peptide_Raw.xlsx", collapse = " "))
  return(peptide_out)
}

#----------------------------------------------------------------------------------------
prepare_data <- function(session, input, output, params) {  #function(data_type, data_file_path){
  cat(file = stderr(), "Function prepare_data...", "\n")
  showModal(modalDialog("Preparing Data...", footer = NULL))
  
  if (params$raw_data_format == "protein_peptide") {
    cat(file = stderr(), "prepare data_type 1", "\n")
    protein_to_peptide()
    protein_to_protein()
    params$current_data_format <- "peptide"
  }else if (params$raw_data_format == "protein" & params$data_source == "SP") {
    cat(file = stderr(), "prepare data_type 2", "\n")
    bg_sp_protein_to_protein <- callr::r_bg(func = sp_protein_to_protein_bg, args = list(params), stderr = str_c(params$error_path, "//error_sp_protein_to_protein.txt"), supervise = TRUE)
    bg_sp_protein_to_protein$wait()
    print_stderr("error_sp_protein_to_protein.txt")
    params$current_data_format <- "protein"
  }else if (params$raw_data_format == "peptide") {
    cat(file = stderr(), "prepare data_type 3", "\n")
    peptide_to_peptide()
    params$current_data_format <- "peptide"
  }else if (params$raw_data_format == "precursor" & params$data_output == "Protein" & params$data_source == "SP") {
    cat(file = stderr(), "prepare data_type 4", "\n")
    bg_precursor_to_precursor <- callr::r_bg(func = precursor_to_precursor_bg, args = list(params), stderr = str_c(params$error_path, "//error_preparedata.txt"), supervise = TRUE)
    bg_precursor_to_precursor$wait()
    print_stderr("error_preparedata.txt")
    params$current_data_format <- "precursor"
  }else if (params$raw_data_format == "precursor" & params$data_output == "Peptide" & params$data_source == "SP") {
    cat(file = stderr(), "prepare data_type 5", "\n")
    bg_precursor_to_precursor_ptm <- callr::r_bg(func = precursor_to_precursor_ptm_bg, args = list(params), stderr = str_c(params$error_path, "//error_preparedata.txt"), supervise = TRUE)
    bg_precursor_to_precursor_ptm$wait()
    print_stderr("error_preparedata.txt")
    params$current_data_format <- "precursor"
  }else if (params$raw_data_format == "precursor" & params$data_output == "Protein" & params$data_source == "PD") {
    cat(file = stderr(), "prepare data_type 6", "\n")
    bg_precursor_to_precursor_PD <- callr::r_bg(func = precursor_to_precursor_PD_bg, args = list(params), stderr = str_c(params$error_path, "//error_preparedata.txt"), supervise = TRUE)
    bg_precursor_to_precursor_PD$wait()
    print_stderr("error_preparedata.txt")
    params$current_data_format <- "precursor"
  }else if (params$raw_data_format == "fragment") {
    cat(file = stderr(), "prepare data_type 7", "\n")
    peptide_to_peptide()
    params$current_data_format <- "fragment"
  }else{
    shinyalert("Oops!", "Invalid input output in design file", type = "error") 
  }
  
  if (params$use_isoform) {
    isoform_to_isoform()
  }
  
  write_table_try("params", params, params)
  params <<- params
  
  cat(file = stderr(), "Function prepare_data...end", "\n\n")
  removeModal()
}


#----------------------------------------------------------------------------------------
precursor_to_precursor_PD_bg <- function(params){
  cat(file = stderr(), "Function precursor_to_precursor_PD_bg", "\n")
  source("Shiny_Rollup.R")
  source("Shiny_File.R")
  #  load(file="zdfloadunknowndata")
  
  df <- read_table_try("precursor_raw", params)
  
  df_colnames <- c("Accession", "Description", "Sequence", "Modifications", "PrecursorId")  
  n_col <- length(df_colnames)
  
  df <- df |> dplyr::select(contains('Master.Protein.Accessions'), contains('Master.Protein.Descriptions'), 'Sequence', contains('Modifications'), 'Charge', contains("Abundance."))
  
  colnames(df)[1:n_col] <- df_colnames 
  
  #add Name column
  add_name <- gsub(".*OS=(.+)[OG].*", "\\1", df$Description)
  add_name <- gsub(" OX.+$", "", add_name)
  add_name <- trimws(add_name, which=c("right"))
  df <- df |> tibble::add_column(Name = add_name, .before = "Sequence")
  n_col <- n_col + 1
  
  #add Gene column
  add_gene <- stringr::str_extract(df$Description, "GN=\\w*")
  add_gene <- gsub("GN=", "", add_gene)
  df <- df |> tibble::add_column(Genes = add_gene, .before = "Sequence")
  n_col <- n_col + 1
  
  #create Precusor.ID
  df$PrecursorId <- paste(df$Sequence, ".", df$PrecursorId, sep = "")
  
  if (ncol(df) != (n_col + params$sample_number))
  {
    cat(file = stderr(), "Number of columns extracted is not as expected", "\n")
  }
  
  
  df[(n_col + 1):ncol(df)] <- as.data.frame(lapply(df[(n_col + 1):ncol(df)], as.numeric))
  
  write_table_try("precursor_start", df, params)
  
  cat(file = stderr(), "precursor_to_precursor_PD_bg complete", "\n\n")
}

#----------------------------------------------------------------------------------------
precursor_to_precursor_bg <- function(params){
  cat(file = stderr(), "Function precursor_to_precursor_bg", "\n")
  source("Shiny_Rollup.R")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, "precursor_raw")
  
  
  df_colnames <- c("Accession", "Description", "Name", "Genes", "Organisms", "Sequence", "PrecursorId", "PeptidePosition")  
  n_col <- length(df_colnames)
  
  df <- df |> dplyr::select(contains('ProteinAccessions'), contains('ProteinDescriptions'), contains('ProteinNames'), contains('Genes'), contains('Organisms'),
                            contains('ModifiedSequence'), contains('PrecursorId'), contains('PeptidePosition'),
                            contains("TotalQuantity"))
  
  if (ncol(df) != (n_col + params$sample_number))
  {
    shinyalert::shinyalert("Oops!", "Number of columns extracted is not as expected", type = "error") 
    cat(file = stderr(), "Number of columns extracted is not as expected", "\n")
  }
  
  colnames(df)[1:n_col] <- df_colnames  
  
  # set "Filtered" in TotalQuantity to NA
  df[df ==  "Filtered"] <- NA
  df[df ==  0] <- NA
  df[(n_col + 1):ncol(df)] <- as.data.frame(lapply(df[(n_col + 1):ncol(df)], as.numeric))
  
  df$Description <- stringr::str_c(df$Description, ", org=", df$Organisms) 
  df$Organisms <- NULL
  
  RSQLite::dbWriteTable(conn, "precursor_start", df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "precursor_to_precursor_bg complete", "\n\n")
}

#----------------------------------------------------------------------------------------
precursor_to_precursor_ptm_bg <- function(params){
  cat(file = stderr(), "Function precursor_to_precursor_bg", "\n")
  source("Shiny_Rollup.R")
  source("Shiny_Data.R")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, "precursor_raw")
  
  df_phos_prob <- df |> dplyr::select(contains('PTMProbabilities..Phospho')) 
  
  df_colnames <- c("Accession", "Description", "Name", "Genes", "Organisms", "Sequence", "PrecursorId", "PeptidePosition", "ProteinPTMLocations")  
  n_col <- length(df_colnames)
  
  df <- df |> dplyr::select(contains('ProteinAccessions'), contains('ProteinDescriptions'), contains('ProteinNames'), contains('Genes'), contains('Organisms'),
                            contains('ModifiedSequence'), contains('PrecursorId'), contains('PeptidePosition'),contains('ProteinPTMLocations'),
                            contains("TotalQuantity"))
  
  if (ncol(df) != (n_col + params$sample_number))
  {
    shinyalert::shinyalert("Oops!", "Number of columns extracted is not as expected", type = "error") 
    cat(file = stderr(), "Number of columns extracted is not as expected", "\n")
  }
  
  colnames(df)[1:n_col] <- df_colnames  
  
  # set "Filtered" in TotalQuantity to NA
  df[df ==  "Filtered"] <- NA
  df[df ==  0] <- NA
  df[(n_col + 1):ncol(df)] <- as.data.frame(lapply(df[(n_col + 1):ncol(df)], as.numeric))
  
  df$Description <- stringr::str_c(df$Description, ", org=", df$Organisms) 
  df$Organisms <- NULL
  
  phos_which <- which(grepl("Phospho", df$Sequence))
  df_phos <- df[phos_which,]
  df_phos_prob <- df_phos_prob[phos_which,]
  df_other <- df[-phos_which,]
  
  local_df <- data.frame(localize_summary(df_phos, df_phos_prob))
  df_phos <- tibble::add_column(df_phos, "Protein_PTM_Loc" = local_df$Protein_PTM_Loc, .after="PrecursorId")
  df_phos <- tibble::add_column(df_phos, "PTM_Loc" = local_df$PTM_Loc, .after="PrecursorId")
  df_phos <- tibble::add_column(df_phos, "Local2" = local_df$Local2, .after="PrecursorId")
  df_phos <- tibble::add_column(df_phos, "Local" = local_df$Local, .after="PrecursorId")

  df_other <- tibble::add_column(df_other, "Protein_PTM_Loc"= "" , .after="PrecursorId")
  df_other <- tibble::add_column(df_other, "PTM_Loc" = "", .after="PrecursorId")
  df_other <- tibble::add_column(df_other, "Local2"= "" , .after="PrecursorId")
  df_other <- tibble::add_column(df_other, "Local" = "", .after="PrecursorId")
  
  df <- rbind(df_phos, df_other)
  
  RSQLite::dbWriteTable(conn, "precursor_start", df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "precursor_to_precursor_bg complete", "\n\n")
}

#----------------------------------------------------------------------------------------
localize_summary <- function(df_phos, df_phos_prob){
  cat(file = stderr(), "Function localize_summary...", "\n")
  
  require(foreach)
  require(doParallel)
  cores <- detectCores()
  cl <- makeCluster(cores - 2)
  registerDoParallel(cl)
  
  #Step 1 consolicate localization into one list of max local for each position
  #create df of just probabilities
  df_phos_prob[df_phos_prob=="Filtered"] <- ""
  
  df_local <- data.frame(cbind(df_phos$Sequence, df_phos$PeptidePosition, df_phos$ProteinPTMLocations))
  colnames(df_local) <- c("ModSequence", "PeptidePosition", "ProteinPTMLocations")
  
  df_local$Stripped <- gsub("\\[.*?\\]", "", df_local$ModSequence)
  df_local$Stripped <- gsub("_", "", df_local$Stripped)
  
  #Step 2 reduce modified sequence to STY with phos residue marked with *
  df_local$phos_seq <- gsub("\\[Phospho \\(STY\\)\\]", "*", df_local$ModSequence)
  df_local$phos_seq <- gsub("_", "", df_local$phos_seq)
  df_local$phos_seq <- gsub("\\[.*?\\]", "", df_local$phos_seq)
  df_local$phos_seq <- gsub("[^STY*]", "", df_local$phos_seq)
  
  
  #new step
  df_local$ModSequence2 <- df_local$ModSequence
  df_local$ModSequence2 <- gsub("S\\[Phospho \\(STY\\)\\]", "s", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("T\\[Phospho \\(STY\\)\\]", "t", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("Y\\[Phospho \\(STY\\)\\]", "y", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("\\[.*?\\]", "", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("_", "", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("\\[.*?\\]", "", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("_", "", df_local$ModSequence2)
  
  
  #new step
  df_local$PTM_Loc <- ""
  
  for (r in (1:nrow(df_local))) {
    find_s <- unlist(stringr::str_locate_all(df_local$ModSequence2[r], "s"))
    find_t <- unlist(stringr::str_locate_all(df_local$ModSequence2[r], "t"))
    find_y <- unlist(stringr::str_locate_all(df_local$ModSequence2[r], "y"))
    
    
    if (length(find_s) > 0) {
      find_s <- unlist(stringr::str_split(paste("S", find_s, collapse = " ", sep = ""), pattern=" "))
      find_s <- find_s[1:(length(find_s)/2)]
    }else{
      find_s <- ""
    }
    
    
    if (length(find_t) > 0) {
      find_t <- unlist(stringr::str_split(paste("T", find_t, collapse = " ", sep = ""), pattern=" "))
      find_t <- find_t[1:(length(find_t)/2)]
    }else{
      find_t <- ""
    }
    
    if (length(find_y) > 0) {
      find_y <- unlist(stringr::str_split(paste("Y", find_y, collapse = " ", sep = ""), pattern=" "))
      find_y <- find_y[1:(length(find_y)/2)]
    }else{
      find_y <- ""
    }
    
    final_all <- c(find_s, find_t, find_y)
    final_all <- final_all[final_all != ""]
    final_all <- paste(final_all, collapse = ",", sep = ",")
    df_local$PTM_Loc[r] <- final_all  
  }
  
  
  #new step
  df_local$Protein_PTM_Loc <- gsub("([CM][0-9]+)", "", df_local$ProteinPTMLocations) 
  df_local$Protein_PTM_Loc <- gsub("\\(,", "\\(",  df_local$Protein_PTM_Loc) 
  df_local$Protein_PTM_Loc <- gsub("\\),", "\\)",  df_local$Protein_PTM_Loc) 
  df_local$Protein_PTM_Loc <- gsub(",\\(", "\\(",  df_local$Protein_PTM_Loc) 
  df_local$Protein_PTM_Loc <- gsub(",\\)", "\\)",  df_local$Protein_PTM_Loc) 
  df_local$Protein_PTM_Loc <- gsub("\\(", "",  df_local$Protein_PTM_Loc)  
  df_local$Protein_PTM_Loc <- gsub("\\)", "",  df_local$Protein_PTM_Loc)  
  df_local$Protein_PTM_Loc <- gsub(",,,,", ",",  df_local$Protein_PTM_Loc)  
  df_local$Protein_PTM_Loc <- gsub(",,,", ",",  df_local$Protein_PTM_Loc) 
  df_local$Protein_PTM_Loc <- gsub(",,", ",",  df_local$Protein_PTM_Loc)  
  
  # determines residue location for phos on sequence reduced to STY
  parallel_result1 <- foreach(r = 1:nrow(df_local), .combine = c) %dopar% {
    phos_count <- stringr::str_count(df_local$phos_seq[r], "\\*")
    temp_list <- c()
    if (phos_count >= 1) {
      phos_loc <- stringr::str_locate_all(df_local$phos_seq[r], "\\*")
      for(c in (1:phos_count)){
        temp_list <- c(temp_list, (phos_loc[[1]][[c]] - c))
      }
    }
    list(temp_list)
  }
  
  df_local$phos_res <- parallel_result1
  
  
  #consolidates probabilities for each sample and takes the highest prob for each residue
  parallel_result2 <- foreach(r = 1:nrow(df_phos_prob), .combine = c) %dopar% {
    first_value <- FALSE
    for (c in (1:ncol(df_phos_prob))) {
      if (!first_value) { 
        temp1 <- unlist(stringr::str_split(df_phos_prob[[r,c]], ";")) |> as.numeric() 
        if (!is.na(temp1[[1]])) {
          first_value <- TRUE
        }
      }else {
        temp2 <- unlist(stringr::str_split(df_phos_prob[[r,c]], ";")) |> as.numeric()
        if (!is.na(temp2[[1]])) {
          temp1 <- pmax(temp1, temp2)
        }
      }
    }
    list(temp1)
  }
  
  df_local$pr <- parallel_result2
  
  #mark as localized or not
  parallel_result3 <- foreach(r = 1:nrow(df_local), .combine = rbind) %dopar% {
    prob <- unlist(df_local$pr[r])
    residue <- unlist(df_local$phos_res[r])
    local <- c()
    for (c in length(residue)) {
      local <- c(local, prob[residue]) 
    }
    if (max(local) >= 0.75) {
      if (min(local) >= 0.75) {
        local2 <- "Y"
      } else {
        local2 <- "P"
      }
    }else {
      local2 <- "N"
    }
    list(local, local2)
  }
  
  parallel_result3 <- data.frame(parallel_result3)
  colnames(parallel_result3) <- c("Local", "Local2")
  row.names(parallel_result3) <- NULL
  
  numlist_to_string <- function(x) {
    return(toString(paste(unlist(x$Local) |> as.character() |> paste(collapse = ","))))
  }
  
  numlist_to_string2 <- function(x) {
    return(toString(paste(unlist(x$Local2) |> as.character() |> paste(collapse = ","))))
  }
  
  parallel_result3$Local <- apply(parallel_result3, 1, numlist_to_string)
  parallel_result3$Local2 <- apply(parallel_result3, 1, numlist_to_string2)
  
  parallel_result3$Protein_PTM_Loc <- df_local$Protein_PTM_Loc
  parallel_result3$PTM_Loc <-  df_local$PTM_Loc
  
  stopCluster(cl) 
  cat(file = stderr(), "Function localize_summary...end", "\n")
  return(parallel_result3) 
}


#----------------------------------------------------------------------------------------
localize_summary_trash <- function(df){
  cat(file = stderr(), "Function localize_summary...", "\n")
  #--
  require(foreach)
  require(doParallel)
  cores <- detectCores()
  cl <- makeCluster(cores - 2)
  registerDoParallel(cl)
  
  df[df=="Filtered"] <- ""
  df[is.na(df)] <- ""
  
  parallel_result <- foreach(c = colnames(df), .combine = cbind) %dopar% {
    #for (c in colnames(df)){
    test <- df[[c]]
    find_rows <- which(stringr::str_detect(test, ";"))
    for (r in find_rows) {
      if (grepl(";", test[r])) {
        test[r] <- max(strsplit(test[r], ";") |> unlist() |> as.numeric())
      }
    }
    test
  }
  stopCluster(cl) 
  
  parallel_result <- data.frame(parallel_result)
  
  max_result  <- apply(parallel_result, 1, FUN = max, na.rm = TRUE)
  
  cat(file = stderr(), "Function localize_summary...end", "\n")
  return(max_result)  
}


#----------------------------------------------------------------------------------------
sp_protein_to_protein_bg <- function(params){
  cat(file = stderr(), "Function sp_protein_to_protein_bg...", "\n")
  source("Shiny_File.R")
  
  df <- read_table_try("protein_raw", params)
  
  df_colnames <- c("Accession", "Description", "Name", "Genes", "Organisms", "Precursors")  
  n_col <- length(df_colnames)
  
  df <- df |> dplyr::select(contains('ProteinAccessions'), contains('ProteinDescriptions'), contains('ProteinNames'), contains('Genes'), contains('Organisms'),
                            contains('Experiment.Wide'), contains("Quantity"))
  
  if (ncol(df) != (n_col + params$sample_number))
  {
    shinyalert("Oops!", "Number of columns extracted is not as expected", type = "error") 
    cat(file = stderr(), "Number of columns extracted is not as expected", "\n")
  }
  
  colnames(df)[1:n_col] <- df_colnames  
  
  # set "Filtered" in TotalQuantity to NA
  df[df ==  "Filtered"] <- NA
  df[df ==  0] <- NA
  df[(n_col + 1):ncol(df)] <- as.data.frame(lapply(df[(n_col + 1):ncol(df)], as.numeric))
  
  df$Description <- stringr::str_c(df$Description, ", org=", df$Organisms) 
  df$Organisms <- NULL
  
  #remove any row with an NA
  df <- df[complete.cases(df),]
  
  write_table_try("protein_impute", df, params)
  
  cat(file = stderr(), "Function sp_protein_to_protein_bg...end ", "\n")
}

#----------------------------------------------------------------------------------------
protein_to_protein <- function(){
  cat(file = stderr(), "protein_to_protein", "\n")
  protein <- dpmsr_set$data$data_raw_protein
  
  if (dpmsr_set$x$data_source == "PD") {
    cat(file = stderr(), "data type  -> PD", "\n")
    protein <- subset(protein, Master %in% ("IsMasterProtein"))
    protein <- subset(protein, Protein.FDR.Confidence.Combined %in% ("High"))
    protein_out <- protein %>% dplyr::select(Accession, Description, Number.of.Protein.Unique.Peptides, 
                                             contains("Abundance"), -contains("Abundance.Count"))
    colnames(protein_out)[1:3] <- c("Accession", "Description", "Unique.Peptides")
  }
  else if (dpmsr_set$x$data_source == "SP") { 
    cat(file = stderr(), "data type  -> SP", "\n")
    protein_out <- protein %>% dplyr::select(contains("ProteinAccessions"), contains("ProteinDescriptions"), 
                                             contains("ProteinNames"), contains("Genes"), contains("Quantity"))
    precursor_col <- protein %>% dplyr::select(contains("Precursors"))
    precursor_col$average <- round(rowMeans(precursor_col), 1)
    
    protein_out <- protein_out %>% add_column(precursor_col$average, .after = "PG.Genes")
    colnames(protein_out)[1:5] <- c("Accession", "Description", "ProteinName", "Gene", "PrecursorsAvg")
    
    #in case missing values reported as NaN
    protein_out[, 5:ncol(protein_out)] <- sapply(protein_out[, 5:ncol(protein_out)], as.numeric)
    protein_out[5:ncol(protein_out)][protein_out[5:ncol(protein_out)] == "NaN"] <- 0
    
  }else {
    cat(file = stderr(), "protein_to_protein data source not recognized", "\n")
  }
  Simple_Excel(protein_out, "Protein_Protein_Raw", str_c(dpmsr_set$file$extra_prefix, "_Protein_Protein_Raw", "_Protein_to_Protein_Raw.xlsx", collapse = " "))
  return(protein_out)
}

#----------------------------------------------------------------------------------------
peptide_to_peptide <- function(){
  cat(file = stderr(), "peptide_to_peptide", "\n")
  peptide_groups <- dpmsr_set$data$data_raw_peptide
  
  if (dpmsr_set$x$data_source == "PD") {
    cat(file = stderr(), "peptide_to_peptide, PD data", "\n")
    peptide_out <- peptide_groups %>% dplyr::select(Confidence, Master.Protein.Accessions, Master.Protein.Descriptions, 
                                                    Sequence, Modifications, 
                                                    (starts_with("Positions.in.") & ends_with("Proteins")), 
                                                    (starts_with("Modifications.in.") & ends_with("Proteins")), 
                                                    contains('RT.in.min.by.Search.Engine.'), 
                                                    contains('Percolator.SVM'),  
                                                    contains("Percolator.q.Value"), contains("Abundance.F"))
    
    if (ncol(peptide_out) != (10 + dpmsr_set$y$sample_number))
    {
      shinyalert("Oops!", "Number of columns extracted is not as expected", type = "error")  
    }
    
    colnames(peptide_out)[1:10] <- c("Confidence", "Accession", "Description", "Sequence", "Modifications", "PositionMaster", "ModificationMaster",
                                     "Retention.Time", "SVM.Score", "q-Value")
    peptide_out <- subset(peptide_out, Confidence %in% ("High"))
    
  }else if (dpmsr_set$x$data_source == "SP") {
    cat(file = stderr(), "peptide_to_peptide, SP data", "\n")
    
    peptide_out <- peptide_groups %>% dplyr::select(contains('ProteinAccessions'), contains('ProteinDescriptions'), contains('Genes'), 
                                                    contains('ModifiedSequence'), contains('PeptidePosition'),
                                                    contains('ProteinPTMLocations'),
                                                    contains("TotalQuantity"))
    
    if (ncol(peptide_out) != (6 + dpmsr_set$y$sample_number))
    {
      shinyalert("Oops!", "Number of columns extracted is not as expected", type = "error")  
    }
    
    colnames(peptide_out)[1:6] <- c("Accession", "Description", "Genes", "Sequence", "PeptidePosition", "PTMLocations")  
  }
  
  # set "Filtered" in TotalQuantity to NA
  peptide_out[peptide_out ==  "Filtered"] <- NA
  peptide_out[8:ncol(peptide_out)] <- as.data.frame(lapply(peptide_out[8:ncol(peptide_out)], as.numeric))
  
  Simple_Excel(peptide_out, "Peptide_Peptide_Raw",  str_c(dpmsr_set$file$extra_prefix, "_Peptide_to_Peptide_Raw.xlsx", collapse = " "))
  cat(file = stderr(), "peptide_to_peptide complete", "\n")
  return(peptide_out)
}


#----------------------------------------------------------------------------------------
precursor_PTM_to_precursor_PTM <- function(){
  cat(file = stderr(), "precursor_PTM_to_precursor_PTM", "\n")
  
  precursor_groups <- dpmsr_set$data$data_raw_precursor
  precursor_colnames <- c("Accession", "Description", "Genes", "Organisms", "Stripped_Seq", "Sequence", "PrecursorId", "PeptidePosition", "PTMLocations") 
  n_col <- length(precursor_colnames)
  
  precursor_out <- precursor_groups %>% dplyr::select(contains('ProteinAccessions'), contains('ProteinDescriptions'), contains('Genes'), contains('Organisms'),
                                                      contains('StrippedSequence'), contains('ModifiedSequence'), contains('PrecursorId'), contains('PeptidePosition'),
                                                      contains('ProteinPTMLocations'), contains("TotalQuantity"))
  
  if (ncol(precursor_out) != (n_col + dpmsr_set$y$sample_number))
  {
    shinyalert("Oops!", "Number of columns extracted is not as expected", type = "error")  
  }
  
  colnames(precursor_out)[1:n_col] <- precursor_colnames  
  
  # set "Filtered" in TotalQuantity to NA
  precursor_out[precursor_out ==  "Filtered"] <- NA
  precursor_out[(n_col + 1):ncol(precursor_out)] <- as.data.frame(lapply(precursor_out[(n_col + 1):ncol(precursor_out)], as.numeric))
  
  precursor_out$Description <- str_c(precursor_out$Description, ", org=", precursor_out$Organisms) 
  precursor_out$Organisms <- NULL
  
  Simple_Excel(precursor_out, "precursor_precursor_Raw",  str_c(dpmsr_set$file$extra_prefix, "_Precursor_to_Precursor_Raw.xlsx", collapse = " "))
  cat(file = stderr(), "precursor_to_precursor complete", "\n")
  return(precursor_out)
}


#Top.Apex.RT.in.min,
#----------------------------------------------------------------------------------------
isoform_to_isoform <- function(){
  cat(file = stderr(), "isoform_to_isoform", "\n")
  
  if (is.null(dpmsr_set$data$data_raw_isoform)) {
    cat(file = stderr(), "isoform text file NOT found", "\n")
    shinyalert("Oops!", "Isoform data not imported.  TMT datasets do not automatically export isoform data.", type = "error")
  }
  else {
    cat(file = stderr(), "isoform text file found", "\n")
    peptide_groups <- dpmsr_set$data$data_raw_isoform
    peptide_out <- try(peptide_groups %>% dplyr::select(contains("Confidence.by"), Master.Protein.Accessions, Master.Protein.Descriptions, 
                                                        Sequence, Modifications, 
                                                        (starts_with("Positions.in.") & ends_with("Proteins")), 
                                                        (starts_with("Modifications.in.") & ends_with("Proteins")), 
                                                        Top.Apex.RT.in.min, 
                                                        contains('Percolator.SVM'),  
                                                        contains("Percolator.q.Value"), contains("Abundance.F")))
    if (class(peptide_out) == 'try-error') {
      cat(file = stderr(), "column select error - retry", "\n")
      peptide_out <- peptide_groups %>% dplyr::select(contains("Confidence.by"), Master.Protein.Accessions, Master.Protein.Descriptions,
                                                      Sequence, Modifications, 
                                                      (starts_with("Positions.in.") & ends_with("Proteins")), 
                                                      (starts_with("Modifications.in.") & ends_with("Proteins")), 
                                                      contains("Positions."),
                                                      contains('RT.in.min.by.'), 
                                                      contains('Percolator.SVM'), 
                                                      contains("Percolator.q.Value"), contains("Abundance.F"))
    }
    
    
    cat(file = stderr(), str_c("There are ", ncol(peptide_out) - dpmsr_set$y$sample_number, "/10 info columns"), "\n")
    
    if ((ncol(peptide_out) - dpmsr_set$y$sample_number) < 10) {
      cat(file = stderr(), "If this is TMT phos you will need to manually export the isoform text file, load the correct layout file before export", "\n")
    }
    
    
    if (ncol(peptide_out) != (10 + dpmsr_set$y$sample_number))
    {
      shinyalert("Oops!", "Number of isoform columns extracted is not as expected", type = "error")  
    }
    
    colnames(peptide_out)[1:10] <- c("Confidence", "Accession", "Description", "Sequence", "Modifications", "PositionMaster", "ModificationMaster",
                                     "Retention.Time", "SVM.Score", "q-Value")
    
    peptide_out <- subset(peptide_out, Confidence %in% ("High"))
    Simple_Excel_bg(peptide_out, "Protein_Peptide_Raw", str_c(dpmsr_set$file$extra_prefix, "_Isoform_to_Isoform_Raw.xlsx", collapse = " "))
    cat(file = stderr(), "isoform_to_isoform complete", "\n")
    return(peptide_out)
  }
}


#----------------------------------------------------------------------------------------
order_rename_columns <- function(){
  cat(file = stderr(), "Function order_rename_columns...", "\n")
  showModal(modalDialog("Order and rename Data...", footer = NULL))
  
  if (params$raw_data_format == "precursor") {
    cat(file = stderr(), "Function order_rename_columns...precursor", "\n")
    bg_order <- callr::r_bg(func = order_rename_columns_bg, args = list("precursor_start", params), stderr = str_c(params$error_path, "//error_orderrename.txt"), supervise = TRUE)
    bg_order$wait()
    print_stderr("error_orderrename.txt")
    bg_order_list <- bg_order$get_result()
    params$info_col_precursor <<- bg_order_list[[1]]
    Sample_ID_alert <- bg_order_list[[2]]
  }
  
  if (Sample_ID_alert) {
    shinyalert("Oops!", "Sample ID order does not match sample list!", type = "error")
  }
  
  cat(file = stderr(), "order_rename_columns end", "\n\n")
  removeModal()
}


# Rearrange columns if raw data is psm, PD does not organize
order_rename_columns_bg <- function(table_name, params) {
  cat(file = stderr(), "Function order_columns_bg...", "\n")
  source('Shiny_Data.R')
  source('Shiny_Rollup.R')
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)
  design <- RSQLite::dbReadTable(conn, "design")
  
  #save(df, file = "testdf"); save(design, file="testdesign")
  #  load(file = "testdf"); load(file="testdesign")
  
  #confirm sample ID's
  Sample_ID_alert <- check_sample_id(df, design, params)
  
  info_columns <- ncol(df) - params$sample_number
  annotate_df <- df[, 1:(ncol(df) - params$sample_number)]
  df <- df[, (ncol(df) - params$sample_number + 1):ncol(df)]
  df <- df[, (design$Raw_Order)]
  
  #make sure data is numeric
  df <- dplyr::mutate_all(df, function(x) as.numeric(as.character(x)))
  
  colnames(df) <- design$Header1
  df <- cbind(annotate_df, df)
  
  #save copy of raw peptide (from precursor start)
  if (params$ptm) {
    raw_peptide_list <- collapse_precursor_ptm_raw(df, params$sample_number, info_columns = 0, stats = FALSE, add_miss = FALSE, df_missing = NULL, params)
    raw_peptide <- raw_peptide_list[[1]]
  }else{
    raw_peptide <- collapse_precursor_raw(df, info_columns = 0, stats = FALSE, params)
  }
  
  RSQLite::dbWriteTable(conn, "raw_peptide", raw_peptide, overwrite = TRUE)
  RSQLite::dbWriteTable(conn, "precursor_start", df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "order columns end", "\n")
  return(list(info_columns, Sample_ID_alert))
}

#----------------------------------------------------------------------------------------
check_sample_id <- function(df, design, params) {
  cat(file = stderr(), "Function check_sample_id...", "\n")
  
  sample_ids <- design[order(design$Raw_Order),]$ID
  sample_ids <- gsub("_.+", "", sample_ids)
  
  if (params$data_source == "SP") {
    if (params$raw_data_format == "Protein") {
      test_data <- colnames(df |> dplyr::select(contains("Quantity")))
    }else if (params$raw_data_format == "Precursor" || params$raw_data_format == "Precursor_PTM") {
      test_data <- colnames(df |> dplyr::select(contains("TotalQuantity")))
    }else {
      test_data <- colnames(df |> dplyr::select(contains("Quantity")))
    }
  }else{
    test_data <- colnames(df |> dplyr::select(contains("Abundance.F")))
  }
  
  
  cat(file = stderr(), "check_sample_id...1", "\n")
  Sample_ID_alert <- FALSE
  for (i in 1:length(sample_ids)) {
    confirm_id <- grepl(sample_ids[i], test_data[i])
    if (!confirm_id) {
      cat(file = stderr(), stringr::str_c("count=", i, "  ", sample_ids[i], " vs ", test_data[i]), "\n")
      Sample_ID_alert <- TRUE
      #shinyalert("Oops!", "Sample ID order does not match sample list!", type = "error")
    }
  }
  
  cat(file = stderr(), "Function check_sample_id...end", "\n")
  return(Sample_ID_alert)
}
