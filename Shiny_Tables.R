cat(file = stderr(), "Shiny_Tables.R", "\n")

#------------------------------------------------------------------------
#load design table
create_config_table <- function(session, input, output){
  cat(file = stderr(), "Function create_config_table", "\n")
  #showModal(modalDialog("Creating design table...", footer = NULL))
  
  bg_config_table <- callr::r_bg(create_config_table_bg, args = list(params$database_path), stderr = str_c(params$error_path, "//error_config_table.txt"), supervise = TRUE)
  bg_config_table$wait()
  print_stderr("error_config_table.txt")
  
  config_DT <- bg_config_table$get_result()
  output$config_table <- renderRHandsontable(config_DT)
  
  cat(file = stderr(), "Function create_config_table...end", "\n\n")
  #removeModal()
}

#--------------------------------

create_config_table_bg <- function(database_path){
  cat(file = stderr(), "Function create_config_table_bg", "\n")
  
  #get raw_datadata
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
  config <- RSQLite::dbReadTable(conn, "Analytes", config)
  RSQLite::dbDisconnect(conn)
    
  config <- config |> dplyr::mutate_all(as.character)
  
  config_DT <- rhandsontable::rhandsontable(config, readOnly = TRUE, rowHeaders = NULL, digits = 0) #|> 
  #  rhandsontable::hot_col(col = 'ID', halign = 'htCenter') |>
  #  rhandsontable::hot_col(col = 'Replicate', halign = 'htCenter') |>
  # rhandsontable::hot_col(col = 'Label', halign = 'htCenter') |>
  #  rhandsontable::hot_col(col = 'Group', halign = 'htCenter')
  
  cat(file = stderr(), "Function create_config_table_bg...end", "\n")
  return(config_DT)   
}

#------------------------------------------------------------------------
#load design table
create_stats_design_table <- function(session, input, output){
  cat(file = stderr(), "Function create_stats_design_table", "\n")
  #showModal(modalDialog("Creating stats design table...", footer = NULL))
  
  bg_designtable <- callr::r_bg(create_stats_design_table_bg, args = list(params$database_path), stderr = str_c(params$error_path, "//error_statsdesigntable.txt"), supervise = TRUE)
  bg_designtable$wait()
  print_stderr("error_statsdesigntable.txt")
  
  design_DT <- bg_designtable$get_result()
  output$stats_design_table2 <-  renderRHandsontable(design_DT)

  cat(file = stderr(), "Function create_stats_design_table...end", "\n")
  #removeModal()
}

#--------------------------------

create_stats_design_table_bg <- function(database_path){
  cat(file = stderr(), "Function build_design_table_bg", "\n")
  
  #get design data
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
  design <- RSQLite::dbReadTable(conn, "design", design)
  RSQLite::dbDisconnect(conn)

  design <- design[, c("ID", "Replicate", "Label", "Group")] |> dplyr::mutate_all(as.character)
  colnames(design) <- c("ID", "Replicate", "Label", "Group")
  
  design_DT <- rhandsontable::rhandsontable(design, readOnly = TRUE, rowHeaders = NULL, digits = 0) |> 
    rhandsontable::hot_col(col = 'ID', halign = 'htCenter') |>
    rhandsontable::hot_col(col = 'Replicate', halign = 'htCenter') |>
    rhandsontable::hot_col(col = 'Label', halign = 'htCenter') |>
    rhandsontable::hot_col(col = 'Group', halign = 'htCenter')
  
  cat(file = stderr(), "Function create_stats_design_table_bg...end", "\n")
  return(design_DT)   
}

#--------------------------------------------------------------------------------------------------------------------


#load design table
create_data_table <- function(session, input, output, params, table_name){
  cat(file = stderr(), "Function create_data_table...", "\n")
  require('DT')
  require('shinyjs')
  
  bg_datatable <- callr::r_bg(create_data_table_bg, args = list(params, table_name), stderr = stringr::str_c(params$error_path, "//error_datatable.txt"), supervise = TRUE)
  bg_datatable$wait()
  print_stderr("error_datatable.txt")

  
  data_table_DT <- bg_datatable$get_result()
  output$data_table <- DT::renderDataTable(data_table_DT)
  
  cat(file = stderr(), "Function create_data_table...end", "\n")
}

#--------------------------------

create_data_table_bg <- function(params, table_name){
  cat(file = stderr(), "Function create_data_table_bg...", "\n")
  source("Shiny_File.R")
  
  #get design data
  df <- read_table_try(table_name, params)
  
  options <- list(
    selection = 'single',
    plugins = "ellipsis",
    autoWidth = TRUE,
    scrollX = TRUE,
    scrollY = 500,
    scrollCollapse = TRUE,
    columnDefs = list(
      list(
        targets = c(0,1),
        visibile = TRUE,
        "width" = '5',
        className = 'dt-center'
      ),
      list(
        targets = c(2,3),
        visibile = TRUE,
        "width" = '20',
        className = 'dt-center'
      ),
      list(
       targets = c(4),
       render = DT::JS("DataTable.render.ellipsis(5, false )")
      )
      # list(
      #   targets = c(16),
      #   width = '20',
      #   render = DT::JS(
      #     "function(data, type, row, meta) {",
      #     "return type === 'display' && data.length > 20 ?",
      #     "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
      #     "}"
      #   )
      # )
    ),
    ordering = TRUE,
    orderClasses = TRUE,
    fixedColumns = list(leftColumns = 1),
    pageLength = 25,
    lengthMenu = c(10, 50, 100, 200)
  )
  
  
  data_table_DT <-  DT::datatable(df, rownames = FALSE, options = options)
  
  #disable text wrapping in data_table_DT
  data_table_DT <- data_table_DT |> DT::formatStyle(names(df), whiteSpace = 'nowrap')
  
  cat(file = stderr(), "Function create_data_table_bg...end", "\n")
  
  
  
  dat <- data.frame(
    A = c("fnufnufroufrcnoonfrncacfnouafc", "fanunfrpn frnpncfrurnucfrnupfenc"),
    B = c("DZDOPCDNAL DKODKPODPOKKPODZKPO", "AZERTYUIOPQSDFGHJKLMWXCVBN")
  )
  
  data_table_DT <- DT::datatable(
    dat, 
    plugins = "ellipsis",
    options = list(
      columnDefs = list(list(
        targets = c(1,2),
        render = DT::JS("$.fn.dataTable.render.ellipsis( 17, false )")
      ))
    )
  )
  
  
  
  return(data_table_DT)   
}

#--------------------------------------------------------------------------
#load design table
create_cv_table <- function(session, input, output, params){
  cat(file = stderr(), "Function create_cv_table", "\n")
  
  bg_cvtable <- callr::r_bg(create_cv_table_bg, args = list(params), stderr = str_c(params$error_path, "//error_cvtable.txt"), supervise = TRUE)
  bg_cvtable$wait()
  print_stderr("error_cvtable.txt")
  
  cv_DT <- bg_cvtable$get_result()
  output$qc_cv_table <-  renderRHandsontable(cv_DT)
  
  cat(file = stderr(), "Function create_cv_table...end", "\n")
}

#--------------------------------

create_cv_table_bg <- function(params){
  cat(file = stderr(), "Function create_cv_table_bg", "\n")
  
  #get design data
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  cv_table <- RSQLite::dbReadTable(conn, "summary_cv")
  RSQLite::dbDisconnect(conn)
  
  cv_table[,-1] <- round(cv_table[,-1], digits = 1)
  cv_table <- cv_table |> dplyr::mutate_all(as.character)
  
  data_CV <- rhandsontable::rhandsontable(cv_table, readOnly = TRUE, rowHeaders = NULL, digits = 0)

  cat(file = stderr(), "Function create_cv_table_bg...end", "\n")
  
  return(data_CV)   
}

#------------------------------------------------------------------------------------------------------------------

protein_table <- function(df, start_sample_col, sample_number, spqc_number){
  cat(file = stderr(), "Function protein_table...", "\n")
  require('DT')
  source('Shiny_Misc_Functions.R')
  
  # pval_cols <- which(stringr::str_detect(colnames(df), "pval"))
  # cv_cols <- which(stringr::str_detect(colnames(df), "CV"))
  # fc_cols <- which(stringr::str_detect(colnames(df), "FC"))
  # mf_cols <- which(stringr::str_detect(colnames(df), "mf"))
  #stat_col <- ncol(df) - 1
  
  #df <- df |> dplyr::mutate(dplyr::across(start_sample_col:(start_sample_col + sample_number + spqc_number - 1), round, 1))
  # df <- df |> dplyr::mutate(dplyr::across(pval_cols, round, 7))
  # df <- df |> dplyr::mutate(dplyr::across(cv_cols, round, 2))
  # df <- df |> dplyr::mutate(dplyr::across(fc_cols, round, 2))
  
  df <- round_columns(df,  rep(start_sample_col:(start_sample_col + sample_number + spqc_number - 1)), 1)
  df <- round_columns(df, "pval", 7)
  df <- round_columns(df, "cv_cols", 2)
  df <- round_columns(df, "fc_cols", 2)
  
  
  options <- list(
    selection = 'single',
    #dom = 'Bfrtipl',
    autoWidth = TRUE,
    scrollX = TRUE,
    scrollY = 500,
    scrollCollapse = TRUE,
    columnDefs = list(
      list(
        targets = c(0),
        visibile = TRUE,
        "width" = '30',
        className = 'dt-center'
      ),
      list(
        targets = c(2),
        visible = TRUE,
        "width" = '20',
        className = 'dt-center'
      ),
      list(
        targets = c(1),
        width = '250',
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 35 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
          "}"
        )
      ),
      list(
        targets = c(3),
        width = '100',
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 20 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
          "}"
        )
      )
    ),
    ordering = TRUE,
    orderClasses = TRUE,
    fixedColumns = list(leftColumns = 1),
    pageLength = 10,
    lengthMenu = c(10, 50, 100, 200)
    #formatRound(columns = c(sample_col_numbers + 1), digits = 0)
  )
  
  cat(file = stderr(), "Function protein_table...end", "\n")
  return(list(df, options))   
}

#------------------------------------------------------------------------------------------------------------------

peptide_table <- function(df, start_sample_col, sample_number, spqc_number){
  cat(file = stderr(), "Function peptide_table...", "\n")
  require('DT')
  source('Shiny_Misc_Functions.R')
  
  df <- round_columns(df,  rep(start_sample_col:(start_sample_col + sample_number + spqc_number - 1)), 1)
  df <- round_columns(df, "pval", 7)
  df <- round_columns(df, "cv_cols", 2)
  df <- round_columns(df, "fc_cols", 2)
  
  options <- list(
    selection = 'single',
    #dom = 'Bfrtipl',
    autoWidth = TRUE,
    scrollX = TRUE,
    scrollY = 500,
    scrollCollapse = TRUE,
    columnDefs = list(
      list(
        targets = c(0),
        visibile = TRUE,
        "width" = '30',
        className = 'dt-center'
      ),
      list(
        targets = c(2),
        visible = TRUE,
        "width" = '20',
        className = 'dt-center'
      ),
      list(
        targets = c(1),
        width = '250',
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 35 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
          "}"
        )
      ),
      list(
        targets = c(3),
        width = '100',
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 20 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
          "}"
        )
      )
    ),
    ordering = TRUE,
    orderClasses = TRUE,
    fixedColumns = list(leftColumns = 1),
    pageLength = 10,
    lengthMenu = c(10, 50, 100, 200)
    #formatRound(columns = c(sample_col_numbers + 1), digits = 0)
  )
  
  cat(file = stderr(), "Function peptide_table...end", "\n")
  return(list(df, options))   
}


#------------------------------------------------------------------------------------------------------------------

protein_peptide_table <- function(df_peptide, peptide_pos_lookup, start_sample_col){
  cat(file = stderr(), "Function protein_peptide_table...", "\n")
  require('DT')
  source('Shiny_Misc_Functions.R')

  # df_peptide <- df 
  
  df_peptide$Sequence <- gsub("_", "", df_peptide$Sequence)
  df_peptide <- merge(df_peptide, peptide_pos_lookup, by = (c("Accession", "Sequence"))    )
  df_peptide$Start <- as.numeric(df_peptide$Start)
  df_peptide$Stop <- as.numeric(df_peptide$Stop)
  df_peptide <- df_peptide |> dplyr::select(Stop, everything())
  df_peptide <- df_peptide |> dplyr::select(Start, everything())
  df_peptide <- df_peptide[order(df_peptide$Start, df_peptide$Stop), ]
  
  #df_peptide <- df_peptide |> dplyr::mutate(dplyr::across((start_sample_col_peptide+2):(ncol(df_peptide)), round, 1))
  df_peptide <- round_columns(df_peptide, rep(start_sample_col+2):((ncol(df_peptide)-1) ), 3)
  
  options <- list(
    selection = 'single',
    #dom = 'Bfrtipl',
    autoWidth = TRUE,
    scrollX = TRUE,
    scrollY = 500,
    scrollCollapse = TRUE,
    columnDefs = list(
      list(
        targets = c(0),
        visibile = TRUE,
        "width" = '6',
        className = 'dt-center'
      ),
      list(
        targets = c(1),
        visibile = TRUE,
        "width" = '6',
        className = 'dt-center'
      ),
      list(
        targets = c(2),
        visible = TRUE,
        "width" = '15',
        className = 'dt-center'
      ),
      list(
        targets = c(3),
        width = '10',
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 35 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
          "}"
        )
      ),
      list(
        targets = c(4),
        width = '100',
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 20 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
          "}"
        )
      )
    ),
    ordering = TRUE,
    orderClasses = TRUE,
    fixedColumns = list(leftColumns = 1),
    pageLength = 10,
    lengthMenu = c(10, 50, 100, 200)
    #formatRound(columns = c(sample_col_numbers + 1), digits = 0)
  )
  
  cat(file = stderr(), "Function protein_peptide_table...end", "\n")
  return(list(df_peptide, options))   
}


#--------------------------------
#------------------------------------------------------------------------------------------------------------------

protein_table_backup <- function(df){
  cat(file = stderr(), "Function protein_table...", "\n")
  require('DT')

  pval_cols <- colnames(df |> dplyr::select(contains("pval") ) )
  pval_col_numbers <- list(match(pval_cols, names(df)))
  pval_col_numbers <- unlist(pval_col_numbers)
  sample_cols <- c(colnames(df |> dplyr::select(contains("Normalized"))),
                   colnames(df |> dplyr::select(contains("Imputed"))) )
  sample_col_numbers <- list(match(sample_cols, names(df)))
  sample_col_numbers <- unlist(sample_col_numbers)
  cv_cols <- colnames(df |> dplyr::select(contains("CV") ) )
  mf_cols <- colnames(df |> dplyr::select(contains("MF") ) )
  stat_col <- ncol(df) - 1
  
  stats_DT <-  DT::datatable(df,
                             rownames = FALSE,
                             selection = 'single',
                             extensions = c("FixedColumns"), #, "Buttons"),
                             #editable = list(target='cell', disable = list(columns=c(0:(stat_col-1))) ) ,
                             options=list(
                               selection = 'single',
                               #dom = 'Bfrtipl',
                               autoWidth = TRUE,
                               scrollX = TRUE,
                               scrollY = 500,
                               scrollCollapse = TRUE,
                               columnDefs = list(list(targets = c(0), visibile = TRUE, "width" = '30', className = 'dt-center'),
                                                 list(targets = c(2), visible = TRUE, "width" = '20', className = 'dt-center'),
                                                 list(
                                                   targets = c(1),
                                                   width = '250',
                                                   render = JS(
                                                     "function(data, type, row, meta) {",
                                                     "return type === 'display' && data.length > 35 ?",
                                                     "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
                                                     "}")
                                                 ),
                                                 list(
                                                   targets = c(3),
                                                   width = '100',
                                                   render = JS(
                                                     "function(data, type, row, meta) {",
                                                     "return type === 'display' && data.length > 20 ?",
                                                     "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                                     "}")
                                                 )
                               ),
                               ordering = TRUE,
                               orderClasses = TRUE,
                               fixedColumns = list(leftColumns = 1),
                               pageLength = 10, lengthMenu = c(10,50,100,200)),
                             #buttons=c('copy', 'csv', 'excelHtml5', 'pdf')),
                             callback = DT::JS('table.page(3).draw(false);'
                             ))
  
  stats_DT <- stats_DT %>%  formatRound(columns = c(sample_col_numbers + 1), digits = 0)
  
  cat(file = stderr(), "Function protein_table...end", "\n")
  return(stats_DT)   
}




#--------------------------------

peptide_table_backup <- function(session, input, output, filter_df){
  require('DT')
  
  pval_cols <- colnames(filter_df %>% dplyr::select(contains("pval") ) )
  pval_col_numbers <- list(match(pval_cols, names(filter_df)))
  pval_col_numbers <- unlist(pval_col_numbers)
  sample_cols <- c(colnames(filter_df %>% dplyr::select(contains("Normalized"))),
                   colnames(filter_df %>% dplyr::select(contains("Imputed"))) )
  sample_col_numbers <- list(match(sample_cols, names(filter_df)))
  sample_col_numbers <- unlist(sample_col_numbers)
  cv_cols <- colnames(filter_df %>% dplyr::select(contains("CV") ) )
  mf_cols <- colnames(filter_df %>% dplyr::select(contains("MF") ) )
  stat_col <- ncol(filter_df) -1
  
  
  stats_DT <-  DT::datatable(filter_df,
                             rownames = FALSE,
                             extensions = c("FixedColumns"), #, "Buttons"),
                             #editable = list(target='cell', disable = list(columns=c(0:(stat_col-1))) ) ,
                             options=list(
                               #dom = 'Bfrtipl',
                               autoWidth = TRUE,
                               scrollX = TRUE,
                               scrollY=500,
                               scrollCollapse=TRUE,
                               columnDefs = list(list(targets = c(0), visibile = TRUE, "width"='30', className = 'dt-center'),
                                                 list(targets = c(1), visible = TRUE, "width"='20', className = 'dt-center'),
                                                 list(
                                                   targets = c(2),
                                                   width = '250',
                                                   render = JS(
                                                     "function(data, type, row, meta) {",
                                                     "return type === 'display' && data.length > 35 ?",
                                                     "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
                                                     "}")
                                                 ),
                                                 list(
                                                   targets = c(4),
                                                   width = '200',
                                                   render = JS(
                                                     "function(data, type, row, meta) {",
                                                     "return type === 'display' && data.length > 20 ?",
                                                     "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                                     "}")
                                                 ),
                                                 list(
                                                   targets = c(8),
                                                   width = '150',
                                                   render = JS(
                                                     "function(data, type, row, meta) {",
                                                     "return type === 'display' && data.length > 20 ?",
                                                     "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                                     "}")
                                                 )
                               ),
                               ordering = TRUE,
                               orderClasses= TRUE,
                               fixedColumns = list(leftColumns = 2),
                               pageLength = 100, lengthMenu = c(10,50,100,200)),
                             #buttons=c('copy', 'csv', 'excelHtml5', 'pdf')),
                             callback = JS('table.page(3).draw(false);'
                             ))
  
  stats_DT <- stats_DT %>%  formatRound(columns=c(sample_col_numbers), digits=0)
  
  return(stats_DT)   
  
}


