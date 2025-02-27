cat(file = stderr(), "Shiny_Tables.R", "\n")
#--------------------------------------------------------------------------------------------------------------------


#load design table
create_data_table <- function(session, input, output, params, table_name, input_table){
  cat(file = stderr(), "Function create_data_table...", "\n")
  require('DT')
  require('shinyjs')
  
  bg_datatable <- callr::r_bg(create_data_table_bg, args = list(params, table_name), stderr = stringr::str_c(params$error_path, "//error_datatable.txt"), supervise = TRUE)
  bg_datatable$wait()
  print_stderr("error_datatable.txt")
  
  data_table_DT <- bg_datatable$get_result()
  
  if (input_table == "QC"){
    output$data_table <- DT::renderDataTable(data_table_DT) 
  }

  if (input_table == "Samples"){
    output$material_table <- DT::renderDataTable(data_table_DT) 
  }
  
  if (input_table == "Samples_Filtered"){
    output$filter_material_table <- DT::renderDataTable(data_table_DT) 
  }
  
  if (input_table == "Norm_Samples"){
    output$norm_material_table <- DT::renderDataTable(data_table_DT) 
  }
  
  if (input_table == "SPQC"){
    output$spqc_material_table <- DT::renderDataTable(data_table_DT) 
  }
  
  cat(file = stderr(), "Function create_data_table...end", "\n")
}

#--------------------------------

create_data_table_bg <- function(params, table_name){
  cat(file = stderr(), "Function create_data_table_bg...", "\n")
  source("Shiny_File.R")
  require('DT')
  
  #get design data
  df <- read_table_try(table_name, params)
  
  options <- list(
    selection = 'single',
    autoWidth = TRUE,
    scrollX = TRUE,
    scrollY = 500,
    scrollCollapse = TRUE,
    columnDefs = list(
      list(
        targets = c(1),
        visibile = TRUE,
        "width" = '5',
        className = 'dt-center'
      ),
      list(
        targets = c(2),
        visible = TRUE,
        "width" = '50',
        className = 'dt-center'
      )
      #list(
        #targets = c(5),
        #width = '20',
        # render = JS(
        #   "function(data, type, row, meta) {",
        #   "return type === 'display' && data.length > 35 ?",
        #   "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
        #   "}"
        # )
      #)
    ),
    ordering = TRUE,
    orderClasses = TRUE,
    fixedColumns = list(leftColumns = 1),
    pageLength = 10,
    lengthMenu = c(10, 50, 100, 200)
  )
  
  data_table_DT <-  DT::datatable(df, rownames = FALSE, extensions = "FixedColumns", options = options)
  
  cat(file = stderr(), "Function create_data_table_bg...end", "\n")
  
  return(data_table_DT)   
}

#------------------------------------------------------------------------
#load design table
create_report_table <- function(session, input, output, params, table_name){
  cat(file = stderr(), "Function create_report_table", "\n")
  #showModal(modalDialog("Creating design table...", footer = NULL))
  
  bg_report_table <- callr::r_bg(create_report_table_bg, args = list(params, table_name), stderr = str_c(params$error_path, "//error_report_table.txt"), supervise = TRUE)
  bg_report_table$wait()
  print_stderr("error_report_table.txt")
  
  df <- bg_report_table$get_result()[[1]]
  options <- bg_report_table$get_result()[[2]]
  
  report_table_DT <-  DT::datatable(df, rownames = FALSE, extensions = "FixedColumns", options = options)
  output$report_table <- DT::renderDataTable(report_table_DT)
  
  cat(file = stderr(), "Function create_report_table...end", "\n\n")
  #removeModal()
}

#--------------------------------

create_report_table_bg <- function(params, table_name){
  cat(file = stderr(), "Function create_report_table_bg", "\n")
  source("Shiny_File.R")
  
  #get raw_datadata
  df_report <- read_table_try(table_name, params)

  #save(df_report, file="z1")    #  load(file="z1") 
  
  options <- list(
    selection = 'single',
    #autoWidth = TRUE,
    scrollX = TRUE,
    scrollY = 500,
    scrollCollapse = TRUE,
    # columnDefs = list(
    #   list(
    #     targets = c(1),
    #     visibile = TRUE,
    #     "width" = '5',
    #     className = 'dt-center'
    #   ),
    #   list(
    #     targets = c(2),
    #     visible = TRUE,
    #     "width" = '50',
    #     className = 'dt-center'
    #   )
    # ),
    ordering = TRUE,
    orderClasses = TRUE,
    fixedColumns = list(leftColumns = 1),
    pageLength = 20,
    lengthMenu = c(20, 50, 100)
  )
  
  cat(file = stderr(), "Function create_report_table_bg...end", "\n")
  
  return(list(df_report, options))   
}


#------------------------------------------------------------------------
#load design table
create_qc_table <- function(session, input, output, params){
  cat(file = stderr(), "Function qc_table", "\n")
  #showModal(modalDialog("Creating design table...", footer = NULL))
  
  bg_qc_table <- callr::r_bg(create_qc_table_bg, args = list(params), stderr = str_c(params$error_path, "//error_qc_table.txt"), supervise = TRUE)
  bg_qc_table$wait()
  print_stderr("error_qc_table.txt")
  
  df <- bg_qc_table$get_result()[[1]]
  options <- bg_qc_table$get_result()[[2]]
  
  qc_table_DT <-  DT::datatable(df, rownames = FALSE, extensions = "FixedColumns", options = options)
  output$qc_table <- DT::renderDataTable(qc_table_DT)
  
  cat(file = stderr(), "Function create_qc_table...end", "\n\n")
  #removeModal()
}

#--------------------------------

create_qc_table_bg <- function(params){
  cat(file = stderr(), "Function create_qc_table_bg", "\n")
  source("Shiny_File.R")
  
  #get raw_datadata
  df_qc <- read_table_try("QC_Report", params)
  
  options <- list(
    fixedColumns = list(leftColumns = 1),
    autoWidth = TRUE,
    scrollX = TRUE,
    scrollY = 500,
    scrollCollapse = TRUE,
    columnDefs = list(
      list(
        targets = c(1),
        visibile = TRUE,
        "width" = '5',
        className = 'dt-center'
      ),
      list(
        targets = c(2),
        visible = TRUE,
        "width" = '50',
        className = 'dt-center'
      )
    ),
    ordering = TRUE,
    orderClasses = TRUE,
    pageLength = 20,
    lengthMenu = c(20, 50, 100)
  )
  
  cat(file = stderr(), "Function create_qc_table_bg...end", "\n")
  
  return(list(df_qc, options))   
}


#------------------------------------------------------------------------
#load design table
create_explore_table <- function(session, input, output, params){
  cat(file = stderr(), "Function explore_table", "\n")
  #showModal(modalDialog("Creating design table...", footer = NULL))
  
  input_material_explore <- input$material_explore
  input_data_type <- input$data_type
  
  bg_explore_table <- callr::r_bg(create_explore_table_bg, 
                                  args = list(params, input_material_explore, input_data_type),
                                  stderr = str_c(params$error_path, "//error_explore_table.txt"), supervise = TRUE)
  bg_explore_table$wait()
  print_stderr("error_explore_table.txt")
  
  df <- bg_explore_table$get_result()[[1]]
  options <- bg_explore_table$get_result()[[2]]
  
  explore_table_DT <-  DT::datatable(df, rownames = FALSE, extensions = "FixedColumns", options = options)
  output$explore_table <- DT::renderDataTable(explore_table_DT)
  
  cat(file = stderr(), "Function create_explore_table...end", "\n\n")
  #removeModal()
}

#--------------------------------

create_explore_table_bg <- function(params, input_material_explore, input_data_type){
  cat(file = stderr(), "Function create_explore_table_bg", "\n")
  source("Shiny_File.R")
  
  if (input_data_type == 1) {
    table_name <- input_material_explore
  } else {
    table_name <- stringr::str_c("Filtered_", input_material_explore)
  }
  
  df <- read_table_try(table_name, params)
  
  
  options <- list(
    fixedColumns = list(leftColumns = 1),
    autoWidth = TRUE,
    scrollX = TRUE,
    scrollY = 500,
    scrollCollapse = TRUE,
    columnDefs = list(
      list(
        targets = c(1),
        visibile = TRUE,
        "width" = '5',
        className = 'dt-center'
      ),
      list(
        targets = c(2),
        visible = TRUE,
        "width" = '50',
        className = 'dt-center'
      )
    ),
    ordering = TRUE,
    orderClasses = TRUE,
    pageLength = 20,
    lengthMenu = c(20, 50, 100)
  )
  
  cat(file = stderr(), "Function create_explore_table_bg...end", "\n")
  
  return(list(df, options))   
}



















