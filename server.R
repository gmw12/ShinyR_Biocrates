options(shiny.maxRequestSize = 4000*1024^2)
cat(file = stderr(), "server.R started", "\n")

#app_version <- '2025.01.08'
source("Shiny_Setup.R")
source("Shiny_Startup.R")

if (!exists('params')) {
  cat(file = stderr(), "initialize... params file does not exist...", "\n")
  
  #clear memory
  rm(list = ls())
  gc()
  #   .rs.restartR()
  
  #set user
  set_user()
  
} else {
  cat(file = stderr(), "initialize... params file exists...", "\n")
}



shinyServer(function(session, input, output) {
  cat(file = stderr(), "\n\n", "Shiny Server started ...1", "\n")
  showModal(modalDialog("Loading app...", footer = NULL))
  
  source("Shiny_Source.R")
  
  #set file choosers
  set_file_choosers(session, input, output, volumes)
  
  if (exists('params')) {
    cat(file = stderr(), "params file exists...", "\n\n")
    ui_render_load_data(session, input, output)
    ui_render_process_data(session, input, output, params)
    
    
    if ("Report" %in% list_tables(params)) {
      create_report_table(session, input, output, params, "Report")
    }else {
      create_report_table(session, input, output, params, "Report_template")
    }
    
    
    if ("data_start" %in% list_tables(params)) {
      create_data_table(session, input, output, params, "data_start")
    }
    
    if ("QC_Report" %in% list_tables(params)) {
      create_qc_table(session, input, output, params)
    }
    
    update_widgets(session, input, output, params)

  }else {
    #fresh start, create default params

    create_default_params(volumes, python_path) 
  }
  
  #button observers
  observe_buttons(session, input, output)
  
  #------------------------------------------------------------------------------------------------------  
  #Load design file
  observeEvent(input$sfb_config_file, {
    
    cat(file = stderr(), "\n\n", "sfb_design_file button clicked...", "\n")
    
    if (is.list(input$sfb_config_file)) {
      
      #load design from excel, create database
      load_config_file(session, input, output)
      
      #backup design table
      config_sbf <- parseFilePaths(volumes, input$sfb_config_file)
      save_data(config_sbf$datapath)
      
      #reset default dd for data
      set_file_choosers_data(session, input, output, volumes) 
      
      #update UI
      ui_render_load_config(session, input, output)
      
      #create_config_table(session, input, output)
    }
    
  })
  
  #------------------------------------------------------------------------------------------------------  
  #Load data file
  observeEvent(input$sfb_data_file, {
    
    cat(file = stderr(), "\n\n","sfb_data_file button clicked...", "\n")
    
    if (is.list(input$sfb_data_file)) {
      showModal(modalDialog("Loading and preprocessing data...", footer = NULL))
      
      #read data files
      load_data_file(session, input, output, params)
      
      #update UI
      ui_render_load_data(session, input, output)
      
      #remove status columns
      remove_status_cols(session, input, output, params)
      
      #remove indicators
      remove_indicators(session, input, output, params)
      
      #separate info and sample data
      separate_data(session, input, output, params)
      
      ui_render_process_data(session, input, output, params)
      
      create_data_table(session, input, output, params, "data_start")
      
      report_template(session, input, output, params)
      
      create_report_table(session, input, output, params, "Report_template")
      
      update_widgets(session, input, output, params)

      removeModal()
    }
    
  }) 
  
  #------------------------------------------------------------------------------------------------------  
  observeEvent(input$replace_lod, {
    
    cat(file = stderr(), "\n\n","replace_lod clicked...", "\n")
    
    replace_lod(session, input, output, params)
    
    create_data_table(session, input, output, params, "data_impute")

    cat(file = stderr(), "\n\n","replace_lod clicked...end", "\n")
    
  }) 
  
  #------------------------------------------------------------------------------------------------------  
  observeEvent(input$spqc_qc_calc, {
    
    cat(file = stderr(), "\n\n","spqc_qc_calc clicked...", "\n")
    
    spqc_calc(session, input, output, params)
    
    qc_calc(session, input, output, params)
    
    create_report_table(session, input, output, params, "Report")
    
    create_qc_table(session, input, output, params)
    
    cat(file = stderr(), "\n\n","spqc_qc_calc clicked...end", "\n")
    
  }) 

  #------------------------------------------------------------------------------------------------------  
  observeEvent(input$material_calc, {
    
    cat(file = stderr(), "\n\n","material_calc clicked...", "\n")
    
    material_calc(session, input, output, params)
    
    cat(file = stderr(), "\n\n","material_calc clicked...end", "\n")
    
  }) 
  
  #------------------------------------------------------------------------------------------------------  
  observeEvent(input$filter_calc, {
    
    cat(file = stderr(), "\n\n","material_calc clicked...", "\n")
    
    spqc_missing_filter(session, input, output, params)
    
    cat(file = stderr(), "\n\n","material_calc clicked...end", "\n")
    
  }) 
  
  
  
  #------------------------------------------------------------------------------------------------------  
  observeEvent(input$explore_start, {
    
    cat(file = stderr(), "\n\n","explore_start clicked...", "\n")
    
    create_explore_table(session, input, output, params)
    
    explore_start(session, input, output, params)
    
    interactive_pca2d(session, input, output, params)
    
    cat(file = stderr(), "\n\n","explore_start clicked...end", "\n")
    
  }) 
  
  removeModal()     
})
