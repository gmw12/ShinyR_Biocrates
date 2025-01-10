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
    

  }else {
    #fresh start, create default params

    create_default_params(volumes, python_path) 
  }
  
  
  
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
      
    }
    
  })
  
  
  
  removeModal()     
})
