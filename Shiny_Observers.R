cat(file = stderr(), "Shiny_Observers.R", "\n")
#---------------------------------------------------------
observe_buttons <- function(session, input , output) {
  cat(file = stderr(), "observe buttons loaded...", "\n")
  
  observeEvent(input$sfb_config_file,{
    runjs('document.getElementById("sfb_config_file").style.backgroundColor = "green";')
  })
  
  observeEvent(input$sfb_data_file,{
    runjs('document.getElementById("sfb_data_file").style.backgroundColor = "green";')
  })
  
  observeEvent(input$sfb_archive_file,{
    runjs('document.getElementById("sfb_archive_file").style.backgroundColor = "green";')
  })
  
  observeEvent(input$replace_lod,{
    runjs('document.getElementById("replace_lod").style.backgroundColor = "green";')
  })
  
  observeEvent(input$qc_calc,{
    runjs('document.getElementById("qc_calc").style.backgroundColor = "green";')
  }) 

  observeEvent(input$filter_calc,{
    runjs('document.getElementById("filter_calc").style.backgroundColor = "green";')
  }) 
  
  observeEvent(input$material_calc,{
    runjs('document.getElementById("material_calc").style.backgroundColor = "green";')
  }) 
  
}

