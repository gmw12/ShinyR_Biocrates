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

  observeEvent(input$process_material,{
    runjs('document.getElementById("process_material").style.backgroundColor = "green";')
  }) 

  
}

