cat(file = stderr(), "load Shiny_UI_Update.R", "\n")

#-------------------------------------------------------------------------------------------
ui_render_load_config <- function(session, input, output) {
  cat(file = stderr(), "Function ui_render_load_config", "\n")
  
  output$data_source <- renderText({str_c("Source:  ", params$data_source)})
  output$file_prefix <- renderText({params$file_prefix})
  output$config_file_name <- renderText({stringr::str_c('Config File:  ', params$config_file) })
  
  cat(file = stderr(), "Function ui_render_load_config...end", "\n\n")
}

#-------------------------------------------------------------------------------------------
ui_render_load_data <- function(session, input, output) {
  cat(file = stderr(), "Function ui_render_load_data", "\n")
  
  output$data_file_name <- renderText({ stringr::str_c('Data File:  ', params$data_file) })
  
  cat(file = stderr(), "Function ui_render_load_data... end", "\n")
}

#-------------------------------------------------------------------------------------------
ui_render_process_data <- function(session, input, output, params) {
  cat(file = stderr(), "Function ui_render_process_data", "\n")
  
  output$plate_names <- renderText({ stringr::str_c('Plates:  ', params$plates) })
  
  output$plate_count <- renderText({ stringr::str_c('Plates Count:  ', params$plate_number) })
  
  output$material_names <- renderText({ stringr::str_c('Materials:  ', params$materials) })
  
  output$material_count <- renderText({ stringr::str_c('Material Count:  ', params$material_number) })
  
  cat(file = stderr(), "Function ui_render_process_data... end", "\n")
}

#-------------------------------------------------------------------------------------------
ui_render_qc_plots <- function(session, input, output) {
  cat(file = stderr(), "function ui_render_qc_plots...", "\n")
  
  output$qc_bar <- renderImage({
    list(src = str_c(params$plot_path,"QC_barplot.png"), contentType = 'image/png', width = 800, height = 600, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  output$qc_box <- renderImage({
    list(src = str_c(params$plot_path,"QC_boxplot.png"), contentType = 'image/png', width = 800, height = 600, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  cat(file = stderr(), "function ui_render_qc_plots...end", "\n")
}

#-------------------------------------
update_widgets <- function(session, input, output, params) {
  cat(file = stderr(), "Function - update_widgets...", "\n")
  
  material_types <- as.list(strsplit(params$materials, ",")[[1]])
  updatePickerInput(session, "material_select", choices = material_types)
  
  if (length(params$material_select) >0){
    selected_materials <- as.list(strsplit(params$material_select, ",")[[1]])
    updatePickerInput(session, "material_select", selected = selected_materials)
    updateSelectInput(session, "material_explore", choices = selected_materials)
  }
  
 cat(file = stderr(), "Function - update_widgets...end", "\n")
}