cat(file = stderr(), "load Shiny_UI_Update.R", "\n")
#-------------------------------------------------------------------------------------------


ui_render_startup <- function(session, input, output, params) {
  cat(file = stderr(), "Function ui_render_startup", "\n")
  
  ui_render_load_config(session, input, output)
  
  ui_render_load_data(session, input, output)
  ui_render_process_data(session, input, output, params)
  
  
  if ("Report" %in% list_tables(params)) {
    create_report_table(session, input, output, params, "Report")
  }else {
    create_report_table(session, input, output, params, "Report_template")
  }
  
  
  if ("data_impute" %in% list_tables(params)) {
    create_data_table(session, input, output, params, "data_impute", "QC")
  } else if ("data_start" %in% list_tables(params)) {
    create_data_table(session, input, output, params, "data_start", "QC")
  }
  
  
  
  if ("QC_Report" %in% list_tables(params)) {
    create_qc_table(session, input, output, params)
    ui_render_qc_plots(session, input, output)
  }
  
  if (stringr::str_c("SPQC_Report_", params$material_select) %in% list_tables(params)) {
    ui_render_sample_tables(session, input, output, params)
    ui_render_spqc_plots(session, input, output, params)
  }
  
  
  
  update_widgets(session, input, output, params)
  
  
  
  cat(file = stderr(), "Function ui_render_startup...end", "\n\n")
}

#-------------------------------------------------------------------------------------------
ui_render_sample_tables <- function(session, input, output, params) {
  cat(file = stderr(), "Function ui_render_sample_tables", "\n")

  create_data_table(session, input, output, params, params$material_select, "Samples")
  create_data_table(session, input, output, params, stringr::str_c("Filtered_",params$material_select), "Samples_Filtered")
  
  if (params$norm_select != "None") {
    create_data_table(session, input, output, params, stringr::str_c(params$norm_select, "_Filtered_Norm_",params$material_select), "Samples_Filtered")
    create_data_table(session, input, output, params, stringr::str_c(params$norm_select, "_Norm_", params$material_select), "Norm_Samples")
  } 
  
  create_data_table(session, input, output, params, stringr::str_c("SPQC_Report_", params$material_select), "SPQC")
  
  cat(file = stderr(), "Function ui_render_sample_tables...end", "\n")
}
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

#-------------------------------------------------------------------------------------------
ui_render_spqc_plots <- function(session, input, output, params) {
  cat(file = stderr(), "function ui_render_qc_plots...", "\n")

  output$spqc_bar <- renderImage({
    list(src = str_c(params$plot_path,"SPQC_", params$material_select, "_barplot.png"), contentType = 'image/png', width = 800, height = 600, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  output$spqc_box <- renderImage({
    list(src = str_c(params$plot_path,"SPQC_", params$material_select, "_boxplot.png"), contentType = 'image/png', width = 800, height = 600, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  if(params$norm_select != "None"){
    output$spqc_line <- renderImage({
      list(src = str_c(params$plot_path, params$material_select, "_Norm_factor_line_plot.png"), contentType = 'image/png', width = 1200, height = 600, alt = "this is alt text")
    }, deleteFile = FALSE)
  }

  
  cat(file = stderr(), "function ui_render_qc_plots...end", "\n")
}

#-------------------------------------
update_widgets <- function(session, input, output, params) {
  cat(file = stderr(), "Function - update_widgets...", "\n")
  
  updateNumericInput(session, "qc_acc", value = params$qc_acc)
  updateCheckboxInput(session, "fixed_lod", value = params$fixed_lod) 
  
  material_types <- as.list(strsplit(params$materials, ",")[[1]])
  updateSelectInput(session, "material_select", choices = material_types)
  
  if (length(params$material_select) >0){
    selected_materials <- as.list(strsplit(params$material_select, ",")[[1]])
    updateSelectInput(session, "material_select", selected = selected_materials)
  }
  
  updateSelectInput(session, "norm_select", selected = params$norm_select)
  updateCheckboxInput(session, "spqc_filter", value = params$spqc_filter)
  updateNumericInput(session, "spqc_filter_value", value = params$spqc_filter_value)
  updateCheckboxInput(session, "missing_filter", value = params$missing_filter)
  updateNumericInput(session, "missing_filter_value", value = params$missing_filter_value)

  updateTextInput(session, "file_prefix", value = params$file_prefix)
  
 cat(file = stderr(), "Function - update_widgets...end", "\n")
}