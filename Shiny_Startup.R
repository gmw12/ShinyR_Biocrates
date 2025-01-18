cat(file = stderr(), "Shiny_Startup.R", "\n")

set_user <- function() {
  cat(file = stderr(), "Function - set_user", "\n")
  
  #set user to unkown to force app to find correct usr
  site_user <<- "unknown"
  volumes <<- "unknown"
  database_dir <<- stringr::str_c(getwd(), "/database")
  
  while (site_user == "unknown") {
    if (Sys.info()["nodename"] == "oldmac") {
      volumes <<- c(dd = '/Users/gregwaitt/Documents/Data', wd = '.', Home = fs::path_home(),  getVolumes()())
      #version determines website content
      site_user <<- "dpmsr"
    }else if (Sys.info()["nodename"] == "titanshinyu20") {
      #for titan_black VM
      volumes <<- c(dd = '/home/dpmsr/shared/h_drive', dd2 = '/home/dpmsr/shared/other_black', RawData = '/home/dpmsr/shared/RawData', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
    }else if (Sys.info()["nodename"] == "greg-GS63VR-7RF") {
      #for greg linux laptop
      volumes <<- c(dd = '/home/dpmsr/shared', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
    }else if (Sys.info()["nodename"] == "greg-ThinkPad-W550s") {
      #for greg linux laptop
      volumes <<- c(dd = '/home/dpmsr/shared', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
    }else if (Sys.info()["nodename"] == "bob") {
      volumes <<- c(dd = '/home/dpmsr/mnt/h_black2', h1 = '/home/dpmsr/mnt/h_black1', h2 = '/home/dpmsr/mnt/h_black2', dc = 'home/dpmsr/mnt/RawData', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
      python_path <<- "/home/dpmsr/anaconda3/envs/PDP/bin/python3"
    }else if (Sys.info()["nodename"] == "waittblack") {
      volumes <<- c(dd = '/data', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
      python_path <<- "/home/dpmsr/anaconda3/envs/PDP/bin/python3"
    }else if (Sys.info()["nodename"] == "shiny-titan") {
      volumes <<- c(dd = '/mnt/h_black2', h1 = '/mnt/h_black1', h2 = '/mnt/h_black2', dc = '/mnt/RawData', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
      python_path <<- "/home/user/anaconda3/envs/python38/bin/python3"
    }else if (Sys.info()["nodename"] == "gregorys-mbp.lan" | Sys.info()["nodename"] == "Gregorys-MacBook-Pro.local" ) {
      volumes <<- c(dd = '/Users/gregwaitt/Data', dd2 = '/Users/gregwaitt/Cloud-Drive/R', dc = '/mnt/RawData', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
      python_path <<- "/home/user/anaconda3/envs/python38/bin/python3"
    }else{
      #for public website
      volumes <<- c(dd = '/data', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "not_dpmsr"
      database_dir <<- "/data/database"
      python_path <<- "/usr/bin/python3"
    }
  }
  
  #testing shiny
  #site_user <<- "not_dpmsr"
  
  cat(file = stderr(), str_c("site_user set to -->  ", site_user), "\n")
  cat(file = stderr(), str_c("volumes --> ", volumes), "\n")
  
  cat(file = stderr(), "Function - set_user...end", "\n\n")
  return()
}

#---------------------------------------------------------------------------------------------------------

create_default_params <- function(volumes, python_path) {
  cat(file = stderr(), "Function - create_default_params...", "\n")

  params <<- data.frame(
    "volumes" = toString(volumes),
    "volumes_name" = toString(names(volumes)),
    "database_dir" = stringr::str_c(getwd(), "/database"),
    "python_path" = python_path,
    "database_path" = "", #stringr::str_c(getwd(), "/database"),
    "config_path" = "",
    "config_file" = "",
    "file_prefix" = str_c("project_", strftime(Sys.time(), format = "%m%d%y")),
    "data_path" = "",
    "data_file" = "",
    "backup_path" = "",
    "extra_path" = "",
    "error_path" = "",
    "qc_path" = "",
    "app_path" = ""
    )
  
  cat(file = stderr(), "Function - create_default_params...end", "\n\n")
}


#---------------------------------------------------------------------------------------------------------


set_file_choosers <- function(session, input, output, volumes) {
  cat(file = stderr(), "Function - set_file_choosers...", "\n")
  cat(file = stderr(), stringr::str_c("Volumes ---> ", volumes), "\n")
  
  shinyFileChoose(input, 'sfb_config_file', session = session, roots = volumes, filetypes = c('', 'xlsx'))

  cat(file = stderr(), "Function - set_file_choosers...end", "\n\n")
}

#---------------------------------------------------------------------------------------------------------


set_file_choosers_data <- function(session, input, output, volumes) {
  cat(file = stderr(), "Function - set_file_choosers_data...", "\n")
  cat(file = stderr(), stringr::str_c("Volumes ---> ", volumes), "\n")
  
  shinyFileChoose(input, 'sfb_data_file', session = session, roots = volumes, filetypes = c('', 'tsv', 'txt'))
  
  cat(file = stderr(), "Function - set_file_choosers_data...end", "\n")
}

#---------------------------------------------------------------------------------------------------------


app_startup <- function(session, input, output) {
  cat(file = stderr(), "Function - app_startup", "\n")
  source('Shiny_UI_Update.R')
  
  #Check if database file present
  if (params$database_path != "") {
    cat(file = stderr(), "params exists", "\n")
    loaded_database <- basename(params$database_path)
    loaded_prefix <- params$file_prefix
    
    #updateUI
    ui_render_load_design(session, input, output)
    ui_render_load_data(session, input, output)
    ui_render_parameters(session, input, output)
    render_parameters_graphs(session, input, output)
    ui_render_filter(session, input, output)
    render_noise_graphs(session, input, output)
    render_filter_graphs(session, input, output)
    render_norm_graphs(session, input, output)
    render_norm_apply_graphs(session, input, output)
    render_filter_histogram_graphs(session, input, output)
    render_impute_graphs(session, input, output) 
    #create_impute_table(session, input, output) 
    render_qc_graphs(session, input, output) 
 
      
    #update Widgets
    update_widgets(session, input, output, params)

    
  }else{
    loaded_database <- "none"
    loaded_prefix <- "none"
  }
  

  output$loaded_database <- renderText(str_c("Loaded Database:  ", loaded_database))
  output$loaded_prefix <- renderText(str_c("Loaded File Prefix:  ", loaded_prefix))
  
  #observers
  observe_comp_names(session, input, output)
  observe_plot_type1(session, input, output)   
  observe_plot_type2(session, input, output)  
  
  
  cat(file = stderr(), "Function - app_startup...end", "\n\n")
}

#--------------------------------------------------------------
archive_update_app <- function(session, input, output, params, archive_path){
  cat(file = stderr(), "Function update_dirs...", "\n")
  
  if(fs::is_dir(params$design_path)) {
    cat(file = stderr(), "Current params dir structure is valid...", "\n")
  }else {
    cat(file = stderr(), stringr::str_c("Updating params dir structure... archive_path = ", archive_path), "\n")
    test <- unlist(str_split(params$design_path, "/"))
    test <- test[nzchar(test)]
    test <- test[length(test)]
    
    params$design_path <- stringr::str_c(archive_path, test, "/")
    params$data_path <- stringr::str_c(params$design_path, params$file_prefix, "/")
    params$backup_path <- stringr::str_c(params$design_path, "Backup/")
    params$extra_path <- stringr::str_c(params$design_path, "Extra/")
    params$error_path <- stringr::str_c(params$design_path, "Error/")
    params$qc_path <- stringr::str_c(params$design_path, "QC/")
    params$string_path <- stringr::str_c(params$design_path, "String/")
    params$phos_path <- stringr::str_c(params$design_path, "Phos/") 
    params$app_path <- stringr::str_c(params$design_path, "Backup//App/") 
    
    path_list <- c(params$design_path, params$data_path, params$backup_path, params$extra_path, params$error_path, params$qc_path, params$string_path, params$phos_path, params$app_path)
    
    for (path in path_list) {
      if (site_user == "dmpsr") {
        create_dir(path)
      }else{
        create_dir_only(path)
      }
    }

    params <<- params
    
    #only update if DPMSR, customer does not have accesses 
    if (site_user == "dpmsr") {
      #update plots
      if (params$raw_data_format != "protein") {
        parameter_create_plots(sesion, input, output, params)
        filter_histogram_plot(sesion, input, output, params, "precursor_start", "Precursor_Start_Histogram")
        render_noise_graphs(session, input, output)
        filter_histogram_plot(sesion, input, output, params, "precursor_noise", "Precursor_NoiseFiltered_Histogram")
        filter_create_plots(sesion, input, output, params)
      }
      
      ui_render_parameters(session, input, output)
      render_norm_graphs(session, input, output)
      render_norm_apply_graphs(session, input, output)
      impute_meta_data()
      impute_create_plots(session, input, output, params)
      render_impute_graphs(session, input, output)
      qc_stats(session, input, output, params)
      render_qc_graphs(session, input, output)
      
    }
  }
  
  cat(file = stderr(), "Function update_dirs...end", "\n")
  return(params)
}




#-------------------------------------------------------------------
named_list <- function(input_string) {
  cat(file = stderr(), "Function named_list...", "\n")
  
  input_string <- params$norm_type
  named_list <- strsplit(input_string, ", ")
  list_names <- named_list
  named_list <- as.list(named_list)
  test <- c(named_list)
  test <- unlist(test)
  
  names(named_list) <- c(test)
  cat(file = stderr(), "Function named_list...end", "\n")
  return(named_list)
}