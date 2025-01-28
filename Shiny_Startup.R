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
    }else if (Sys.info()["nodename"] == "Gregorys-MBP.wired.duke.local" |Sys.info()["nodename"] == "gregorys-mbp.lan" | Sys.info()["nodename"] == "mac.lan" | Sys.info()["nodename"] == "Gregorys-MacBook-Pro.local" ) {
      volumes <<- c(dd = '/Users/gregwaitt/Cloud-Drive/R/ShinyR_Biocrates', dd2 = '/Users/gregwaitt/Cloud-Drive/R', dc = '/mnt/RawData', wd = '.', Home = fs::path_home(), getVolumes()())
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
  shinyFileChoose(input, 'sfb_archive_file', session = session, roots = volumes, filetypes = c('', 'zip'))

  cat(file = stderr(), "Function - set_file_choosers...end", "\n\n")
}

#---------------------------------------------------------------------------------------------------------


set_file_choosers_data <- function(session, input, output, volumes) {
  cat(file = stderr(), "Function - set_file_choosers_data...", "\n")
  cat(file = stderr(), stringr::str_c("Volumes ---> ", volumes), "\n")
  
  shinyFileChoose(input, 'sfb_data_file', session = session, roots = volumes, filetypes = c('', 'tsv', 'txt'))
  
  cat(file = stderr(), "Function - set_file_choosers_data...end", "\n")
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