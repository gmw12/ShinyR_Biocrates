cat(file = stderr(), "Shiny_Misc_Functions.R", "\n")

str_to_num <- function(df, str_list){
  
  col_select <- strsplit(unlist(str_list), ",")
  col_select <- as.numeric(unlist(col_select))
  
  return(df[,col_select])
}



str_to_numlist <- function(str_in) {
  
  num_out <- strsplit(str_in, ",") |> unlist() |> as.numeric()
  
  return(num_out)
}


str_to_numlist_max <- function(str_in) {
  if (str_in == "") {
    num_out <- ""
  }else {
    num_out <- max(strsplit(str_in, ",") |> unlist() |> as.numeric())
  }
  return(num_out)
}


round_columns <- function(df, search_text, round_digits) {
  if (is.numeric(search_text[1])) {
    select_cols = search_text
  }else {
    select_cols <- which(stringr::str_detect(colnames(df), search_text))
  }
  for (col in select_cols){
    df[,col] <- df |> dplyr::select(dplyr::all_of(col)) |> round(digits = round_digits)
  }
  return(df)
}

