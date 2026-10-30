cat(file = stderr(), "Shiny_Misc_Functions.R", "\n")

#---------------------------------------------------------------------
str_to_num <- function(df, str_list){
  
  col_select <- strsplit(unlist(str_list), ",")
  col_select <- as.numeric(unlist(col_select))
  
  return(df[,col_select])
}


#---------------------------------------------------------------------
str_to_numlist <- function(str_in) {
  
  num_out <- strsplit(str_in, ",") |> unlist() |> as.numeric()
  
  return(num_out)
}

#---------------------------------------------------------------------
str_to_numlist_max <- function(str_in) {
  if (str_in == "") {
    num_out <- ""
  }else {
    num_out <- max(strsplit(str_in, ",") |> unlist() |> as.numeric())
  }
  return(num_out)
}

#---------------------------------------------------------------------
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

#---------------------------------------------------------------------
simple_plate_name <- function(df) {
  cat(file = stderr(), "Function - simple_plate_name...", "\n")
  
  #  df <- df_data_raw
  
  plates <- unique(df$Plate.bar.code)
  plates <- paste(plates, collapse = " | ")
  plates <- strsplit(plates, " | ")
  plates <- unlist(plates)
  plates <- plates[plates != ""]
  plates <- plates[plates != "|"]
  plates <- unique(plates)
  
  #need 2+ plates to find a common prefix/suffix (a single plate would loop forever and simplify to "")
  if (length(plates) < 2) {
    cat(file = stderr(), "Function - simple_plate_name...single plate, names unchanged", "\n\n")
    return(df)
  }
  
  min_len <- min(nchar(plates))
  
  #longest prefix shared by all plates
  i <- 0
  while (i < min_len && length(unique(substr(plates, 1, i + 1))) == 1) { i <- i + 1 }
  common_left_len <- i
  
  #longest suffix shared by all plates (not overlapping the prefix)
  i <- 0
  while (i < min_len - common_left_len && length(unique(substr(plates, nchar(plates) - i, nchar(plates)))) == 1) { i <- i + 1 }
  common_right_len <- i
  
  cat(file = stderr(), stringr::str_c("common prefix=", substr(plates[1], 1, common_left_len),
                                      " common suffix=", substr(plates[1], nchar(plates[1]) - common_right_len + 1, nchar(plates[1]))), "\n")
  
  new_plate <- substr(plates, common_left_len + 1, nchar(plates) - common_right_len)
  
  if (any(new_plate == "") || any(duplicated(new_plate))) {
    cat(file = stderr(), "Function - simple_plate_name...simplified names not unique, names unchanged", "\n\n")
    return(df)
  }
  
  for (i in 1:length(plates)) {
    df <- df |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::everything(),
        ~stringr::str_replace( ., plates[[i]], new_plate[[i]] )
      ) )
  }
  
  cat(file = stderr(), "Function - simple_plate_name...end", "\n\n")
  return(df)
}
#---------------------------------------------------------------------
clean_dataframe <- function(df) {
  cat(file = stderr(), "Function - clean_dataframe...", "\n")
  # Define problematic characters that typically cause database issues
  
  problem_chars_all <- c(
    # Special characters that often cause SQL issues
    "'", "\"", "\\", "/", "%", "_", 
    # Control characters
    "\b", "\f", "\n", "\r", "\t",
    # Other potentially problematic symbols
    ";", ":", "?", "&", "<", ">", "|", "*"
  )
  
  problem_chars <- c(";", ":", "?", "&", "*")
  
  # Define replacement character
  replacement_char <- "_"
  
  # Process only character columns
  char_cols <- sapply(df, is.character)
  
  colnames(df) <- gsub("\\[.*?mg\\]", "[ul/mg]", colnames(df), useBytes = TRUE)
  
  if(any(char_cols)) {
    for(col in names(df)[char_cols]) {
        # Replace each problematic character with the replacement
        df[[col]] <- gsub("\\[.*?M\\]", "[uM]", df[[col]], useBytes = TRUE)
    }
  }
  
  if(any(char_cols)) {
    for(col in names(df)[char_cols]) {
      for(char in problem_chars) {
        # Replace each problematic character with the replacement
        df[[col]] <- gsub(char, replacement_char, df[[col]], fixed = TRUE)
      }
    }
  }
  cat(file = stderr(), "Function - clean_dataframe...end", "\n\n")
  return(df)
}