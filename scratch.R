#clear memory
rm(list = ls())
gc()
.rs.restartR()

params$database_path <- stringr::str_c(getwd(),"/database/test.db")
database_path <- stringr::str_c("/Users/gregwaitt/Data/project_101224.db")

create_db(params$database_path)

create_db <- function(db_name) {
  conn <- dbConnect(RSQLite::SQLite(), db_name) 
  RSQLite::dbDisconnect(conn)
}

read_table <- function(table_name, params){
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  df <- dbReadTable(conn, table_name)
  RSQLite::dbDisconnect(conn)
  return(df)
}

write_table <- function(table_name, df, params){
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  RSQLite::dbWriteTable(conn, table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
}

list_tables <- function(table_name, params){
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  table_list <- dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  return(table_list)
}

filter_db <- function(table_name, column_name, key_word, params) {
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  query <- stringr::str_c("SELECT * FROM ", table_name, " WHERE ", column_name, " LIKE '", accession,"'") 
  df <- dbGetQuery(conn, query)
  RSQLite::dbDisconnect(conn)
  return(df)
}

get_max_rowid <- function(table_name, params) {
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  query <- stringr::str_c("SELECT max(RowId) FROM ", table_name) 
  df <- dbGetQuery(conn, query)
  RSQLite::dbDisconnect(conn)
  return(df)
}

get_max_rowid('Analytes', params)

list_tables(params)

df_params <- read_table('params', params)
df_analytes <- read_table('Analytes', params)
df_qc <- read_table('QC', params)
df_qc_report <- read_table('QC_Report', params)
df_report <- read_table('Report', params)
df_report_template <- read_table('Report_template', params)
df_data_raw <- read_table('data_raw', params)
df_data_start <- read_table('data_start', params)
df_data_status <- read_table('data_status', params)
df_info <- read_table('data_info', params)
df_data_start <- read_table('data_start', params)
df_no_ind <- read_table_try("data_no_indicators", params)
df_plasma <- read_table('plasma', params)
df_norm <- read_table('SPQC_Norm_brain_tissue', params)

df_data_impute <- read_table('data_impute', params)
df_spqc_factor <- read_table('SPQC_Norm_Factor_brain_tissue', params)
df_spqc_report <- read_table("SPQC_Report_brain_tissue" , params)
df_spqc_report <- read_table("SPQC_Report_plasma" , params)
#-------------------------------------------------------------------------------------------
#subset df_plasma where Sample.description contains SPQC
df_report <- read_table('QC_Report', params)
df_plasma_spqc <- df_plasma[grep("SPQC", df_plasma$Sample.description),]
df <- read_table('data_raw', params)

# Create a sample data frame in wide format
wide_df <- data.frame(
  ID = c(1, 2, 3),
  Name = c("Ali", "Boby", "Charles"),
  Test1 = c(85, 90, 92),
  Test2 = c(88, 89, 95),
  Test3 = c(82, 87, 91)
)
print(wide_df)

# Load tidyr package
library(tidyr)
# Reshape data from wide to long format
long_df <- pivot_longer(wide_df, cols = starts_with("Test"), names_to = "Test", 
                        values_to = "Score")
print(long_df)

df_box_wide <- pivot_longer(df_box, cols = colnames(df_box), names_to = "Sample", 
                        values_to = "CV")

df_mtcars <- mtcars

ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")

ggplot(df_box_wide, aes(x=as.factor(Sample), y=CV, fill = Sample)) + 
  geom_boxplot(alpha=0.2) +
  coord_flip() +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = "Boxplot of CVs", x = "CV")
  

#-------

df <- read_table_try("data_impute", params)
df_report <- read_table_try("Report_template", params)

#subset of data with "SPQC" in Sample.description
df_spqc <- df[grep("SPQC", df$Sample.description),]
plates <- unique(df_spqc$Plate.bar.code)
materials <- unique(df_spqc$Material)


colnames(df_spqc_factor) <- gsub("SPQC Mean ", "", colnames(df_spqc_factor))
df_spqc_factor$analyte <- df_report$Abbreviation
test_df <- tidyr::pivot_longer(df_spqc_factor, cols = colnames(df_spqc_factor)[1:(ncol(df_spqc_factor)-1)], names_to = "Sample", values_to = "Mean")


ggplot2::ggplot(data=test_df, ggplot2::aes(x=analyte, y=Mean, group=Sample)) +
  ggplot2::geom_line(ggplot2::aes(color=Sample))+
  ggplot2::theme_classic()+
  ggplot2::geom_point(ggplot2::aes(color=Sample)) +
  ggplot2::ggtitle("Normalization Factors by Plate") + 
  #ggplot2::xlab(NULL) +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size=12), 
        axis.title = ggplot2::element_text(size=8, color="black"),
        #axis.text.x = element_text(size=8, angle = 45, hjust=1, color="black"),
        axis.text.y = ggplot2::element_text(size=8,  color="black"),
        axis.text.x = ggplot2::element_blank()
  ) 


ggplot(df_out, aes(x=get('PC1'), y=get('PC1'), color=x_gr )) +
  geom_point(alpha=0.5, size=1) +
  theme(legend.title=element_blank()) +
  ggtitle("title") + 
  ylab('PC1') +
  xlab('PC2') +
  scale_color_manual(values = rev(unique(color_list))) +
  theme(plot.title = element_text(hjust = 0.5, size=8), 
        axis.title = element_text(size=8, color="black"),
        axis.text.x = element_text(size=8, angle = 90,  color="black"),
        axis.text.y = element_text(size=8,  color="black"),
  ) 

#-------------------------------------------------------------------------------------------
# count lines in all *.R files in the current directory

#-------------------------------------------------------------------------------------------
# Create a function to count lines in a file
count_lines <- function(file) {
  lines <- readLines(file)
  return(length(lines))
}
#-------------------------------------------------------------------------------------------
# Get a list of all *.R files in the current directory
#-------------------------------------------------------------------------------------------
r_files <- list.files(pattern = "\\.R$", full.names = TRUE)
#-------------------------------------------------------------------------------------------
# Use lapply to apply the function to each file and get the results
#-------------------------------------------------------------------------------------------
line_counts <- lapply(r_files, count_lines)
#-------------------------------------------------------------------------------------------
# Combine the results into a data frame
#-------------------------------------------------------------------------------------------
line_counts_df <- data.frame(
  File = r_files,
  Lines = unlist(line_counts)
)
#-------------------------------------------------------------------------------------------
# Print the data frame
#-------------------------------------------------------------------------------------------
print(line_counts_df)
print(sum(line_counts_df$Lines))
    







#-------------------------------------------------------------------------------------------


plates <- unique(df$Plate.bar.code)
plates <- paste(plates, collapse = " | ")
plates <- strsplit(plates, " | ")
plates <- unlist(plates)
plates <- plates[plates != ""]
plates <- plates[plates != "|"]
plates <- unique(plates)

common_left_str <- ""
common <- TRUE
i <- 1

while (common) {
  test_str <- substr(plates[1], 1, i)
  cat(file = stderr(), str_c("test_str=", test_str), "\n")
  for (j in 1:length(plates)) {
    if (substr(plates[j], 1, i) != test_str) {
      cat(file = stderr(), str_c("i=",i," j=",j," test_str=", substr(plates[j], 1, i)), "\n")
      common <- FALSE
      break
    }
  }
  if (common) {common_left_str <- test_str}
  i <- i + 1
}


common_right_str <- ""
common <- TRUE
i <- 0

while (common) {
  test_str <- substr(plates[1], nchar(plates[1])-i, nchar(plates[1]))
  cat(file = stderr(), str_c("test_str=", test_str), "\n")
  for (j in 1:length(plates)) {
    if (substr(plates[j], nchar(plates[1])-i, nchar(plates[1])) != test_str) {
      cat(file = stderr(), str_c("i=",i," j=",j," test_str=", substr(plates[1], nchar(plates[j])-i, nchar(plates[1]))), "\n")
      common <- FALSE
      break
    }
  }
  if (common) {common_right_str <- test_str}
  i <- i + 1
}


new_plate <- gsub(common_left_str, "", plates)
new_plate <- gsub(common_right_str, "", new_plate)

for (i in 1:length(plates)) {
  df <- df |>
    mutate( across(
      .cols = everything(),
      ~str_replace( ., plates[[i]], test[[i]] )
    ) )
}

