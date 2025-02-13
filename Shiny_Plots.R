cat(file = stderr(), "Shiny_PCA.R", "\n")


#------------------------------------------------------------------------------------------------------
create_qc_plots <- function(sesion, input, output, params){
  cat(file = stderr(), "Function create_qc_plots", "\n")
  showModal(modalDialog("Creating Plots...", footer = NULL))
  
  bg_qc_bar <- callr::r_bg(func = qc_grouped_plot_bg, args = list("QC", params), stderr = str_c(params$error_path,  "//error_qcbarplot.txt"), supervise = TRUE)
  bg_qc_box <- callr::r_bg(func = box_plot_bg, args = list("QC", params), stderr = str_c(params$error_path, "//error_qcboxplot.txt"), supervise = TRUE)
  
  bg_qc_box$wait()
  bg_qc_bar$wait()

  print_stderr("error_qcbarplot.txt")
  print_stderr("error_qcboxplot.txt")

  wait_cycle <- 0
  while (!file.exists(str_c(params$plot_path,"QC_boxplot.png"))) {
    if (wait_cycle < 10) {
      Sys.sleep(0.5)
      wait_cycle <- wait_cycle + 1
    }
  }
  
  ui_render_qc_plots(session, input, output)
  
  cat(file = stderr(), "create_qc_plots...end", "\n")
  removeModal()
}

#------------------------------------------------------------------------------------------------------
create_spqc_plots <- function(sesion, input, output, params){
  cat(file = stderr(), "Function create_spqc_plots", "\n")
  showModal(modalDialog("Creating Plots...", footer = NULL))
  
  bg_spqc_bar <- callr::r_bg(func = qc_grouped_plot_bg, args = list("SPQC", params), stderr = str_c(params$error_path,  "//error_spqcbarplot.txt"), supervise = TRUE)
  bg_spqc_box <- callr::r_bg(func = box_plot_bg, args = list("SPQC", params), stderr = str_c(params$error_path, "//error_spqcboxplot.txt"), supervise = TRUE)
  bg_norm_line <- callr::r_bg(func = norm_line_bg, args = list(params), stderr = str_c(params$error_path, "//error_normlineplot.txt"), supervise = TRUE)
 
  bg_spqc_box$wait()
  bg_spqc_bar$wait()
  #bg_norm_line$wait()
  
  print_stderr("error_spqcbarplot.txt")
  print_stderr("error_spqcboxplot.txt")
  print_stderr("error_normlineplot.txt")
  
  wait_cycle <- 0
  
  while (!file.exists(stringr::str_c(params$plot_path, "SPQC_", params$material_select, "_barplot.png"))) {
    if (wait_cycle < 10) {
      Sys.sleep(0.5)
      wait_cycle <- wait_cycle + 1
    }
  }
  
  ui_render_spqc_plots(session, input, output, params)
  
  cat(file = stderr(), "create_spqc_plots...end", "\n")
  removeModal()
}

#------------------
qc_grouped_plot_bg <- function(plot_title, params) {
  cat(file = stderr(), stringr::str_c("function qc_grouped_plot_bg...."), "\n")
  source('Shiny_File.R')
    
  
  if (plot_title == "QC"){
    df <- read_table_try("QC_Report", params)
    df_qc1 <- df |> dplyr::select(contains("QC1.Level"))
    row_remove <- which(df_qc1 == 0.0010, arr.ind = TRUE)
    if(length(row_remove) > 0) {df <- df[-row_remove,]}
    test <- df |> dplyr::select(contains("Accuracy")) 
    lower_limit <- 100 - params$qc_acc
    upper_limit <- 100 + params$qc_acc
    test[test < lower_limit | test > upper_limit] <- 0
    test[test > 0] <- 1
    lower_limit_text <- stringr::str_c("Accuracy ", lower_limit, "-", upper_limit)
    upper_limit_text <- stringr::str_c("Accuracy Outside Range")
    file_name <- stringr::str_c(params$plot_path, plot_title, "_barplot.png")
  }
  
  if (plot_title == "SPQC"){
    df <- read_table_try(stringr::str_c("SPQC_Report_", params$material_select), params)
    test <- df |> dplyr::select(contains("SPQC"))
    test <- test |> dplyr::select(contains("CV"))
    #replace na with 0
    test[is.na(test)] <- 0
    test[test > params$qc_acc] <- 0
    test[test > 0] <- 1
    lower_limit_text <- stringr::str_c("CV < ", params$qc_acc)
    upper_limit_text <- stringr::str_c("CV > ", params$qc_acc)
    file_name <- stringr::str_c(params$plot_path, plot_title, "_", params$material_select, "_barplot.png")
  }
  
  
  
  good <- colSums(test)
  bad <- nrow(test) - good
  
  data_plot = data.frame(matrix(vector(), ncol(test)*2, 3,
                         dimnames=list(c(), c("Sample", "QC", "Count"))),
                  stringsAsFactors=F)
  
  colnames(test) <- gsub("Accuracy", "", colnames(test))
  data_plot$Sample <- c(colnames(test), colnames(test))
  data_plot$Count <- c(good, bad)
  data_plot$QC <- c(rep(lower_limit_text, ncol(test)), rep(upper_limit_text, ncol(test)))
  
  plot_title <- stringr::str_c(plot_title, " Barplot")
  
  # Grouped
  ggplot2::ggplot(data_plot, ggplot2::aes(fill = QC, y = Count, x = Sample)) + 
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::ggtitle(plot_title) + 
    ggplot2::xlab(NULL) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), 
                   axis.text.x = ggplot2::element_text(size = 5, angle = 45, hjust = 1, color = "black"),
                   axis.text.y = ggplot2::element_text(size = 5,  color = "black"))
  ggplot2::ggsave(file_name, width = 8, height = 6)
  
  cat(file = stderr(), stringr::str_c("function qc_grouped_plot_bg....end"), "\n")
}


#Box plot-------------------------------------------------
box_plot_bg <- function(plot_title, params) {
  cat(file = stderr(), "Function box_plot_bg", "\n")
  
  source("Shiny_File.R")
  
  if (plot_title == "QC"){ 
    df <- read_table_try("QC_Report", params)
    df_qc1 <- df |> dplyr::select(contains("QC1.Level"))
    row_remove <- which(df_qc1 == 0.0010, arr.ind = TRUE)
    if(length(row_remove) > 0) {df <- df[-row_remove,]}
    test <- df |> dplyr::select(contains("Accuracy")) 
    df_box <- df |> dplyr::select(contains("Accuracy"))
    #remove "Accuracy" from column names
    colnames(df_box) <- gsub("Accuracy", "", colnames(df_box))
    df_box_wide <- tidyr::pivot_longer(df_box, cols = colnames(df_box), names_to = "Sample", 
                                values_to = "Stat")
    x_name <- "Accuracy"
    file_name <- stringr::str_c(params$plot_path, plot_title, "_boxplot.png")
    }
  
  
  if (plot_title == "SPQC"){ 
    df <- read_table_try(stringr::str_c("SPQC_Report_",params$material_select), params)
    df_box <- df |> dplyr::select(contains("SPQC"))
    df_box <- df_box |> dplyr::select(contains("CV"))
    #colnames(df_box) <- gsub("X", "", colnames(df_box))
    df_box_wide <- tidyr::pivot_longer(df_box, cols = colnames(df_box), names_to = "Sample", 
                                values_to = "Stat")
    df_box_wide$Sample <- gsub("X.CV.", "", df_box_wide$Sample)
    df_box_wide$Sample <- gsub("SPQC.uM.", "", df_box_wide$Sample)
    x_name <- "CV"
    file_name <- stringr::str_c(params$plot_path, plot_title,"_", params$material_select, "_boxplot.png")
  }
  
  df_box <- df_box |>  dplyr::mutate(across(!where(is.numeric), as.numeric))
  

  plottitle <- stringr::str_c(plot_title, " ", params$material_select, " Boxplot")
  
  #create color_list length of ncol df_acc
  color_list <- c("red", "blue", "darkgreen", "purple", "orange", "black", "brown", "cyan", "magenta", "yellow", "pink", "grey", "lightblue", "green", "darkred", "darkblue", "darkgreen", "purple", "darkorange", "hotpink", "maroon", "darkcyan", "darkmagenta", "yellow4", "maroon4", "darkgrey", "skyblue", "darkgreen", "gold", "khaki")
  
  ggplot2::ggplot(df_box_wide, ggplot2::aes(x=as.factor(Sample), y=Stat, fill = Sample)) + 
    ggplot2::geom_boxplot(alpha=0.2) +
    ggplot2::scale_color_manual(values = rev(unique(color_list))) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none", 
          axis.title.y = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5) ) +
    ggplot2::labs(title = plottitle, x = x_name) +
    ggplot2::scale_x_discrete(limits = rev) +
    ggplot2::coord_flip() 
  ggplot2::ggsave(file_name, width = 8, height = 6)
  
  
  
  cat(file = stderr(), "Function box_plot_bg...end", "\n")
}


#------------------
norm_line_bg <- function(params) {
  cat(file = stderr(), stringr::str_c("function norm_line_bg...."), "\n")
  source('Shiny_File.R')
  
  df_spqc_factor <- read_table_try(stringr::str_c(params$norm_select, "_Norm_Factor_", params$material_select), params)
  df_report <- read_table_try("Report", params)
  
  colnames(df_spqc_factor) <- gsub("SPQC.Mean.", "", colnames(df_spqc_factor))
  df_spqc_factor$analyte <- df_report$Name
  test_df <- tidyr::pivot_longer(df_spqc_factor, cols = colnames(df_spqc_factor)[1:(ncol(df_spqc_factor)-1)], names_to = "Sample", values_to = "Mean")
  test_df$Sample <- gsub(gsub(" ", ".", params$material_select), "", test_df$Sample)
  test_df$Sample <- gsub(params$norm_select, "", test_df$Sample)
  #remove trailing and leading periods
  test_df$Sample <- gsub("^\\.|\\.$", "", test_df$Sample)
  
  file_name <- stringr::str_c(params$plot_path, params$material_select, "_Norm_factor_line_plot.png")
  
  
  if (params$data_source == "BileAcid") {
    ggplot2::ggplot(data=test_df, ggplot2::aes(x=analyte, y=Mean, group=Sample)) +
      ggplot2::geom_line(ggplot2::aes(color=Sample))+
      ggplot2::theme_classic()+
      ggplot2::geom_point(ggplot2::aes(color=Sample)) +
      ggplot2::ggtitle(stringr::str_c(params$material_select, " Normalization Factors by Plate")) + 
      #ggplot2::xlab(NULL) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size=12), 
                     axis.title = ggplot2::element_text(size=8, color="black"),
                     axis.text.x = ggplot2::element_text(size=8, angle = 45, hjust=1, color="black"),
                     axis.text.y = ggplot2::element_text(size=8,  color="black"),
                     #axis.text.x = ggplot2::element_blank()
      ) 
    ggplot2::ggsave(file_name, width = 12, height = 6)
  }else{
    ggplot2::ggplot(data=test_df, ggplot2::aes(x=analyte, y=Mean, group=Sample)) +
      ggplot2::geom_line(ggplot2::aes(color=Sample))+
      ggplot2::theme_classic()+
      ggplot2::geom_point(ggplot2::aes(color=Sample), size=1) +
      ggplot2::ggtitle(stringr::str_c(params$material_select, " Normalization Factors by Plate")) + 
      #ggplot2::xlab(NULL) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size=12), 
                     axis.title = ggplot2::element_text(size=8, color="black"),
                     #axis.text.x = ggplot2::element_text(size=8, angle = 45, hjust=1, color="black"),
                     axis.text.y = ggplot2::element_text(size=8,  color="black"),
                     axis.text.x = ggplot2::element_blank()
      )
    ggplot2::ggsave(file_name, width = 12, height = 6)
  }
  
  cat(file = stderr(), stringr::str_c("function norm_line_bg....end"), "\n")
}




#------------------------------------------------------------------------------------------------------------------------

interactive_pca2d <- function(session, input, output, params) {
  cat(file = stderr(), "interactive_pca2d..." , "\n")
  
  if(input$data_type == 1){
    if(input$norm_option == 1){
      table_name <- input$material_select
    }else{
      table_name <- stringr::str_c(params$norm_select, "_Norm_", params$material_select)
    }
  }else{
    if(input$norm_option == 1){
      table_name <- stringr::str_c("Filtered_", params$material_select)  
    }else{
      table_name <- stringr::str_c(params$norm_select, "_Filtered_Norm_", params$material_select)  
    }
  }
  
 cat(file = stderr(), str_c("table name = ", table_name), "\n")  
 df <- read_table_try(table_name, params)
 df_report <- read_table_try("Report", params)
 
 updateTextInput(session, "stats_pca2d_title", value = stringr::str_c(input$material_select, " PCA2D"))
 
 df$Sample.description[grep("SPQC", df$Sample.description)] <- "SPQC"
 df$Sample.description[grep("NIST", df$Sample.description)] <- "NIST"
 df$Sample.description[grep("Golden West", df$Sample.description)] <- "Golden_West"
 match_list <- c("SPQC", "NIST", "Golden_West")
 #df$Sample.description[grep("XXXX", df$Sample.description)] <- "Sample"
 df$Sample.description[-grep(paste(match_list, collapse="|"), df$Sample.description)] <- "Sample"

 if(input$remove_gw_nist) {
   if (any(grepl("Golden_West", df$Sample.description))) {
     df <- df[-grep("Golden_West", df$Sample.description),]
   }
   if (any(grepl("NIST", df$Sample.description))) {
     df <- df[-grep("NIST", df$Sample.description),]
   }
 }
 
 if(input$pca_by_plate) {
  df$Sample.description <- stringr::str_c(df$Sample.description, "_", df$Submission.name)    
 }
 
 if(input$pca_spqc_only){
  df <- df[grep("SPQC", df$Sample.description),]
 }
 
 
 df_data <- df |> dplyr::select(any_of(df_report$R_colnames))
 df_data <- as.data.frame(sapply(df_data, as.numeric))
 df_plot <- cbind(df$Sample.description, df_data)
 
 namex <- stringr::str_c(df$Sample.bar.code, "_", df$Sample.description, "_", df$Submission.name)
 
 color_list <- c("red", "blue", "orange", "purple", "darkgreen", "black", "brown", "cyan", "magenta", "yellow", "green", "pink", "grey", "lightblue", "darkred", "darkblue", "lightgreen", "orchid", "darkorange", "maroon", "yellow4", "darkcyan", "darkmagenta", "yellow2", "royalblue", "purple1", "skyblue", "deeppink")
 color_list <- color_list[1:length(unique(df$Sample.description))]
 
 x_pca <- prcomp(df_plot[,-1], scale=TRUE)
 test_this <- df_plot[,1]

 x_gr <- factor(unlist(test_this))
 cat(file=stderr(), "interactive_pca2d...2" , "\n")
 summary(x_gr)
 df_out <- as.data.frame(x_pca$x)
 #df_out_test <<- df_out
 df_xgr <- data.frame(x_gr)
 #df_xgr_test <<- df_xgr
 #df_xgr$x_gr <- as.character(df_xgr$x_gr)
  
 cat(file=stderr(), "interactive_pca2d...3" , "\n")
 hover_data <- data.frame(cbind(namex, df_out[[input$stats_pca2d_x]], df_out[[input$stats_pca2d_y]]), stringsAsFactors = FALSE  )
 colnames(hover_data) <- c("Sample", "get(input$stats_pca2d_x)", "get(input$stats_pca2d_y)")
 hover_data$`get(input$stats_pca2d_x)` <- as.numeric(hover_data$`get(input$stats_pca2d_x)`)
 hover_data$`get(input$stats_pca2d_y)` <- as.numeric(hover_data$`get(input$stats_pca2d_y)`)
  
  #hover_data_test <<- hover_data
  create_stats_pca2d <- reactive({
    ggplot(df_out, aes(x=get(input$stats_pca2d_x), y=get(input$stats_pca2d_y), color=x_gr )) +
      geom_point(alpha=0.5, size=input$stats_pca2d_dot_size) +
      theme(legend.title=element_blank()) +
      ggtitle(input$stats_pca2d_title) + 
      ylab(input$stats_pca2d_y) +
      xlab(input$stats_pca2d_x) +
      scale_color_manual(values = rev(unique(color_list))) +
      theme(plot.title = element_text(hjust = 0.5, size=input$stats_pca2d_title_size), 
            axis.title = element_text(size=input$stats_pca2d_label_size, color="black"),
            axis.text.x = element_text(size=input$stats_pca2d_label_size, angle = 90,  color="black"),
            axis.text.y = element_text(size=input$stats_pca2d_label_size,  color="black"),
      ) 
  })
  
  output$stats_pca2d <- renderPlot({
    req(create_stats_pca2d())
    create_stats_pca2d()
  })
  
  output$download_stats_pca2d <- downloadHandler(
    file = function(){
      stringr::str_c("stats_pca2d_", params$material_select, ".png", collapse = " ")
    },
    content = function(file){
      req(create_stats_pca2d())
      ggsave(file, plot = create_stats_pca2d(), device = 'png')
    }
  )
  
  output$hover_pca2d_info <- renderUI({
    hover <- input$plot_pca2d_hover
    point <- nearPoints(hover_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- left_pct * (hover$range$right - hover$range$left)
    top_px <- top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    
    if(top_pct > 0.3){
      top_custom <- 10
    }else{
      top_custom <- 200
    }
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", 10, "px; top:", top_custom, "px;")
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Sample: </b>", point$Sample, "<br/>")))
    )
  })
  
  
  cat(file = stderr(), "interactive_pca2d...end" , "\n")
}
