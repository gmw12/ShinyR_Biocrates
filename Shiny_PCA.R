cat(file = stderr(), "Shiny_PCA.R", "\n")


#------------------------------------------------------------------------------------------------------
create_qc_plots <- function(sesion, input, output, params){
  cat(file = stderr(), "Function create_qc_plots", "\n")
  showModal(modalDialog("Creating Plots...", footer = NULL))
  
  input_qc_acc <- input$qc_acc
  
  bg_qc_bar <- callr::r_bg(func = qc_grouped_plot_bg, args = list("QC", "QC_Report", input_qc_acc, params), stderr = str_c(params$error_path,  "//error_qcbarplot.txt"), supervise = TRUE)
  bg_qc_box <- callr::r_bg(func = box_plot_bg, args = list("QC", "QC_Report", params), stderr = str_c(params$error_path, "//error_qcboxplot.txt"), supervise = TRUE)
  
  bg_spqc_bar <- callr::r_bg(func = qc_grouped_plot_bg, args = list("SPQC", "QC_Report", input_qc_acc, params), stderr = str_c(params$error_path,  "//error_spqcbarplot.txt"), supervise = TRUE)
  bg_spqc_box <- callr::r_bg(func = box_plot_bg, args = list("SPQC", "QC_Report", params), stderr = str_c(params$error_path, "//error_spqcboxplot.txt"), supervise = TRUE)
  
  bg_qc_box$wait()
  bg_qc_bar$wait()
  bg_spqc_box$wait()
  bg_spqc_bar$wait()
  
  print_stderr("error_qcbarplot.txt")
  print_stderr("error_qcboxplot.txt")
  print_stderr("error_spqcbarplot.txt")
  print_stderr("error_spqcboxplot.txt")
  
  wait_cycle <- 0
  while (!file.exists(str_c(params$plot_path,"SPQC_barplot.png"))) {
    if (wait_cycle < 10) {
      Sys.sleep(0.5)
      wait_cycle <- wait_cycle + 1
    }
  }
  
  ui_render_qc_plots(session, input, output)
  
  cat(file = stderr(), "create_qc_plots...end", "\n")
  removeModal()
}

#------------------
qc_grouped_plot_bg <- function(plot_title, table_name, input_qc_acc, params) {
  cat(file = stderr(), stringr::str_c("function qc_grouped_plot_bg...."), "\n")
  source('Shiny_File.R')
    
  lower_limit <- 100 - input_qc_acc
  upper_limit <- 100 + input_qc_acc
  lower_limit_text <- stringr::str_c("Accuracy < ", lower_limit)
  upper_limit_text <- stringr::str_c("Accuracy > ", lower_limit)
  
  
  df <- read_table_try(table_name, params)
  test <- df |> dplyr::select(contains("Accuracy")) 
  test[test < lower_limit | test > upper_limit] <- 0
  test[test > 0] <- 1
  
  good <- colSums(test)
  bad <- nrow(test) - good
  
  data_plot = data.frame(matrix(vector(), ncol(test)*2, 3,
                         dimnames=list(c(), c("Sample", "QC", "Count"))),
                  stringsAsFactors=F)
  colnames(test) <- gsub("Accuracy", "", colnames(test))
  data_plot$Sample <- c(colnames(test), colnames(test))
  data_plot$Count <- c(good, bad)
  data_plot$QC <- c(rep(lower_limit_text, ncol(test)), rep(upper_limit_text, ncol(test)))
  
  
  file_name <- stringr::str_c(params$plot_path, plot_title, "_barplot.png")
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
box_plot_bg <- function(plot_title, table_name, params) {
  cat(file = stderr(), "Function box_plot_bg", "\n")
  
  source("Shiny_File.R")
  
  df <- read_table_try(table_name, params)
  df_acc <- df |> dplyr::select(contains("Accuracy"))
  #remove "Accuracy" from column names
  colnames(df_acc) <- gsub("Accuracy", "", colnames(df_acc))
  
  df_acc <- df_acc |>  dplyr::mutate(across(!where(is.numeric), as.numeric))
  data_box <- df_acc
  #reverse column order in data_box
  data_box <- data_box[,ncol(data_box):1]
  
  file_name <- stringr::str_c(params$plot_path, plot_title, "_boxplot.png")
  plot_title <- stringr::str_c(plot_title, " Boxplot")
  
  
  #create color_list length of ncol df_acc
  color_list <- c("red", "blue", "darkgreen", "purple", "orange", "black", "brown", "cyan", "magenta", "yellow")
  
  png(filename = file_name, width = 800, height = 600)
  graphics::boxplot(data_box, 
                    col = color_list, 
                    notch = TRUE, 
                    boxwex = 0.8,
                    #ylab = design$Label,
                    main = c(plot_title),
                    axes = TRUE,
                    horizontal = TRUE,
                    las = 1,
                    graphics::par(mar = c(2,10,4,1))) #bottom, left, top, right
  dev.off()
  cat(file = stderr(), "Function box_plot_bg...end", "\n")
}
#------------------------------------------------------------------------------------------------------------------------

interactive_pca2d <- function(session, input, output, params) {
  cat(file = stderr(), "interactive_pca2d..." , "\n")
  
  if(input$data_type == 1){
    table_name <- input$material_explore
  }else{
    table_name <- stringr::str_c("filtered_", input$material_explore)  
  }
  
 df <- read_table_try(table_name, params)
 df_report <- read_table_try("Report", params)
 
 updateTextInput(session, "stats_pca2d_title", value = stringr::str_c(input$material_explore, " PCA2D"))
 
 df$Sample.description[grep("SPQC", df$Sample.description)] <- "SPQC"
 df$Sample.description[grep("NIST", df$Sample.description)] <- "NIST"
 df$Sample.description[grep("Golden West", df$Sample.description)] <- "GW"
 df$Sample.description[grep("XXXX", df$Sample.description)] <- "Sample"
 #df$Sample.description <- stringr::str_c(df$Sample.description, "_", df$Submission.name)    

 df_data <- df |> dplyr::select(any_of(df_report$R_colnames))
 
 df_data <- as.data.frame(sapply(df_data, as.numeric))
 df_plot <- cbind(df$Sample.description, df_data)
 
 namex <- stringr::str_c(df$Sample.bar.code, "_", df$Sample.description, "_", df$Submission.name)
 
 color_list <- c("red", "blue", "darkgreen", "purple", "orange", "black", "brown", "cyan", "magenta", "yellow")
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
    filename = function(){
      str_c("stats_pca2d_", comp_name, ".png", collapse = " ")
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
