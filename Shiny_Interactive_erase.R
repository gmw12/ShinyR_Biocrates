cat(file = stderr(), "Shiny_Interactive.R", "\n")
#----------------------------------------------------------

interactive_barplot <- function(session, input, output, df, namex, color_list, output_name, comp_name, plot_number)
{
  cat(file = stderr(), "Function interactive_barplot...", "\n")

  #save(list = c("df", "namex", "color_list", "output_name", "comp_name", "plot_number"), file="z4322")
  #. load(file="z4322")
  
  datay <- colSums(df, na.rm = TRUE)
  df2 <- data.frame(namex)
  df2$Total_Intensity <- datay
  colnames(df2) <- c("Sample", "Total_Intensity")
  df2$Sample <- factor(df2$Sample, levels = df2$Sample)
  ymax <- max(datay)
  
  cat(file = stderr(), "interactive_barplot...1", "\n")
  create_stats_barplot <- reactive({
    ggplot(data = df2, aes(x = Sample, y = Total_Intensity)) +
      geom_bar(stat = "identity", fill = color_list) + theme_classic() + 
      ggtitle(input[[stringr::str_c(plot_number, "_",output_name, "_title")]]) + 
      ylab(input[[stringr::str_c(plot_number, "_",output_name, "_y_axis_label")]]) +
      xlab(NULL) +
      #scale_y_discrete(labels = NULL) +
      coord_cartesian(ylim = NULL, expand = TRUE) +
      theme(plot.title = element_text(hjust = 0.5, size = input[[stringr::str_c(plot_number, "_", output_name, "_title_size")]]), 
            axis.title = element_text(size = input[[stringr::str_c(plot_number, "_", output_name, "_label_size")]], color = "black"),
            axis.text.x = element_text(size = input[[stringr::str_c(plot_number, "_", output_name, "_label_size")]], angle = 90,  color = "black"),
            axis.text.y = element_text(size = input[[stringr::str_c(plot_number, "_", output_name, "_label_size")]],  color = "black"),
      ) 
  })
  
  output[[stringr::str_c(plot_number, "_",output_name)]] <- renderPlot({
    req(create_stats_barplot())
    create_stats_barplot()
  })
  
  cat(file = stderr(), "interactive_barplot...2", "\n")
  output[[stringr::str_c(plot_number, "_download_", output_name)]] <- downloadHandler(
    filename = function(){
      stringr::str_c(plot_number, "_Stats_Barplot_", comp_name,  ".png", collapse = " ")
    },
    content = function(file){
      req(create_stats_barplot())
      ggsave(file, plot = create_stats_barplot(), device = 'png')
    }
  )
  cat(file = stderr(), "Function interactive_barplot...end", "\n")
}

#------------------------------------------------------------------------------------------------------------------------


interactive_boxplot <- function(session, input, output, df, namex, color_list, comp_string, plot_number)
{
  cat(file = stderr(), "interactive_boxplot" , "\n")
  
  colnames(df) <- namex
  df3 <- log2(df) %>% gather(Sample, Intensity, colnames(df))
  df3$Sample <- factor(df3$Sample, levels = rev(namex))
  
  create_stats_boxplot <- reactive({
    ggplot2::ggplot(data = df3, ggplot2::aes(x = Sample, y = Intensity)) +
      ggplot2::geom_boxplot(notch = TRUE, outlier.colour = "red", outlier.shape = 1,
                            outlier.size = 1, fill = rev(color_list)) + ggplot2::theme_classic() + 
      ggplot2::coord_flip() +
      ggplot2::xlab(input[[stringr::str_c(plot_number, "_stats_boxplot_x_axis_label")]]) +
      ggplot2::ggtitle(input[[stringr::str_c(plot_number, "_stats_boxplot_title")]]) + 
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = input[[stringr::str_c(plot_number, "_stats_boxplot_title_size")]]), 
                     axis.title = ggplot2::element_text(size = input[[stringr::str_c(plot_number, "_stats_boxplot_label_size")]], color = "black"),
                     axis.text.x = ggplot2::element_text(size = input[[stringr::str_c(plot_number, "_stats_boxplot_label_size")]], angle = 90,  color = "black"),
                     axis.text.y = ggplot2::element_text(size = input[[stringr::str_c(plot_number, "_stats_boxplot_label_size")]],  color = "black"),
      ) 
  })
  
  output[[stringr::str_c(plot_number, "_stats_boxplot")]] <- renderPlot({
    req(create_stats_boxplot())
    #callr::r_bg(create_stats_boxplot, args = list(df3=df3), supervise = TRUE)
    create_stats_boxplot()
  })
  
  output[[stringr::str_c(plot_number, "_download_stats_boxplot")]]  <- downloadHandler(
    filename = function(){
      stringr::str_c(plot_number, "_stats_Boxplot_", comp_string, ".png", collapse = " ")
    },
    content = function(file){
      req(create_stats_boxplot())
      ggsave(file, plot = create_stats_boxplot(), device = 'png')
    }
  )
  
}
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

interactive_pca2d <- function(session, input, output, df, namex, color_list, groupx, comp_name, plot_number)
{
  #save(df, file="pcadf"); save(groupx, file="pca_groupx")
  #load(file="pcadf"); load(file="pca_groupx")
  df_sd <-apply(df, 1, sd) 
  
  
  cat(file = stderr(), "interactive_pca2d..." , "\n")
  x_transpose <- t(df)
  x_transpose <- data.frame(x_transpose)
  cat(file = stderr(), "interactive_pca2d...1" , "\n")
  row.names(x_transpose) <- NULL
  x_transpose <- cbind(groupx, x_transpose)
  
  x_pca <- prcomp(x_transpose[,-1], scale = TRUE)
  
  test_this <- x_transpose[,1]
  x_gr <- factor(unlist(test_this))
  cat(file = stderr(), "interactive_pca2d...2" , "\n")
  summary(x_gr)
  df_out <- as.data.frame(x_pca$x)
  #df_out_test <<- df_out
  df_xgr <- data.frame(x_gr)
  #df_xgr_test <<- df_xgr
  #df_xgr$x_gr <- as.character(df_xgr$x_gr)
  
  cat(file = stderr(), "interactive_pca2d...3" , "\n")
  
  pca_y <- input[[stringr::str_c(plot_number, "_stats_pca2d_x")]]
  pca_x <- input[[stringr::str_c(plot_number, "_stats_pca2d_y")]]
  
  hover_data <- data.frame(cbind(namex, df_out[[pca_x]], df_out[[pca_y]]), stringsAsFactors = FALSE  )
  colnames(hover_data) <- c("Sample", "get(pca_x)", "get(pca_y)")
  hover_data$`get(pca_x)` <- as.numeric(hover_data$`get(pca_x)`)
  hover_data$`get(pca_y)` <- as.numeric(hover_data$`get(pca_y)`)
  
  cat(file = stderr(), "interactive_pca2d...4" , "\n")
  create_stats_pca2d <- reactive({
    ggplot(df_out, aes(x = get(pca_x), y = get(pca_y), color = x_gr )) +
      geom_point(alpha = 0.8, size = input[[stringr::str_c(plot_number, "_stats_pca2d_dot_size")]] ) +
      theme(legend.title = element_blank()) +
      ggtitle(input[[stringr::str_c(plot_number, "_stats_pca2d_title")]]) + 
      ylab(pca_y) +
      xlab(pca_x) +
      scale_color_manual(values = rev(unique(color_list))) +
      theme(plot.title = element_text(hjust = 0.5, size = input[[stringr::str_c(plot_number, "_stats_pca2d_title_size")]]), 
            axis.title = element_text(size = input[[stringr::str_c(plot_number, "_stats_pca2d_label_size")]], color = "black"),
            axis.text.x = element_text(size =  input[[stringr::str_c(plot_number, "_stats_pca2d_label_size")]], angle = 90,  color = "black"),
            axis.text.y = element_text(size =  input[[stringr::str_c(plot_number, "_stats_pca2d_label_size")]],  color = "black"),
      ) 
  })
  
  cat(file = stderr(), "interactive_pca2d...5" , "\n")
  output[[stringr::str_c(plot_number, "_stats_pca2d")]] <- renderPlot({
    req(create_stats_pca2d())
    create_stats_pca2d()
  })
  
  output[[stringr::str_c(plot_number, "_download_stats_pca2d")]] <- downloadHandler(
    filename = function(){
      stringr::str_c(plot_number, "_stats_pca2d_", comp_name, ".png", collapse = " ")
    },
    content = function(file){
      req(create_stats_pca2d())
      ggsave(file, plot = create_stats_pca2d(), device = 'png')
    }
  )
  
  cat(file = stderr(), "interactive_pca2d...6" , "\n")
  output[[stringr::str_c(plot_number, "_hover_pca2d_info")]] <- renderUI({
    hover <- input[[stringr::str_c(plot_number, "_plot_pca2d_hover")]]
    point <- nearPoints(hover_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- left_pct * (hover$range$right - hover$range$left)
    top_px <- top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    
    cat(file = stderr(), "interactive_pca2d...7" , "\n")
    if (top_pct > 0.3) {
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

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

interactive_pca3d <- function(session, input, output, df, namex, color_list, groupx, comp_name, plot_number)
{
  cat(file = stderr(), "interactive_pca3d" , "\n")
  
  #   save(df, file="z1"); save(groupx, file="z2"); save(namex, file="z3")
  #    load(file="z1"); load(file="z2"); load(file="z3")
  
  x_transpose <- t(df)
  x_transpose <- data.frame(x_transpose)
  row.names(x_transpose) <- NULL
  x_transpose <- cbind(groupx, x_transpose)
  
  x_pca <- prcomp(x_transpose[,-1], scale = TRUE)
  
  test_this <- x_transpose[,1]
  x_gr <- factor(unlist(test_this))
  summary(x_gr)
  
  if (input[[stringr::str_c(plot_number, "_stats_pca3d_show_input")]]) {
    show_labels = namex
    }else{
      show_labels = ""
    }
  
  create_stats_pca3d <- reactive({
    pca3d(x_pca, 
          group = x_gr,
          new = FALSE,
          legend = "right",
          show.labels = show_labels,
          palette = rev(unique(color_list)), 
          radius = input[[stringr::str_c(plot_number, "_stats_pca3d_dot_size")]],
          title = input[[stringr::str_c(plot_number, "_stats_pca3d_title")]]
    )
  })
  
  output[[stringr::str_c(plot_number, "_stats_pca3d")]] <- renderRglwidget({
    #try(rgl.close())
    try(rgl::close3d)
    req(create_stats_pca3d())
    create_stats_pca3d()
    rglwidget()
  })
  
  output[[stringr::str_c(plot_number, "_download_stats_pca3d")]] <- downloadHandler(
    filename = function(){
      stringr::str_c(plot_number, "_stats_pca3d_", comp_name, ".png", collapse = " ")
    },
    content = function(file){
      req(create_stats_pca3d())
      snapshotPCA3d(file)
      #ggsave(file, plot = create_stats_pca3d(), device = 'png')
    }
  )
}

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

interactive_cluster <- function(session, input, output, df, namex, comp_name, plot_number)
{
  cat(file = stderr(), "interactive_cluster" , "\n")
  require('factoextra')
  
  colnames(df) <- namex
  
  df <- t(df)
  df <- data.frame(df)
  #row.names(df) <- NULL
  #df <- na.omit(df)
  df[] <- lapply(df, as.numeric)
  df <- scale(df)
  
  
  create_stats_cluster <- reactive({
    distance <- get_dist(df, method = "euclidean")
    fviz_dist(distance,  show_labels = TRUE, gradient = list(low = input[[stringr::str_c(plot_number, "_cluster_low_color")]], mid = "white", high = input[[stringr::str_c(plot_number, "_cluster_high_color")]] )) +
      ggtitle(input[[stringr::str_c(plot_number, "_stats_cluster_title")]]) +
      theme(plot.title = element_text(hjust = 0.5, size = input[[stringr::str_c(plot_number, "_stats_cluster_title_size")]] ), 
            axis.text.x = element_text(size = input[[stringr::str_c(plot_number, "_stats_cluster_label_size")]], angle = 90,  color = "black"),
            axis.text.y = element_text(size = input[[stringr::str_c(plot_number, "_stats_cluster_label_size")]],  color = "black"))
  })
  
  output[[stringr::str_c(plot_number, "_stats_cluster")]] <- renderPlot({
    req(create_stats_cluster())
    create_stats_cluster()
  })
  
  output[[stringr::str_c(plot_number, "_download_stats_cluster")]] <- downloadHandler(
    filename = function(){
      stringr::str_c(plot_number, "_stats_cluster_", comp_name, ".png", collapse = " ")
    },
    content = function(file){
      req(create_stats_cluster())
      ggsave(file, plot = create_stats_cluster(), device = 'png')
    }
  )
  
  
}


#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

interactive_heatmap <- function(session, input, output, df, namex, groupx, comp_name, params, plot_number)
{
  cat(file = stderr(), "interactive_heatmap..." , "\n")
  if (site_user == "dpmsr") {
    heatmap_filename <- stringr::str_c(params$database_dir, "/", plot_number, "_erasemyheatmap.png") #"erasemyheatmap.png"
  }else{
    heatmap_filename <- stringr::str_c(params$database_dir, "/", plot_number,  "/erasemyheatmap.png")
  }
  
  #if norm by protein one protein will have 0 stdev - which will crash the calc...
  if (params$data_output == "Protein") {
    df$stdev <-  apply(df[1:ncol(df)], 1, sd) 
    df <- df[df$stdev > 0.1,]
    df <- df[,1:ncol(df) - 1] 
  }
  
  cat(file = stderr(), "interactive_heatmap...1" , "\n")
  colnames(df) <- namex
  df <- log2(df)
  df <- data.matrix(df)
  
  ## Row- and column-wise clustering 
  hr <- hclust(as.dist(1 - cor(t(df), method = "pearson")), method = "complete")
  hc <- hclust(as.dist(1 - cor(df, method = "spearman")), method = "complete") 
  ## Tree cutting
  mycl <- cutree(hr, h = max(hr$height)/1.5); mycolhc <- rainbow(length(unique(mycl)), start = 0.1, end = 0.9); mycolhc <- mycolhc[as.vector(mycl)] 
  ## Plot heatmap 
  mycol <- redgreen(75)
  
  
  create_stats_heatmap <- reactive({
    heatmap.2(df, Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = mycol, labCol = groupx, 
              scale = "row", density.info = "none", trace = "none", RowSideColors = mycolhc, main = input[[stringr::str_c(plot_number, "_stats_heatmap_title")]],
              margins = c(10,10))
    #heatmap_filename <- stringr::str_c(dpmsr_set$file$output_dir, dpmsr_set$data$stats$final_comp, "//", "erasemyheatmap.png")
    png(filename = heatmap_filename, units = "px", width = 1776, height = 1776)  
    heatmap.2(df, Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = mycol, labCol = groupx, 
              scale = "row", density.info = "none", trace = "none", RowSideColors = mycolhc, main = input[[stringr::str_c(plot_number, "_stats_heatmap_title")]],
              margins = c(20,20))
    dev.off()
  })
  
  cat(file = stderr(), "interactive_heatmap...2" , "\n")
  
  output[[stringr::str_c(plot_number, "_stats_heatmap")]] <- renderPlot({
    req(create_stats_heatmap())
    create_stats_heatmap()
  })
  
  output[[stringr::str_c(plot_number, "_download_stats_heatmap")]] <- downloadHandler(
    filename = function(){
      stringr::str_c(plot_number, "_stats_heatmap_", comp_name, ".png", collapse = " ")
    },
    content = function(file){
      #heatmap_filename <- stringr::str_c(dpmsr_set$file$output_dir, dpmsr_set$data$stats$final_comp, "//", "erasemyheatmap.png")
      file.copy(heatmap_filename, file)
    }
  )
  
  cat(file = stderr(), stringr::str_c("heatmap file exists? ", heatmap_filename, "   ", file.exists(heatmap_filename)), "\n")
  #try(file_delete(heatmap_filename), silent = TRUE)
  cat(file = stderr(), stringr::str_c("deleting temp heatmap file ", file.exists(heatmap_filename)), "\n")
  
}




#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

interactive_stats_volcano <- function(session, input, output, df, xmax, ymax, highlight_list, highlight_df, highlight_stat_up, 
                                       highlight_stat_down, stats_plot_comp, plot_number) {
  cat(file=stderr(), stringr::str_c("interactive_stats_volcano...      ", stats_plot_comp, "   ", plot_number), "\n")
  
  #save(stats_plot_comp, file="z1") #load(file="z1")
  
  cat(file=stderr(), "Interactive stats volcano...3" , "\n")
  volcano_stats_plot <- reactive({
    ggplot(df, aes(x = log_fc, y = log_pvalue)) +
      theme_minimal() +
      geom_point(alpha=0.4, size=input[[stringr::str_c(plot_number, "_volcano_stats_plot_dot_size")]], color = input[[stringr::str_c(plot_number, "_volcano_stats_dot_color")]] ) +
      xlab(input[[stringr::str_c(plot_number, "_volcano_stats_plot_x_axis_label")]]) + 
      ylab(input[[stringr::str_c(plot_number, "_volcano_stats_plot_y_axis_label")]]) +
      scale_colour_gradient(low = input[[stringr::str_c(plot_number, "_volcano_stats_dot_color")]], high = input[[stringr::str_c(plot_number, "_volcano_stats_dot_color")]] ) +
      ggtitle(input[[stringr::str_c(plot_number, "_volcano_stats_plot_title")]])+    
      xlim(-xmax, xmax) +
      ylim(0, ymax) +
      theme(plot.title = element_text(size=input[[stringr::str_c(plot_number, "_volcano_stats_plot_title_size")]], hjust = 0.5),
            axis.title = element_text(size=input[[stringr::str_c(plot_number, "_volcano_stats_plot_label_size")]], color="black"),
            axis.text.x = element_text(size=10, color="black"),
            axis.text.y = element_text(size=10,  color="black"),
            legend.position = "none")+
      geom_vline(aes(xintercept = log(input$foldchange_cutoff, 2)),  linetype = "dotted", color = "black")  + 
      geom_vline(aes(xintercept = -log(input$foldchange_cutoff, 2)),  linetype = "dotted", color = "black")  + 
      geom_hline(aes(yintercept = -log(input$pvalue_cutoff, 10)),  linetype = "dotted", color = "black") +
      geom_point(data=highlight_df, aes(x=log_fc, y=log_pvalue), color=input[[stringr::str_c(plot_number, "_volcano_highlight_color")]], size=input[[stringr::str_c(plot_number, "_volcano_highlight_dot_size")]], 
                 alpha=input[[stringr::str_c(plot_number, "_volcano_highlight_alpha")]]) +
      geom_point(data=highlight_stat_up, aes(x=log_fc, y=log_pvalue), color=input[[stringr::str_c(plot_number, "_volcano_highlight_color_up")]], size=input[[stringr::str_c(plot_number, "_volcano_highlight_dot_size")]], 
                 alpha=input[[stringr::str_c(plot_number, "_volcano_highlight_alpha")]])  +
      geom_point(data=highlight_stat_down, aes(x=log_fc, y=log_pvalue), color=input[[stringr::str_c(plot_number, "_volcano_highlight_color_down")]], size=input[[stringr::str_c(plot_number, "_volcano_highlight_dot_size")]], 
                 alpha=input[[stringr::str_c(plot_number, "_volcano_highlight_alpha")]])
  })
  
  cat(file=stderr(), "Interactive stats volcano...4" , "\n")
  plot_name <- stringr::str_c(plot_number, "_volcano_stats_plot")
  download_name <- stringr::str_c(plot_number, "_download_stats_volcano")
  hover_name <- stringr::str_c(plot_number, "_volcano_stats_hover_info")
  hover_stats_name <- stringr::str_c(plot_number, "_volcano_stats_hover")
  
  output[[plot_name]]<- renderPlot({
    req(volcano_stats_plot())
    volcano_stats_plot()
  })
  
  output[[download_name]] <- downloadHandler(
    filename = function(){
      stringr::str_c(plot_number, "_Volcano_", stats_plot_comp, ".png", collapse = " ")
    },
    content = function(file){
      req(volcano_stats_plot())
      ggsave(file, plot = volcano_stats_plot(), device = 'png')
    }
  )
  
  cat(file=stderr(), "Interactive stats volcano...5" , "\n")
  output[[hover_name]] <- renderUI({
    hover <- input[[hover_stats_name]]
    point <- nearPoints(df, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- left_pct * (hover$range$right - hover$range$left)
    top_px <- top_pct * (hover$range$bottom - hover$range$top)
    
    #cat(file=stderr(),stringr::str_c("top_pct = ", top_pct), "\n")
    #cat(file=stderr(), stringr::str_c("top_px = ", top_px), "\n")
    
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
      p(HTML(paste0("<b> Accession: </b>", point$Accession, "<br/>",
                    "<b> Description: </b>", point$Description, "<br/>",
                    "<b> FC: </b>", point$fc, "<br/>",
                    "<b> pvalue: </b>", point$pval, "<br/>")))
    )
  })
  
  cat(file=stderr(), "Interactive stats volcano...end" , "\n")
  
}


#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------


interactive_grouped_peptide_barplot <- function(session, input, output, new_df, input_stats_onepeptide_plot_comp, color_list) {
  cat(file=stderr(), "Function interactive_grouped_peptide_barplot...", "\n")
  
  cat(file=stderr(), "Interactive group barplot...10" , "\n")
  # Grouped
  create_stats_barplot <- reactive({
    ggplot(new_df, aes(fill=Comp, y=y_mean, x=Sequence)) + 
      geom_col(color="black", width = 0.5,
               position=position_dodge(0.5))+
      theme_classic() + 
      ggtitle(input$stats_onepeptide_grouped_barplot_title) + 
      ylab(input$stats_onepeptide_grouped_barplot_y_axis_label) +
      xlab(input$stats_onepeptide_grouped_barplot_x_axis_label) +
      coord_cartesian(ylim=NULL, expand = TRUE) +
      theme(plot.title = element_text(hjust = 0.5, size=input$stats_onepeptide_grouped_barplot_title_size), 
            axis.title = element_text(size=input$stats_onepeptide_grouped_barplot_label_size, color="black"),
            axis.text.x = element_text(size=input$stats_onepeptide_grouped_barplot_label_size, 
                                       angle = input$stats_onepeptide_grouped_barplot_axis_angle, 
                                       vjust = input$stats_onepeptide_grouped_barplot_axis_vjust, color="black"),
            axis.text.y = element_text(size=input$stats_onepeptide_grouped_barplot_label_size,  color="black"),
      ) +
      scale_fill_manual(values = color_list)+
      geom_errorbar(aes(ymin=y_mean-sd, ymax=y_mean+sd), width=.25,
                    position=position_dodge(0.5)) +
      geom_hline(yintercept = 0, linetype="dotted", color = "black")+
      geom_hline(yintercept = 1, linetype="dotted", color = "black")+
      geom_hline(yintercept = -1, linetype="dotted", color = "black")
    
  })
  
  output$stats_onepeptide_grouped_barplot <- renderPlot({
    req(create_stats_barplot())
    create_stats_barplot()
  })
  
  output$download_stats_onepeptide_grouped_barplot <- downloadHandler(
    filename = function(){
      stringr::str_c("Grouped_Barplot_", as.character(input$stats_onepeptide_accession), "_", input_stats_onepeptide_plot_comp,  ".png", collapse = " ")
    },
    content = function(file){
      req(create_stats_barplot())
      ggsave(file, plot = create_stats_barplot(), device = 'png')
    }
  )  
  
  cat(file=stderr(), "Function interactive_grouped_peptide_barplot...end", "\n")
}




#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#match(namesdf, names(df))

interactive_grouped_barplot <- function(session, input, output, df, comp_name, color_list)
{
  cat(file=stderr(), "function interactive_grouped_barplot..." , "\n")

  if(input$stats_use_zscore){  
    updateTextInput(session, "stats_oneprotein_grouped_barplot_title", value = stringr::str_c(as.character(input$stats_oneprotein_accession), " - Average Peptide Zscore" )  )
  }else{
    updateTextInput(session, "stats_oneprotein_grouped_barplot_title", value = stringr::str_c(as.character(input$stats_oneprotein_accession), " - Average Peptide Intensity" )  )
  }

  # Grouped
  create_stats_barplot <- reactive({
    ggplot(df, aes(fill=Comp, y=y_mean, x=Position   )) + 
      geom_col(color="black", width = 0.5,
               position=position_dodge(0.5))+
      theme_classic() + 
      ggtitle(input$stats_oneprotein_grouped_barplot_title) + 
      ylab(input$stats_oneprotein_grouped_barplot_y_axis_label) +
      xlab(input$stats_oneprotein_grouped_barplot_x_axis_label) +
      coord_cartesian(ylim=NULL, expand = TRUE) +
      theme(plot.title = element_text(hjust = 0.5, size=input$stats_oneprotein_grouped_barplot_title_size), 
            axis.title = element_text(size=input$stats_oneprotein_grouped_barplot_label_size, color="black"),
            axis.text.x = element_text(size=input$stats_oneprotein_grouped_barplot_label_size, angle = 90,  color="black"),
            axis.text.y = element_text(size=input$stats_oneprotein_grouped_barplot_label_size,  color="black"),
      ) +
      scale_fill_manual(values = color_list)+
      geom_errorbar(aes(ymin=y_mean-sd, ymax=y_mean+sd), width=.25,
                    position=position_dodge(0.5)) +
      geom_hline(yintercept = 0, linetype="dotted", color = "black")+
      geom_hline(yintercept = 1, linetype="dotted", color = "black")+
      geom_hline(yintercept = -1, linetype="dotted", color = "black")
    
  })
  
  
  output$stats_oneprotein_grouped_barplot <- renderPlot({
    req(create_stats_barplot())
    create_stats_barplot()
  })
  
  output$download_stats_oneprotein_grouped_barplot <- downloadHandler(
    filename = function(){
      stringr::str_c("Grouped_Barplot_", as.character(input$stats_oneprotein_accession), "_", comp_name,  ".png", collapse = " ")
    },
    content = function(file){
      req(create_stats_barplot())
      ggsave(file, plot = create_stats_barplot(), device = 'png')
    }
  )  
  
  cat(file=stderr(), "Function interactive group barplot...end" , "\n")
}


#------------------------------------------------------------------------------------------------------------------------

interactive_go_volcano <- function(session, input, output, volcano_data)
{
  cat(file=stderr(), "Function interactive_go_volcano..." , "\n")
  
  volcano_go_plot <- reactive({
    ggplot(volcano_data, aes(x = log_fc, y = log_pvalue)) +
      theme_minimal() +
      geom_point(alpha=0.4, size=input$plot_dot_size, color = input$volcano_dot_color) +
      xlab(input$plot_x_axis_label) + ylab(input$plot_y_axis_label) +
      scale_colour_gradient(low = input$volcano_dot_color, high = input$volcano_dot_color) +
      ggtitle(input$plot_title)+    
      xlim(-max(volcano_data$log_fc), max(volcano_data$log_fc)) +
      geom_vline(aes(xintercept = log(input$foldchange_cutoff, 2)),  linetype = "dotted", color = "black")  + 
      geom_vline(aes(xintercept = -log(input$foldchange_cutoff, 2)),  linetype = "dotted", color = "black")  + 
      geom_hline(aes(yintercept = -log(input$pvalue_cutoff, 10)),  linetype = "dotted", color = "black") +
      theme(plot.title = element_text(size=input$plot_title_size, hjust = 0.5),
            axis.title = element_text(size=input$plot_label_size, color="black"),
            axis.text.x = element_text(size=10, color="black"),
            axis.text.y = element_text(size=10,  color="black"),
            legend.position = "none")
  })
  
  output$volcano_go_plot <- renderPlot({
    req(volcano_go_plot())
    volcano_go_plot()
  })
  
  output$download_go_volcano <- downloadHandler(
    filename = function(){
      stringr::str_c("GoVolcano_", input$select_data_comp_go, "_", input$go_volcano_id, "_", 
            input$select_ont_go, ".png", collapse = " ")
    },
    content = function(file){
      req(volcano_go_plot())
      ggsave(file, plot = volcano_go_plot(), device = 'png')
    }
  )
  
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(volcano_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    # left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    # top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    left_px <- left_pct * (hover$range$right - hover$range$left)
    top_px <- top_pct * (hover$range$bottom - hover$range$top)
    
    #cat(file=stderr(), stringr::str_c("hoverrr=", hover$range$right, "   hoverrl=", hover$range$left, "  left_pct=", left_pct), "\n")
    #cat(file=stderr(), stringr::str_c("hoverrb=", hover$range$bottom, "   hoverrt=", hover$range$top, "  top_pct=", top_pct), "\n")
    #cat(file=stderr(), stringr::str_c("leftpx=", left_px, "   toppx=", top_px), "\n", "\n"  )
    
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
      p(HTML(paste0("<b> Accession: </b>", point$Accession, "<br/>",
                    "<b> Description: </b>", point$Description, "<br/>",
                    "<b> FC: </b>", point$foldchange, "<br/>",
                    "<b> pvalue: </b>", point$pvalue, "<br/>")))
    )
  })
  
  cat(file=stderr(), "Function interactive_go_volcano...end" , "\n")
  return(volcano_data)
}









