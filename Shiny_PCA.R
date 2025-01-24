cat(file = stderr(), "Shiny_PCA.R", "\n")

#------------------------------------------------------------------------------------------------------------------------

interactive_pca2d <- function(session, input, output, params)
{
  
  if(input$data_type == 1){
    table_name <- input$material_explore
  }else{
    table_name <- stringr::str_c("filtered_", input$material_explore)  
  }
  
  df <- read_table_try(table_name, params)
 df_report <- read_table_try("Report", params)
 
 updateTextInput(session, "stats_pca2d_title", value = "Plasma PCA 2D")
 
 df$Sample.description[grep("SPQC", df$Sample.description)] <- "SPQC"
 df$Sample.description[grep("NIST", df$Sample.description)] <- "NIST"
 df$Sample.description[grep("Golden West", df$Sample.description)] <- "GW"
 df$Sample.description[grep("XXXX", df$Sample.description)] <- "Sample"
 #df$Sample.description <- stringr::str_c(df$Sample.description, "_", df$Submission.name)    

 df_data <- df |> dplyr::select(any_of(df_report$R_colnames))
 
 df_data <- as.data.frame(sapply(df_data, as.numeric))
 df_plot <- cbind(df$Sample.description, df_data)
 
 namex <- df$Sample.bar.code
 color_list <- c("red", "blue", "green", "purple", "orange", "black", "brown", "cyan", "magenta", "yellow")
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
      geom_point(alpha=0.8, size=input$stats_pca2d_dot_size) +
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
  
}
