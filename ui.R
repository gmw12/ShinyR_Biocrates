cat(file = stderr(), "ui.R started", "\n")

source('Shiny_Libraries.R')


sidebar <- dashboardSidebar(width = 165,
                            useShinyjs(),
                            sidebarMenu(
                              menuItem("Welcome", tabName = "welcome", selected = TRUE),
                              menuItem("Load", tabName = "load", selected = FALSE),
                              menuItem("QC", tabName = "qc", selected = FALSE),
                              menuItem("Samples", tabName = "samples", selected = FALSE),
                              menuItem("Admin", tabName = "admin", selected = FALSE)
                            )
)

body <- dashboardBody(
  useShinyjs(),
  tabItems(
    
    tabItem(tabName = "welcome",
            fluidRow(
              column(width = 12, align = "center",
                     br(),
                     br(),
                     br(),
                     br(),
                     h1("Welcome to Duke Biocrates Data Processing Tool", style = "font-size:60px"),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     img(src = 'Biocrates-Logo-1.png', align = "center", width = 600, height = 134 )
              )
            )
    ),

    
    # Design 
    tabItem(tabName = "load",
            fluidRow(
              column(width = 4,
                     fluidRow(
                       box(title = "Current State", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 150,
                           span(textOutput("data_source"), style = "color:blue; font-size:16px")
                       ),
                       box(title = "Start from Scratch", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 375,
                           tags$h4("1. Enter file prefix for data output"),
                           fluidRow(align = "center", textInput("file_prefix", label = "", width = 300, value = "project_date")),
                           
                           tags$h4("2. Select and Load the Biocrates configuration file..."),
                           
                           fluidRow(align = "center", shinyFilesButton('sfb_config_file', label = 'Load Configuration File', title = 'Please select excel design file', multiple = FALSE,
                                                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; display:center")),
                           
                           tags$h4("3. Select data file(s).."),
                           
                           fluidRow(align = "center", shinyFilesButton('sfb_data_file', label = 'Select Data File', title = 'Please select data file(s)', multiple = FALSE,
                                                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                          
                       ),
                       
                       box(title = "Start from Previous Analysis", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "center", width = 12, height = 200,
                           tags$h3("Select database file"),
                           fluidRow(align = "center", shinyFilesButton('sfb_archive_file', label = 'Select Archive/Zip File', title = 'Please select zip file', multiple = FALSE,
                                                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                           span(textOutput("archive_file_name"), style = "color:blue; font-size:16px")
                        
                       )
                     )),
              
              
              column(width = 8,  
                     box(title = "Log", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 725,
                         
                         span(textOutput("config_file_name"), style = "color:blue; font-size:16px"), 
                         br(),
                         span(textOutput("data_file_name"), style = "color:blue; font-size:16px"),
                         br(),
                         span(textOutput("plate_names"), style = "color:blue; font-size:16px"), 
                         br(),
                         span(textOutput("plate_count"), style = "color:blue; font-size:16px"),
                         br(),
                         span(textOutput("material_names"), style = "color:blue; font-size:16px"), 
                         br(),
                         span(textOutput("material_count"), style = "color:blue; font-size:16px"),
                         br()
                         
                     ))
            )
    ),


    #Parameters
    tabItem(tabName = "qc",
            fluidRow(
              column(width = 2,
                     fluidRow(
                       box(id = "qc_box", title = "Process Biocrates Data...", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 750,
                          br(),
                          br(),
                          tags$h5("<LOD Imputation"),
                          checkboxInput("fixed_lod", label = "Use Fixed LOD", value = FALSE),
                          actionButton("replace_lod", label = "Impute LOD", width = 150,
                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          br(),
                          hr(),
                          tags$b("Process SPQC and QC data"),
                          br(),
                          numericInput("qc_acc", label = "QC %Accuracy Limit", value = 30, min = 0, max = 100),
                          br(),
                          actionButton("qc_calc", label = "QC Calc", width = 150,
                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                          
                       )
                       
                       )),
              
              column(width = 10,  
                     fluidRow(
                       tabBox(id="process_data", width = 12, height = 750,
                        
                        tabPanel("Sample Data",
                                 column(width =12, offset =0,
                                        hr(),
                              tags$head(tags$style("#data_table{color: blue; font-size: 12px;}")),
                              DT::dataTableOutput("data_table", width ='100%')
                           )
                        ),                              
                              
                        tabPanel("Summary",
                           column(width =12, offset =0,
                              hr(),
                              tags$head(tags$style("#report_table{color: blue; font-size: 12px;}")),
                              DT::dataTableOutput("report_table", width ='100%')
                           )
                          ),
                        
                        tabPanel("QC Data",
                                 column(width =12, offset =0,
                                        hr(),
                                        tags$head(tags$style("#qc_table{color: blue; font-size: 12px;}")),
                                        DT::dataTableOutput("qc_table", width ='100%')
                                 )
                        ),
                        
                        tabPanel("QC Barplot",
                                 column(width =12, offset =0,
                                        hr(),
                                        imageOutput("qc_bar")
                                 )
                        ),
                        
                        tabPanel("QC Boxplot",
                                 column(width =12, offset =0,
                                        hr(),
                                        imageOutput("qc_box")
                                 )
                        )
                        

                       )
            )
      )
    )
    ),
    
    #Parameters
    tabItem(tabName = "samples",
            fluidRow(
              column(width = 2,
                     fluidRow(
                       box(id = "sample_box", title = "Process Biocrates Data...", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 750,
                           selectInput(inputId = "material_select", label = h3("Select Materials for Data Output"),  choices = c("1", "2", "3"), 
                                       selected = "1"),
                           hr(),
                           tags$h3("Preprocess/Filter Data"),
                           checkboxInput("spqc_filter", label = "SPQC %CV Filter", value = FALSE),
                           numericInput("spqc_filter_value", label = "Max %CV", value = 30, min = 0, max = 100),
                           checkboxInput("missing_filter", label = "Max% <LOD values", value = FALSE),
                           numericInput("missing_filter_value", label = "Max % <LOD", value = 50, min = 0, max = 100),
                           br(),
                           br(),
                           actionButton("filter_calc", label = "Preprocess Data", width = 150,
                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                           
                       )
                       
                     )),
              
              column(width = 10,  
                     fluidRow(
                       tabBox(id="explore_data", width = 12, height = 750,
                              
                              tabPanel("Data",
                                       column(width =12, offset =0,
                                              hr(),
                                              tags$head(tags$style("#material_data{color: blue; font-size: 12px;}")),
                                              DT::dataTableOutput("material_table", width ='100%')
                                       )
                              ),
                              
                              tabPanel("Filtered Data",
                                       column(width =12, offset =0,
                                              hr(),
                                              tags$head(tags$style("#filter_material_data{color: blue; font-size: 12px;}")),
                                              DT::dataTableOutput("filter_material_table", width ='100%')
                                       )
                              ),
                              
                              tabPanel("SPQC Barplot",
                                       column(width =12, offset =0,
                                              hr(),
                                              imageOutput("spqc_bar")
                                       )
                              ),
                              
                              tabPanel("SPQC Boxplot",
                                       column(width =12, offset =0,
                                              hr(),
                                              imageOutput("spqc_box")
                                       )
                              ),
                              
                              tabPanel("SPQC Norm Factor",
                                       column(width =12, offset =0,
                                              hr(),
                                              imageOutput("spqc_line")
                                       )
                              ),
                              
                              
                              tabPanel("PCA",
                                       column(width =3, offset =0,
                                              radioButtons("data_type", label = h3("Data Type"),
                                                           choices = list("Unfiltered" = 1, "Filtered" = 2), 
                                                           selected = 1),
                                              br(),
                                              checkboxInput("pca_by_plate", label = "Separate by Plate?", value = FALSE),
                                              br(),
                                              checkboxInput("pca_spqc_only", label = "Show only SPQC?", value = FALSE),
                                              br(),
                                              actionButton("create_pca", label = "Create PCA", width = 150,
                                                           style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              hr(),
                                              br(),
                                              br(),
    
                                              dropdownButton(
                                                selectInput("stats_pca2d_x", label = "pca xaxis", choices = list("PC1", "PC2", "PC3", "PC4", "PC5"), 
                                                            selected = "PC1"),
                                                selectInput("stats_pca2d_y", label = "pca yaxis", choices = list("PC1", "PC2", "PC3", "PC4", "PC5"), 
                                                            selected = "PC2"),
                                                textInput("stats_pca2d_title", label="plot title", value = "pca2d", width = 200),
                                                sliderInput("stats_pca2d_label_size", label = h5("Label Size"), min = 1, 
                                                            max = 50, value = 11),
                                                sliderInput("stats_pca2d_title_size", label = h5("Title Size"), min = 10, 
                                                            max = 50, value = 20),
                                                sliderInput("stats_pca2d_dot_size", label = h5("Point Size"), min = 1, 
                                                            max = 10, value = 2),
                                                circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
                                                tooltip = tooltipOptions(title = "Click to see inputs !")
                                              )),
                                           column(width =9, offset =0,
                                                div(
                                                  style = "position:relative",
                                                  plotOutput("stats_pca2d", width = 800, height = 600,
                                                             hover = hoverOpts("plot_pca2d_hover", delay = 100, delayType = "debounce")),
                                                  uiOutput("hover_pca2d_info")
                                                ),
                                                downloadButton('download_stats_pca2d')
                                       )  
                              ),
                              
                              tabPanel("Excel Output",
                                       column(width =12, offset =0,
                                              br(),
                                              tags$h3("Create Excel Data Return"),
                                              br(),
                                              
                                              checkboxInput("excel_raw_data", label = "Raw Data", value = TRUE),
                                              checkboxInput("excel_raw_data_no_indicator", label = "Raw Data no status/indicators", value = FALSE),
                                              checkboxInput("excel_impute_data", label = "Imputed Data", value = TRUE),
                                              checkboxInput("excel_report", label = "Sample Info Report", value = TRUE),
                                              checkboxInput("excel_qc_report", label = "QC Report", value = TRUE),
                                              checkboxInput("excel_samples", label = "Analyzed Materials", value = TRUE),
                                              
                                              textInput("excel_filename", label = "File Name", value = "final_data_return.xlsx", width = 250),
                                              
                                              actionButton("create_excel", label = "Create Excel", width = 150,
                                                           style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")

                                       )
                              ),
                              
                       )
                     )
              )
            )
    ),
    

    tabItem(tabName = "admin",
            fluidRow(
              column(width = 12, align = "center",
                     br(),
                     br(),
                     h1("Admin Functions", style = "font-size:60px"),
                     br(),
                     br(),
                     br(),
                     textInput("archive_data_filename", label="File Name", value = "mydata.zip", width = 250),
                     br(),
                     br(),
                     actionButton("archive_data", label = "Save Data", width = 100,
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     actionButton("clean_environment", label = "Clean Environment", width = 150,
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
              )
            )
    )
    
    
    
    #---------------------------------------------------------------------
    
    
    
    
    
  )
)


dashboardPage(
  dashboardHeader(title = "Duke Biocrates Data Processing", titleWidth = 325),
  sidebar,
  body
)
#end of ui.R