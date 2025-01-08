cat(file = stderr(), "ui.R started", "\n")

source('Shiny_Libraries.R')


sidebar <- dashboardSidebar(width = 165,
                            useShinyjs(),
                            sidebarMenu(
                              menuItem("Welcome", tabName = "welcome", selected = TRUE),
                              menuItem("Load", tabName = "load", selected = FALSE),
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
                           span(textOutput("data_source"), style = "color:blue; font-size:16px"),
                           span(textOutput("data_input"), style = "color:blue; font-size:16px"),
                           span(textOutput("data_table_format"), style = "color:blue; font-size:16px"),
                           span(textOutput("data_ptm"), style = "color:blue; font-size:16px")
                       ),
                       box(title = "Start from Scratch", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 375,
                           tags$h4("1. Enter file prefix for data output"),
                           fluidRow(align = "center", textInput("file_prefix", label = "", width = 300, value = "project_date")),
                           
                           tags$h4("2. Select and Load the study design file..."),
                           
                           fluidRow(align = "center", shinyFilesButton('sfb_config_file', label = 'Load Configuration File', title = 'Please select excel design file', multiple = FALSE,
                                                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; display:center")),
                           
                           span(textOutput("design_file_name"), style = "color:blue; font-size:16px"),
                           
                           tags$h4("3. Select data file(s).."),
                           
                           fluidRow(align = "center", shinyFilesButton('sfb_data_file', label = 'Select Data File', title = 'Please select data file(s)', multiple = FALSE,
                                                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                           span(textOutput("data_file_name"), style = "color:blue; font-size:16px")
                       ),
                       
                       box(title = "Start from Previous Analysis", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "center", width = 12, height = 200,
                           tags$h3("Select database file"),
                           fluidRow(align = "center", shinyFilesButton('sfb_archive_file', label = 'Select Archive/Zip File', title = 'Please select zip file', multiple = FALSE,
                                                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                           span(textOutput("archive_file_name"), style = "color:blue; font-size:16px")
                       )
                     )),
              
              
              column(width = 8,  
                     box(title = "Study Design Table", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 725,
                         rHandsontableOutput("stats_design_table")
                         #div(style = 'overflow-x: scroll; overflow-y: scroll;', DT::dataTableOutput("stats_design_table")) #), width = '100%'))
                         
                     ))
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
                     actionButton("clean_environment", label = "Clean Environment", width = 100,
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
              )
            )
    )
    
    
    
    #---------------------------------------------------------------------
    
    
    
    
    
  )
)




dashboardPage(
  dashboardHeader(title = "Duke Proteomics Data Processing", titleWidth = 325),
  sidebar,
  body
)  