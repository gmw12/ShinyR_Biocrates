cat(file = stderr(), "ui.R started", "\n")

source("Shiny_Libraries.R")
source("Shiny_UI.R")
#height_factor <- 1


sidebar <- dashboardSidebar(width = 165,
                            useShinyjs(),
                            #shinybrowser::detect(),
                            sidebarMenu(
                              menuItem("Welcome", tabName = "welcome", selected = TRUE),
                              menuItemOutput("menu_load"),
                              menuItemOutput("menu_load_customer"),
                              menuItemOutput("menu_parameters"),
                              menuItemOutput("menu_noise"),
                              menuItemOutput("menu_filter"),
                              menuItemOutput("menu_normalize"),
                              menuItemOutput("menu_impute"),
                              menuItemOutput("menu_rollup"),
                              menuItemOutput("menu_qc"),
                              menuItemOutput("menu_stats"),
                              menuItemOutput("menu_pathway"),
                              menuItemOutput("menu_phos"),
                              menuItemOutput("menu_admin")
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
                     h1("Welcome to Duke Proteomics Data Processing Tool", style = "font-size:60px"),
                     br(),
                     img(src = 'astral.png', align = "center", width = 400, height = 400 )
              )
            )
    ),
    
    tabItem(tabName = "load_customer",
            fluidRow(
              column(width = 6, align = "center",   
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     box(title = "Welcome", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 500,   
                         br(),
                         br(),
                         tags$h3("Welcome to the Duke Proteomics and Metabolomics Core Facility Data processing tool."),
                         tags$h3("This tools is designed to integrate directly with your specific proteomic dataset and allow various statistical analysis options, global data reduction visualizations such as PCA and clustering, gene ontology, classifications, pathway analysis and string-db based protein interaction visualizations."),
                          tags$h3("Note that this tool is developed in-house by DPMCF staff and is continuing to be optimized for less common experimental variables."), 
                          tags$h3("We appreciate feedback on any bugs that you may encounter (greg.waitt@duke.edu)."),
                     )
              ),
              column(width = 6, align = "center",   
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                box(title = "Load database file", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "center", width = 12, height = 500,   
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  tags$h3("Select database file"),
                 fluidRow(align = "center", 
                          #shinyFilesButton('sfb_archive_customer_file', label = 'Select Archive/Zip File', title = 'Please select zip file', multiple = FALSE,
                          #                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                          fileInput("sfb_archive_customer_file", "Please select zip file",
                                    multiple = FALSE, width=300,
                                    accept = c(".dpmsr_set", ".zip")),
                          bsTooltip("sfb_archive_customer_file", "Select zip file and wait for the progress bar to complete.",
                                    "left", trigger = "hover", options = list(container = "body")),
                          ),
                 br(),
                 actionButton("load_customer_archive_file", label = "Process File", width = 200, 
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 bsTooltip("load_customer_archive_file", "Click to process the zip file and load the data.",
                           "left", trigger = "hover", options = list(container = "body")),
                 #span(textOutput("archive_file_name_customer"), style = "color:blue; font-size:16px")
                )
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
                           
                           fluidRow(align = "center", shinyFilesButton('sfb_design_file', label = 'Load Design File', title = 'Please select excel design file', multiple = FALSE,
                                                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; display:center")),
                           
                           span(textOutput("design_file_name"), style = "color:blue; font-size:16px"),
                           
                           tags$h4("3. Select data file(s).."),
                           
                           fluidRow(align = "center", shinyFilesButton('sfb_data_file', label = 'Select Data File(s)', title = 'Please select data file(s)', multiple = TRUE,
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
    
    #Parameters
    tabItem(tabName = "parameters",
            fluidRow(
              column(width = 3,
                     fluidRow(
                       box(id = "param_box", title = "Set analysis parameters...", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 750,
                           
                           selectInput("primary_group", label = "Full or primary group for filter and impute",
                                       choices = list("Full", "Primary"), selected = "Full", width = 300),
                           selectInput("data_output", label = ("Select Output Data Type"),
                                       choices = list("Protein", "Peptide"),
                                       selected = "Protein", width = 300),
                           
                           checkboxInput("ptm", label = "PTM Analysis?", value = 0, width = 300),
                           
                           textInput("ptm_grep", label = "PTM grep", value = "Phospho", width = 300),
                           
                           numericInput("ptm_local", label = "PTM localization min", value = 0.75, width = 300),
                           
                           checkboxInput("multi_tmt", label = "SPQC Normalized TMT sets"),
                           
                           selectInput("peptide_select", label = h5("Peptides to Use"),
                                       choices = list("Razor", "Unique", "Shared"),
                                       selected = "Razor"),
                           
                           checkboxInput("use_isoform", label = "Use Peptide Isoform?"),
                           
                           fluidRow(align = "center", 
                                    actionButton("accept_parameters", label = "Accept Parameters",
                                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                       ))),
              
              column(width = 7,  
                     fluidRow(
                       box(title = "Raw Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 750,
                           fluidRow(
                             column(width = 5, 
                                    imageOutput("raw_bar"),
                                    imageOutput("raw_box")),
                             column(width = 7,  
                                    imageOutput("start_histogram"))
                           )
                       ))),
              
              column(width = 2,  
                     fluidRow(
                       box(title = "Raw Meta Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 750,
                           fluidRow(
                             column(width = 12,
                                    span(textOutput("meta_parameters_precursor_raw"), style = "color:blue; font-size:16px"),
                                    span(textOutput("meta_parameters_peptide_raw"), style = "color:blue; font-size:16px"),
                                    span(textOutput("meta_parameters_protein_raw"), style = "color:blue; font-size:16px"),
                                    br(),
                                    br(),
                                    span(textOutput("meta_parameters_precursor_ptm"), style = "color:blue; font-size:16px"),
                                    br(),
                                    span(textOutput("meta_parameters_precursor_phos_all"), style = "color:blue; font-size:16px"),
                                    span(textOutput("meta_parameters_precursor_phos_local"), style = "color:blue; font-size:16px"),
                                    span(textOutput("meta_parameters_precursor_phos_percent"), style = "color:blue; font-size:16px"),
                                    span(textOutput("meta_parameters_precursor_phos_local_percent"), style = "color:blue; font-size:16px"),
                                    br(),
                                    span(textOutput("meta_parameters_phos_site_unique_all"), style = "color:blue; font-size:16px"),
                                    span(textOutput("meta_parameters_phos_site_unique_local"), style = "color:blue; font-size:16px")
                             )
                           )
                       )))
            )
    ),
    
    
    #Noise
    tabItem(tabName = "noise",
            fluidRow(
              column(width = 3,
                     fluidRow(
                       box(id = "noise_box", title = "Set noise reduction parameters...", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 750,
                           br(),
                           br(),
                           fluidRow(align = "center", 
                                    actionButton("noise_inflection_apply", label = "Calculate Noise Inflection Point",
                                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                           br(),
                           hr(),
                           br(),
                           selectInput("noise_type", label = "Select noise reduction strategy",
                                       choices = list("None" = "none",
                                                      "Fixed" = "fixed",
                                                      "Dynamic" = "dynamic"), selected = "none"),
                           
                           fluidRow( 
                             column(width = 6, numericInput("noise_baseline_value", label = "Enter fixed baseline", value = 1))
                           ),
                           br(),
                           hr(),
                           br(),
                           fluidRow(align = "center", 
                                    actionButton("noise_apply", label = "Apply Noise",
                                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                       ))),
              
              column(width = 7,  
                     fluidRow(
                       box(title = "Noise Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 750,
                           fluidRow(
                             column(width = 7,  imageOutput("noise_plot"))
                           )
                       ))),
              
              column(width = 2, 
                     fluidRow(
                       box(title = "Noise Meta Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 750,
                           fluidRow(
                             column(width = 12, 
                                    span(textOutput("noise_inflection"), style = "color:blue; font-size:16px"),
                                    br(),
                                    span(textOutput("noise_total"), style = "color:blue; font-size:16px"),
                                    br(),
                                    br(),
                                    span(textOutput("dynamic_noise_count"), style = "color:blue; font-size:16px"),
                                    span(textOutput("dynamic_noise_percent"), style = "color:blue; font-size:16px"),
                                    br(),
                                    br(),
                                    span(textOutput("custom_noise_count"), style = "color:blue; font-size:16px"),
                                    span(textOutput("custom_noise_percent"), style = "color:blue; font-size:16px")
                             ))
                       ))
              )
            )
    ),     
    
    
    #Filter
    tabItem(tabName = "filter",
            fluidRow(
              column(width = 3,
                     fluidRow(
                       box(id = "filter_box", title = "Set filter parameters...", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 750,
                           
                           numericInput("filter_min_measured_all", label = "Enter minimum # measured values (all samples)", value = 2),
                           hr(),
                           fluidRow(
                             column(width = 6, checkboxInput("filter_x_percent", label = "Require X% measured values in at least one group?", value=TRUE)),
                             column(width = 6, numericInput("filter_x_percent_value", label = "Enter X% measured values", value = 50))
                           ),
                           fluidRow(
                             column(width = 4, checkboxInput("filter_cv", label = "Filter on Group CV?")),
                             column(width = 4, selectInput("filter_cv_group", label = "Group?", 
                                                           choices = list("SPQC"), selected = "SPQC")),
                             column(width = 4, numericInput("filter_cv_value", label = "Cutoff%?", value = 99))
                           ),
                           fluidRow(
                             column(width = 12, checkboxInput("filter_ptm", label = "Keep only PTM?")),
                           ),
                           hr(),
                           fluidRow(
                             column(width = 6, checkboxInput("checkbox_misaligned", label = "Misaligned Filter?")),
                             column(width = 6,
                                    dropdownButton(
                                      fluidRow(
                                        column(width = 12, tags$h5("When there are missing values present in a group this filter examines the values that are
                                                                     measured.  If the percent of missing is above the threshold below the filter compares
                                                                     the intensity to the cutoff threshold below.  If it is above then the values are either
                                                                     removed fromt the group (NA) or the entire precursor can be removed from the dataset."))
                                      ),
                                      fluidRow(
                                        column(width = 12, numericInput("intensity_cutoff_sd", label = "Misaligned intensity cutoff = mean+(x*stdev)", value = 0.5, width = '100%'))
                                      ),
                                      fluidRow(   
                                        column(width = 12, numericInput("misaligned_cutoff", label = "X% missing values to be considered for misalignment if average > intensity cutoff", value = 51, width = '100%'))
                                      ),
                                      fluidRow(
                                        column(width = 12, selectInput("misaligned_target", label = "Remove misalignment from group or dataset?", 
                                                                       choices = list("group", "dataset"), selected = "dataset"))
                                      ),
                                      circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
                                      tooltip = tooltipOptions(title = "Click to see misalignment options!")
                                    )
                             )
                           ),
                           hr(),
                           fluidRow(
                             column(width = 6, checkboxInput("precursor_quality", label = "Precusor Quality Filter")),
                             column(width = 6,
                                    dropdownButton(
                                      fluidRow(
                                        column(width = 12, tags$h5("This filter compares the percentage contribution of a precursor to the protein
                                                                     across all samples.  Values below minimum for consideration are set to NA.  If the 
                                                                     percent standard deviation and average intensity is above the 
                                                                     thresholds then the precusor is removed.  The filter examines the highest intensity 
                                                                     precursor first.  It is then removed from the filter calculation steps for the next
                                                                     abundant precursor.  Minimum of 3 precursors required."))
                                      ),
                                      fluidRow(
                                        column(width = 12, numericInput("precursor_quality_sd", label = "Percent SD cuttof", value = 50))),
                                      fluidRow(
                                        column(width = 12, numericInput("precursor_quality_intensity", label = "Minimum Average Intensity", value = 500))),
                                      fluidRow(
                                        column(width = 12, numericInput("precursor_quality_min", label = "Minimum Intensity for Consideration", value = 300))),
                                      circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
                                      tooltip = tooltipOptions(title = "Click to see misalignment options!")
                                    )
                             )
                           ),
                           hr(),
                           fluidRow(
                             column(width = 6, checkboxInput("precursor_spqc_ratio", label = "Precursor SPQC Accuracy Filter")),
                             column(width = 6,
                                    dropdownButton(
                                      fluidRow(
                                        column(width = 12, tags$h5("This filter compares the average intensity of samples to the average intensity
                                                                     of spqc.  Assumption is the the SPQC should be the average sample.  If the accuracy
                                                                     is outside of the designated tolerance and above the minimum intensity it is 
                                                                     removed."))
                                      ),
                                      fluidRow(
                                        column(width = 12, numericInput("precursor_spqc_accuracy", label = "+- Percent SPQC Accuracy", value = 50))),
                                      fluidRow(
                                        column(width = 12, numericInput("precursor_spqc_intensity", label = "Minimum Intensity of SPQC Samples", value = 500))),
                                      circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
                                      tooltip = tooltipOptions(title = "Click to see misalignment options!")
                                    )
                             )
                           ),
                           hr(),
                           fluidRow(align = "center", 
                                    actionButton("filter_cutoff", label = "Recalculate Cutoff",
                                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                    actionButton("filter_apply", label = "Apply Filters",
                                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                       ))),
              
              column(width = 7,  
                     fluidRow(
                       box(title = "Filter Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 750,
                           fluidRow(
                             column(width = 7,  imageOutput("impute_histogram")),
                             column(width = 5, 
                                    imageOutput("filter_bar"),
                                    imageOutput("filter_box"))
                           )
                       ))),
              
              column(width = 2, 
                     fluidRow(
                       box(title = "Filter Meta Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 750,
                           fluidRow(
                             column(width = 12, 
                                    span(textOutput("meta_filter_precursor_raw"), style = "color:blue; font-size:16px"),
                                    span(textOutput("meta_filter_peptide_raw"), style = "color:blue; font-size:16px"),
                                    span(textOutput("meta_filter_protein_raw"), style = "color:blue; font-size:16px"),
                                    br(),
                                    span(textOutput("impute_total_na"), style = "color:blue; font-size:16px"),
                                    span(textOutput("impute_total_misaligned"), style = "color:blue; font-size:16px"),
                                    br(),
                                    span(textOutput("meta_filter_precursor_filtered"), style = "color:blue; font-size:16px"),
                                    span(textOutput("meta_filter_peptide_filtered"), style = "color:blue; font-size:16px"),
                                    span(textOutput("meta_filter_protein_filtered"), style = "color:blue; font-size:16px"))
                             
                           )
                       ))
              )
            )
    ),
    
    
    #Normalize
    tabItem(tabName = "normalize",
            fluidRow(
              column(width = 8,
                     box(id = "norm_param_box", title = "Set normalization parameters...", status = "primary",
                         solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 230,
                         fluidRow(
                           column(width = 4,
                                  selectInput("norm_exclude", label = "Exclude from Normalization", 
                                              choices = list("Not_Used" = "not_used", "Accession" = "accession", "Description" = "description", 
                                                             "Name" = "name", "Genes" = "genes", "Sequence" = "sequence"), selected = "description"),
                                  textInput("norm_exclude_grep", label = "Norm exclude grep (sep=|, no space)", value = "trypsin|keratin|casein")),
                           column(width = 4, 
                                  selectInput("norm_include", label = "Include only for Normalization", 
                                              choices = list("Not_Used"= "not_used", "Accession" = "accession", "Description" = "description", 
                                                             "Name" = "name", "Genes" = "genes", "Sequence" = "sequence"), selected = "description"),
                                  textInput("norm_include_grep", label = "Norm include grep (sep=|, no space)", value = "Phospho")),
                           #checkboxInput("norm_include", label = "Include only grep for norm (trypsin|keratin|casein)"),
                           #textInput("include_norm_grep", label = "Filter Include grep", value = "trypsin|keratin|casein"),
                           column(width = 4,
                                  br(),
                                  br(),
                                  br(),
                                  
                                  #checkboxInput("norm_exclude", label = "Exclude grep from norm (trypsin|keratin|casein)", value = TRUE),
                                  #textInput("exclude_norm_grep", label = "Filter Exclude grep", value = "trypsin|keratin|casein"),
                                  actionButton("norm_parameters", label = "Apply Normalizaton Parameters",
                                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                           
                         )
                     ) 
              ),
              
              column(width = 4,
                     box(id = "norm_box", title = "Normalization strategy...", status = "primary",
                         solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 230, 
                         fluidRow(
                           column(width = 12,
                                  selectInput("norm_type", label = "Select normalization strategy",
                                              choices = list("Sample Loading - Total" = "sl",
                                                             "Trimmed Mean" = "tmm",
                                                             "Sample Loading Trimmed Mean" = "sltmm",
                                                             #"Protein" = "protein",
                                                             "DirectLFQ" = "directlfq",
                                                             "Quantile" = "quantile",
                                                             "Linear Regression" = "lr",
                                                             "LOESS" = "loess",
                                                             "VSN" = "vsn",
                                                             "Median of Total Intensity" = "mti",
                                                             "Median Intensity" = "mi",
                                                             "Average Intensity" = "ai"), width = 300, selected = "SLTMM"))
                         ),
                         fluidRow(
                           column(width = 12,
                                  br(),
                                  actionButton("norm_apply", label = "Apply Normalization",
                                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                           )))
                     # fluidRow(
                     #   column(width = 4,
                     #   selectInput("protein_norm_search_field", label = "Search Header",
                     #               choices = list("Accession" = "accession", "Description" = "description",
                     #                              "Name" = "name", "Genes" = "genes"),
                     #               selected = "accession")),
                     #   column(width = 8,
                     #   textInput("protein_norm_grep", label = "Norm grep (sep=|, no space)", value = ""))
                     # )
              )
            ),
            
            fluidRow(
              box(title = "Normalized Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 500,
                  fluidRow(
                    column(width = 4, imageOutput("norm_data_start_bar")),
                    column(width = 4, imageOutput("norm_normdata_bar")),
                    column(width = 4, imageOutput("norm_bar"))
                  )
              )
            )
    ),     
    
    
    
    
    
    #Impute
    tabItem(tabName = "impute",
            fluidRow(
              column(width = 3,
                     box(id = "impute_box", title = "Imputation strategy...", status = "primary",
                         solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 125, 
                         selectInput("impute_type", label = "Select imputation strategy",
                                     choices = list("Duke" = "duke",
                                                    "BottomX" = "bottomx",
                                                    "Floor" = "floor",
                                                    "Minimum" = "min",
                                                    "Average/Group" = "avg_grp",
                                                    "Average/Global" = "avg_glb",
                                                    "KNN" = "knn",
                                                    "LocalLeastSquares" = "lls",
                                                    "MLE" = "mle"))
                     ),
                     
                     box(id = "duke_param_box", title = "Impute parameters...", status = "primary",
                         solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 350,
                         # fluidRow(
                         #  column(width = 6, checkboxInput("impute_ptm", label = "Impute Distribution based on PTM?", value = 0, width = 300)),
                         #  column(width = 6, textInput("impute_ptm_grep", label = "Impute PTM grep", value = "Phospho", width = 300))
                         # ),
                         fluidRow(
                           column(width = 12,
                                  numericInput("bottom_x", label = "Bottom X%", value = "2"),
                                  numericInput("missing_cutoff", label = "%minimum  measured values in group to allow imputation in measured range", value = 50, width = '100%')
                           )
                         )
                     ),
                     box(id = "impute_apply_box", title = "Start...", status = "primary",
                         solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 150, 
                         fluidRow(align = "center", 
                                  column(width = 6, actionButton("impute_parameters", label = "Set Parameters",
                                                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                  column(width = 6, actionButton("impute_apply", label = "Apply Imputation",
                                                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                         )
                     )
                     
              ),
              
              column(width = 7,
                     box(title = "Plots", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = 'left', width = 12, height = 750,
                         fluidRow(
                           column(width = 6, imageOutput("missing_bar_plot")),
                           column(width = 6, imageOutput("missing_percent_plot"))
                         )
                     )),
              
              column(width = 2,  
                     fluidRow(
                       box(title = "Impute Meta Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 750,
                           fluidRow(
                             column(width = 12,
                                    #span(textOutput("meta_impute_na"), style = "color:blue; font-size:16px"),
                                    DT::dataTableOutput("impute_meta_table")
                             )
                           )
                       )))
            )),   
    
    
    #Rollup
    tabItem(tabName = "rollup",
            fluidRow(
              column(width = 3,
                     box(id = "rollup_box", title = "Rollup strategy...", status = "primary",
                         solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 500, 
                         radioButtons("rollup_method", label = NULL,
                                      choices = list("Sum" = "sum", "Median" = "median", "Median_Polish" = "median_polish", "Mean" = "mean",
                                                     "IQ_MaxLFQ" = "iq_maxlfq", "TopN" = "topn"),
                                      selected = "sum"),
                         br(),
                         selectInput("rollup_topn", label = "topN rollup", width = 150,
                                     choices = list(1,2,3,4,5), 
                                     selected = 3),
                         checkboxInput("maxlfq_scale", label = "Scale IQ_MaxLFQ Protein Output to summed protein intensities?", value = 0, width = 300),
                         br(),
                         br(),
                         actionButton("rollup_apply", label = "Apply Rollup", width = 300,
                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                     )
              )
            )),    
    
    
    #QC
    tabItem(tabName = "qc",
            fluidRow(
              box(id = "qc_box", title = "QC...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 750, 
                  tabBox(id = "qc_box", height = 675, width = 12,
                         tabPanel("CV",
                                  rHandsontableOutput("qc_cv_table"),
                                  br(),
                                  imageOutput("cv_plot")
                         ),
                         tabPanel("Norm Comparison",
                                  column(width = 3,
                                         imageOutput("qc_norm_comp1"),
                                         imageOutput("qc_norm_comp5")),
                                  column(width = 3,
                                         imageOutput("qc_norm_comp2"),
                                         imageOutput("qc_norm_comp6")),
                                  column(width = 3,
                                         imageOutput("qc_norm_comp3"),
                                         imageOutput("qc_norm_comp7")),
                                  column(width = 3,
                                         imageOutput("qc_norm_comp4"),
                                         imageOutput("qc_norm_comp8"))
                         ),
                         tabPanel("Protein",
                                  fluidRow(
                                    column(width = 3,
                                           textInput("qc_plot_accession", label = "Protein Accession", value = "P00330", width = 300)),
                                    column(width = 3,
                                           selectInput("qc_norm_type", label = "Select normalization strategy", width = 300,
                                                       choices = list("impute" = "impute"), selected = "impute")
                                    ),
                                    column(width = 3,
                                           actionButton("qc_protein_plot_apply", label = "Show Plot", width = 300,
                                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                  ),
                                  fluidRow(
                                    br(),
                                    column(width = 6, imageOutput("qc_protein_barplot"))
                                  )
                         ),
                         
                         tabPanel("Protein Spike",
                                  fluidRow(
                                    column(width = 3,
                                           textInput("spike_plot_accession", label = "Protein Accession", value = "P02662,P02663", width = 300)),
                                    column(width = 3,
                                           selectInput("spike_norm_type", label = "Select normalization strategy", width = 300,
                                                       choices = list("impute" = "impute"), selected = "impute")
                                    ),
                                    column(width = 3,
                                           actionButton("qc_spike_plot_apply", label = "Show Plot", width = 300,
                                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                  ),
                                  fluidRow(
                                    rHandsontableOutput("qc_spike_table"),
                                    br(),
                                    column(width = 6, imageOutput("spike_protein_barplot"))
                                  )
                         )
                  )
              ))),
    
    
    
    #Stats_Design
    tabItem(tabName = "stats_setup",
            fluidRow(
              box(id = "stats_setup_box", title = "Stats Setup...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 2, height = 750,
                  numericInput("pvalue_cutoff", label = "Pvalue Cutoff", value = .05),
                  checkboxInput("pair_comp", label = "Pairwise Comparisons"),
                  checkboxInput("checkbox_adjpval", label = "Include adjusted pvalue?", value = TRUE),
                  bsTooltip("checkbox_adjpval", "add adjusted pvalue to stat output",
                            "left", trigger = "hover", options = list(container = "body")),
                  hidden(selectInput("padjust_options", label = "p.adjust method", choices = list("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr"), 
                                     selected = "fdr")),
                  numericInput("foldchange_cutoff", label = "Fold Change cutoff", value = 1.5),
                  
                  numericInput("missing_factor", label = "Measured % (decimal)", value = 0.6),
                  bsTooltip("missing_factor", "Requires X% measured values in one group in order to be considered stat signifigant",
                            "right", options = list(container = "body")),
                  
                  hr(),
                  tags$b(style = "color:blue", 'Final Excel Options'),
                  checkboxInput("checkbox_report_ptm", label = "Report Only PTM?"),
                  hidden(textInput("peptide_report_grep", label = "Report PTM grep", value = "Enter value here")),
                  checkboxInput("checkbox_report_accession", label = "Report Specific Accession(s) Only"),
                  hidden(textInput("report_accession", label = "Protein Accessions for Final Report", value = "Enter value"))
              ),
              
              box(id = "stats_setup_box2", title = "Stats Setup...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 3, height = 750,
                  tags$b(style = "color:blue", 'Precursor Filters'),
                  checkboxInput("peptide_refilter", label = "Refilter precursors/peptides? (remove 100% imputed precursors for the comparison samples)", value = FALSE),
                  checkboxInput("peptide_missing_filter", label = "Refilter precursors/peptides by requiring X% measured values in one group?", value = FALSE),
                  numericInput("peptide_missing_factor", label = "Peptide X% measured cutoff (decimal)", value = 0.1),
                  checkboxInput("peptide_cv_filter", label = "Refilter precursors/peptides by requiring X %CV one group?"),
                  hidden(numericInput("peptide_cv_factor", label = "Peptide X CV% cutoff (decimal)", value = 0.5)),
                  tags$b(style = "color:blue", 'Final Filters'),
                  checkboxInput("stats_spqc_cv_filter", label = "Filter by SPQC CV"),
                  hidden(numericInput("stats_spqc_cv_filter_factor", label = "SPQC %CV Cutoff", value = 50)),
                  checkboxInput("stats_comp_cv_filter", label = "Require one group CV below cuttoff"),
                  hidden(numericInput("stats_comp_cv_filter_factor", label = "Comp %CV Cutoff", value = 50)),
                  
                  checkboxInput("stats_peptide_minimum", label = "Require minimum # of peptides per protein", value = 0),
                  hidden(numericInput("stats_peptide_minimum_factor", label = "Peptide Minimum", value = 1)),
                  hr(),
                  tags$b(style = "color:blue", 'Extra Stats'),
                  
                  hidden(checkboxInput("checkbox_filter_adjpval", label = "Filter with adjusted pvalue?")),
                  bsTooltip("checkbox_filter_adjpval", "use adjpval to define stat signifigance",
                            "left", trigger = "hover", options = list(container = "body")),
                  
                  checkboxInput("checkbox_cohensd", label = "Include Cohen's D?"),
                  bsTooltip("checkbox_cohensd", "use coehnsD for effect size (R package = effsize)",
                            "left", trigger = "hover", options = list(container = "body")),
                  
                  checkboxInput("checkbox_cohensd_hedges", label = "Use Hedge's Correction (low N)?"),
                  bsTooltip("checkbox_cohensd_hedges", "Correction for small sample size",
                            "left", trigger = "hover", options = list(container = "body")),

                  checkboxInput("checkbox_limmapvalue", label = "Include Limma Pvalue?"),
                  bsTooltip("checkbox_limmapvalue", "R package = limma",
                            "left", trigger = "hover", options = list(container = "body")),
                  
                  br(),
                  actionButton("stat_options", label = "Save Stat Options", width = 300,
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
              ),
              
              box(id = "stats_design_box", title = "Stats Design...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 7, height = 750,
                  rHandsontableOutput("stats_design_table2")       
              )
            )
    ),
    
    #Stats_Setup
    tabItem(tabName = "stats_compare",
            fluidRow(
              box(id = "stats_setup_box3", title = "Stats Setup...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 2, height = 750,
                  selectInput("stats_norm_type", label = "Normalization", width = 150,
                              choices = list("impute" = "impute"), selected = "impute"),
                  hr(),
                  selectInput("comp_number", label = "Comp #", width = 150,
                              choices = list(1,2,3,4,5,6,7,8,9), 
                              selected = 1),
                  hr(),
                  pickerInput(inputId = "comp_spqc", label = "SPQC Group?",  choices = "None", 
                              options = list(`actions-box` = TRUE,size = 10,
                                             `selected-text-format` = "count > 3"),  multiple = TRUE),
                  hr(),
                  #tags$head(tags$style("#stat2_N_1{color: blue; font-size: 16px; font-style: bold;}")),
                  actionButton("check_stats", label = "Set Comparisons", width = 200,
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  br(),
                  actionButton("start_stats", label = "Start Anlaysis", width = 200,
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  br(),
                  textInput("final_stats_name", label = "Final Stats Excel Name", 
                            value = str_c("Final_CompHere_stats.xlsx")),
                  br(),
                  actionButton("save_stats", label = "Create Excel Output File", width = 200,
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  br(),
                  br(),
                  br(),
                  downloadButton('download_stats_excel')
              ),
              
              box(id = "stats_setup_box2", title = "Stats Setup...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 10, height = 750,
                  #create comparisons from loop function
                  create_comp(1),
                  create_comp(2),
                  create_comp(3),
                  create_comp(4),
                  create_comp(5),
                  create_comp(6),
                  create_comp(7),
                  create_comp(8),
                  create_comp(9)
              )
              
              
              
            )
    ),
    
    #Plots
    tabItem(tabName = "stats_plots",
            fluidRow(
              box(id = "stats_plot_box1", title = "Plot 1...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 6, height = 750,
                  
                  fluidRow(
                    column(width = 3, offset = 0,
                           pickerInput(inputId = "plot_type1", label = "Plot type",  choices = c("Bar", "Box", "PCA_2D", "PCA_3D", "Cluster", "Heatmap", "Volcano"), 
                                       selected = "Bar", options = list(`actions-box` = TRUE, size = 100,
                                                                        `selected-text-format` = "count > 5"),  multiple = FALSE)
                    ),
                    column(width = 6, offset = 0,
                           pickerInput(inputId = "stats_plot_comp1", label = "Comparison(s) to plot",  choices = "None", 
                                       options = list(`actions-box` = TRUE, size = 100,
                                                      `selected-text-format` = "count > 5"),  multiple = TRUE)
                    ),
                    column(width = 2, offset = 0, 
                           br(),
                           actionButton("create_stats_plots1", label = "Create Plot", width = 100,
                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
                  ),
                  
                  uiOutput("stats_plots1")
                  
              ),
              
              box(id = "stats_plot_box2", title = "Plot 2...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 6, height = 750,
                  
                  fluidRow(
                    column(width = 3, offset = 0,
                           pickerInput(inputId = "plot_type2", label = "Plot type",  choices = c("Bar", "Box", "PCA_2D", "PCA_3D", "Cluster", "Heatmap", "Volcano"), 
                                       selected = "Bar", options = list(`actions-box` = TRUE, size = 100,
                                                                        `selected-text-format` = "count > 5"),  multiple = FALSE)
                    ),
                    column(width = 6, offset = 0,
                           pickerInput(inputId = "stats_plot_comp2", label = "Comparison(s) to plot",  choices = "None", 
                                       options = list(`actions-box` = TRUE, size = 100,
                                                      `selected-text-format` = "count > 5"),  multiple = TRUE)
                    ),
                    column(width = 2, offset = 0, 
                           br(),
                           actionButton("create_stats_plots2", label = "Create Plot", width = 100,
                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
                  ),
                  
                  uiOutput("stats_plots2")
                  
              )
              
            )
    ),
    
    
    tabItem(tabName = "stats_data",
            fluidRow( 
              box(id = "stats_data_parameters", title = "Data Parameters...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 125,
                  column(width =2, offset =0,
                         selectInput("stats_select_data_comp", label = "comparison", 
                                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                     selected = 1)
                  ),
                  column(width =1, offset =0,
                         checkboxInput("stats_add_filters", label="Apply stat filters (from setup)?", value = 0)
                  ),
                  column(width =2, offset =0,
                         selectInput("stats_search_field", label = "Search Header", 
                                     choices = list("Accession" = "accession", "Description" = "description", 
                                                    "Name" = "name", "Genes" = "genes", "TopN" = "topn"), 
                                     selected = "accession")
                  ),
                  column(width =2, offset =0,
                         textInput("stats_data_description", label="Description", value = "", width = 200)
                  ),
                  
                  column(width =1, offset =0,
                         br(),
                         actionButton("stats_data_show", label = "Create Table", width = 100,
                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                  ),
                  column(width =2, offset =0,
                         textInput("stats_data_filename", label="File Name", value = "my_data.xlsx", width = 250)
                  ),
                  column(width =1, offset =0,
                         br(),
                         actionButton("stats_data_save", label = "Save Data", width = 100,
                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                  ),
                  column(width = 1, offset = 0,
                         br(),
                         downloadButton('download_stats_data_save')
                  )
              )
            ),
            
            fluidRow(
              verbatimTextOutput('stats_data_final_protein')
            ),
            
            fluidRow(
              box(id = "stats_data_table", title = "Data Table...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 750,
                  column(width =12, offset =0,
                         hr(),
                         tags$head(tags$style("#stats_data_final{color: blue;
                                                             font-size: 12px;
                                                             }"
                         )
                         ),
                         DT::dataTableOutput("stats_data_final", width ='100%')
                  )
              )
            ),
            br(),
            
            fluidRow(
              actionButton("stats_data_update", label = "Remove from Stats", width = 200,
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            )       
    ),
    
    
    tabItem("stats_protein_plots", 
            fluidRow(
              box(id = "stats_protein_parameters", title = "Protein Plot Parameters...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 125,
                  column(width =1, offset =0,
                         textInput("stats_oneprotein_accession", label="Accession", value = "0", width = 100)
                  ),
                  column(width =3, offset =0,
                         selectInput("stats_oneprotein_plot_comp", label = "comparison", 
                                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                     selected = 1)
                  ),
                  column(width =1, offset =0,
                         br(),
                         checkboxInput("stats_oneprotein_plot_spqc", label = "Add SPQC?")
                  ),
                  column(width =1, offset =0,
                         br(),
                         checkboxInput("stats_use_zscore", label = "Use zscore?")
                  ),
                  column(width =1, offset =0,
                         br(),
                         actionButton("create_stats_oneprotein_plots", label = "Create Plots", width = 100,
                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                  ),
                  column(width =2, offset =1,
                         textInput("stats_oneprotein_data_filename", label="File Name", value = "my_protein_data.xlsx", width = 250)
                  ),
                  column(width =1, offset =0,
                         br(),
                         actionButton("stats_oneprotein_data_save", label = "Save Data", width = 100,
                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                  ),
                  column(width =1, offset =0,
                         br(),
                         downloadButton('download_stats_oneprotein_data_save')
                  )
              )
            ),
            
            fluidRow(
              tabBox(title = "Protein Plots", width = 12, height = 700,
                     tabPanel("Protein",
                              column(width =12, offset =0,
                                     dropdownButton(
                                       textInput("1_stats_oneprotein_barplot_y_axis_label", label="y axis label", value = "Intensity", width = 200),
                                       textInput("1_stats_oneprotein_barplot_title", label="plot title", value = "Total Summed Intensity", width = 200),
                                       sliderInput("1_stats_oneprotein_barplot_label_size", label = h5("Label Size"), min = 1, 
                                                   max = 50, value = 11),
                                       sliderInput("1_stats_oneprotein_barplot_title_size", label = h5("Title Size"), min = 10, 
                                                   max = 50, value = 20),
                                       circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
                                       tooltip = tooltipOptions(title = "Click to see inputs !")
                                     ),
                                     div(
                                       style = "position:relative",
                                       plotOutput("1_stats_oneprotein_barplot", width = 1200, height = 600)
                                     ),
                                     downloadButton('1_download_stats_oneprotein_barplot')
                              )
                     ),
                     tabPanel("Peptide",
                              column(width =12, offset =0,
                                     dropdownButton(
                                       textInput("stats_oneprotein_grouped_barplot_y_axis_label", label="y axis label", value = "Intensity", width = 200),
                                       textInput("stats_oneprotein_grouped_barplot_x_axis_label", label="x axis label", value = "Sequence", width = 200),
                                       textInput("stats_oneprotein_grouped_barplot_title", label="plot title", value = "Total Summed Intensity", width = 200),
                                       sliderInput("stats_oneprotein_grouped_barplot_label_size", label = h5("Label Size"), min = 1, 
                                                   max = 50, value = 11),
                                       sliderInput("stats_oneprotein_grouped_barplot_title_size", label = h5("Title Size"), min = 10, 
                                                   max = 50, value = 20),
                                       circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
                                       tooltip = tooltipOptions(title = "Click to see inputs !")
                                     ),
                                     div(
                                       style = "position:relative",
                                       plotOutput("stats_oneprotein_grouped_barplot", width = 1000, height = 400)
                                     ),
                                     downloadButton('download_stats_oneprotein_grouped_barplot')
                              )
                     ),
                     tabPanel("Peptide Data",
                              column(width =12, offset =0,
                                     hr(),
                                     tags$head(tags$style("#oneprotein_peptide_table{color: blue;
                                                           font-size: 12px;
                                                           }"
                                     )
                                     ),
                                     DT::dataTableOutput("oneprotein_peptide_table", width ='100%')
                              )
                     )
              )
              
            )  
    ),
#------------------------------------------------------------------------------ 
    
    


          tabItem("stats_peptide_plots",
              fluidRow(
                box(id = "stats_protein_parameters", title = "Protein Plot Parameters...", status = "primary",
                    solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 125,
                   column(width =1, offset =0,
                          textInput("stats_onepeptide_accession", label="Accession", value = "0", width = 100)
                   ),
                   column(width =2, offset =0,
                          textInput("stats_onepeptide_sequence", label="Peptide Sequence", width = 250)
                   ),
                   column(width =2, offset =0,
                          textInput("stats_onepeptide_modification", label="Peptide Modification", width = 250)
                   ),
                   column(width =3, offset =0,
                          selectInput("stats_onepeptide_plot_comp", label = "comparison",
                                      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                      selected = 1)
                   ),
    
                   column(width =1, offset =0,
                          checkboxInput("stats_onepeptide_plot_spqc", label = "Add SPQC?")
                   ),
                   column(width =1, offset =0,
                          checkboxInput("stats_onepeptide_use_zscore", label = "Use zscore?")
                   ),
                   column(width =1, offset =0,
                          numericInput("stats_onepeptide_residue", label="Residue", value = 0, width = 100)
                   ),
                   column(width =1, offset =0,
                          actionButton("create_stats_onepeptide_plots", label = "Create Plots", width = 100,
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                   )
                )
              ),
             fluidRow(
               tabBox(title = "Peptide Plots", width = 12, height = 700,
                      tabPanel("Peptide",
                        column(width =12, offset =0,
                          dropdownButton(
                            textInput("1_stats_onepeptide_barplot_y_axis_label", label="y axis label", value = "Intensity", width = 200),
                            textInput("1_stats_onepeptide_barplot_title", label="plot title", value = "Total Summed Intensity", width = 200),
                            sliderInput("1_stats_onepeptide_barplot_label_size", label = h5("Label Size"), min = 1,
                                        max = 50, value = 11),
                            sliderInput("1_stats_onepeptide_barplot_title_size", label = h5("Title Size"), min = 10,
                                        max = 50, value = 20),
                            circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
                            tooltip = tooltipOptions(title = "Click to see inputs !")
                          ),
                          div(
                            style = "position:relative",
                            plotOutput("1_stats_onepeptide_barplot", width = 1200, height = 400)
                          ),
                          downloadButton('1_download_stats_onepeptide_barplot')
                          )
                        ),
                    tabPanel("GroupedPeptide",
                      column(width =12, offset =0,
                             dropdownButton(
                               textInput("stats_onepeptide_grouped_barplot_y_axis_label", label="y axis label", value = "Intensity", width = 200),
                               textInput("stats_onepeptide_grouped_barplot_x_axis_label", label="x axis label", value = "Sequence", width = 200),
                               textInput("stats_onepeptide_grouped_barplot_title", label="plot title", value = "Total Summed Intensity", width = 200),
                               sliderInput("stats_onepeptide_grouped_barplot_label_size", label = h5("Label Size"), min = 1,
                                           max = 50, value = 11),
                               sliderInput("stats_onepeptide_grouped_barplot_title_size", label = h5("Title Size"), min = 10,
                                           max = 50, value = 20),
                               sliderInput("stats_onepeptide_grouped_barplot_axis_angle", label = h5("X-axis angle"), min = 0,
                                           max = 90, value = 45),
                               numericInput("stats_onepeptide_grouped_barplot_axis_vjust", label="X-axis vjust", value = 0.5),
                               circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
                               tooltip = tooltipOptions(title = "Click to see inputs !")
                             ),
                             div(
                               style = "position:relative",
                               plotOutput("stats_onepeptide_grouped_barplot", width = 1200, height = 400)
                             ),
                             downloadButton('download_stats_onepeptide_grouped_barplot')
                      )
                    ),
                    tabPanel("PeptideData",   
                         column(width =12, offset =0,
                                hr(),
                                tags$head(tags$style("#onepeptide_peptide_table{color: blue;
                                                                         font-size: 12px;
                                                                         }"
                                )
                                ),
                                DT::dataTableOutput("onepeptide_peptide_table", width ='100%')
                         ),
                         column(width =2, offset =0,
                                textInput("stats_onepeptide_data_filename", label="File Name", value = "my_peptide_data.xlsx", width = 250)
                         ),
                         column(width =1, offset =0,
                                actionButton("stats_onepeptide_data_save", label = "Save Data", width = 100,
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                         )
                       )
                    )
               )
    ),



    #-----
    tabItem("pathway_setup",
            fluidRow(
              box(title = "Pathway Setup", width = 12, height = 1200,
                  tabPanel("Organism", value = "tp_save", align = "center",
                           br(),
                           br(),
                           br(),
                           br(),
                           tags$h1("Select organism for pathway analysis/enrichment..."),
                           hr(),
                           selectInput("select_organism", label = "organism", 
                                       choices = list("Human", "Mouse", "Rat"), #, "Rat", "Danio", "Arabidopsis", "Ecoli"), 
                                       selected = "Human", width = 400),
                           br(),
                           actionButton("set_pathway", label = "Set Pathway", width = 300, 
                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                  )))),
    
    tabItem("pathway_wiki",
            fluidRow(
              box(id = "wiki_parameters", title = "WikiPathway Parameters...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 2, height = 800,
                  br(),
                  br(),
                  selectInput("select_data_comp_wiki", label = "comparison", 
                              choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                              selected = 1),
                  br(),
                  radioButtons("wiki_direction", label = "Fold Change Direction", choices = list("Up", "Down", "UpDown"),  selected = "Up", width = 200),
                  br(),
                  br(),
                  textInput("wiki_data_filename", label="File Name", value = "wiki_data.xlsx", width = 250),
                  br(),
                  br(),
                  actionButton("wiki_show", label = "Find WikiPathway", width = 150,
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  #actionButton("wiki_data_save", label = "Save Data", width = 100,
                  #             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  br(),
                  downloadButton('download_wiki_table', label = "Download Table")
              ),
              box(id = "wiki_plot", title = "WikiPathways...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 10, height = 800, 
                  tags$head(tags$style("#wiki_table{color: blue; font-size: 12px;}")),
                  DT::dataTableOutput("wiki_table", width ='100%')
              )
            )
    ),
    
    tabItem("pathway_go_profile",
            fluidRow(
              box(id = "go_profile_parameters", title = "Go Profile Parameters...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 2, height = 800,
                  selectInput("select_go_profile_data_comp", label = "comparison", 
                              choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                              selected = 1),
                  radioButtons("profile_direction", label="Fold Change Direction", choices = list("Up", "Down", "UpDown"),  selected = "Up", width = 200),
                  selectInput("select_ont_profile", label = "ontology", 
                              choices = list("CC", "BP", "MF"), 
                              selected = "BP"),
                  selectInput("select_level_profile", label = "ontology level", 
                              choices = list("1", "2", "3", "4", "5", "6"), 
                              selected = "3"),
                  br(),
                  br(),
                  textInput("go_profile_filename", label="File Name", value = "go_profile_data.xlsx", width = 250),
                  br(),
                  br(),
                  actionButton("profile_go_show", label = "Find Go Profile", width = 150,
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  #actionButton("profile_go_excel", label = "Save Data", width = 150,
                  #             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  br(),
                  downloadButton(label =  "Download Table", 'download_go_profile_table'),
                  br(),
                  br(),
                  downloadButton(label = "Download Plot", 'download_go_profile_plot')
              ),
              
              tabBox(title = "Go Profile", width = 10, height = 800,
                     tabPanel("Go Profile Table",
                              tags$head(tags$style("#go_profile_table{color: blue; font-size: 12px;}")),
                              DT::dataTableOutput("go_profile_table", width ='100%')
                     ),
                     tabPanel("Go Profile Plot",
                              plotOutput("go_profile_plot")
                     )
              )
            )
    ),
    
    tabItem("pathway_go_analysis",
            fluidRow(
              box(id = "go_anlaysis_parameters", title = "Go Analysis Parameters...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 2, height = 800,
                  selectInput("select_go_data_comp", label = "comparison", 
                              choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                              selected = 1),
                  br(),
                  radioButtons("go_direction", label="Fold Change Direction", choices = list("Up", "Down", "UpDown"),  selected = "Up", width = 200),
                  br(),
                  selectInput("select_ont_go", label = "ontology", 
                              choices = list("CC", "BP", "MF"), 
                              selected = "BP"),
                  br(),
                  br(),
                  textInput("go_filename", label="File Name", value = "go_analysis_data.xlsx", width = 250),
                  br(),
                  br(),
                  actionButton("go_show", label = "Go Analysis", width = 100,
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  #actionButton("go_excel", label = "Save Data", width = 150,
                  #             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  br(),
                  downloadButton(label =  "Download Table", 'download_go_table')
              ),
              
              box(id = "go_anlaysis", title = "Go Analysis...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 10, height = 800,
                  verbatimTextOutput('go_volcano_selection'), #('stats_data_final_protein')
                  tags$head(tags$style("#go_table{color: blue; font-size: 12px;}")),
                  DT::dataTableOutput("go_table", width ='100%')
              )
              
            )
    ), 
    
    tabItem("pathway_go_volcano",
            fluidRow(
              box(id = "go_volcano_parameters", title = "Go Volcano Parameters...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 2, height = 800,
                  textInput("go_volcano_id", label="GO ID", value = "", width = 200),
                  br(),
                  dropdownButton(
                    textInput("plot_y_axis_label", label="y axis label", value = "-log_pvalue", width = 200),
                    textInput("plot_x_axis_label", label="x axis label", value = "log_FC", width = 200),
                    textInput("plot_title", label="plot title", value = "Go Volcano", width = 200),
                    sliderInput("plot_dot_size", label = h5("Point Size"), min = 1, 
                                max = 10, value = 2),
                    sliderInput("plot_label_size", label = h5("Label Size"), min = 1, 
                                max = 50, value = 20),
                    sliderInput("plot_title_size", label = h5("Title Size"), min = 10, 
                                max = 50, value = 20),  
                    colourpicker::colourInput("volcano_dot_color", "Select Color", "blue"),
                    circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
                    tooltip = tooltipOptions(title = "Click to see inputs !")
                  ),
                  br(),
                  br(),
                  textInput("go_volcano_filename", label="File Name", value = "go_volcano_data.xlsx", width = 250),
                  br(),
                  br(),
                  actionButton("start_go_volcano", label = "Create Volcano", width = 150,
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  br(),
                  #actionButton("go_volcano_excel", label = "Save Data", width = 150,
                  #             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  br(),
                  downloadButton(label =  "Download Table", 'download_go_volcano_table'),
                  br(),
                  downloadButton(label =  "Download Plot", 'download_go_volcano')
              ),
              
              tabBox(id = "go_volcano", title = "Go Volcano...", width = 10, height = 800,
                     tabPanel(title = "Go Volcano Plot",
                              div(
                                style = "position:relative",
                                plotOutput("volcano_go_plot", width = 800, height = 600,
                                           hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                                uiOutput("hover_info")
                              )
                     ),
                     tabPanel(title = "Go Volcano Data",
                              tags$head(tags$style("#volcano_data_final{color: blue; font-size: 12px;}")),
                              DT::dataTableOutput("volcano_data_final", width ='100%')
                     )
              )
            )
    ),  
    
    
    tabItem("pathway_string", 
            fluidRow(
              
              box(id = "string_parameters", title = "StringDB Parameters...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 2, height = 800,
                  br(),
                  selectInput("select_data_comp_string", label = "comparison", 
                              choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                              selected = 1),
                  br(),
                  radioButtons("string_direction", label="Fold Change Direction", choices = list("Up", "Down", "UpDown"),  selected = "Up", width = 200),
                  br(),
                  selectInput("protein_number", label = "Max #Proteins", 
                              choices = list(10, 25, 50, 75, 100), #150, 200, 250, 300, 350, 400), 
                              selected = 100),
                  br(),
                  actionButton("get_string", label = "String Analysis", width = 150,
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  br(),
                  br(),
                  tags$head(tags$style("#string_link{color: blue;
                                font-size: 12px;
                                 }")),
                  uiOutput("string_link"),
                  br(),
                  br(),
                  br(),
                  downloadButton(label =  "Download Plot", 'download_string_plot')
              ),
              
              box(id = "string_plot", title = "StringDB Plot...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 10, height = 800,
                  
                  plotOutput("string_plot")  
              )
            )
    ),
    
    
    tabItem("pathway_string_enrich", 
            fluidRow(
              box(id = "string_enrich_parameters", title = "StringDB Enrich Parameters...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 2, height = 800,
                  br(),
                  br(),
                  selectInput("select_data_comp_string_enrich", label = "comparison", 
                              choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                              selected = 1),
                  br(),
                  radioButtons("string_enrich_direction", label="Fold Change Direction", choices = list("Up", "Down", "UpDown"),  selected = "Up", width = 200),
                  br(),
                  br(),
                  textInput("string_enrich_data_filename", label="File Name", value = "string_data.xlsx", width = 250),
                  br(),
                  actionButton("get_string_enrich", label = "String Enrichment", width = 150,
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  #actionButton("string_enrich_data_save", label = "Save Data", width = 100,
                  #             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  br(),
                  downloadButton(label =  "Download Table", 'download_string_enrich_table')
              ),
              
              box(id = "string_enrich_table", title = "StringDB Enrich Table...", status = "primary",
                  solidHeader = TRUE, collapsible = FALSE, align = "left", width = 10, height = 800,
                  tags$head(tags$style("#string_table{color: blue; font-size: 12px;}"
                  )),
                  DT::dataTableOutput("string_table", width ='100%')
                  
                  
              )
            )
    ),
    



    tabItem("phos_setup",
      fluidRow(
        box(id = "phos_fasta_setup", title = "Phos Database Parameters...", status = "primary",
            solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 150,
            column(width =2, offset =0,
              br(),
              shinyFilesButton('motif_fasta_file', label='Select Motif-X FASTA', title='Please select fasta file to format for motif-x', multiple=FALSE,
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            ),
            column(width =2, offset =0,
              textInput("fasta_grep1", label = "Accession start...", value = ">sp\\|"),
            ),
            column(width =2, offset =0,
              textInput("fasta_grep2", label = "Accession start...", value = "\\|[^.]*$"),
            ),
            column(width =2, offset =0,
              br(),
              actionButton("parse_fasta", label = "Format fasta", width = 150,
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
            column(width =3, offset =0,
                br(),
                 textOutput("fasta_file_name"),
                 tags$head(tags$style("#fasta{color: blue; font-size: 18px; font-style: bold;}"))
          )
          )
        ), 
        
        fluidRow(
          box(id = "phos_fasta_old", title = "Original Phos Database...", status = "primary",
              solidHeader = TRUE, collapsible = FALSE, align = "left", width = 4, height = 600,
              br(),
              br(),
              rHandsontableOutput("start_fasta_example")
          ),
          
          box(id = "phos_fasta_new", title = "Formated Phos Database...", status = "primary",
              solidHeader = TRUE, collapsible = FALSE, align = "left", width = 8, height = 600,
              br(),
              br(),
              rHandsontableOutput("end_fasta_example")
        )
        )
      ),



    tabItem("phos_motif",
      fluidRow(
        column(width =2, offset =0,
          fluidRow(
              box(id = "phos_motif_setup", title = "Motif Parameters...", status = "primary",
                    solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 800,
                  selectInput("select_data_comp_motif", label = "comparison", 
                    choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), selected = 1, width = 150),
                  numericInput("pval_motif", label="MotifX pval (x1e-5)", value =1, width = 150),
                  numericInput("motif_min_seq", label="Min sequences", value =20, width = 150),
                  textInput("protein_filter", label="Protein Accession Filter", value ="", width = 150),
                  br(),
                  br(),
                  actionButton("motif_show", label = "Send to MotifX", width = 150,
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  br(),
                  br(),
                  textInput("motif_data_filename", label="File Name", value = "my_data.xlsx", width = 150),
                  br(),
                  br(),
                  actionButton("motif_data_save", label = "Save Data", width = 150,
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  br(),
                  br(),
                  br(),
                  downloadButton(label="Download Table", 'download_motif_table')
                )
                )
              ),
      
      column(width =9, offset =0,
        box(id = "phos_motif_table", title = "Motif Table...", status = "primary",
            solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 800,
          hr(),
          tags$head(tags$style("#data_final{color: blue;
             font-size: 12px;
             }")),
          DT::dataTableOutput("motif_table", width ='100%')
        )
      )
      )
    ),           

tabItem("phos_momo",
        fluidRow(
          column(width =4, offset =0,
                 fluidRow(
                   box(id = "phos_momo", title = "MoMo Parameters...", status = "primary",
                       solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 800,
                       br(),
                       br(),
                       br(),
                       selectInput("select_data_comp_momo", label = "comparison", 
                                   choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), selected = 1, width = 150),
                       br(),
                       br(),
                       br(),
                       radioButtons("momo_direction", label="Fold Change Direction", choices = list("Up", "Down", "UpDown"),  selected = "Up", width = 200),
                       br(),
                       br(),
                       br(),
                       actionButton("momo_create", label = "Create Files", width = 150,
                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       downloadButton(label="Download MOMO ptm_file", 'download_momo_file1'),
                       br(),
                       br(),
                       br(),
                       downloadButton(label="Download MOMO context file", 'download_momo_file2'),

    
                   )
                 )
          ),
          column(width =8, offset =0,
                 fluidRow(
                   box(id = "phos_momo", title = "MoMo Directions...", status = "primary",
                       solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 800,
                       h1("MEME Suite", style = "font-size:50px"),
                       br(),
                       h1("Step1", style = "font-size:20px"),
                       h1("On the left panel pick the comparison and the fold change direction.", style = "font-size:15px"),
                       h1("Click create files.", style = "font-size:15px"),
                       h1("Download files.", style = "font-size:15px"),
                       h1("The ptm_file and context files for MOMO will be saved to your Downloads folder.", style = "font-size:15px"),
                       br(),
                       h1("Step2", style = "font-size:20px"),
                       h1("Click on the link below to open the MEME Suite: MOMO", style = "font-size:15px"),
                       h4("Link: ", a(target="_blank", "meme-suite.org/meme/tools/momo",  href = "https://meme-suite.org/meme/tools/momo"), style = "font-size:15px"),
                       br(),
                       h1("Step3", style = "font-size:20px"),
                       h1("Upload the ptm_file and context file (or use built in context)", style = "font-size:15px"),
                       h1("Under advanced change the sequence length to 15 (2 places)", style = "font-size:15px"),
                       h1("Start search", style = "font-size:15px"),
                       h1("For more detailed instructions please see the website instructions", style = "font-size:15px")
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