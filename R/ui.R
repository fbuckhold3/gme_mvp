# =============================================================================
# R/ui.R - User Interface
# =============================================================================

ui <- page_fluid(
  theme = bs_theme(version = 5, primary = "#2C3E50"),
  title = "GME Milestone Visualization Platform",
  
  # Header
  div(class = "gmed-header",
      style = "background: linear-gradient(90deg, #2C3E50 0%, #3498DB 100%); color: white; padding: 20px; margin-bottom: 20px;",
      h1("GME Milestone Visualization Platform", style = "margin: 0; font-weight: 300; font-size: 2.5rem;"),
      uiOutput("program_subtitle")
  ),
  
  # Main content
  navset_card_tab(
    id = "main_tabs",
    
    # Data Upload Tab
    nav_panel("Data Upload",
              icon = icon("upload"),
              
              fluidRow(
                column(6,
                       div(class = "card",
                           div(class = "card-header",
                               h4(icon("file-csv"), "Upload ACGME Milestone Data")
                           ),
                           div(class = "card-body",
                               fileInput("csv_files", 
                                         "Choose CSV File(s)",
                                         multiple = TRUE,
                                         accept = c(".csv")),
                               
                               actionButton("process_csv", "Process Data", 
                                            class = "btn-primary btn-lg"),
                               
                               br(), br(),
                               
                               verbatimTextOutput("status_text")
                           )
                       )
                ),
                
                column(6,
                       div(class = "card",
                           div(class = "card-header", h5("Summary")),
                           div(class = "card-body",
                               htmlOutput("quick_summary")
                           )
                       )
                )
              )
    ),
    
    # Program Overview Tab
    nav_panel("Program Overview",
              icon = icon("chart-line"),
              
              conditionalPanel(
                condition = "output.data_loaded",
                
                fluidRow(
                  # Left sidebar with controls
                  column(3,
                         div(class = "card",
                             div(class = "card-header", h5("Analysis Controls")),
                             div(class = "card-body",
                                 
                                 # Assessment Period Selection
                                 h6("Assessment Period:", style = "font-weight: bold; margin-top: 10px;"),
                                 radioButtons("period_selection", "",
                                              choices = list(
                                                "Most Recent End-Year" = "recent_end",
                                                "Most Recent Mid-Year" = "recent_mid", 
                                                "All Periods Combined" = "all_periods"
                                              ),
                                              selected = "recent_end"),
                                 
                                 conditionalPanel(
                                   condition = "input.period_selection == 'specific'",
                                   selectInput("specific_period", "Choose Period:",
                                               choices = NULL)
                                 ),
                                 
                                 hr(),
                                 
                                 # PGY Level Selection
                                 h6("PGY Levels to Include:", style = "font-weight: bold; margin-top: 10px;"),
                                 checkboxInput("select_all_pgy", "Select All", value = TRUE),
                                 uiOutput("pgy_checkboxes"),
                                 
                                 hr(),
                                 
                                 # Display Options
                                 h6("Display Options:", style = "font-weight: bold; margin-top: 10px;"),
                                 checkboxInput("show_program_means", "Show Program Means", value = TRUE),
                                 checkboxInput("tables_use_filters", "Apply Filters to Tables", value = TRUE),
                                 
                                 br(),
                                 
                                 # Quick info
                                 div(class = "alert alert-info", style = "font-size: 0.85em;",
                                     HTML("<strong>Info:</strong><br>
                                          • Tables show End-Year data by default<br>
                                          • Check 'Apply Filters to Tables' to use period/level selections<br>
                                          • Spider plot always uses selected filters"))
                             )
                         )
                  ),
                  
                  # Main content area
                  column(9,
                         
                         # Performance insights
                         fluidRow(
                           column(6,
                                  div(class = "card",
                                      div(class = "card-header", h5(icon("exclamation-triangle"), "Areas for Improvement")),
                                      div(class = "card-body",
                                          htmlOutput("improvement_description"),
                                          DT::dataTableOutput("improvement_areas")
                                      )
                                  )
                           ),
                           column(6,
                                  div(class = "card",
                                      div(class = "card-header", h5(icon("star"), "Areas of Strength")),
                                      div(class = "card-body",
                                          htmlOutput("strength_description"),
                                          DT::dataTableOutput("strength_areas")
                                      )
                                  )
                           )
                         ),
                         
                         br(),
                         
                         # Spider plot
                         fluidRow(
                           column(12,
                                  div(class = "card",
                                      div(class = "card-header", 
                                          h5(icon("chart-area"), "Program Performance Spider Plot - All Sub-Competencies")),
                                      div(class = "card-body",
                                          plotlyOutput("program_spider", height = "600px")
                                      )
                                  )
                           )
                         ),
                         
                         br(),
                         
                         # Trend lines and heatmap with their own controls
                         fluidRow(
                           column(6,
                                  div(class = "card",
                                      div(class = "card-header", 
                                          h5(icon("chart-line"), "Sequential Trend Analysis")),
                                      div(class = "card-body",
                                          selectInput("trend_competency", "Filter by Competency:",
                                                      choices = c("All Competencies" = "all"),
                                                      selected = "all"),
                                          plotlyOutput("program_trends", height = "500px")
                                      )
                                  )
                           ),
                           column(6,
                                  div(class = "card",
                                      div(class = "card-header", 
                                          h5(icon("th"), "Performance Heatmap")),
                                      div(class = "card-body",
                                          selectInput("heatmap_metric", "Heatmap Metric:",
                                                      choices = list(
                                                        "Mean Score" = "median",
                                                        "Performance Category" = "performance_category"
                                                      ),
                                                      selected = "median"),
                                          plotlyOutput("program_heatmap", height = "500px")
                                      )
                                  )
                           )
                         )
                  )
                )
              ),
              
              conditionalPanel(
                condition = "!output.data_loaded",
                div(class = "alert alert-info",
                    icon("info-circle"), " Please upload and process CSV data first.")
              )
    ),
    
    # Individual Residents Tab
    nav_panel("Individual Residents",
              icon = icon("user"),
              
              conditionalPanel(
                condition = "output.data_loaded",
                
                # Filters
                fluidRow(
                  column(4,
                         selectInput("selected_resident", "Select Resident:",
                                     choices = NULL)
                  ),
                  column(4,
                         selectInput("individual_period", "Period:",
                                     choices = c("Total (All Periods)" = "total"),
                                     selected = "total")
                  ),
                  column(4,
                         selectInput("individual_competency", "Trend Filter:",
                                     choices = c("All Competencies" = "all"),
                                     selected = "all")
                  )
                ),
                
                # Individual performance summary
                fluidRow(
                  column(12,
                         div(class = "card",
                             div(class = "card-header", 
                                 h5(icon("user-circle"), "Individual Performance Summary")),
                             div(class = "card-body",
                                 DT::dataTableOutput("individual_summary")
                             )
                         )
                  )
                ),
                
                br(),
                
                # Individual spider plot
                fluidRow(
                  column(12,
                         div(class = "card",
                             div(class = "card-header", 
                                 h5(icon("chart-area"), "Individual vs Program Performance")),
                             div(class = "card-body",
                                 plotlyOutput("individual_spider", height = "600px")
                             )
                         )
                  )
                ),
                
                br(),
                
                # Individual trends and peer comparison
                fluidRow(
                  column(6,
                         div(class = "card",
                             div(class = "card-header", 
                                 h5(icon("chart-line"), "Individual Progress Over Time")),
                             div(class = "card-body",
                                 plotlyOutput("individual_trends", height = "500px")
                             )
                         )
                  ),
                  column(6,
                         div(class = "card",
                             div(class = "card-header", 
                                 h5(icon("users"), "Performance vs Peers")),
                             div(class = "card-body",
                                 plotlyOutput("peer_comparison", height = "500px")
                             )
                         )
                  )
                )
              ),
              
              conditionalPanel(
                condition = "!output.data_loaded",
                div(class = "alert alert-info",
                    icon("info-circle"), " Please upload and process CSV data first.")
              )
    ),
    
    # Data Overview Tab
    nav_panel("Data Overview",
              icon = icon("table"),
              
              conditionalPanel(
                condition = "output.data_loaded",
                
                h4("Milestone Definitions"),
                p("Sub-competencies extracted from your data:"),
                DT::dataTableOutput("milestone_table"),
                
                br(),
                
                h4("Sample Evaluation Data"),
                p("Sample processed evaluation records:"),
                DT::dataTableOutput("evaluation_sample")
              ),
              
              conditionalPanel(
                condition = "!output.data_loaded",
                div(class = "alert alert-info",
                    icon("info-circle"), " Please upload and process CSV data first.")
              )
    )
  )
)
