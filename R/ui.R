# =============================================================================
# R/ui.R - Clean User Interface
# =============================================================================

ui <- fluidPage(
  # Link to external CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$title("GME Milestone Platform")
  ),
  
  # Clean header
  div(class = "main-container",
      div(class = "gmed-header",
          style = "background: linear-gradient(90deg, #2C3E50 0%, #3498DB 100%); 
                   color: white; padding: 30px 0; margin-bottom: 30px; text-align: center;",
          h1("GME Milestone Platform", style = "margin: 0; font-weight: 300; font-size: 2.2rem;"),
          uiOutput("program_subtitle")
      ),
      
      # Clean navigation
      navset_card_tab(
        id = "main_tabs",
        
        # ===================================================================
        # DATA UPLOAD - SIMPLIFIED
        # ===================================================================
        nav_panel("Get Started",
                  icon = icon("upload"),
                  
                  # Tab Description
                  fluidRow(
                    column(12,
                           div(class = "alert alert-info mb-4",
                               h6("Get Started", style = "margin-bottom: 10px; color: #2c3e50;"),
                               p("Welcome to the GME Milestone Visualization Platform. This tool helps residency programs analyze ACGME Milestone 2.0 evaluation data to identify trends, track resident progression, and support educational outcomes. Upload your WebADS milestone CSV files or try our demo data to explore the platform's capabilities.",
                                 style = "margin-bottom: 0; font-size: 0.95em;")
                           )
                    )
                  ),
                  
                  # Quick start section
                  fluidRow(
                    column(8, offset = 2,
                           div(class = "content-card card",
                               div(class = "card-body text-center",
                                   h3("Welcome to GME Milestone Platform"),
                                   p("Analyze your ACGME Milestone evaluations with powerful visualizations and insights.", 
                                     class = "lead"),
                                   
                                   hr(),
                                   
                                   # Two main options
                                   fluidRow(
                                     column(6,
                                            div(class = "content-card card border-primary",
                                                div(class = "card-header bg-primary text-white text-center",
                                                    h5(icon("database"), "Upload Your Data", class = "mb-0")
                                                ),
                                                div(class = "card-body",
                                                    p("Load your ACGME Milestone 2.0 data (in .csv files). To get the most from this, we recommend uploading multiple (both Mid-Year and Year-End) evaluations from a number of years.", 
                                                      style = "font-size: 0.95em;"),
                                                    
                                                    fileInput("csv_files", 
                                                              "Choose CSV File(s)",
                                                              multiple = TRUE,
                                                              accept = c(".csv")),
                                                    
                                                    actionButton("process_csv", "Process Data", 
                                                                 class = "btn-primary btn-block"),
                                                    
                                                    br(),
                                                    verbatimTextOutput("status_text", placeholder = TRUE)
                                                )
                                            )
                                     ),
                                     
                                     column(6,
                                            div(class = "content-card card border-success",
                                                div(class = "card-header bg-success text-white text-center",
                                                    h5(icon("play-circle"), "Try Demo Data", class = "mb-0")
                                                ),
                                                div(class = "card-body",
                                                    p(strong("No data? Try using our demo data!"), 
                                                      style = "color: #28a745; margin-bottom: 10px;"),
                                                    p("Explore features with sample Internal Medicine data", 
                                                      style = "font-size: 0.95em;"),
                                                    
                                                    actionButton("load_demo_data", 
                                                                 "Load Demo Data", 
                                                                 class = "demo-button btn-block",
                                                                 icon = icon("play-circle")),
                                                    
                                                    br(),
                                                    div(class = "alert alert-info",
                                                        style = "font-size: 0.9em; margin-top: 15px;",
                                                        "Demo includes 4 years of realistic milestone data")
                                                )
                                            )
                                     )
                                   )
                               )
                           )
                    )
                  ),
                  
                  # Data summary section
                  conditionalPanel(
                    condition = "output.data_loaded",
                    fluidRow(
                      column(8, offset = 2,
                             div(class = "content-card card border-success",
                                 div(class = "card-header bg-success text-white",
                                     h5(icon("check-circle"), "Data Loaded Successfully", class = "mb-0")
                                 ),
                                 div(class = "card-body",
                                     htmlOutput("quick_summary"),
                                     hr(),
                                     p("Navigate to Program Overview or Milestone Analysis to begin exploring your data.",
                                       class = "text-center text-muted")
                                 )
                             )
                      )
                    )
                  ),
                  
                  # Collapsible help section
                  fluidRow(
                    column(10, offset = 1,
                           br(),
                           div(class = "card",
                               div(class = "card-header",
                                   h5(icon("question-circle"), "Need Help Getting Data?",
                                      tags$button(class = "btn btn-link btn-sm float-right", 
                                                  `data-toggle` = "collapse", 
                                                  `data-target` = "#help-section",
                                                  "Show/Hide Help")
                                   )
                               ),
                               div(id = "help-section", class = "collapse card-body",
                                   fluidRow(
                                     column(6,
                                            h6("Getting Data from WebADS:"),
                                            tags$ol(
                                              tags$li("Login to ACGME WebADS"),
                                              tags$li("Go to Milestones tab"),
                                              tags$li("Download evaluation data by period"),
                                              tags$li("Upload CSV files here")
                                            )
                                     ),
                                     column(6,
                                            h6("Milestones 2.0 Requirements:"),
                                            p("This platform requires Milestones 2.0 data."),
                                            actionButton("show_milestone_dates", 
                                                         "View Implementation Timeline", 
                                                         class = "btn-outline-secondary btn-sm")
                                     )
                                   )
                               )
                           )
                    )
                  )
        ),
        
        # ===================================================================
        # PROGRAM OVERVIEW - SIMPLIFIED
        # ===================================================================
        nav_panel("Program Overview",
                  icon = icon("chart-line"),
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    # Tab Description
                    fluidRow(
                      column(12,
                             div(class = "alert alert-info mb-4",
                                 h6("Program Overview", style = "margin-bottom: 10px; color: #2c3e50;"),
                                 p("Get a comprehensive view of your program's milestone performance across all residents and competencies. This dashboard identifies areas of strength and improvement while providing visual insights into overall program trends. Use the controls to filter by specific time periods and training levels for targeted analysis.",
                                   style = "margin-bottom: 0; font-size: 0.95em;")
                             )
                      )
                    ),
                    
                    fluidRow(
                      # Sidebar
                      column(3,
                             div(class = "sidebar-card js-sticky-sidebar",
                                 div(class = "card-header", h6("Analysis Settings")),
                                 div(class = "card-body",
                                     
                                     # Controls explanation
                                     div(class = "alert alert-light mb-3",
                                         style = "font-size: 0.85em; padding: 10px;",
                                         HTML("<strong>Controls Guide:</strong><br>
                                               • <strong>Assessment Period:</strong> Choose evaluation timeframe<br>
                                               • <strong>Training Levels:</strong> Select PGY levels to include<br>
                                               • <strong>Display Options:</strong> Customize chart and table displays")
                                     ),
                                     
                                     # Simplified period selection
                                     div(class = "section-title", "Assessment Period"),
                                     radioButtons("period_selection", "",
                                                  choices = list(
                                                    "Latest End-Year" = "recent_end",
                                                    "Latest Mid-Year" = "recent_mid",
                                                    "Specific Period" = "specific",
                                                    "All Periods" = "all_periods"
                                                  ),
                                                  selected = "recent_end"),
                                     
                                     conditionalPanel(
                                       condition = "input.period_selection == 'specific'",
                                       selectInput("specific_period", "Choose Period:",
                                                   choices = NULL)
                                     ),
                                     
                                     # PGY selection
                                     div(class = "section-title", "Training Levels"),
                                     checkboxInput("select_all_pgy", "Select All", value = TRUE),
                                     div(class = "checkbox-group",
                                         uiOutput("pgy_checkboxes")
                                     ),
                                     
                                     # Display options
                                     div(class = "section-title", "Display Options"),
                                     checkboxInput("show_program_means", "Show Program Averages", value = TRUE),
                                     checkboxInput("tables_use_filters", "Apply Filters to Tables", value = TRUE)
                                 )
                             )
                      ),
                      
                      # Main content
                      column(9,
                             # Performance insights
                             fluidRow(
                               column(6,
                                      div(class = "content-card card",
                                          div(class = "card-header", 
                                              h6(icon("exclamation-triangle", style = "color: #fd7e14;"), "Areas for Improvement")
                                          ),
                                          div(class = "card-body",
                                              DT::dataTableOutput("improvement_areas")
                                          )
                                      )
                               ),
                               column(6,
                                      div(class = "content-card card",
                                          div(class = "card-header", 
                                              h6(icon("star", style = "color: #28a745;"), "Areas of Strength")
                                          ),
                                          div(class = "card-body",
                                              DT::dataTableOutput("strength_areas")
                                          )
                                      )
                               )
                             ),
                             
                             # Spider plot
                             div(class = "content-card card",
                                 div(class = "card-header", 
                                     h6(icon("chart-pie"), "Program Performance Overview")
                                 ),
                                 div(class = "card-body",
                                     plotlyOutput("program_spider", height = "600px")
                                 )
                             ),
                             
                             # Milestone reference
                             div(class = "content-card card",
                                 div(class = "card-header", 
                                     h6(icon("list-alt"), "Milestone Reference Guide")
                                 ),
                                 div(class = "card-body",
                                     DT::dataTableOutput("milestone_reference_table")
                                 )
                             )
                      )
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "!output.data_loaded",
                    div(class = "alert alert-info text-center",
                        icon("info-circle"), " Please load data from the Get Started tab.")
                  )
        ),
        
        # ===================================================================
        # MILESTONE ANALYSIS - SIMPLIFIED
        # ===================================================================
        nav_panel("Milestone Analysis",
                  icon = icon("graduation-cap"),
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    # Tab Description
                    fluidRow(
                      column(12,
                             div(class = "alert alert-info mb-4",
                                 h6("Milestone Analysis", style = "margin-bottom: 10px; color: #2c3e50;"),
                                 p("Dive deeper into milestone performance with readiness analysis, training level comparisons, and progression tracking. This section helps identify residents who may need additional support and tracks how competencies develop throughout training. Set your proficiency threshold and filters to customize the analysis.",
                                   style = "margin-bottom: 0; font-size: 0.95em;")
                             )
                      )
                    ),
                    
                    fluidRow(
                      # Sidebar
                      column(3,
                             div(class = "sidebar-card js-sticky-sidebar",
                                 div(class = "card-header", h6("Analysis Settings")),
                                 div(class = "card-body",
                                     
                                     # Controls explanation
                                     div(class = "alert alert-light mb-3",
                                         style = "font-size: 0.85em; padding: 10px;",
                                         HTML("<strong>Controls Guide:</strong><br>
                                               • <strong>Proficiency Threshold:</strong> Score considered 'proficient' (typically 7)<br>
                                               • <strong>Filters:</strong> Narrow analysis to specific periods/competencies<br>
                                               • <strong>Training Levels:</strong> Select PGY years to compare")
                                     ),
                                     
                                     # Risk level legend
                                     div(class = "alert alert-warning mb-3", 
                                         style = "font-size: 0.85em; padding: 10px;",
                                         HTML("<strong>Risk Levels:</strong><br>
                                               🟢 <strong>Excellent:</strong> <2.5% below threshold<br>
                                               🟢 <strong>Good:</strong> 2.5-5% below threshold<br>
                                               🟠 <strong>Concerning:</strong> 5-7.5% below threshold<br>
                                               🔴 <strong>High Risk:</strong> >7.5% below threshold")
                                     ),
                                     
                                     # Threshold
                                     div(class = "section-title", "Proficiency Threshold"),
                                     sliderInput("milestone_threshold", "",
                                                 min = 1, max = 9, value = 7, step = 1),
                                     p("7 = Proficient level", 
                                       style = "font-size: 0.8em; color: #6c757d;"),
                                     
                                     # Period and competency filters
                                     div(class = "section-title", "Filters"),
                                     selectInput("milestone_period", "Period:",
                                                 choices = list("All Periods" = "all"),
                                                 selected = "all"),
                                     
                                     selectInput("milestone_competency", "Competency:",
                                                 choices = list("All Competencies" = "all"),
                                                 selected = "all"),
                                     
                                     # PGY selection for all levels analysis
                                     div(class = "section-title", "Training Levels"),
                                     div(class = "checkbox-group",
                                         checkboxGroupInput("all_levels_pgy", "",
                                                            choices = NULL)
                                     )
                                 )
                             )
                      ),
                      
                      # Main content
                      column(9,
                             # Graduation readiness
                             div(class = "content-card card",
                                 div(class = "card-header", 
                                     h6(icon("graduation-cap"), "Graduation Readiness Analysis")
                                 ),
                                 div(class = "card-body",
                                     plotlyOutput("graduation_readiness_chart", height = "500px")
                                 )
                             ),
                             
                             # All levels analysis
                             div(class = "content-card card",
                                 div(class = "card-header", 
                                     h6(icon("users"), "All Training Levels Analysis")
                                 ),
                                 div(class = "card-body",
                                     plotlyOutput("all_levels_chart", height = "600px")
                                 )
                             ),
                             
                             # Trend analysis
                             div(class = "content-card card",
                                 div(class = "card-header", 
                                     h6(icon("line-chart"), "Sub-Competency Progression")
                                 ),
                                 div(class = "card-body",
                                     fluidRow(
                                       column(4,
                                              selectInput("trend_subcompetency", 
                                                          "Select Sub-Competency:",
                                                          choices = NULL),
                                              
                                              h6("Add Graduation Classes:", 
                                                 style = "margin-top: 20px; color: #6c757d;"),
                                              checkboxGroupInput("selected_cohorts", "",
                                                                 choices = NULL),
                                              
                                              div(class = "btn-group-sm",
                                                  actionButton("select_recent_cohorts", "Recent 2", 
                                                               class = "btn-outline-secondary"),
                                                  actionButton("clear_cohorts", "Clear", 
                                                               class = "btn-outline-secondary")
                                              )
                                       ),
                                       column(8,
                                              plotlyOutput("cohort_trend_plot", height = "400px")
                                       )
                                     )
                                 )
                             ),
                             
                             # Summary tables
                             div(class = "content-card card",
                                 div(class = "card-header", h6(icon("table"), "Detailed Metrics")),
                                 div(class = "card-body",
                                     navset_card_tab(
                                       nav_panel("Graduation", DT::dataTableOutput("graduation_table")),
                                       nav_panel("All Levels", DT::dataTableOutput("all_levels_table"))
                                     )
                                 )
                             )
                      )
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "!output.data_loaded",
                    div(class = "alert alert-info text-center",
                        icon("info-circle"), " Please load data from the Get Started tab.")
                  )
        ),
        
        # Individual Assessment
        create_individual_assessment_ui(),
        
        # Data Overview
        nav_panel("Data Overview",
                  icon = icon("table"),
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    # Tab Description
                    fluidRow(
                      column(12,
                             div(class = "alert alert-info mb-4",
                                 h6("Data Overview", style = "margin-bottom: 10px; color: #2c3e50;"),
                                 p("Review your uploaded milestone data structure and definitions. This section displays the milestone framework extracted from your data and provides a sample of the processed evaluation records for verification.",
                                   style = "margin-bottom: 0; font-size: 0.95em;")
                             )
                      )
                    ),
                    
                    fluidRow(
                      column(12,
                             div(class = "content-card card",
                                 div(class = "card-header", h6("Milestone Definitions")),
                                 div(class = "card-body",
                                     DT::dataTableOutput("milestone_table")
                                 )
                             ),
                             
                             div(class = "content-card card",
                                 div(class = "card-header", h6("Sample Evaluation Data")),
                                 div(class = "card-body",
                                     DT::dataTableOutput("evaluation_sample")
                                 )
                             )
                      )
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "!output.data_loaded",
                    div(class = "alert alert-info text-center",
                        icon("info-circle"), " Please load data from the Get Started tab.")
                  )
        )
      )
  )
)