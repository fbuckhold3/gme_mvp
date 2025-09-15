ui <- page_fluid(
  # Theme configuration
  theme = bs_theme(
    version = 5,
    primary = "#2C3E50",
    secondary = "#3498DB", 
    success = "#27AE60",
    warning = "#F39C12",
    danger = "#E74C3C",
    info = "#17A2B8",
    "bg-body" = "#F8F9FA",
    "card-bg" = "#FFFFFF",
    "font-size-base" = "0.95rem"
  ),
  
  title = "GME Milestone Visualization Platform",
  
  # Header
  div(class = "gmed-header",
      style = "background: linear-gradient(90deg, #2C3E50 0%, #3498DB 100%); color: white; padding: 20px; margin-bottom: 20px;",
      h1("GME Milestone Visualization Platform", style = "margin: 0; font-weight: 300; font-size: 2.5rem;"),
      p("ACGME Milestone Analysis from CSV Data", 
        style = "margin: 5px 0 0 0; opacity: 0.9; font-size: 1.1rem;")
  ),
  
  # Main navigation
  navset_card_tab(
    id = "main_tabs",
    
    # ======================================================================
    # DATA UPLOAD TAB
    # ======================================================================
    nav_panel("Data Upload",
              icon = icon("upload"),
              
              fluidRow(
                column(8,
                       div(class = "card",
                           div(class = "card-header",
                               h4(icon("file-csv"), "Upload ACGME Milestone Data")
                           ),
                           div(class = "card-body",
                               fileInput("csv_files", 
                                         "Choose CSV File(s)",
                                         multiple = TRUE,
                                         accept = c(".csv")),
                               
                               div(style = "margin-top: 15px;",
                                   actionButton("process_csv", "Process Data", 
                                                class = "btn-primary btn-lg"),
                                   uiOutput("csv_status", inline = TRUE, style = "margin-left: 15px;")
                               ),
                               
                               conditionalPanel(
                                 condition = "output.data_loaded == true",
                                 div(class = "alert alert-success mt-3",
                                     icon("check-circle"), " Data loaded successfully! ",
                                     textOutput("data_summary", inline = TRUE))
                               )
                           )
                       )
                ),
                
                column(4,
                       div(class = "card",
                           div(class = "card-header",
                               h5("Expected CSV Format")
                           ),
                           div(class = "card-body",
                               tags$p("Your CSV should contain ACGME milestone evaluation data with these key columns:"),
                               tags$ul(
                                 tags$li(tags$strong("Resident ID"), " - Unique identifier"),
                                 tags$li(tags$strong("First Name, Last Name"), " - Resident names"),
                                 tags$li(tags$strong("Resident Year"), " - Training level"),
                                 tags$li(tags$strong("Question Key"), " - Milestone identifier"),
                                 tags$li(tags$strong("Report Category"), " - Competency area"),
                                 tags$li(tags$strong("Int Response Value"), " - Numeric score (1-9)"),
                                 tags$li(tags$strong("Schedule Window Description"), " - Assessment period")
                               ),
                               
                               tags$hr(),
                               tags$small("Uses your existing gmed package functions for processing.",
                                          class = "text-muted")
                           )
                       )
                )
              )
    ),
    
    # ======================================================================
    # PROGRAM OVERVIEW TAB
    # ======================================================================
    nav_panel("Program Overview",
              icon = icon("chart-bar"),
              
              # Controls row
              fluidRow(
                column(3,
                       selectInput("program_period", "Assessment Period:",
                                   choices = c("Total (All Periods)" = "total"), 
                                   selected = "total")
                ),
                column(3,
                       selectInput("program_level", "Training Year:",
                                   choices = c("All Years" = "all"),
                                   selected = "all")
                ),
                column(6,
                       conditionalPanel(
                         condition = "output.data_loaded != true",
                         div(class = "alert alert-warning",
                             icon("exclamation-triangle"), " Please upload CSV data first")
                       )
                )
              ),
              
              # Main content
              conditionalPanel(
                condition = "output.data_loaded == true",
                
                # Summary stats row
                fluidRow(
                  column(6,
                         div(class = "card",
                             div(class = "card-header", h5("Program Strengths")),
                             div(class = "card-body", DT::dataTableOutput("strengths_table"))
                         )
                  ),
                  column(6,
                         div(class = "card",
                             div(class = "card-header", h5("Areas for Improvement")),
                             div(class = "card-body", DT::dataTableOutput("improvements_table"))
                         )
                  )
                ),
                
                # Visualization row
                fluidRow(
                  column(6,
                         div(class = "card",
                             div(class = "card-header", h5("Competency Spider Plot")),
                             div(class = "card-body", plotlyOutput("program_spider", height = "400px"))
                         )
                  ),
                  column(6,
                         div(class = "card",
                             div(class = "card-header", h5("Competency Trend Lines")),
                             div(class = "card-body", plotlyOutput("competency_trends", height = "400px"))
                         )
                  )
                ),
                
                # Heat map row
                fluidRow(
                  column(12,
                         div(class = "card",
                             div(class = "card-header", 
                                 h5("Sub-Competency Heatmap"),
                                 div(class = "float-end",
                                     selectInput("heatmap_metric", NULL,
                                                 choices = c("Overall Median" = "overall",
                                                             "Period Median" = "period", 
                                                             "Year Median" = "year"),
                                                 selected = "overall",
                                                 width = "150px")
                                 )
                             ),
                             div(class = "card-body", plotlyOutput("milestone_heatmap", height = "600px"))
                         )
                  )
                )
              )
    ),
    
    # ======================================================================
    # INDIVIDUAL RESIDENTS TAB  
    # ======================================================================
    nav_panel("Individual Residents",
              icon = icon("user"),
              
              # Controls row
              fluidRow(
                column(3,
                       selectInput("selected_resident", "Select Resident:",
                                   choices = NULL)
                ),
                column(3,
                       selectInput("resident_period", "Assessment Period:",
                                   choices = c("Total (All Periods)" = "total"),
                                   selected = "total")
                ),
                column(6,
                       conditionalPanel(
                         condition = "output.data_loaded != true",
                         div(class = "alert alert-warning",
                             icon("exclamation-triangle"), " Please upload CSV data first")
                       )
                )
              ),
              
              # Main content
              conditionalPanel(
                condition = "output.data_loaded == true && input.selected_resident != null",
                
                # Resident info and spider plot
                fluidRow(
                  column(4,
                         div(class = "card",
                             div(class = "card-header", h5("Resident Information")),
                             div(class = "card-body", 
                                 uiOutput("resident_info"))
                         )
                  ),
                  column(8,
                         div(class = "card",
                             div(class = "card-header", h5("Individual Competency Profile")),
                             div(class = "card-body", plotlyOutput("resident_spider", height = "400px"))
                         )
                  )
                ),
                
                # Milestone progression
                fluidRow(
                  column(12,
                         div(class = "card",
                             div(class = "card-header", 
                                 h5("Sub-Competency Progression Over Time"),
                                 div(class = "float-end",
                                     selectInput("selected_milestone", "Select Sub-Competency:",
                                                 choices = NULL,
                                                 width = "200px")
                                 )
                             ),
                             div(class = "card-body", plotlyOutput("milestone_progression", height = "400px"))
                         )
                  )
                ),
                
                # Detailed scores table
                fluidRow(
                  column(12,
                         div(class = "card",
                             div(class = "card-header", h5("Detailed Sub-Competency Scores")),
                             div(class = "card-body", DT::dataTableOutput("resident_scores_table"))
                         )
                  )
                )
              )
    )
  )
)
