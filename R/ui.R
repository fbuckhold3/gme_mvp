# =============================================================================
# R/ui.R - User Interface (FIXED VERSION)
# =============================================================================

# Safe theme creation
create_theme <- function() {
  tryCatch({
    return(bslib::bs_theme(version = 5, primary = "#2C3E50"))
  }, error = function(e) {
    warning("bslib theme failed, using default: ", e$message)
    return(NULL)
  })
}

ui <- fluidPage(
  theme = create_theme(),  # Will be NULL if bslib fails
  
  # Custom CSS backup
  tags$head(
    tags$style(HTML("
      .btn-primary { background-color: #2C3E50 !important; border-color: #2C3E50 !important; }
      .nav-tabs > li.active > a { background-color: #2C3E50 !important; color: white !important; }
    ")),
    tags$title("GME Milestone Visualization Platform")
  ),
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
                                 
                                 # Assessment Period Selection - FIXED
                                 h6("Assessment Period:", style = "font-weight: bold; margin-top: 10px;"),
                                 radioButtons("period_selection", "",
                                              choices = list(
                                                "Specific Period" = "specific",
                                                "Most Recent End-Year" = "recent_end",
                                                "Most Recent Mid-Year" = "recent_mid", 
                                                "All Periods Combined" = "all_periods"
                                              ),
                                              selected = "recent_end"),
                                 
                                 # Show specific period dropdown when "Specific Period" is selected
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
                                 
                                 # Spider Plot Specific Controls - FIXED CONDITION
                                 h6("Spider Plot Options:", style = "font-weight: bold; margin-top: 10px; color: #2c3e50;"),
                                 
                                 # Multiple Period Selection for Spider Plot
                                 conditionalPanel(
                                   condition = "input.period_selection == 'specific'",  # FIXED: was period_selection_type
                                   div(
                                     checkboxInput("spider_multi_period", "Compare Multiple Periods", value = FALSE),
                                     conditionalPanel(
                                       condition = "input.spider_multi_period",
                                       div(style = "margin-left: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                                           h6("Select periods to compare:", style = "font-size: 0.9em; margin-bottom: 8px;"),
                                           uiOutput("spider_period_checkboxes"),
                                           checkboxInput("spider_include_total", "Include 'All Periods' line", value = TRUE)
                                       )
                                     )
                                   )
                                 ),
                                 
                                 hr(),
                                 
                                 # Display Options (simplified - removed program median)
                                 h6("Display Options:", style = "font-weight: bold; margin-top: 10px;"),
                                 checkboxInput("show_program_means", "Show Program Means", value = TRUE),
                                 checkboxInput("tables_use_filters", "Apply Filters to Tables", value = TRUE),
                                 
                                 br(),
                                 
                                 # Updated info box
                                 div(class = "alert alert-info", style = "font-size: 0.85em;",
                                     HTML("<strong>Info:</strong><br>
                                      • Tables show End-Year data by default<br>
                                      • Check 'Apply Filters to Tables' to use period/level selections<br>
                                      • Spider plot supports multiple period comparisons<br>
                                      • Use 'All Periods' to see combined data trends"))
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
                                          DT::dataTableOutput("improvement_areas")
                                      )
                                  )
                           ),
                           column(6,
                                  div(class = "card",
                                      div(class = "card-header", h5(icon("star"), "Areas of Strength")),
                                      div(class = "card-body",
                                          DT::dataTableOutput("strength_areas")
                                      )
                                  )
                           )
                         ),
                         
                         # Add period info display
                         fluidRow(
                           column(12,
                                  div(style = "text-align: center; margin: 10px 0;",
                                      htmlOutput("period_display_info")
                                  )
                           )
                         ),
                         
                         # Spider plot section - FIXED CLOSING PARENTHESES
                         fluidRow(
                           column(12,
                                  div(class = "card",
                                      div(class = "card-header", 
                                          h5(icon("chart-pie"), "Program Performance Spider Plot - All Sub-Competencies")
                                      ),  # ADDED MISSING COMMA
                                      div(class = "card-body",
                                          plotlyOutput("program_spider", height = "600px")
                                      )
                                  )
                           )
                         ),
                         
                         # ADDED MISSING SECTIONS (you can add more plots here)
                         fluidRow(
                           column(12,
                                  div(class = "card",
                                      div(class = "card-header", 
                                          h5(icon("chart-line"), "Additional Analysis")
                                      ),
                                      div(class = "card-body",
                                          p("Additional charts and analyses can be added here.")
                                      )
                                  )
                           )
                         )
                         
                  )  # End column(9,...)
                )  # End fluidRow
              )  # End conditionalPanel
    ),  # End Program Overview nav_panel
    
    # ===================================================================
    # ENHANCED MILESTONE ANALYSIS UI - Replace the existing nav_panel in ui.R
    # ===================================================================
    
    nav_panel("Milestone Analysis",
              icon = icon("graduation-cap"),
              
              conditionalPanel(
                condition = "output.data_loaded",
                
                fluidRow(
                  # Controls Sidebar
                  column(3,
                         div(class = "card",
                             div(class = "card-header", h5("Analysis Settings")),
                             div(class = "card-body",
                                 
                                 # Global threshold setting
                                 sliderInput("milestone_threshold", 
                                             "Proficiency Threshold:",
                                             min = 1, max = 9, value = 7, step = 1,
                                             post = " (Proficient)"),
                                 
                                 # Period filter for readiness analyses
                                 selectInput(
                                   "milestone_period",
                                   "Assessment Period:",
                                   choices = list(
                                     "All Periods" = "all",
                                     "Latest Year-End" = "latest_year_end"
                                   ),
                                   selected = "all",
                                   width = "100%"
                                 ),
                                 
                                 # Competency filter  
                                 selectInput("milestone_competency",
                                             "Competency Focus:",
                                             choices = list("All Competencies" = "all"),
                                             selected = "all"),
                                 
                                 hr(),
                                 
                                 # Graduation Analysis Options
                                 h6("Graduation Analysis:", style = "font-weight: bold; color: #2c3e50;"),
                                 checkboxInput("graduation_by_period", 
                                               "Break down by individual periods", 
                                               value = FALSE),
                                 
                                 hr(),
                                 
                                 # All Levels Analysis Options
                                 h6("All Levels Analysis:", style = "font-weight: bold; color: #2c3e50;"),
                                 checkboxGroupInput("all_levels_pgy",
                                                    "Select PGY Levels:",
                                                    choices = NULL,
                                                    selected = NULL),
                                 
                                 hr(),
                                 
                                 # Risk level legend
                                 div(class = "alert alert-info", style = "font-size: 0.85em;",
                                     HTML("<strong>Risk Levels:</strong><br>
                                      🟢 <strong>Excellent:</strong> <2.5% below threshold<br>
                                      🟢 <strong>Good:</strong> 2.5-5% below threshold<br>
                                      🟠 <strong>Concerning:</strong> 5-7.5% below threshold<br>
                                      🔴 <strong>High Risk:</strong> >7.5% below threshold"))
                             )
                         )
                  ),
                  
                  # Main Content Area
                  column(9,
                         
                         # Section 1: Graduation Readiness (Highest PGY Only)
                         fluidRow(
                           column(12,
                                  div(class = "card",
                                      div(class = "card-header", 
                                          h5(icon("graduation-cap"), "Graduation Readiness Analysis"),
                                          p("Analysis of highest PGY level residents only", 
                                            style = "margin: 0; font-size: 0.9em; color: #666;")
                                      ),
                                      div(class = "card-body",
                                          plotlyOutput("graduation_readiness_chart", height = "500px")
                                      )
                                  )
                           )
                         ),
                         
                         br(),
                         
                         # Section 2: All Training Levels Analysis  
                         fluidRow(
                           column(12,
                                  div(class = "card",
                                      div(class = "card-header", 
                                          h5(icon("users"), "All Training Levels Analysis"),
                                          p("Comparative analysis across selected PGY levels", 
                                            style = "margin: 0; font-size: 0.9em; color: #666;")
                                      ),
                                      div(class = "card-body",
                                          plotlyOutput("all_levels_chart", height = "600px")
                                      )
                                  )
                           )
                         ),
                         
                         br(),
                         
                         # Section 3: Sub-Competency Progression Trends - MOVED CONTROLS TO TOP
                         fluidRow(
                           column(12,
                                  div(class = "card",
                                      div(class = "card-header", 
                                          h5(icon("chart-line"), "Sub-Competency Progression Trends"),
                                          p("Individual sub-competency progression vs period averages", 
                                            style = "margin: 0; font-size: 0.9em; color: #666;")
                                      ),
                                      div(class = "card-body",
                                          
                                          # SINGLE sub-competency selector at top
                                          fluidRow(
                                            column(6,
                                                   selectInput("trend_subcompetency",
                                                               "Select Sub-Competency for Trend Analysis:",
                                                               choices = c("Select sub-competency..." = ""),
                                                               selected = "",
                                                               width = "100%")
                                            ),
                                            column(6,
                                                   div(style = "padding-top: 25px; font-size: 0.9em; color: #666;",
                                                       "Comparison line shows average performance for that period across all sub-competencies")
                                            )
                                          ),
                                          
                                          # Period selection and options
                                          fluidRow(
                                            column(6,
                                                   conditionalPanel(
                                                     condition = "input.trend_subcompetency != ''",
                                                     wellPanel(
                                                       style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 15px;",
                                                       uiOutput("trend_period_checkboxes")
                                                     )
                                                   )
                                            ),
                                            column(6,
                                                   conditionalPanel(
                                                     condition = "input.trend_subcompetency != ''",
                                                     checkboxInput(
                                                       "show_total_average",
                                                       "Show Total Average Line",
                                                       value = TRUE
                                                     )
                                                   )
                                            )
                                          ),
                                          
                                          br(),
                                          
                                          # Show message when no sub-competency selected
                                          conditionalPanel(
                                            condition = "input.trend_subcompetency == null || input.trend_subcompetency == ''",
                                            div(class = "alert alert-info", style = "text-align: center; margin-top: 20px;",
                                                icon("info-circle"), 
                                                " Select a sub-competency above to view progression trends.")
                                          ),
                                          
                                          conditionalPanel(
                                            condition = "input.trend_subcompetency != ''",
                                            div(
                                              class = "alert alert-info",
                                              style = "margin-top: 10px; padding: 10px;",
                                              icon("info-circle"), " ",
                                              strong("Trend Analysis: "), 
                                              "Select specific periods above to focus the analysis, or keep all selected for complete progression view. ",
                                              "The comparison line shows average performance for that period across all sub-competencies."
                                            )
                                          ),
                                          
                                          # Show trend chart when sub-competency selected
                                          conditionalPanel(
                                            condition = "input.trend_subcompetency != null && input.trend_subcompetency != ''",
                                            plotlyOutput("subcompetency_trend_chart", height = "500px")
                                          )
                                      )
                                  )
                           )
                         ),
                         
                         br(),
                         
                         # Section 4: Summary Statistics Table
                         fluidRow(
                           column(12,
                                  div(class = "card",
                                      div(class = "card-header", h5(icon("table"), "Detailed Metrics")),
                                      div(class = "card-body",
                                          
                                          # Tabs for different data views
                                          navset_card_tab(
                                            nav_panel("Graduation Readiness", 
                                                      DT::dataTableOutput("graduation_table")),
                                            nav_panel("All Levels", 
                                                      DT::dataTableOutput("all_levels_table")),
                                            nav_panel("Trend Data", 
                                                      conditionalPanel(
                                                        condition = "input.trend_subcompetency != null && input.trend_subcompetency != ''",
                                                        DT::dataTableOutput("trend_data_table")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.trend_subcompetency == null || input.trend_subcompetency == ''",
                                                        div(class = "alert alert-info",
                                                            "Select a sub-competency to view trend data.")
                                                      ))
                                          )
                                      )
                                  )
                           )
                         )
                  )
                )
              ),
              
              # Show message when no data loaded
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