# ============================================================================
# ui.R - UI for gmed-mvp with ACGME Integration
# ============================================================================

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
  
  # ========================================================================
  # HEADER
  # ========================================================================
  div(class = "gmed-header",
      style = "background: linear-gradient(90deg, #2C3E50 0%, #3498DB 100%); color: white; padding: 20px; margin-bottom: 20px;",
      h1("GME Milestone Visualization Platform", style = "margin: 0; font-weight: 300; font-size: 2.5rem;"),
      p("Comprehensive analysis for RDM milestones and ACGME survey data", 
        style = "margin: 5px 0 0 0; opacity: 0.9; font-size: 1.1rem;")
  ),
  
  # ========================================================================
  # MAIN NAVIGATION TABS
  # ========================================================================
  navset_card_tab(
    id = "main_tabs",
    
    # ======================================================================
    # RDM MILESTONES TAB (Your existing functionality)
    # ======================================================================
    nav_panel("RDM Milestones",
              icon = icon("user-graduate"),
              
              # Data loading section
              fluidRow(
                column(6,
                       div(class = "gmed-card",
                           div(class = "gmed-card-header",
                               h4(icon("database"), "RDM Data Connection", class = "gmed-card-title")
                           ),
                           
                           textInput("rdm_token", "RDM Token:", 
                                     placeholder = "Enter your RDM token..."),
                           
                           div(style = "margin-top: 15px;",
                               actionButton("load_rdm_data", "Load RDM Data", 
                                            class = "btn-primary"),
                               uiOutput("rdm_status", inline = TRUE, style = "margin-left: 15px;")
                           )
                       )
                ),
                
                column(6,
                       conditionalPanel(
                         condition = "output.rdm_data_loaded == true",
                         div(class = "alert alert-success",
                             icon("check-circle"), " RDM data loaded successfully!",
                             tags$br(),
                             textOutput("rdm_summary", inline = TRUE)
                         )
                       )
                )
              ),
              
              # RDM Analysis (Only show when data is loaded)
              conditionalPanel(
                condition = "output.rdm_data_loaded == true",
                
                navset_card_tab(
                  nav_panel("Individual Analysis",
                            icon = icon("user"),
                            
                            fluidRow(
                              column(3,
                                     div(class = "gmed-card",
                                         div(class = "gmed-card-header", h5("Resident Selection")),
                                         
                                         selectInput("rdm_resident_select", "Select Resident:",
                                                     choices = NULL),
                                         
                                         selectInput("rdm_period_select", "Select Period:",
                                                     choices = NULL),
                                         
                                         uiOutput("rdm_resident_info")
                                     )
                              ),
                              
                              column(9,
                                     navset_card_tab(
                                       nav_panel("Milestone Dashboard",
                                                 milestone_dashboard_ui("rdm_milestone_dash")
                                       ),
                                       
                                       nav_panel("Assessment Progress",
                                                 assessment_viz_ui("rdm_assessment_viz")
                                       ),
                                       
                                       nav_panel("Plus/Delta Feedback",
                                                 mod_plus_delta_table_ui("rdm_plus_delta")
                                       )
                                     )
                              )
                            )
                  ),
                  
                  nav_panel("Program Overview",
                            icon = icon("chart-bar"),
                            
                            div(class = "gmed-card",
                                div(class = "gmed-card-header", h5("Program-Level Milestone Analysis")),
                                p("Program-level RDM milestone analysis functionality would go here."),
                                p("This could include program spider plots, cohort comparisons, etc.")
                            )
                  )
                )
              )
    ),
    
    # ======================================================================
    # ACGME ANALYSIS TAB (New functionality)
    # ======================================================================
    nav_panel("ACGME Analysis",
              icon = icon("chart-line"),
              
              # Data Upload Section
              div(class = "gmed-card mb-4",
                  div(class = "gmed-card-header",
                      h4(icon("upload"), "ACGME Data Upload", class = "gmed-card-title")
                  ),
                  
                  p("Upload ACGME milestone survey CSV files. The app will automatically detect your specialty structure and adapt all visualizations accordingly.", 
                    class = "text-muted"),
                  
                  fluidRow(
                    column(4,
                           fileInput("acgme_csv_files", "Upload ACGME Milestone CSV Files:",
                                     multiple = TRUE,
                                     accept = c(".csv"),
                                     placeholder = "Select CSV files...")
                    ),
                    column(4,
                           div(style = "margin-top: 25px;",
                               uiOutput("acgme_data_status"),
                               actionButton("process_acgme_data", "Process Data", 
                                            class = "btn-primary", 
                                            style = "margin-left: 10px;")
                           )
                    ),
                    column(4,
                           conditionalPanel(
                             condition = "output.acgme_data_loaded == true",
                             div(class = "alert alert-success", style = "margin-top: 25px;",
                                 icon("check-circle"), " ACGME data loaded!",
                                 tags$br(),
                                 textOutput("acgme_specialty_detected", inline = TRUE)
                             )
                           )
                    )
                  )
              ),
              
              # ACGME Analysis Tabs (Only show when data is loaded)
              conditionalPanel(
                condition = "output.acgme_data_loaded == true",
                
                navset_card_tab(
                  nav_panel("Program Overview",
                            icon = icon("chart-bar"),
                            
                            div(class = "gmed-card",
                                div(class = "gmed-card-header",
                                    h4(icon("building"), "Program-Level Analysis", class = "gmed-card-title")
                                ),
                                
                                mod_acgme_program_overview_ui("acgme_program")
                            )
                  ),
                  
                  nav_panel("Individual Residents", 
                            icon = icon("user-md"),
                            
                            div(class = "gmed-card",
                                div(class = "gmed-card-header",
                                    h4(icon("user-md"), "Individual Resident Analysis", class = "gmed-card-title")
                                ),
                                
                                # Individual analysis interface
                                fluidRow(
                                  column(4,
                                         div(class = "gmed-card",
                                             div(class = "gmed-card-header", h5("Resident Selection")),
                                             
                                             selectInput("acgme_resident_select", "Select Resident:",
                                                         choices = NULL),
                                             
                                             selectInput("acgme_period_select", "Period:",
                                                         choices = c("Total"), selected = "Total"),
                                             
                                             uiOutput("acgme_resident_info")
                                         )
                                  ),
                                  
                                  column(8,
                                         navset_card_tab(
                                           nav_panel("Performance Overview",
                                                     plotlyOutput("acgme_individual_spider", height = "400px"),
                                                     
                                                     fluidRow(
                                                       column(6,
                                                              div(class = "gmed-card mt-2",
                                                                  div(class = "gmed-card-header", h6("Performance Summary")),
                                                                  uiOutput("acgme_performance_summary")
                                                              )
                                                       ),
                                                       column(6,
                                                              div(class = "gmed-card mt-2",
                                                                  div(class = "gmed-card-header", h6("Quick Stats")),
                                                                  tableOutput("acgme_resident_stats")
                                                              )
                                                       )
                                                     )
                                           ),
                                           
                                           nav_panel("Detailed Breakdown",
                                                     DT::dataTableOutput("acgme_individual_details", height = "500px")
                                           ),
                                           
                                           nav_panel("Cohort Comparison",
                                                     plotlyOutput("acgme_cohort_comparison", height = "400px"),
                                                     
                                                     div(class = "gmed-card mt-3",
                                                         div(class = "gmed-card-header", h6("Comparison Insights")),
                                                         uiOutput("acgme_comparison_summary")
                                                     )
                                           )
                                         )
                                  )
                                )
                            )
                  ),
                  
                  nav_panel("Data Structure",
                            icon = icon("sitemap"),
                            
                            fluidRow(
                              column(6,
                                     div(class = "gmed-card",
                                         div(class = "gmed-card-header", h5("🎯 Discovered Competency Structure")),
                                         p("This table shows the competency structure automatically detected from your ACGME data:", 
                                           class = "text-muted"),
                                         DT::dataTableOutput("acgme_competency_structure")
                                     )
                              ),
                              column(6,
                                     div(class = "gmed-card",
                                         div(class = "gmed-card-header", h5("📊 Dataset Summary")),
                                         tableOutput("acgme_dataset_summary")
                                     )
                              )
                            ),
                            
                            fluidRow(
                              column(12,
                                     div(class = "gmed-card mt-3",
                                         div(class = "gmed-card-header", h5("📋 Milestone Details")),
                                         p("Complete mapping of Question Keys to milestone structure:", 
                                           class = "text-muted"),
                                         DT::dataTableOutput("acgme_milestone_details")
                                     )
                              )
                            )
                  )
                )
              ),
              
              # Instructions when no data loaded
              conditionalPanel(
                condition = "output.acgme_data_loaded == false",
                
                div(class = "gmed-card mt-4",
                    div(class = "gmed-card-header", h4("📋 How to Use ACGME Analysis")),
                    
                    fluidRow(
                      column(6,
                             tags$h5("Step-by-Step Instructions:"),
                             tags$ol(
                               tags$li("Upload your ACGME milestone CSV files using the file input above"),
                               tags$li("Click 'Process Data' to analyze the milestone structure"),
                               tags$li("The app will automatically detect your specialty structure:"),
                               tags$ul(
                                 tags$li("Number and types of competencies"),
                                 tags$li("Sub-competencies per competency (varies by specialty)"),
                                 tags$li("Training years (PGY-1, PGY-2, etc.)"),
                                 tags$li("Evaluation periods")
                               ),
                               tags$li("Use the tabs above to explore your data")
                             )
                      ),
                      
                      column(6,
                             tags$h5("Expected CSV Format:"),
                             tags$p("Your ACGME CSV files should contain these columns:"),
                             tags$ul(
                               tags$li(tags$strong("Resident_ID"), " - Unique identifier"),
                               tags$li(tags$strong("First_Name, Last_Name"), " - Resident names"),
                               tags$li(tags$strong("Resident_Year"), " - Training year (1, 2, 3, etc.)"),
                               tags$li(tags$strong("Question_Key"), " - Milestone ID (e.g., 'Comp1_PC_Q3')"),
                               tags$li(tags$strong("Report_Category"), " - Competency name (e.g., 'Patient Care')"),
                               tags$li(tags$strong("Question_Text"), " - Milestone description"),
                               tags$li(tags$strong("Int_Response_Value"), " - Numeric score (1-9 scale)")
                             )
                      )
                    ),
                    
                    div(class = "alert alert-info mt-3",
                        icon("info-circle"), " ",
                        tags$strong("Multi-specialty support: "),
                        "This app automatically adapts to any ACGME specialty including Internal Medicine, ",
                        "General Surgery, Pediatrics, Psychiatry, Family Medicine, and more. ",
                        "Each specialty's unique milestone structure is discovered and visualized appropriately.")
                )
              )
    ),
    
    # ======================================================================
    # HELP & DOCUMENTATION TAB
    # ======================================================================
    nav_panel("Help",
              icon = icon("question-circle"),
              
              div(class = "gmed-card",
                  div(class = "gmed-card-header",
                      h4(icon("book"), "User Guide & Documentation", class = "gmed-card-title")
                  ),
                  
                  navset_card_tab(
                    nav_panel("Getting Started",
                              tags$h4("Welcome to the GME Milestone Visualization Platform"),
                              
                              tags$p("This platform provides comprehensive analysis tools for both RDM milestone data and ACGME survey data."),
                              
                              tags$h5("RDM Milestones"),
                              tags$ul(
                                tags$li("Connect to your RDM database using your API token"),
                                tags$li("Analyze individual resident progress and assessments"),
                                tags$li("View milestone spider plots and progress tracking"),
                                tags$li("Access plus/delta feedback and assessment summaries")
                              ),
                              
                              tags$h5("ACGME Analysis"),
                              tags$ul(
                                tags$li("Upload CSV files from ACGME milestone surveys"),
                                tags$li("Automatic detection of specialty-specific milestone structures"),
                                tags$li("Program-level and individual resident analysis"),
                                tags$li("Interactive visualizations including spider plots and heat maps")
                              )
                    ),
                    
                    nav_panel("Features",
                              tags$h4("Key Features"),
                              
                              tags$h5("🎯 Universal Compatibility"),
                              tags$p("Works with any ACGME specialty - Internal Medicine, Surgery, Pediatrics, etc."),
                              
                              tags$h5("📊 Rich Visualizations"),
                              tags$ul(
                                tags$li("Interactive spider plots for competency analysis"),
                                tags$li("Heat maps showing performance across milestones"),
                                tags$li("Trend analysis and cohort comparisons"),
                                tags$li("Summary dashboards with key insights")
                              ),
                              
                              tags$h5("🔄 Dynamic Analysis"),
                              tags$ul(
                                tags$li("Automatic milestone structure detection"),
                                tags$li("Adaptive visualizations based on your data"),
                                tags$li("Real-time filtering and exploration"),
                                tags$li("Export capabilities for reports")
                              )
                    ),
                    
                    nav_panel("Support",
                              tags$h4("Support & Contact"),
                              tags$p("For technical support or questions about using this platform, please contact the development team."),
                              
                              tags$h5("Troubleshooting"),
                              tags$ul(
                                tags$li("Ensure CSV files are properly formatted ACGME exports"),
                                tags$li("Check that RDM tokens have appropriate permissions"),
                                tags$li("Refresh the page if visualizations don't load properly")
                              )
                    )
                  )
              )
    )
  ),
  
  # ========================================================================
  # FOOTER
  # ========================================================================
  div(class = "gmed-footer mt-5",
      style = "text-align: center; padding: 20px; background: #f8f9fa; border-top: 1px solid #dee2e6;",
      p("GME Milestone Visualization Platform | Built with gmed-mvp infrastructure", 
        style = "margin: 0; color: #6c757d; font-size: 0.9rem;"),
      p("Supports RDM milestone data and universal ACGME survey analysis", 
        style = "margin: 5px 0 0 0; color: #6c757d; font-size: 0.8rem;")
  )
)