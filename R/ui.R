# =============================================================================
# R/ui.R - Complete Enhanced User Interface
# =============================================================================

# =============================================================================
# HELPER FUNCTIONS FOR TOOLTIPS AND UI ENHANCEMENTS
# =============================================================================

#' Create inline tooltip text
#' @param text The text to display
#' @param tooltip_text The tooltip content (can include HTML)
#' @param placement Tooltip placement ("top", "bottom", "left", "right")
#' @return HTML span with tooltip
create_tooltip <- function(text, tooltip_text, placement = "top") {
  tags$span(
    text,
    `data-toggle` = "tooltip",
    `data-placement` = placement,
    `data-html` = "true",
    title = tooltip_text,
    style = "cursor: help; border-bottom: 1px dotted #6c757d; text-decoration: none;"
  )
}

#' Create help icon with tooltip
#' @param tooltip_text The tooltip content (can include HTML)
#' @param placement Tooltip placement
#' @return HTML span with help icon
help_icon <- function(tooltip_text, placement = "right") {
  tags$span(
    icon("question-circle", class = "text-muted help-icon"),
    `data-toggle` = "tooltip",
    `data-placement` = placement,
    `data-html` = "true",
    title = tooltip_text,
    style = "cursor: help; margin-left: 5px; font-size: 0.85em;"
  )
}

#' Create enhanced section header with optional help
#' @param title Section title
#' @param help_text Optional help text
#' @param icon_name Optional icon name
#' @return HTML header with styling
section_header <- function(title, help_text = NULL, icon_name = NULL) {
  header_content <- tagList(
    if (!is.null(icon_name)) icon(icon_name, class = "me-2"),
    title,
    if (!is.null(help_text)) help_icon(help_text)
  )
  
  tags$h6(header_content, class = "section-title")
}

# =============================================================================
# MAIN UI DEFINITION
# =============================================================================

ui <- fluidPage(
  # PURE JAVASCRIPT STICKY SOLUTION - Replace your tags$head() with this:
  
  tags$head(
    tags$title("GME Milestone Platform"),
    
    # Minimal CSS - remove all sticky positioning attempts
    tags$style(HTML("
    .main-container {
      max-width: 1400px;
      margin: 0 auto;
    }
    
    /* Normal sidebar styling - NO sticky positioning */
    .js-sticky-sidebar {
      background: #f8f9fa;
      border: 1px solid #dee2e6;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      z-index: 1000;
      max-height: calc(100vh - 40px);
      overflow-y: auto;
    }
    
    /* When JavaScript makes it fixed */
    .js-sticky-sidebar.is-fixed {
      position: fixed !important;
      top: 20px !important;
    }
    
    .content-card {
      border-radius: 8px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      margin-bottom: 1.5rem;
      min-height: 500px; /* Ensure scrollable content */
      padding: 20px;
      background: white;
    }
    
    .section-title {
      font-size: 1.1rem;
      font-weight: 600;
      color: #2C3E50;
      margin-bottom: 0.75rem;
      padding-bottom: 0.5rem;
      border-bottom: 2px solid #dee2e6;
    }
    
    .checkbox-group {
      max-height: 200px;
      overflow-y: auto;
      padding: 0.5rem;
      background: white;
      border: 1px solid #dee2e6;
      border-radius: 6px;
    }
    
    .advanced-controls {
      display: none;
      margin-top: 15px;
      padding: 15px;
      background: rgba(0,0,0,0.02);
      border-radius: 5px;
    }
  ")),
    
    # REFINED STICKY SIDEBAR - Replace your JavaScript section with this:
    
    tags$script(HTML("
  $(document).ready(function(){
    console.log('Refined sticky sidebar implementation');
    
    var stickyElements = [];
    
    function initScrollSticky() {
      // Clear previous sticky elements
      stickyElements = [];
      
      $('.js-sticky-sidebar').each(function() {
        var $sidebar = $(this);
        var $container = $sidebar.closest('.row');
        var $column = $sidebar.parent();
        
        // Store initial values - measure when sidebar is in natural position
        var stickyData = {
          sidebar: $sidebar,
          container: $container,
          column: $column,
          initialTop: null, // Will be calculated
          initialWidth: $sidebar.outerWidth(),
          isFixed: false,
          naturalOffsetTop: null
        };
        
        // Ensure sidebar is in natural position for measurement
        $sidebar.css({
          'position': 'static',
          'top': 'auto',
          'width': 'auto'
        }).removeClass('is-fixed');
        
        // Measure natural position after a brief delay
        setTimeout(function() {
          stickyData.naturalOffsetTop = $sidebar.offset().top;
          stickyData.initialTop = stickyData.naturalOffsetTop;
          console.log('Measured natural top position:', stickyData.naturalOffsetTop);
        }, 50);
        
        stickyElements.push(stickyData);
      });
      
      // Set up scroll handler after measurements
      setTimeout(setupScrollHandler, 100);
    }
    
    function setupScrollHandler() {
      var $window = $(window);
      var stickyOffset = 20;
      var releaseThreshold = 10; // Pixels from top to release sidebar
      
      // Remove previous scroll handlers
      $window.off('scroll.customSticky resize.customSticky');
      
      function updateStickyPositions() {
        var scrollTop = $window.scrollTop();
        
        stickyElements.forEach(function(sticky, index) {
          if (!sticky.naturalOffsetTop) return; // Skip if not measured yet
          
          var triggerPoint = sticky.naturalOffsetTop - stickyOffset;
          var shouldBeFixed = scrollTop > triggerPoint;
          var containerBottom = sticky.container.offset().top + sticky.container.outerHeight();
          var sidebarHeight = sticky.sidebar.outerHeight();
          var fitsInViewport = sidebarHeight + stickyOffset * 2 < $window.height();
          var withinContainer = scrollTop + sidebarHeight + stickyOffset < containerBottom;
          
          // Release if we're very close to the top
          var nearTop = scrollTop <= releaseThreshold;
          
          if (shouldBeFixed && fitsInViewport && withinContainer && !nearTop) {
            // Make it fixed
            if (!sticky.isFixed) {
              console.log('Making sidebar', index, 'fixed at scroll:', scrollTop);
              sticky.sidebar.css({
                'position': 'fixed',
                'top': stickyOffset + 'px',
                'width': sticky.column.width() + 'px',
                'z-index': '1000'
              }).addClass('is-fixed');
              sticky.isFixed = true;
            }
          } else {
            // Make it static
            if (sticky.isFixed) {
              console.log('Making sidebar', index, 'static at scroll:', scrollTop);
              sticky.sidebar.css({
                'position': 'static',
                'top': 'auto',
                'width': 'auto',
                'z-index': 'auto'
              }).removeClass('is-fixed');
              sticky.isFixed = false;
            }
          }
        });
      }
      
      // Smooth scroll handler with requestAnimationFrame for better performance
      var isScrolling = false;
      function smoothScrollHandler() {
        if (!isScrolling) {
          requestAnimationFrame(function() {
            updateStickyPositions();
            isScrolling = false;
          });
          isScrolling = true;
        }
      }
      
      $window.on('scroll.customSticky', smoothScrollHandler);
      
      // Handle window resize
      $window.on('resize.customSticky', function() {
        // Reset all to static first
        stickyElements.forEach(function(sticky) {
          sticky.sidebar.css({
            'position': 'static',
            'top': 'auto',
            'width': 'auto',
            'z-index': 'auto'
          }).removeClass('is-fixed');
          sticky.isFixed = false;
        });
        
        setTimeout(function() {
          // Recalculate positions
          stickyElements.forEach(function(sticky) {
            sticky.naturalOffsetTop = sticky.sidebar.offset().top;
            sticky.initialTop = sticky.naturalOffsetTop;
            sticky.initialWidth = sticky.sidebar.outerWidth();
          });
          updateStickyPositions();
        }, 100);
      });
      
      // Initial position check
      setTimeout(updateStickyPositions, 50);
    }
    
    // Initialize tooltips
    function initTooltips() {
      $('[data-toggle=\"tooltip\"]').tooltip({
        html: true,
        delay: { show: 300, hide: 100 },
        container: 'body'
      });
    }
    
    // Progressive disclosure
    $('.show-advanced').on('click', function(e){
      e.preventDefault();
      $(this).closest('.card-body').find('.advanced-controls').slideToggle(300);
      var icon = $(this).find('i');
      if (icon.hasClass('fa-chevron-down')) {
        icon.removeClass('fa-chevron-down').addClass('fa-chevron-up');
      } else {
        icon.removeClass('fa-chevron-up').addClass('fa-chevron-down');
      }
    });
    
    // Navigation helpers
    $('#goto_overview').on('click', function(){
      $('a[data-value=\"Program Overview\"]').click();
      setTimeout(initScrollSticky, 500);
    });
    
    $('#goto_analysis').on('click', function(){
      $('a[data-value=\"Milestone Analysis\"]').click();
      setTimeout(initScrollSticky, 500);
    });
    
    // Initialize everything
    initTooltips();
    initScrollSticky();
    
    // Reinitialize on tab changes
    $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function() {
      console.log('Tab changed - reinitializing sticky');
      setTimeout(initScrollSticky, 300);
    });
    
    // Reinitialize on Shiny updates
    $(document).on('shiny:value shiny:outputinvalidated', function() {
      setTimeout(function() {
        initTooltips();
        initScrollSticky();
      }, 200);
    });
    
    console.log('Refined sticky setup complete');
  });
"))
  ),
  # Main container
  div(class = "main-container",
      
      # Enhanced header with dynamic content
      div(class = "gmed-header",
          style = "background: linear-gradient(90deg, #2C3E50 0%, #3498DB 100%); 
                   color: white; padding: 30px 0; margin-bottom: 30px; text-align: center;
                   box-shadow: 0 4px 12px rgba(0,0,0,0.15);",
          h1("GME Milestone Platform", 
             style = "margin: 0; font-weight: 300; font-size: 2.2rem;"),
          uiOutput("program_subtitle")
      ),
      
      # Progress indicator (hidden by default)
      div(id = "global-progress", class = "progress mb-3", 
          style = "height: 3px; display: none;",
          div(class = "progress-bar progress-bar-striped progress-bar-animated bg-primary", 
              style = "width: 0%", role = "progressbar")
      ),
      
      # Enhanced navigation
      navset_card_tab(
        id = "main_tabs",
        
        # ===================================================================
        # GET STARTED TAB - ENHANCED
        # ===================================================================
        # ===================================================================
        # GET STARTED TAB - CLEANED UP VERSION
        # ===================================================================
        nav_panel("Get Started",
                  icon = icon("upload"),
                  
                  # Simple, clean tab description
                  fluidRow(
                    column(12,
                           div(class = "alert alert-info mb-4",
                               h6("Get Started", style = "margin-bottom: 10px; color: #2c3e50;"),
                               p("Welcome to the GME Milestone Visualization Platform. This tool helps residency programs analyze ACGME Milestone 2.0 evaluation data to identify trends, track resident progression, and support educational outcomes. Upload your WebADS milestone CSV files or try our demo data to explore the platform's capabilities.",
                                 style = "margin-bottom: 0; font-size: 0.95em;")
                           )
                    )
                  ),
                  
                  # Clean, simple quick start section
                  fluidRow(
                    column(8, offset = 2,
                           div(class = "content-card card",
                               div(class = "card-body text-center",
                                   h3("Welcome to GME Milestone Platform"),
                                   p("Analyze your ACGME Milestone evaluations with powerful visualizations and insights.", 
                                     class = "lead"),
                                   
                                   hr(),
                                   
                                   # Two main options - simplified
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
                  
                  # Clean data summary section
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
                  
                  # Simple collapsible help section
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
        # PROGRAM OVERVIEW TAB - ENHANCED
        # ===================================================================
        nav_panel("Program Overview",
                  icon = icon("chart-line"),
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    # Enhanced tab description
                    fluidRow(
                      column(12,
                             div(class = "alert alert-info mb-4",
                                 section_header("Program Overview", 
                                                "Comprehensive analysis of program-wide milestone performance",
                                                "chart-line"),
                                 p(HTML(paste("Get a comprehensive view of your program's", 
                                              create_tooltip("milestone performance", 
                                                             "Analysis based on ACGME milestone scores<br/>across all residents and competency areas"),
                                              "across all residents and competencies. This dashboard identifies",
                                              create_tooltip("areas of strength and improvement", 
                                                             "Statistical analysis identifying highest and lowest<br/>performing sub-competencies"),
                                              "while providing visual insights into overall program trends. Use the controls to filter by specific time periods and training levels for targeted analysis.")),
                                   style = "margin-bottom: 0; font-size: 0.95em;")
                             )
                      )
                    ),
                    
                    fluidRow(
                      # Enhanced sidebar with progressive disclosure
                      column(3,
                             div(class = "sidebar-card card js-sticky-sidebar",
                                 div(class = "card-header", 
                                     h6(icon("sliders-h"), "Analysis Settings",
                                        help_icon("Customize your program analysis"))),
                                 div(class = "card-body",
                                     
                                     # Basic Controls
                                     div(class = "basic-controls",
                                         # Enhanced period selection with tooltips
                                         div(class = "form-group",
                                             tags$label(class = "form-label",
                                                        create_tooltip("Assessment Period", 
                                                                       "Choose which evaluation timeframe to analyze<br/><strong>Latest End-Year:</strong> Most recent year-end evaluations<br/><strong>Latest Mid-Year:</strong> Most recent mid-year evaluations"),
                                                        help_icon("Select evaluation timeframe for analysis")),
                                             radioButtons("period_selection", "",
                                                          choices = list(
                                                            "Latest End-Year" = "recent_end",
                                                            "Latest Mid-Year" = "recent_mid",
                                                            "Specific Period" = "specific",
                                                            "All Periods" = "all_periods"
                                                          ),
                                                          selected = "recent_end")
                                         ),
                                         
                                         conditionalPanel(
                                           condition = "input.period_selection == 'specific'",
                                           div(class = "form-group",
                                               selectInput("specific_period", 
                                                           label = tagList("Choose Period:",
                                                                           help_icon("Select specific evaluation period")),
                                                           choices = NULL)
                                           )
                                         ),
                                         
                                         # Enhanced PGY selection
                                         div(class = "form-group",
                                             tags$label(class = "form-label",
                                                        create_tooltip("Training Levels", 
                                                                       "Select which PGY (Post-Graduate Year) levels to include<br/>in your analysis"),
                                                        help_icon("Choose PGY levels to analyze")),
                                             checkboxInput("select_all_pgy", 
                                                           "Select All Training Levels", 
                                                           value = TRUE),
                                             div(class = "checkbox-group mt-2",
                                                 uiOutput("pgy_checkboxes")
                                             )
                                         )
                                     ),
                                     
                                     hr(),
                                     
                                     # Progressive disclosure for advanced options
                                     tags$button(class = "btn btn-link btn-sm show-advanced w-100 text-left", 
                                                 style = "padding: 5px 0; text-decoration: none;",
                                                 icon("cog"), " ",
                                                 span("Advanced Options", class = "button-text"),
                                                 icon("chevron-down", class = "float-right")
                                     ),
                                     
                                     div(class = "advanced-controls mt-3",
                                         # Display options
                                         div(class = "form-group",
                                             tags$label(class = "form-label",
                                                        "Display Options",
                                                        help_icon("Customize chart and table displays")),
                                             div(
                                               checkboxInput("show_program_means", 
                                                             HTML(paste("Show Program Averages",
                                                                        help_icon("Display average scores across all residents"))), 
                                                             value = TRUE),
                                               checkboxInput("tables_use_filters", 
                                                             HTML(paste("Apply Filters to Tables",
                                                                        help_icon("Use selected period/level filters for strength/improvement tables"))), 
                                                             value = TRUE)
                                             )
                                         ),
                                         
                                         # Multi-period comparison
                                         conditionalPanel(
                                           condition = "input.period_selection == 'specific'",
                                           div(class = "form-group",
                                               checkboxInput("spider_multi_period", 
                                                             HTML(paste("Compare Multiple Periods",
                                                                        help_icon("Select multiple periods for side-by-side comparison"))), 
                                                             value = FALSE),
                                               conditionalPanel(
                                                 condition = "input.spider_multi_period == true",
                                                 div(class = "period-selection mt-2",
                                                     uiOutput("spider_period_checkboxes")
                                                 )
                                               )
                                           )
                                         )
                                     )
                                 )
                             )
                      ),
                      
                      # Enhanced main content with loading indicators
                      column(9,
                             # Current analysis info with enhanced display
                             div(class = "alert alert-light mb-3",
                                 style = "border-left: 4px solid #007bff;",
                                 uiOutput("period_display_info")
                             ),
                             
                             # Performance insights - CHANGED: Now stacked instead of side-by-side
                             fluidRow(
                               column(9, offset = 2,  # Centers the content and reduces width
                                      div(class = "content-card card",
                                          div(class = "card-header", 
                                              h6(icon("exclamation-triangle", style = "color: #fd7e14;"), 
                                                 " Areas for Improvement",
                                                 help_icon("Sub-competencies with lowest average scores<br/>requiring focused attention and development"))
                                          ),
                                          div(class = "card-body compact-table-body",
                                              uiOutput("improvement_description"),
                                              DT::dataTableOutput("improvement_areas")
                                          )
                                      )
                               )
                             ),
                             
                             fluidRow(
                               column(9, offset = 2,  # Centers the content and reduces width
                                      div(class = "content-card card",
                                          div(class = "card-header", 
                                              h6(icon("star", style = "color: #28a745;"), 
                                                 " Areas of Strength",
                                                 help_icon("Sub-competencies with highest average scores<br/>representing program excellence"))
                                          ),
                                          div(class = "card-body compact-table-body",
                                              uiOutput("strength_description"),
                                              DT::dataTableOutput("strength_areas")
                                          )
                                      )
                               )
                             ),
                          
                             
                             br(),
                             
                             # Enhanced spider plot - now takes up more space without the milestone reference table
                             div(class = "content-card card",
                                 div(class = "card-header", 
                                     h6(icon("chart-area"), 
                                        "Program Performance Overview",
                                        help_icon("Radar chart showing mean scores across all sub-competencies<br/>Higher values (toward edge) indicate better performance<br/>Each point represents the average milestone score for that competency"))
                                 ),
                                 div(class = "card-body position-relative",
                                     plotlyOutput("program_spider", height = "700px")  # Increased height since milestone table is removed
                                 )
                             )
                             
                             # REMOVED: Milestone reference table section
                             # This was redundant as mentioned - users can find this in the Data Overview tab
                      )
                    )
                  ),
                  
                  # Enhanced no-data message
                  conditionalPanel(
                    condition = "!output.data_loaded",
                    div(class = "alert alert-info text-center",
                        style = "margin: 50px 0; padding: 40px;",
                        icon("info-circle", class = "fa-2x mb-3"),
                        h5("Load Your Data First"),
                        p("Please upload milestone data or try the demo from the Get Started tab to begin analysis."),
                        actionButton("goto_upload", 
                                     HTML('<i class="fas fa-upload"></i> Go to Get Started'),
                                     class = "btn btn-primary")
                    )
                  )
        ),
        
        # ===================================================================
        # MILESTONE ANALYSIS TAB - ENHANCED
        # ===================================================================
        nav_panel("Milestone Analysis",
                  icon = icon("graduation-cap"),
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    # Enhanced tab description
                    fluidRow(
                      column(12,
                             div(class = "alert alert-info mb-4",
                                 section_header("Milestone Analysis", 
                                                "Deep dive into readiness analysis and progression tracking",
                                                "graduation-cap"),
                                 p(HTML(paste("Dive deeper into milestone performance with",
                                              create_tooltip("readiness analysis", 
                                                             "Graduation readiness based on percentage of evaluations<br/>below your specified proficiency threshold"),
                                              ", training level comparisons, and progression tracking. This section helps identify residents who may need additional support and tracks how competencies develop throughout training. Set your",
                                              create_tooltip("proficiency threshold", 
                                                             "Minimum score considered 'proficient' - typically 7<br/>Residents scoring below this may need additional support"),
                                              "and filters to customize the analysis.")),
                                   style = "margin-bottom: 0; font-size: 0.95em;")
                             )
                      )
                    ),
                    
                    fluidRow(
                      # Enhanced sidebar
                      column(3,
                             div(class = "sidebar-card card js-sticky-sidebar",
                                 div(class = "card-header", 
                                     h6(icon("sliders-h"), "Analysis Settings",
                                        help_icon("Configure milestone analysis parameters"))),
                                 div(class = "card-body",
                                     
                                     # Risk level legend
                                     div(class = "alert alert-warning mb-3", 
                                         style = "font-size: 0.85em; padding: 12px;",
                                         HTML(paste0(
                                           "<strong>", icon("shield-alt"), " Risk Levels:</strong><br>",
                                           "🟢 <strong>Excellent:</strong> <2.5% below threshold<br>",
                                           "🟢 <strong>Good:</strong> 2.5-5% below threshold<br>",
                                           "🟠 <strong>Concerning:</strong> 5-7.5% below threshold<br>",
                                           "🔴 <strong>High Risk:</strong> >7.5% below threshold"
                                         ))
                                     ),
                                     
                                     # Threshold setting with enhanced guidance
                                     div(class = "form-group",
                                         tags$label(class = "form-label",
                                                    create_tooltip("Proficiency Threshold", 
                                                                   "Minimum score considered 'proficient'<br/>Typically 7 for most programs<br/>Residents scoring below this may need additional support"),
                                                    help_icon("Set minimum proficiency level")),
                                         sliderInput("milestone_threshold", "",
                                                     min = 1, max = 9, value = 7, step = 1,
                                                     ticks = TRUE),
                                         p("7 = Proficient level (recommended)", 
                                           style = "font-size: 0.8em; color: #6c757d; margin-top: 5px;")
                                     ),
                                     
                                     # Period and competency filters
                                     div(class = "form-group",
                                         tags$label(class = "form-label", 
                                                    "Analysis Filters",
                                                    help_icon("Narrow analysis to specific periods or competencies")),
                                         selectInput("milestone_period", 
                                                     label = tagList("Period:",
                                                                     help_icon("Choose evaluation periods to analyze")),
                                                     choices = list("All Periods" = "all"),
                                                     selected = "all"),
                                         
                                         selectInput("milestone_competency", 
                                                     label = tagList("Competency:",
                                                                     help_icon("Focus on specific competency domain")),
                                                     choices = list("All Competencies" = "all"),
                                                     selected = "all")
                                     ),
                                     
                                     # PGY selection for all levels analysis
                                     div(class = "form-group",
                                         tags$label(class = "form-label",
                                                    "Training Levels for All Levels Analysis",
                                                    help_icon("Select PGY levels to include in multi-level comparison")),
                                         div(class = "checkbox-group",
                                             checkboxGroupInput("all_levels_pgy", "",
                                                                choices = NULL)
                                         )
                                     ),
                                     
                                     hr(),
                                     
                                     # Progressive disclosure for advanced options
                                     tags$button(class = "btn btn-link btn-sm show-advanced w-100 text-left", 
                                                 style = "padding: 5px 0; text-decoration: none;",
                                                 icon("cog"), " ",
                                                 span("Advanced Options", class = "button-text"),
                                                 icon("chevron-down", class = "float-right")
                                     ),
                                     
                                     div(class = "advanced-controls mt-3",
                                         # Graduation analysis options
                                         div(class = "form-group",
                                             tags$label(class = "form-label",
                                                        "Graduation Analysis Options",
                                                        help_icon("Customize graduation readiness analysis")),
                                             checkboxInput("graduation_by_period", 
                                                           HTML(paste("Break Down by Period",
                                                                      help_icon("Show readiness by evaluation period rather than overall"))), 
                                                           value = FALSE)
                                         )
                                     )
                                 )
                             )
                      ),
                      
                      # Enhanced main content
                      column(9,
                             # Enhanced graduation readiness section
                             div(class = "content-card card",
                                 div(class = "card-header", 
                                     h6(icon("graduation-cap"), 
                                        "Graduation Readiness Analysis",
                                        help_icon("Shows percentage of evaluations below proficiency threshold<br/>for graduating residents (highest PGY level)<br/>Lower percentages indicate better readiness"))
                                 ),
                                 div(class = "card-body position-relative",
                                     plotlyOutput("graduation_readiness_chart", height = "500px")
                                 )
                             ),
                             
                             # Enhanced all levels analysis
                             div(class = "content-card card",
                                 div(class = "card-header", 
                                     h6(icon("users"), 
                                        "All Training Levels Analysis",
                                        help_icon("Compare readiness across different PGY levels<br/>Shows how performance varies by training stage<br/>Expected pattern: lower PGY levels have higher percentages below threshold"))
                                 ),
                                 div(class = "card-body position-relative",
                                     plotlyOutput("all_levels_chart", height = "600px")
                                 )
                             ),
                             
                             # Enhanced trend analysis section
                             div(class = "content-card card",
                                 div(class = "card-header", 
                                     h6(icon("line-chart"), 
                                        "Sub-Competency Progression Analysis",
                                        help_icon("Track how specific sub-competencies develop over time<br/>Compare individual sub-competency performance to program averages<br/>Add graduation classes to see cohort-specific trends"))
                                 ),
                                 div(class = "card-body",
                                     fluidRow(
                                       column(4,
                                              div(class = "controls-panel",
                                                  # Sub-competency selection
                                                  div(class = "form-group",
                                                      selectInput("trend_subcompetency", 
                                                                  label = tagList("Select Sub-Competency:",
                                                                                  help_icon("Choose specific milestone to analyze")),
                                                                  choices = NULL)
                                                  ),
                                                  
                                                  # Cohort selection with enhanced guidance
                                                  div(class = "form-group mt-4",
                                                      tags$label(class = "form-label",
                                                                 "Add Graduation Classes:",
                                                                 help_icon("Compare performance across different graduating classes<br/>Shows how different cohorts performed on this milestone")),
                                                      div(class = "alert alert-info mb-2",
                                                          style = "font-size: 0.85em; padding: 8px;",
                                                          icon("lightbulb"), " Optional: Select graduating classes to overlay their progression"
                                                      ),
                                                      checkboxGroupInput("selected_cohorts", "",
                                                                         choices = NULL),
                                                      
                                                      div(class = "btn-group btn-group-sm mt-2",
                                                          actionButton("select_recent_cohorts", 
                                                                       HTML('<i class="fas fa-clock"></i> Recent 2'), 
                                                                       class = "btn-outline-secondary btn-sm"),
                                                          actionButton("clear_cohorts", 
                                                                       HTML('<i class="fas fa-eraser"></i> Clear'), 
                                                                       class = "btn-outline-secondary btn-sm")
                                                      )
                                                  )
                                              )
                                       ),
                                       column(8,
                                              plotlyOutput("cohort_trend_plot", height = "450px")
                                       )
                                     )
                                 )
                             ),
                             
                             # Enhanced summary tables section
                             div(class = "content-card card",
                                 div(class = "card-header", 
                                     h6(icon("table"), 
                                        "Detailed Performance Metrics",
                                        help_icon("Comprehensive tables showing detailed statistics<br/>for graduation readiness and all training levels"))
                                 ),
                                 div(class = "card-body",
                                     navset_card_tab(
                                       nav_panel("Graduation Readiness", 
                                                 div(class = "table-info mb-3",
                                                     p(HTML(paste("Detailed metrics for", 
                                                                  create_tooltip("graduation readiness", 
                                                                                 "Based on highest PGY level residents<br/>showing percentage below proficiency threshold"),
                                                                  "analysis. Lower percentages indicate better readiness for graduation.")),
                                                       class = "text-muted small")),
                                                 DT::dataTableOutput("graduation_table")
                                       ),
                                       nav_panel("All Training Levels", 
                                                 div(class = "table-info mb-3",
                                                     p(HTML(paste("Performance breakdown across", 
                                                                  create_tooltip("all PGY levels", 
                                                                                 "Shows how readiness varies by training stage<br/>Expected: higher PGY levels perform better"),
                                                                  "showing progression patterns throughout training.")),
                                                       class = "text-muted small")),
                                                 DT::dataTableOutput("all_levels_table")
                                       )
                                     )
                                 )
                             )
                      )
                    )
                  ),
                  
                  # Enhanced no-data message
                  conditionalPanel(
                    condition = "!output.data_loaded",
                    div(class = "alert alert-info text-center",
                        style = "margin: 50px 0; padding: 40px;",
                        icon("info-circle", class = "fa-2x mb-3"),
                        h5("Load Your Data First"),
                        p("Please upload milestone data or try the demo from the Get Started tab to begin milestone analysis."),
                        actionButton("goto_upload", 
                                     HTML('<i class="fas fa-upload"></i> Go to Get Started'),
                                     class = "btn btn-primary")
                    )
                  )
        ),
        
        # ===================================================================
        # INDIVIDUAL ASSESSMENT TAB - ENHANCED
        # ===================================================================
        create_individual_assessment_ui(),
        
        # ===================================================================
        # DATA OVERVIEW TAB - ENHANCED
        # ===================================================================
        nav_panel("Index",
                  icon = icon("table"),
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    # Enhanced tab description
                    fluidRow(
                      column(12,
                             div(class = "alert alert-info mb-4",
                                 section_header("Index", 
                                                "Review data structure and milestone definitions",
                                                "table"),
                                 p(HTML(paste("Review your uploaded milestone data structure and definitions. This section displays the", 
                                              create_tooltip("milestone framework", 
                                                             "The competency structure and sub-competency codes<br/>extracted from your evaluation data"),
                                              "extracted from your data and provides a sample of the processed evaluation records for verification. Use this to confirm your data was loaded correctly.")),
                                   style = "margin-bottom: 0; font-size: 0.95em;")
                             )
                      )
                    ),
                    
                    fluidRow(
                      column(12,
                             # Enhanced milestone definitions section
                             div(class = "content-card card",
                                 div(class = "card-header", 
                                     h6(icon("list-alt"), 
                                        "Milestone Definitions and Structure",
                                        help_icon("All milestone codes and descriptions found in your data<br/>organized by competency domain"))
                                 ),
                                 div(class = "card-body",
                                     div(class = "alert alert-light mb-3",
                                         style = "border-left: 4px solid #17a2b8;",
                                         icon("info-circle"), " ",
                                         "This table shows all milestone sub-competencies extracted from your evaluation data, organized by competency domain."
                                     ),
                                     DT::dataTableOutput("milestone_table")
                                 )
                             ),
                             
                             # Enhanced evaluation sample section
                             div(class = "content-card card",
                                 div(class = "card-header", 
                                     h6(icon("eye"), 
                                        "Sample Evaluation Records",
                                        help_icon("Preview of processed evaluation data<br/>showing resident names, periods, and milestone scores<br/>limited to first 100 records for review"))
                                 ),
                                 div(class = "card-body",
                                     div(class = "alert alert-light mb-3",
                                         style = "border-left: 4px solid #28a745;",
                                         icon("check-circle"), " ",
                                         "Sample of your processed evaluation data. This confirms proper data loading and shows the structure used for analysis."
                                     ),
                                     DT::dataTableOutput("evaluation_sample")
                                 )
                             ),
                             
                             # Data quality summary
                             div(class = "content-card card",
                                 div(class = "card-header", 
                                     h6(icon("chart-bar"), 
                                        "Data Quality Summary",
                                        help_icon("Overview of data completeness and structure<br/>helping identify potential data issues"))
                                 ),
                                 div(class = "card-body",
                                     fluidRow(
                                       column(6,
                                              h6("Data Structure:", class = "text-muted"),
                                              htmlOutput("data_structure_summary")
                                       ),
                                       column(6,
                                              h6("Coverage Analysis:", class = "text-muted"),
                                              htmlOutput("data_coverage_summary")
                                       )
                                     )
                                 )
                             )
                      )
                    )
                  ),
                  
                  # Enhanced no-data message
                  conditionalPanel(
                    condition = "!output.data_loaded",
                    div(class = "alert alert-info text-center",
                        style = "margin: 50px 0; padding: 40px;",
                        icon("info-circle", class = "fa-2x mb-3"),
                        h5("Load Your Data First"),
                        p("Please upload milestone data or try the demo from the Get Started tab to view data structure."),
                        actionButton("goto_upload", 
                                     HTML('<i class="fas fa-upload"></i> Go to Get Started'),
                                     class = "btn btn-primary")
                    )
                  )
        )
      ),
      
      # Enhanced footer with additional information
      div(class = "footer mt-5 py-4 bg-light border-top",
          div(class = "container-fluid",
              fluidRow(
                column(4,
                       h6("GME Milestone Platform", class = "text-muted"),
                       p("Advanced analytics for ACGME Milestone 2.0 evaluations", 
                         class = "small text-muted")
                ),
                column(4, class = "text-center",
                       p(class = "small text-muted",
                         "Built for Graduate Medical Education programs",
                         br(),
                         "Supporting competency-based assessment and improvement")
                ),
                column(4, class = "text-right",
                       p(class = "small text-muted",
                         "Version 2.0 | ",
                         tags$a("Documentation", href = "#", class = "text-muted"),
                         " | ",
                         tags$a("Support", href = "#", class = "text-muted"))
                )
              )
          )
      )
  )
)