# =============================================================================
# R/server.R - Complete Enhanced Server Logic with UX Improvements
# =============================================================================

server <- function(input, output, session) {
  
  # PRODUCTION MODE SETTING (set to FALSE for development debugging)
  PRODUCTION_MODE <- TRUE
  
  # Store loaded data with enhanced state tracking
  values <- reactiveValues(
    uploaded_data = NULL,
    data_loaded = FALSE,
    demo_mode = FALSE,
    program_name = NULL,
    processing = FALSE,        # NEW: Processing state tracker
    current_operation = ""     # NEW: Current operation description
  )
  
  # Enhanced milestone_data reactive
  milestone_data <- reactiveVal(NULL)
  
  # ==========================================================================
  # ENHANCED DATA LOADED INDICATOR
  # ==========================================================================
  output$data_loaded <- reactive({
    !is.null(milestone_data())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # ==========================================================================
  # ENHANCED PROGRAM SUBTITLE WITH LOADING STATES
  # ==========================================================================
  output$program_subtitle <- renderUI({
    data <- milestone_data()
    
    if (values$processing) {
      # Show current operation during processing
      p(HTML(paste0('<i class="fas fa-spinner fa-spin text-primary"></i> ',
                    values$current_operation)), 
        style = "margin: 5px 0 0 0; opacity: 0.8; font-size: 1.1rem; color: #007bff;")
    } else if (is.null(data)) {
      p("Upload CSV data to begin", 
        style = "margin: 5px 0 0 0; opacity: 0.9; font-size: 1.1rem;")
    } else {
      p(paste(data$program_info$specialty_name, "-", data$program_info$program_name), 
        style = "margin: 5px 0 0 0; opacity: 0.9; font-size: 1.1rem;")
    }
  })
  
  # ==========================================================================
  # ENHANCED CSV PROCESSING WITH COMPREHENSIVE LOADING STATES
  # ==========================================================================
  observeEvent(input$process_csv, {
    req(input$csv_files)
    
    # Start loading state
    values$processing <- TRUE
    values$current_operation <- "Initializing data processing"
    
    # Show initial progress notification
    showNotification(
      HTML('<i class="fas fa-cogs"></i> <strong>Starting Processing...</strong><br>Reading your CSV files'),
      type = "message", 
      duration = 3
    )
    
    tryCatch({
      # Update loading message
      values$current_operation <- "Reading and validating CSV files"
      
      # Load the data using existing function
      data <- load_milestone_csv_data(input$csv_files$datapath)
      
      # Update progress
      values$current_operation <- "Processing milestone evaluations"
      milestone_data(data)
      
      # Update interface controls
      values$current_operation <- "Updating interface controls"
      
      # Update specific period choices
      periods <- sort(unique(data$evaluations$Period))
      updateSelectInput(session, "specific_period", 
                        choices = setNames(periods, periods),
                        selected = periods[length(periods)])
      
      # Update milestone period choices with enhanced options
      milestone_period_choices <- get_all_period_choices(data)
      updateSelectInput(session, "milestone_period", 
                        choices = milestone_period_choices,
                        selected = "all")
      
      # Update competency choices for other components
      competencies <- unique(data$evaluations$Competency)
      competency_choices <- c("All Competencies" = "all")
      if (length(competencies) > 0) {
        competency_choices <- c(competency_choices, setNames(competencies, competencies))
      }
      updateSelectInput(session, "trend_competency", choices = competency_choices)
      
      # Complete processing
      values$processing <- FALSE
      values$current_operation <- ""
      
      # Enhanced success notification with statistics
      showNotification(
        HTML(paste0(
          '<i class="fas fa-check-circle"></i> <strong>Processing Complete!</strong><br>',
          '📊 Loaded ', format(data$summary$n_evaluations, big.mark = ","), ' evaluations<br>',
          '👥 ', data$summary$n_residents, ' residents across ', 
          length(data$summary$training_levels), ' training levels<br>',
          '🎯 ', length(unique(data$evaluations$Sub_Competency)), ' sub-competencies analyzed'
        )), 
        type = "message", 
        duration = 8
      )
      
    }, error = function(e) {
      # Reset processing state
      values$processing <- FALSE
      values$current_operation <- ""
      
      # Enhanced error notification with guidance
      error_details <- if (grepl("No valid data", e$message)) {
        "Please check that your files are exported from ACGME WebADS in CSV format."
      } else if (grepl("Question.Key", e$message)) {
        "The CSV files don't appear to contain milestone evaluation data. Please verify the file format."
      } else {
        "There was an issue processing your files. Please try again or contact support."
      }
      
      showNotification(
        HTML(paste0(
          '<i class="fas fa-exclamation-triangle"></i> <strong>Processing Error</strong><br>',
          error_details, '<br><br>',
          '<small><strong>Technical details:</strong> ', e$message, '</small>'
        )), 
        type = "error", 
        duration = 12
      )
      
      # Debug output only in development
      if (!PRODUCTION_MODE) {
        cat("CSV processing error:", e$message, "\n")
        print(traceback())
      }
    })
  })
  
  # ==========================================================================
  # ENHANCED DEMO DATA LOADING
  # ==========================================================================
  observeEvent(input$load_demo_data, {
    
    # Enhanced loading button state with spinner
    updateActionButton(session, "load_demo_data", 
                       label = HTML('<i class="fas fa-spinner fa-spin"></i> Loading Demo Data...'), 
                       icon = NULL)
    
    values$processing <- TRUE
    values$current_operation <- "Loading demonstration dataset"
    
    # Show progress notification
    showNotification(
      HTML('<i class="fas fa-download"></i> <strong>Loading Demo Data...</strong><br>Preparing sample milestone evaluations'),
      type = "message", 
      duration = 3
    )
    
    tryCatch({
      
      # Check if RDS file exists
      if (!file.exists("data/acme_miles_demo_data.rds")) {
        stop("Demo data file not found. Please ensure the demo data is available.")
      }
      
      values$current_operation <- "Processing demonstration evaluations"
      
      # Load demo data (with minimal logging for production)
      demo_data <- readRDS("data/acme_miles_demo_data.rds")
      demo_df <- demo_data$evaluations
      milestone_defs <- demo_data$milestone_definitions
      
      if (!PRODUCTION_MODE) {
        cat("Demo data loaded with", nrow(demo_df), "records\n")
      }
      
      # Process evaluation data
      demo_processed <- demo_df %>%
        mutate(Rating = as.numeric(Rating))
      
      values$current_operation <- "Configuring demonstration interface"
      
      # Create the data structure your app expects
      processed_data <- list(
        evaluations = demo_processed,
        program_info = list(
          program_name = "Acme Internal Medicine Program",
          specialty_name = "Internal Medicine", 
          sponsor_name = "Acme Medical Center/University School of Medicine"
        ),
        summary = list(
          n_residents = length(unique(demo_processed$Resident_ID)),
          n_milestones = length(unique(demo_processed$Sub_Competency)),
          n_evaluations = nrow(demo_processed),
          n_periods = length(unique(demo_processed$Period)),
          training_levels = sort(unique(demo_processed$PGY_Level))
        ),
        milestone_structure = list(
          definitions = milestone_defs
        ),
        raw_data = demo_processed,
        residents = demo_processed %>% 
          select(any_of(c("Resident_ID", "First_Name", "Last_Name", "Resident_Year", 
                          "PGY_Level", "Resident_Name"))) %>%
          distinct()
      )
      
      # Set the milestone_data reactive
      milestone_data(processed_data)
      
      # Update status display with enhanced formatting
      output$status_text <- renderText({
        paste(
          "✅ Demo data loaded successfully!",
          "",
          "📊 Program Details:",
          paste("  •", processed_data$program_info$program_name),
          paste("  • Specialty:", processed_data$program_info$specialty_name),
          "",
          "📈 Dataset Summary:",
          paste("  • Evaluation Records:", format(processed_data$summary$n_evaluations, big.mark = ",")),
          paste("  • Residents:", processed_data$summary$n_residents),
          paste("  • Sub-Competencies:", processed_data$summary$n_milestones),
          paste("  • Assessment Periods:", processed_data$summary$n_periods),
          paste("  • Training Levels:", paste(processed_data$summary$training_levels, collapse = ", ")),
          sep = "\n"
        )
      })
      
      # Enhanced quick summary with better formatting and guidance
      output$quick_summary <- renderUI({
        period_breakdown <- processed_data$evaluations %>%
          count(Period, name = "records") %>%
          mutate(display = paste0(Period, " (", format(records, big.mark = ","), " evaluations)"))
        
        tagList(
          div(class = "alert alert-success fade-in-scale",
              icon("check-circle"), " ", 
              strong("Demo data loaded successfully!")
          ),
          
          div(class = "row mt-3",
              column(6,
                     h6(icon("hospital"), " Program Information:"),
                     tags$ul(class = "list-unstyled",
                             tags$li(icon("building"), " ", strong("Program: "), processed_data$program_info$program_name),
                             tags$li(icon("user-md"), " ", strong("Specialty: "), processed_data$program_info$specialty_name),
                             tags$li(icon("users"), " ", strong("Residents: "), processed_data$summary$n_residents),
                             tags$li(icon("graduation-cap"), " ", strong("Training Levels: "), paste(processed_data$summary$training_levels, collapse = ", "))
                     )
              ),
              column(6,
                     h6(icon("chart-bar"), " Dataset Overview:"),
                     tags$ul(class = "list-unstyled",
                             tags$li(icon("clipboard-list"), " ", strong("Sub-Competencies: "), processed_data$summary$n_milestones),
                             tags$li(icon("calculator"), " ", strong("Total Evaluations: "), format(processed_data$summary$n_evaluations, big.mark = ",")),
                             tags$li(icon("calendar-alt"), " ", strong("Assessment Periods: "), processed_data$summary$n_periods)
                     )
              )
          ),
          
          div(class = "mt-3",
              h6(icon("calendar-check"), " Evaluation Periods Available:"),
              div(class = "row",
                  lapply(split(period_breakdown, ceiling(seq_along(period_breakdown$display)/2)), function(chunk) {
                    div(class = "col-6",
                        tags$ul(class = "list-unstyled small",
                                lapply(chunk$display, function(x) tags$li(icon("dot-circle", class = "text-primary"), " ", x))
                        ))
                  })
              )
          ),
          
          div(class = "alert alert-info mt-4",
              style = "border-left: 4px solid #17a2b8;",
              icon("lightbulb"), " ",
              strong("Ready to Explore!"), 
              " This demo includes 4 years of realistic milestone progressions. ",
              "Visit the Program Overview or Milestone Analysis tabs to see powerful visualizations of this data."
          )
        )
      })
      
      # Update period choices for analysis
      periods <- sort(unique(demo_processed$Period))
      updateSelectInput(session, "specific_period", 
                        choices = setNames(periods, periods),
                        selected = periods[length(periods)])
      
      # Update milestone period choices
      milestone_period_choices <- get_all_period_choices(list(evaluations = demo_processed))
      updateSelectInput(session, "milestone_period", 
                        choices = milestone_period_choices,
                        selected = "all")
      
      # Update competency choices
      competencies <- unique(demo_processed$Competency)
      competency_choices <- c("All Competencies" = "all")
      if (length(competencies) > 0) {
        competency_choices <- c(competency_choices, setNames(competencies, competencies))
      }
      updateSelectInput(session, "milestone_competency", choices = competency_choices)
      updateSelectInput(session, "trend_competency", choices = competency_choices)
      
      # Complete processing
      values$processing <- FALSE
      values$current_operation <- ""
      
      # Enhanced success notification
      showNotification(
        HTML(paste0(
          '<i class="fas fa-rocket"></i> <strong>Demo Ready!</strong><br>',
          '🎉 Explore Program Overview and Milestone Analysis tabs<br>',
          '📊 ', format(processed_data$summary$n_evaluations, big.mark = ","), ' evaluations loaded across ', 
          processed_data$summary$n_periods, ' assessment periods'
        )),
        type = "message",
        duration = 8
      )
      
      # Update button to success state
      updateActionButton(session, "load_demo_data", 
                         label = HTML('<i class="fas fa-check"></i> Demo Data Loaded'), 
                         icon = NULL)
      
      if (!PRODUCTION_MODE) {
        cat("Demo loading complete successfully!\n")
      }
      
    }, error = function(e) {
      values$processing <- FALSE
      values$current_operation <- ""
      
      # Determine specific error type
      error_msg <- if (!file.exists("data/acme_miles_demo_data.rds")) {
        "Demo data file not found. Please ensure 'data/acme_miles_demo_data.rds' exists in your app directory."
      } else if (grepl("readRDS", e$message)) {
        "Error reading demo data file. The file may be corrupted or in wrong format."
      } else {
        paste("Unexpected error loading demo data:", e$message)
      }
      
      showNotification(
        HTML(paste0(
          '<i class="fas fa-exclamation-triangle"></i> <strong>Demo Load Failed</strong><br>',
          error_msg, '<br><br>',
          '<small>Try uploading your own data instead, or contact support if this persists.</small>'
        )),
        type = "error", 
        duration = 10
      )
      
      # Reset button
      updateActionButton(session, "load_demo_data", 
                         label = HTML('<i class="fas fa-play-circle"></i> Try Demo Data'), 
                         icon = NULL)
      
      # Debug output only in development
      if (!PRODUCTION_MODE) {
        cat("Demo loading error:", e$message, "\n")
        print(traceback())
      }
    })
  })
  
  # ==========================================================================
  # ENHANCED PERIOD AND LEVEL MANAGEMENT
  # ==========================================================================
  
  # Get current period selection with error handling
  get_current_period <- reactive({
    req(input$period_selection)
    
    tryCatch({
      switch(input$period_selection,
             "specific" = input$specific_period,
             "recent_end" = "recent_end",
             "recent_mid" = "recent_mid", 
             "all_periods" = "total",
             "recent_end")  # default fallback
    }, error = function(e) {
      if (!PRODUCTION_MODE) {
        cat("Period selection error:", e$message, "\n")
      }
      "recent_end"  # Safe fallback
    })
  })
  
  # Get all available periods from loaded data
  get_available_periods <- reactive({
    req(milestone_data())
    tryCatch({
      periods <- sort(unique(milestone_data()$evaluations$Period))
      return(periods)
    }, error = function(e) {
      if (!PRODUCTION_MODE) {
        cat("Available periods error:", e$message, "\n")
      }
      return(character(0))
    })
  })
  
  # ==========================================================================
  # ENHANCED PGY LEVEL MANAGEMENT
  # ==========================================================================
  
  # Generate PGY checkboxes dynamically
  output$pgy_checkboxes <- renderUI({
    data <- milestone_data()
    if (is.null(data)) return(NULL)
    
    pgy_levels <- sort(unique(data$evaluations$PGY_Level))
    
    pgy_choices <- lapply(pgy_levels, function(level) {
      checkboxInput(paste0("pgy_", level), level, value = TRUE)
    })
    
    do.call(tagList, pgy_choices)
  })
  
  # Reactive function to get selected PGY levels
  selected_pgy_levels <- reactive({
    data <- milestone_data()
    if (is.null(data)) return(NULL)
    
    pgy_levels <- sort(unique(data$evaluations$PGY_Level))
    
    if (input$select_all_pgy) {
      return(pgy_levels)
    } else {
      selected <- c()
      for (level in pgy_levels) {
        checkbox_id <- paste0("pgy_", level)
        if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
          selected <- c(selected, level)
        }
      }
      return(selected)
    }
  })
  
  # Observer to handle "Select All" checkbox
  observeEvent(input$select_all_pgy, {
    data <- milestone_data()
    if (is.null(data)) return()
    
    pgy_levels <- sort(unique(data$evaluations$PGY_Level))
    
    for (level in pgy_levels) {
      checkbox_id <- paste0("pgy_", level)
      updateCheckboxInput(session, checkbox_id, value = input$select_all_pgy)
    }
  })
  
  # ==========================================================================
  # ENHANCED DATA REACTIVES WITH ERROR HANDLING
  # ==========================================================================
  
  # Enhanced graduation readiness data with period breakdown option
  graduation_readiness_data <- reactive({
    req(milestone_data(), input$milestone_threshold)
    
    withProgress(message = 'Calculating graduation readiness...', value = 0, {
      incProgress(0.3, detail = "Analyzing graduating class performance")
      
      tryCatch({
        result <- calculate_graduation_readiness_enhanced(
          data = milestone_data(),
          threshold = input$milestone_threshold,
          period_filter = input$milestone_period,
          competency_filter = input$milestone_competency,
          show_by_period = isTRUE(input$graduation_by_period)
        )
        
        incProgress(1.0, detail = "Analysis complete")
        return(result)
        
      }, error = function(e) {
        if (!PRODUCTION_MODE) {
          cat("Graduation readiness calculation error:", e$message, "\n")
        }
        
        # Return error data frame
        return(data.frame(
          Sub_Competency = "ERROR",
          message = paste("Error calculating graduation readiness:", e$message),
          stringsAsFactors = FALSE
        ))
      })
    })
  })
  
  # Enhanced all levels readiness data with PGY selection
  all_levels_data <- reactive({
    req(milestone_data(), input$milestone_threshold)
    
    withProgress(message = 'Analyzing all training levels...', value = 0, {
      incProgress(0.3, detail = "Processing multi-level data")
      
      # Get selected PGY levels, default to all if none selected
      selected_pgy <- input$all_levels_pgy
      if (is.null(selected_pgy) || length(selected_pgy) == 0) {
        selected_pgy <- sort(unique(milestone_data()$evaluations$PGY_Level))
      }
      
      tryCatch({
        result <- calculate_all_levels_readiness_enhanced(
          data = milestone_data(),
          threshold = input$milestone_threshold,
          period_filter = input$milestone_period,
          competency_filter = input$milestone_competency,
          selected_pgy_levels = selected_pgy
        )
        
        incProgress(1.0, detail = "Analysis complete")
        return(result)
        
      }, error = function(e) {
        if (!PRODUCTION_MODE) {
          cat("All levels calculation error:", e$message, "\n")
        }
        
        return(data.frame(
          PGY_Level = "ERROR",
          Sub_Competency = "ERROR",
          message = paste("Error in all levels analysis:", e$message),
          stringsAsFactors = FALSE
        ))
      })
    })
  })
  
  # Enhanced trend data with period-specific comparisons
  trend_data <- reactive({
    req(milestone_data(), input$trend_subcompetency, input$milestone_threshold)
    
    if (is.null(input$trend_subcompetency) || input$trend_subcompetency == "") {
      return(data.frame())
    }
    
    withProgress(message = paste('Analyzing trends for', input$trend_subcompetency), value = 0, {
      incProgress(0.3, detail = "Processing longitudinal data")
      
      # Get selected periods for trend analysis
      selected_periods <- NULL
      if (!is.null(input$trend_selected_periods) && length(input$trend_selected_periods) > 0) {
        selected_periods <- input$trend_selected_periods
      }
      
      tryCatch({
        result <- calculate_subcompetency_trends_enhanced(
          data = milestone_data(),
          sub_competency = input$trend_subcompetency,
          threshold = input$milestone_threshold,
          selected_periods = selected_periods
        )
        
        incProgress(1.0, detail = "Trend analysis complete")
        return(result)
        
      }, error = function(e) {
        if (!PRODUCTION_MODE) {
          cat("Trend calculation error:", e$message, "\n")
        }
        
        return(data.frame(
          sub_competency = input$trend_subcompetency,
          message = paste("Error in trend analysis:", e$message),
          stringsAsFactors = FALSE
        ))
      })
    })
  })
  
  # ==========================================================================
  # MILESTONE DATES MODAL
  # ==========================================================================
  
  observeEvent(input$show_milestone_dates, {
    showModal(modalDialog(
      title = "Milestones 2.0 Implementation Timeline",
      size = "l",
      
      div(
        h5("Implementation Schedule by Specialty"),
        
        tabsetPanel(
          tabPanel("2021 (Current)",
                   div(class = "mt-3",
                       h6("Internal Medicine and Core Specialties:"),
                       tags$ul(
                         tags$li("Internal Medicine"),
                         tags$li("Internal Medicine-Pediatrics"), 
                         tags$li("Hematology and Medical Oncology"),
                         tags$li("Infectious Disease"),
                         tags$li("Nephrology"),
                         tags$li("And many subspecialties...")
                       )
                   )
          ),
          
          tabPanel("2022",
                   div(class = "mt-3",
                       h6("Surgical and Other Specialties:"),
                       tags$ul(
                         tags$li("Surgery"),
                         tags$li("Obstetrics and Gynecology"),
                         tags$li("Orthopedic Surgery"),
                         tags$li("Plastic Surgery"),
                         tags$li("And many subspecialties...")
                       )
                   )
          ),
          
          tabPanel("2023",
                   div(class = "mt-3",
                       h6("Pediatric Subspecialties:"),
                       tags$ul(
                         tags$li("Pediatric Cardiology"),
                         tags$li("Pediatric Critical Care Medicine"),
                         tags$li("Pediatric Endocrinology"),
                         tags$li("And other pediatric subspecialties...")
                       )
                   )
          ),
          
          tabPanel("2024",
                   div(class = "mt-3",
                       h6("Latest Additions:"),
                       tags$ul(
                         tags$li("Interventional Pulmonology")
                       )
                   )
          )
        ),
        
        br(),
        div(class = "alert alert-info",
            icon("info-circle"), " ",
            "For the complete and most up-to-date list, visit: ",
            tags$a("ACGME Milestones 2.0 Effective Dates", 
                   href = "https://www.acgme.org/globalassets/PDFs/Milestones/Milestones2.0EffectiveDates.pdf",
                   target = "_blank", class = "alert-link")
        )
      ),
      
      footer = modalButton("Close")
    ))
  })
  
  # ==========================================================================
  # UPDATE CHOICES WHEN DATA LOADS
  # ==========================================================================
  
  observeEvent(milestone_data(), {
    req(milestone_data())
    
    # Update competency choices
    competencies <- unique(milestone_data()$evaluations$Competency)
    competency_choices <- c("All Competencies" = "all")
    if (length(competencies) > 0) {
      competency_choices <- c(competency_choices, setNames(competencies, competencies))
    }
    updateSelectInput(session, "milestone_competency", choices = competency_choices)
    
    # Update sub-competency choices for trend analysis
    subcompetencies <- sort(unique(milestone_data()$evaluations$Sub_Competency))
    subcomp_choices <- c("Select sub-competency..." = "")
    if (length(subcompetencies) > 0) {
      subcomp_choices <- c(subcomp_choices, setNames(subcompetencies, subcompetencies))
    }
    updateSelectInput(session, "trend_subcompetency", choices = subcomp_choices)
    
    # Update PGY level choices for all levels analysis
    pgy_levels <- sort(unique(milestone_data()$evaluations$PGY_Level))
    updateCheckboxGroupInput(session, "all_levels_pgy", 
                             choices = setNames(pgy_levels, pgy_levels),
                             selected = pgy_levels)  # Default all selected
  })
  
  # ==========================================================================
  # SPIDER PLOT ENHANCEMENTS
  # ==========================================================================
  
  # Generate period checkboxes for spider plot multi-period selection
  output$spider_period_checkboxes <- renderUI({
    req(milestone_data(), input$spider_multi_period)
    
    periods <- get_available_periods()
    
    # Create checkbox list
    checkbox_list <- lapply(periods, function(period) {
      checkboxInput(
        paste0("spider_period_", make.names(period)), 
        label = period,
        value = TRUE  # Default all to selected
      )
    })
    
    div(checkbox_list)
  })
  
  # Enhanced period display info
  output$period_display_info <- renderUI({
    req(milestone_data(), input$period_selection)
    
    if (input$period_selection == "specific" && isTRUE(input$spider_multi_period)) {
      spider_periods <- get_spider_periods()
      if (length(spider_periods) > 1) {
        period_text <- paste("Comparing:", paste(spider_periods, collapse = ", "))
      } else {
        period_text <- paste("Period:", spider_periods[1])
      }
    } else {
      period_text <- switch(input$period_selection,
                            "specific" = paste("Specific Period:", input$specific_period),
                            "recent_end" = "Most Recent End-Year",
                            "recent_mid" = "Most Recent Mid-Year", 
                            "all_periods" = "All Periods Combined")
    }
    
    pgy_text <- if (length(selected_pgy_levels()) == length(sort(unique(milestone_data()$evaluations$PGY_Level)))) {
      "All Training Levels"
    } else {
      paste("Training Levels:", paste(selected_pgy_levels(), collapse = ", "))
    }
    
    HTML(paste0(
      '<i class="fas fa-info-circle text-primary"></i> ',
      '<strong>Current Analysis:</strong> ', period_text, ' • ', pgy_text
    ))
  })
  
  # Get selected periods for spider plot
  get_spider_periods <- reactive({
    req(milestone_data())
    
    # If not in specific mode or multi-period not enabled, return single period
    if (is.null(input$period_selection) || 
        input$period_selection != "specific" || 
        is.null(input$spider_multi_period) || 
        !isTRUE(input$spider_multi_period)) {
      return(get_current_period())
    }
    
    # Get selected periods from checkboxes with NULL checks
    periods <- get_available_periods()
    selected_periods <- c()
    
    for (period in periods) {
      checkbox_id <- paste0("spider_period_", make.names(period))
      checkbox_value <- input[[checkbox_id]]
      
      # Only include if checkbox exists and is TRUE
      if (!is.null(checkbox_value) && isTRUE(checkbox_value)) {
        selected_periods <- c(selected_periods, period)
      }
    }
    
    # If no periods selected, fall back to current period
    if (length(selected_periods) == 0) {
      return(get_current_period())
    }
    
    return(selected_periods)
  })
  
  # ==========================================================================
  # ENHANCED STATUS OUTPUTS
  # ==========================================================================
  
  # Status text with enhanced formatting
  output$status_text <- renderText({
    if (is.null(input$csv_files)) {
      "📄 No files selected"
    } else if (is.null(milestone_data())) {
      paste("📋 Ready to process", nrow(input$csv_files), "files:",
            paste(input$csv_files$name, collapse = ", "))
    } else {
      "✅ Data processing completed successfully!"
    }
  })
  
  # Enhanced quick summary with better error handling
  output$quick_summary <- renderUI({
    data <- milestone_data()
    if (is.null(data)) {
      div(class = "alert alert-light",
          icon("info-circle"), " No data loaded")
    } else {
      tryCatch({
        tagList(
          div(class = "row",
              column(6,
                     h6(icon("building"), " Program Information:"),
                     tags$ul(class = "list-unstyled",
                             tags$li(icon("hospital"), " ", strong("Program: "), data$program_info$program_name),
                             tags$li(icon("stethoscope"), " ", strong("Specialty: "), data$program_info$specialty_name)
                     )
              ),
              column(6,
                     h6(icon("chart-bar"), " Dataset Summary:"),
                     tags$ul(class = "list-unstyled",
                             tags$li(icon("users"), " ", strong("Residents: "), data$summary$n_residents),
                             tags$li(icon("clipboard-list"), " ", strong("Sub-Competencies: "), data$summary$n_milestones),
                             tags$li(icon("calculator"), " ", strong("Evaluation Records: "), format(data$summary$n_evaluations, big.mark = ",")),
                             tags$li(icon("calendar-alt"), " ", strong("Assessment Periods: "), data$summary$n_periods),
                             tags$li(icon("graduation-cap"), " ", strong("Training Levels: "), paste(data$summary$training_levels, collapse = ", "))
                     )
              )
          )
        )
      }, error = function(e) {
        div(class = "alert alert-warning",
            icon("exclamation-triangle"), " Error displaying summary: ", e$message)
      })
    }
  })
  
  # ==========================================================================
  # PROGRAM OVERVIEW OUTPUTS WITH ENHANCED LOADING
  # ==========================================================================
  
  # Areas for improvement table with loading states
  output$improvement_areas <- DT::renderDataTable({
    req(milestone_data())
    
    withProgress(message = 'Identifying improvement areas...', value = 0, {
      incProgress(0.5, detail = "Analyzing lowest performing sub-competencies")
      
      tryCatch({
        if (input$tables_use_filters) {
          result <- identify_improvement_areas_filtered(
            milestone_data(), 
            n_items = 5,
            period_selection = get_current_period(),
            selected_pgy_levels = selected_pgy_levels()
          )
        } else {
          result <- identify_improvement_areas(
            milestone_data(), 
            n_items = 5, 
            focus_end_year = TRUE
          )
        }
        
        incProgress(1.0, detail = "Complete")
        return(result)
        
      }, error = function(e) {
        if (!PRODUCTION_MODE) {
          cat("Improvement areas error:", e$message, "\n")
        }
        return(data.frame(Error = paste("Error identifying improvement areas:", e$message)))
      })
    })
  }, options = list(
    pageLength = 5, 
    dom = 't', 
    scrollX = TRUE,
    columnDefs = list(
      list(className = 'dt-center', targets = c(1, 2, 4))
    )
  ))
  
  # Areas of strength table with loading states
  output$strength_areas <- DT::renderDataTable({
    req(milestone_data())
    
    withProgress(message = 'Identifying strength areas...', value = 0, {
      incProgress(0.5, detail = "Analyzing highest performing sub-competencies")
      
      tryCatch({
        if (input$tables_use_filters) {
          result <- identify_strength_areas_filtered(
            milestone_data(), 
            n_items = 5,
            period_selection = get_current_period(),
            selected_pgy_levels = selected_pgy_levels()
          )
        } else {
          result <- identify_strength_areas(
            milestone_data(), 
            n_items = 5, 
            focus_end_year = TRUE
          )
        }
        
        incProgress(1.0, detail = "Complete")
        return(result)
        
      }, error = function(e) {
        if (!PRODUCTION_MODE) {
          cat("Strength areas error:", e$message, "\n")
        }
        return(data.frame(Error = paste("Error identifying strength areas:", e$message)))
      })
    })
  }, options = list(
    pageLength = 5, 
    dom = 't', 
    scrollX = TRUE,
    columnDefs = list(
      list(className = 'dt-center', targets = c(1, 2, 4))
    )
  ))
  
  # Dynamic descriptions for the tables
  output$improvement_description <- renderUI({
    if (input$tables_use_filters) {
      period_text <- switch(get_current_period(),
                            "recent_end" = "Most Recent End-Year",
                            "recent_mid" = "Most Recent Mid-Year", 
                            "total" = "All Periods",
                            get_current_period())
      
      pgy_text <- if (length(selected_pgy_levels()) == length(sort(unique(milestone_data()$evaluations$PGY_Level)))) {
        "All Training Levels"
      } else {
        paste(selected_pgy_levels(), collapse = ", ")
      }
      
      HTML(paste0(
        '<small><i class="fas fa-filter text-muted"></i> ',
        '<strong>Analysis Scope:</strong> ', period_text, ' • ', pgy_text, '</small>'
      ))
    } else {
      HTML('<small><i class="fas fa-chart-bar text-muted"></i> <strong>Analysis Scope:</strong> End-Year evaluations across all training levels</small>')
    }
  })
  
  output$strength_description <- renderUI({
    if (input$tables_use_filters) {
      period_text <- switch(get_current_period(),
                            "recent_end" = "Most Recent End-Year",
                            "recent_mid" = "Most Recent Mid-Year", 
                            "total" = "All Periods",
                            get_current_period())
      
      pgy_text <- if (length(selected_pgy_levels()) == length(sort(unique(milestone_data()$evaluations$PGY_Level)))) {
        "All Training Levels"
      } else {
        paste(selected_pgy_levels(), collapse = ", ")
      }
      
      HTML(paste0(
        '<small><i class="fas fa-filter text-muted"></i> ',
        '<strong>Analysis Scope:</strong> ', period_text, ' • ', pgy_text, '</small>'
      ))
    } else {
      HTML('<small><i class="fas fa-chart-bar text-muted"></i> <strong>Analysis Scope:</strong> End-Year evaluations across all training levels</small>')
    }
  })
  
  # Enhanced program spider plot with comprehensive loading
  output$program_spider <- renderPlotly({
    req(milestone_data(), selected_pgy_levels())
    
    withProgress(message = 'Creating program overview visualization...', value = 0, {
      incProgress(0.2, detail = "Preparing milestone data")
      
      spider_periods <- get_spider_periods()
      
      incProgress(0.5, detail = "Calculating competency averages")
      
      tryCatch({
        if (length(spider_periods) == 1) {
          incProgress(0.8, detail = "Generating spider plot")
          result <- create_multi_level_spider_plot(
            milestone_data(),
            period_type = spider_periods[1],
            selected_pgy_levels = selected_pgy_levels(),
            show_medians = input$show_program_means
          )
        } else {
          incProgress(0.8, detail = "Creating multi-period comparison")
          # Note: You'll need to implement create_multi_period_spider_plot 
          # or modify this to use existing functions
          result <- create_multi_level_spider_plot(
            milestone_data(),
            period_type = spider_periods[1],  # Use first period as fallback
            selected_pgy_levels = selected_pgy_levels(),
            show_medians = input$show_program_means
          )
        }
        
        incProgress(1.0, detail = "Visualization complete")
        return(result)
        
      }, error = function(e) {
        if (!PRODUCTION_MODE) {
          cat("Spider plot error:", e$message, "\n")
        }
        
        return(plot_ly() %>% 
                 add_annotations(
                   text = paste("Unable to create visualization. Please try different settings or check your data."), 
                   x = 0.5, y = 0.5, 
                   showarrow = FALSE,
                   font = list(size = 14, color = "#dc3545")
                 ) %>%
                 layout(
                   title = "Program Performance Overview",
                   margin = list(t = 60, b = 40, l = 40, r = 40)
                 ))
      })
    })
  })
  
  # Enhanced milestone reference table
  output$milestone_reference_table <- DT::renderDataTable({
    req(milestone_data())
    
    withProgress(message = 'Preparing milestone reference...', value = 0, {
      incProgress(0.5, detail = "Formatting milestone definitions")
      
      tryCatch({
        result <- create_milestone_reference_table(milestone_data())
        incProgress(1.0, detail = "Reference table ready")
        return(result)
        
      }, error = function(e) {
        if (!PRODUCTION_MODE) {
          cat("Milestone reference error:", e$message, "\n")
        }
        return(data.frame(
          Code = "Error",
          Description = paste("Error creating reference table:", e$message)
        ))
      })
    })
  }, options = list(
    pageLength = 25,
    dom = 'ft',
    scrollX = FALSE,
    autoWidth = FALSE,
    columnDefs = list(
      list(width = '60px', targets = 0, className = 'dt-center'),
      list(width = 'calc(100% - 60px)', targets = 1)
    ),
    rowCallback = JS("
      function(row, data) {
        $(row).find('td').css({'border': 'none', 'padding': '0px'});
        $(row).find('td:eq(0)').css({'width': '60px', 'max-width': '60px'});
      }")
  ), escape = FALSE)
  
  # ==========================================================================
  # MILESTONE ANALYSIS OUTPUTS WITH ENHANCED LOADING
  # ==========================================================================
  
  # Enhanced graduation readiness chart
  output$graduation_readiness_chart <- renderPlotly({
    req(graduation_readiness_data())
    
    tryCatch({
      if (isTRUE(input$graduation_by_period) && input$milestone_period == "all") {
        readiness_data <- graduation_readiness_data()
        
        # Create period-breakdown chart
        fig <- plot_ly(readiness_data)
        
        periods <- unique(readiness_data$Period)
        colors <- c("#4CAF50", "#8BC34A", "#FF9800", "#F44336")
        names(colors) <- c("Excellent (<2.5%)", "Good (2.5-5%)", "Concerning (5-7.5%)", "High Risk (>7.5%)")
        
        for (i in seq_along(periods)) {
          period_data <- readiness_data %>% filter(Period == periods[i])
          
          fig <- fig %>% add_trace(
            y = ~reorder(competency_period, percent_below_threshold),
            x = ~percent_below_threshold,
            type = 'bar',
            orientation = 'h',
            name = periods[i],
            data = period_data,
            marker = list(color = colors[period_data$readiness_category]),
            hovertemplate = paste0(
              '<b>', period_data$Sub_Competency, '</b><br>',
              'Period: ', period_data$Period, '<br>',
              'Below Threshold: ', round(period_data$percent_below_threshold, 1), '%<br>',
              'Risk Level: ', period_data$readiness_category,
              '<extra></extra>'
            )
          )
        }
        
        fig <- fig %>% layout(
          title = list(
            text = paste0("<b>Graduation Readiness by Period - ", readiness_data$graduating_class[1], "</b>"),
            font = list(size = 16)
          ),
          xaxis = list(title = "Percentage Below Threshold (%)"),
          yaxis = list(title = "Sub-Competency by Period"),
          barmode = 'group',
          margin = list(l = 150, b = 100)
        )
        
        return(fig)
      } else {
        # Use standard graduation chart
        return(create_graduation_readiness_chart(graduation_readiness_data()))
      }
    }, error = function(e) {
      if (!PRODUCTION_MODE) {
        cat("Graduation chart error:", e$message, "\n")
      }
      
      return(plot_ly() %>% 
               add_annotations(
                 text = "Unable to create graduation readiness chart. Please check your data and settings.", 
                 x = 0.5, y = 0.5, 
                 showarrow = FALSE,
                 font = list(size = 14, color = "#dc3545")
               ) %>%
               layout(
                 title = "Graduation Readiness Analysis",
                 margin = list(t = 60, b = 40, l = 40, r = 40)
               ))
    })
  })
  
  # Enhanced all levels chart
  output$all_levels_chart <- renderPlotly({
    req(all_levels_data())
    
    tryCatch({
      return(create_all_levels_chart(all_levels_data()))
    }, error = function(e) {
      if (!PRODUCTION_MODE) {
        cat("All levels chart error:", e$message, "\n")
      }
      
      return(plot_ly() %>% 
               add_annotations(
                 text = "Unable to create all levels analysis chart. Please try different settings.", 
                 x = 0.5, y = 0.5, 
                 showarrow = FALSE,
                 font = list(size = 14, color = "#dc3545")
               ) %>%
               layout(
                 title = "All Training Levels Analysis", 
                 margin = list(t = 60, b = 40, l = 40, r = 40)
               ))
    })
  })
  
  # Enhanced sub-competency trend chart
  output$subcompetency_trend_chart <- renderPlotly({
    req(trend_data())
    
    if (nrow(trend_data()) == 0) {
      return(plot_ly() %>% 
               add_annotations(
                 text = paste("No trend data available for selected sub-competency:",
                              ifelse(is.null(input$trend_subcompetency) || input$trend_subcompetency == "",
                                     "None selected", input$trend_subcompetency)), 
                 x = 0.5, y = 0.5, 
                 showarrow = FALSE,
                 font = list(size = 14, color = "#6c757d")
               ) %>%
               layout(
                 title = "Sub-Competency Trends",
                 margin = list(t = 60, b = 40, l = 40, r = 40)
               ))
    }
    
    tryCatch({
      # Get selected periods for filtering
      selected_periods <- selected_trend_periods()
      
      # Filter trend data if specific periods are selected
      filtered_trend_data <- trend_data()
      if (length(selected_periods) > 0 && length(selected_periods) < length(available_trend_periods())) {
        filtered_trend_data <- trend_data() %>%
          filter(Period %in% selected_periods)
      }
      
      if (nrow(filtered_trend_data) == 0) {
        return(plot_ly() %>% 
                 add_annotations(
                   text = "No data available for selected periods and sub-competency", 
                   x = 0.5, y = 0.5, 
                   showarrow = FALSE,
                   font = list(size = 14, color = "#6c757d")
                 ) %>%
                 layout(
                   title = "Sub-Competency Trends",
                   margin = list(t = 60, b = 40, l = 40, r = 40)
                 ))
      }
      
      # Create chart with option to show/hide total average
      show_total <- if(is.null(input$show_total_average)) TRUE else input$show_total_average
      return(create_trend_chart_enhanced(filtered_trend_data, show_total_average = show_total))
      
    }, error = function(e) {
      if (!PRODUCTION_MODE) {
        cat("Trend chart error:", e$message, "\n")
      }
      
      return(plot_ly() %>% 
               add_annotations(
                 text = "Unable to create trend chart. Please try a different sub-competency or check your data.", 
                 x = 0.5, y = 0.5, 
                 showarrow = FALSE,
                 font = list(size = 14, color = "#dc3545")
               ) %>%
               layout(
                 title = "Sub-Competency Trends",
                 margin = list(t = 60, b = 40, l = 40, r = 40)
               ))
    })
  })
  
  # ==========================================================================
  # ENHANCED COHORT AND TREND ANALYSIS
  # ==========================================================================
  
  # Available trend periods reactive
  available_trend_periods <- reactive({
    req(milestone_data())
    
    tryCatch({
      periods <- milestone_data()$evaluations %>%
        pull(Period) %>%
        unique() %>%
        sort()
      
      return(periods)
    }, error = function(e) {
      if (!PRODUCTION_MODE) {
        cat("Available trend periods error:", e$message, "\n")
      }
      return(character(0))
    })
  })
  
  # Generate period checkboxes for trend analysis
  output$trend_period_checkboxes <- renderUI({
    req(milestone_data())
    
    periods <- available_trend_periods()
    
    # Create checkbox list
    checkbox_list <- lapply(periods, function(period) {
      checkboxInput(
        paste0("trend_period_", make.names(period)), 
        label = period,
        value = TRUE  # Default all to selected
      )
    })
    
    div(
      h6("Select Periods to Include:"),
      div(checkbox_list, style = "max-height: 200px; overflow-y: auto;"),
      br(),
      actionButton("select_all_trend_periods", "Select All", class = "btn btn-outline-secondary btn-sm", style = "margin-right: 10px;"),
      actionButton("deselect_all_trend_periods", "Deselect All", class = "btn btn-outline-secondary btn-sm")
    )
  })
  
  # Observer for select/deselect all trend periods
  observeEvent(input$select_all_trend_periods, {
    periods <- available_trend_periods()
    for (period in periods) {
      checkbox_id <- paste0("trend_period_", make.names(period))
      updateCheckboxInput(session, checkbox_id, value = TRUE)
    }
  })
  
  observeEvent(input$deselect_all_trend_periods, {
    periods <- available_trend_periods()
    for (period in periods) {
      checkbox_id <- paste0("trend_period_", make.names(period))
      updateCheckboxInput(session, checkbox_id, value = FALSE)
    }
  })
  
  # Reactive to get selected trend periods
  selected_trend_periods <- reactive({
    req(milestone_data())
    
    periods <- available_trend_periods()
    selected <- c()
    
    for (period in periods) {
      checkbox_id <- paste0("trend_period_", make.names(period))
      checkbox_value <- input[[checkbox_id]]
      
      if (!is.null(checkbox_value) && isTRUE(checkbox_value)) {
        selected <- c(selected, period)
      }
    }
    
    return(selected)
  })
  
  # Update cohort choices when data loads
  observeEvent(milestone_data(), {
    req(milestone_data())
    
    tryCatch({
      # Calculate and update cohort choices - SORT BY YEAR
      cohort_info <- calculate_cohort_information(milestone_data())
      available_cohorts <- cohort_info %>%
        count(cohort_label, sort = TRUE) %>%
        mutate(
          # Extract year from "Class of YYYY" for sorting
          graduation_year = as.numeric(str_extract(cohort_label, "\\d{4}"))
        ) %>%
        arrange(desc(graduation_year)) %>%  # Most recent first
        pull(cohort_label)
      
      updateCheckboxGroupInput(session, "selected_cohorts",
                               choices = setNames(available_cohorts, available_cohorts),
                               selected = NULL)  # Start with no cohorts selected
      
    }, error = function(e) {
      if (!PRODUCTION_MODE) {
        cat("Cohort update error:", e$message, "\n")
      }
    })
  })
  
  # Cohort selection buttons
  observeEvent(input$select_recent_cohorts, {
    req(milestone_data())
    
    tryCatch({
      cohort_info <- calculate_cohort_information(milestone_data())
      recent_cohorts <- cohort_info %>%
        count(cohort_label, sort = TRUE) %>%
        mutate(graduation_year = as.numeric(str_extract(cohort_label, "\\d{4}"))) %>%
        arrange(desc(graduation_year)) %>%  # Most recent first
        head(2) %>%
        pull(cohort_label)
      updateCheckboxGroupInput(session, "selected_cohorts", selected = recent_cohorts)
    }, error = function(e) {
      if (!PRODUCTION_MODE) {
        cat("Recent cohorts selection error:", e$message, "\n")
      }
    })
  })
  
  observeEvent(input$clear_cohorts, {
    updateCheckboxGroupInput(session, "selected_cohorts", selected = character(0))
  })
  
  # Enhanced cohort trend plot
  output$cohort_trend_plot <- renderPlotly({
    req(milestone_data(), input$trend_subcompetency)
    
    if (is.null(input$trend_subcompetency) || input$trend_subcompetency == "") {
      return(plot_ly() %>%
               add_annotations(
                 text = "Please select a sub-competency to analyze trends",
                 x = 0.5, y = 0.5,
                 showarrow = FALSE,
                 font = list(size = 14, color = "#6c757d")
               ) %>%
               layout(
                 title = "Training Progression Analysis",
                 margin = list(t = 60, b = 40, l = 40, r = 40)
               ))
    }
    
    tryCatch({
      return(create_cohort_trend_analysis(
        milestone_data(),
        selected_sub_competency = input$trend_subcompetency,
        selected_cohorts = input$selected_cohorts
      ))
    }, error = function(e) {
      if (!PRODUCTION_MODE) {
        cat("Cohort trend error:", e$message, "\n")
      }
      
      return(plot_ly() %>%
               add_annotations(
                 text = "Unable to create cohort analysis. Please try a different sub-competency.",
                 x = 0.5, y = 0.5,
                 showarrow = FALSE,
                 font = list(size = 14, color = "#dc3545")
               ) %>%
               layout(
                 title = "Training Progression Analysis",
                 margin = list(t = 60, b = 40, l = 40, r = 40)
               ))
    })
  })
  
  # ==========================================================================
  # MILESTONE ANALYSIS TABLES WITH ENHANCED ERROR HANDLING
  # ==========================================================================
  
  output$graduation_table <- DT::renderDataTable({
    req(graduation_readiness_data())
    
    tryCatch({
      table_data <- graduation_readiness_data()
      
      # Check for error conditions
      if (nrow(table_data) == 1 && "message" %in% names(table_data)) {
        return(data.frame(
          Message = table_data$message[1]
        ))
      }
      
      if ("Period" %in% names(table_data)) {
        # Period breakdown table
        formatted_data <- table_data %>%
          select(
            `Sub-Competency` = Sub_Competency,
            `Period` = Period,
            `Competency` = Competency,
            `% Below Threshold` = percent_below_threshold,
            `Risk Level` = readiness_category,
            `Mean Score` = mean_score,
            `Residents` = total_residents,
            `Evaluations` = total_evaluations
          ) %>%
          mutate(
            `% Below Threshold` = round(`% Below Threshold`, 2),
            `Mean Score` = round(`Mean Score`, 2)
          )
      } else {
        # Standard table
        formatted_data <- table_data %>%
          select(
            `Sub-Competency` = Sub_Competency,
            `Competency` = Competency,
            `% Below Threshold` = percent_below_threshold,
            `Risk Level` = readiness_category,
            `Mean Score` = mean_score,
            `Residents` = total_residents,
            `Evaluations` = total_evaluations,
            `Below Count` = below_threshold
          ) %>%
          mutate(
            `% Below Threshold` = round(`% Below Threshold`, 2),
            `Mean Score` = round(`Mean Score`, 2)
          )
      }
      
      return(formatted_data)
      
    }, error = function(e) {
      if (!PRODUCTION_MODE) {
        cat("Graduation table error:", e$message, "\n")
      }
      return(data.frame(
        Error = paste("Error creating graduation table:", e$message)
      ))
    })
  }, options = list(
    pageLength = 15,
    scrollX = TRUE,
    order = list(list(2, 'desc')),
    columnDefs = list(
      list(className = 'dt-center', targets = c(2, 4, 5, 6, 7))
    )
  ))
  
  # Enhanced all levels table
  output$all_levels_table <- DT::renderDataTable({
    req(all_levels_data())
    
    tryCatch({
      table_data <- all_levels_data()
      
      # Check for error conditions
      if (nrow(table_data) == 1 && "message" %in% names(table_data)) {
        return(data.frame(
          Message = table_data$message[1]
        ))
      }
      
      formatted_data <- table_data %>%
        select(
          `Sub-Competency` = Sub_Competency,
          `Training Level` = PGY_Level,
          `Competency` = Competency,
          `% Below Threshold` = percent_below_threshold,
          `Risk Level` = readiness_category,
          `Mean Score` = mean_score,
          `Residents` = total_residents,
          `Evaluations` = total_evaluations
        ) %>%
        mutate(
          `% Below Threshold` = round(`% Below Threshold`, 2),
          `Mean Score` = round(`Mean Score`, 2)
        )
      
      return(formatted_data)
      
    }, error = function(e) {
      if (!PRODUCTION_MODE) {
        cat("All levels table error:", e$message, "\n")
      }
      return(data.frame(
        Error = paste("Error creating all levels table:", e$message)
      ))
    })
  }, options = list(
    pageLength = 15,
    scrollX = TRUE,
    order = list(list(1, 'asc'), list(3, 'desc')),
    columnDefs = list(
      list(className = 'dt-center', targets = c(1, 3, 5, 6, 7))
    )
  ))
  
  # ==========================================================================
  # INDIVIDUAL ASSESSMENT INTEGRATION (ENHANCED)
  # ==========================================================================
  
  # Update resident choices when data changes
  observeEvent(milestone_data(), {
    update_individual_residents(session, milestone_data())
  })
  
  # Update level choices when resident changes
  observeEvent(input$individual_resident, {
    req(milestone_data(), input$individual_resident)
    update_individual_levels(session, milestone_data(), input$individual_resident)
  })
  
  # Summary statistics for cards
  individual_stats <- reactive({
    req(milestone_data(), input$individual_resident, input$individual_level)
    tryCatch({
      return(calculate_individual_summary_stats(milestone_data(), input$individual_resident, input$individual_level))
    }, error = function(e) {
      if (!PRODUCTION_MODE) {
        cat("Individual stats error:", e$message, "\n")
      }
      return(list(
        total_evaluations = 0,
        individual_avg = 0,
        program_avg = 0,
        percentile = 0
      ))
    })
  })
  
  # Individual assessment outputs with enhanced error handling
  output$individual_total_evaluations <- renderText({
    req(individual_stats())
    as.character(individual_stats()$total_evaluations)
  })
  
  output$individual_avg_score <- renderText({
    req(individual_stats())
    as.character(individual_stats()$individual_avg)
  })
  
  output$program_avg_score <- renderText({
    req(individual_stats())
    as.character(individual_stats()$program_avg)
  })
  
  output$individual_percentile <- renderText({
    req(individual_stats())
    paste0(individual_stats()$percentile, "%")
  })
  
  # Resident info display
  output$resident_info_display <- renderUI({
    req(milestone_data(), input$individual_resident, input$individual_level)
    
    if (is.null(input$individual_level) || input$individual_level == "all") {
      info_text <- "Showing all evaluations across all periods"
    } else {
      level_parts <- strsplit(input$individual_level, "\\|\\|\\|")[[1]]
      if (length(level_parts) == 2) {
        info_text <- paste("Evaluation Period:", level_parts[1], "<br>",
                           "Training Level:", level_parts[2])
      } else {
        info_text <- "Selected evaluation level"
      }
    }
    
    HTML(paste0(
      '<i class="fas fa-user-check text-primary"></i> ',
      '<strong>Current Selection:</strong><br>', info_text
    ))
  })
  
  # Summary statistics for cards
  individual_stats <- reactive({
    req(milestone_data(), input$individual_resident, input$individual_level)
    calculate_individual_summary_stats(milestone_data(), input$individual_resident, input$individual_level)
  })
  
  # Card outputs
  output$individual_total_evaluations <- renderText({
    req(individual_stats())
    as.character(individual_stats()$total_evaluations)
  })
  
  output$individual_avg_score <- renderText({
    req(individual_stats())
    as.character(individual_stats()$individual_avg)
  })
  
  output$program_avg_score <- renderText({
    req(individual_stats())
    as.character(individual_stats()$program_avg)
  })
  
  output$individual_percentile <- renderText({
    req(individual_stats())
    paste0(individual_stats()$percentile, "%")
  })
  
  # Enhanced spider plot
  output$individual_spider_enhanced <- renderPlotly({
    req(milestone_data(), input$individual_resident, input$individual_level)
    
    tryCatch({
      create_individual_spider_enhanced(
        milestone_data(), 
        input$individual_resident, 
        input$individual_level
      )
    }, error = function(e) {
      plot_ly() %>% 
        add_annotations(text = paste("Error creating spider plot:", e$message), 
                        x = 0.5, y = 0.5, showarrow = FALSE,
                        font = list(color = "red"))
    })
  })
  
  # Enhanced trend chart
  output$individual_trend_enhanced <- renderPlotly({
    req(milestone_data(), input$individual_resident)
    
    tryCatch({
      create_individual_trend_enhanced(
        milestone_data(), 
        input$individual_resident
      )
    }, error = function(e) {
      plot_ly() %>% 
        add_annotations(text = paste("Error creating trend chart:", e$message), 
                        x = 0.5, y = 0.5, showarrow = FALSE,
                        font = list(color = "red"))
    })
  })
  
  # Detailed performance table
  output$individual_detail_table <- DT::renderDataTable({
    req(milestone_data(), input$individual_resident, input$individual_level)
    
    detail_data <- create_individual_detail_table(
      milestone_data(), 
      input$individual_resident, 
      input$individual_level
    )
    
    if (nrow(detail_data) == 0) {
      return(data.frame(Message = "No data available for selected criteria"))
    }
    
    detail_data
  }, options = list(
    pageLength = 15,
    scrollX = TRUE,
    scrollY = "400px",
    columnDefs = list(
      list(targets = c(2, 3, 4, 8), className = 'dt-center')
    )
  ), server = TRUE)
  
  # Enhanced trend data table  
  output$trend_data_table <- DT::renderDataTable({
    req(trend_data())
    
    if (nrow(trend_data()) == 0) {
      return(data.frame(
        Message = "No trend data available for selected sub-competency"
      ))
    }
    
    tryCatch({
      # Get selected periods for filtering
      selected_periods <- selected_trend_periods()
      
      # Filter trend data if specific periods are selected
      filtered_trend_data <- trend_data()
      if (length(selected_periods) > 0 && length(selected_periods) < length(available_trend_periods())) {
        filtered_trend_data <- trend_data() %>%
          filter(Period %in% selected_periods)
      }
      
      if (nrow(filtered_trend_data) == 0) {
        return(data.frame(
          Message = "No data available for selected periods"
        ))
      }
      
      formatted_data <- filtered_trend_data %>%
        select(
          `Period` = Full_Period_Label,
          `Training Stage` = Period_Label,
          `PGY Level` = PGY_Level,
          `Mean Score` = mean_score,
          `Period Average` = period_specific_mean,
          `Difference` = paste0(ifelse(mean_score >= period_specific_mean, "+", ""), 
                                round(mean_score - period_specific_mean, 2)),
          `% Below Threshold` = percent_below_threshold,
          `Residents` = total_residents,
          `Evaluations` = total_evaluations
        ) %>%
        mutate(
          `Mean Score` = round(`Mean Score`, 2),
          `Period Average` = round(`Period Average`, 2),
          `% Below Threshold` = round(`% Below Threshold`, 1)
        )
      
      return(formatted_data)
      
    }, error = function(e) {
      if (!PRODUCTION_MODE) {
        cat("Trend table error:", e$message, "\n")
      }
      return(data.frame(
        Error = paste("Error creating trend table:", e$message)
      ))
    })
  }, options = list(
    pageLength = 10,
    scrollX = TRUE,
    order = list(list(0, 'asc')),
    columnDefs = list(
      list(className = 'dt-center', targets = c(3, 4, 5, 6, 7, 8)),
      list(width = '20%', targets = 0),
      list(width = '15%', targets = 1)
    )
  ))
  
  
  
  # =========================================================================
  # DATA OVERVIEW OUTPUTS
  # =========================================================================
  
  # Milestone definitions table
  output$milestone_table <- DT::renderDataTable({
    req(milestone_data())
    
    data <- milestone_data()
    data$milestone_structure$definitions %>%
      select(Competency, Sub_Competency, Milestone_Description)
  }, options = list(pageLength = 10, scrollX = TRUE, scrollY = "400px"))
  
  # Sample evaluation data
  output$evaluation_sample <- DT::renderDataTable({
    req(milestone_data())
    
    data <- milestone_data()
    # Show first 100 rows as sample
    data$evaluations %>%
      head(100) %>%
      select(Resident_Name, PGY_Level, Period, Competency, Sub_Competency, Rating)
  }, options = list(pageLength = 10, scrollX = TRUE, scrollY = "400px"))
  
  # ==========================================================================
  # NAVIGATION ENHANCEMENT OBSERVERS
  # ==========================================================================
  
  # Navigation button observers
  observeEvent(input$goto_overview, {
    updateTabsetPanel(session, "main_tabs", selected = "Program Overview")
    showNotification("Navigated to Program Overview", type = "message", duration = 2)
  })
  
  observeEvent(input$goto_analysis, {
    updateTabsetPanel(session, "main_tabs", selected = "Milestone Analysis")
    showNotification("Navigated to Milestone Analysis", type = "message", duration = 2)
  })
  
  observeEvent(input$goto_upload, {
    updateTabsetPanel(session, "main_tabs", selected = "Get Started")
    showNotification("Navigated to Get Started", type = "message", duration = 2)
  })
  
  # ==========================================================================
  # FINAL INITIALIZATION AND CLEANUP
  # ==========================================================================
  
  # Welcome message (only in development)
  if (!PRODUCTION_MODE) {
    observe({
      showNotification(
        HTML(paste0(
          '<i class="fas fa-code"></i> <strong>Development Mode Active</strong><br>',
          'Debug output enabled - remember to set PRODUCTION_MODE = TRUE for deployment'
        )),
        type = "message",
        duration = 4
      )
    })
  }
  
  # Initialize UI state
  observe({
    # Set default values for UI elements that need them
    if (is.null(input$milestone_threshold)) {
      updateSliderInput(session, "milestone_threshold", value = 7)
    }
    
    if (is.null(input$period_selection)) {
      updateRadioButtons(session, "period_selection", selected = "recent_end")
    }
    
    # Initialize progressive disclosure states
    if (!is.null(input$show_program_means)) {
      if (is.na(input$show_program_means)) {
        updateCheckboxInput(session, "show_program_means", value = TRUE)
      }
    }
    
    if (!is.null(input$tables_use_filters)) {
      if (is.na(input$tables_use_filters)) {
        updateCheckboxInput(session, "tables_use_filters", value = TRUE)
      }
    }
  })
  
  # Session information logging
  observe({
    if (!PRODUCTION_MODE) {
      cat("=== GME MILESTONE PLATFORM SESSION INFO ===\n")
      cat("Session ID:", session$token, "\n")
      cat("Production Mode:", PRODUCTION_MODE, "\n")
      cat("Data Loaded:", !is.null(milestone_data()), "\n")
      if (!is.null(milestone_data())) {
        data <- milestone_data()
        cat("Program:", data$program_info$program_name, "\n")
        cat("Evaluations:", nrow(data$evaluations), "\n")
        cat("Residents:", length(unique(data$evaluations$Resident_Name)), "\n")
      }
      cat("===========================================\n")
    }
  })
  
  # Performance monitoring (development only)
  if (!PRODUCTION_MODE) {
    observe({
      # Monitor reactive execution times
      start_time <- Sys.time()
      
      if (!is.null(milestone_data())) {
        end_time <- Sys.time()
        execution_time <- as.numeric(end_time - start_time)
        
        if (execution_time > 1) {  # Log slow operations
          cat("PERFORMANCE WARNING: Operation took", round(execution_time, 2), "seconds\n")
        }
      }
    })
  }
  
  # Enhanced session cleanup
  session$onSessionEnded(function() {
    if (!PRODUCTION_MODE) {
      cat("=== SESSION CLEANUP ===\n")
      cat("Cleaning up milestone data and reactive values...\n")
    }
    
    # Clean up reactive values
    values$uploaded_data <- NULL
    values$data_loaded <- FALSE
    values$processing <- FALSE
    values$current_operation <- ""
    
    # Clear milestone data
    milestone_data(NULL)
    
    # Clear any temporary files or memory
    gc()  # Garbage collection
    
    if (!PRODUCTION_MODE) {
      cat("Session cleanup completed\n")
      cat("======================\n")
    }
  })
  
  # Enhanced error recovery and monitoring
  observe({
    # Global error monitoring and recovery
    if (!is.null(milestone_data())) {
      tryCatch({
        data <- milestone_data()
        
        # Comprehensive data integrity checks
        if (is.null(data$evaluations) || nrow(data$evaluations) == 0) {
          showNotification(
            HTML(
              '<i class="fas fa-exclamation-triangle"></i> <strong>Data Integrity Issue</strong><br>',
              'Your milestone data appears to be incomplete. Please try reloading your data.'
            ),
            type = "warning",
            duration = 8
          )
        }
        
        # Check for required columns
        required_cols <- c("Resident_Name", "PGY_Level", "Period", "Sub_Competency", "Rating")
        missing_cols <- setdiff(required_cols, names(data$evaluations))
        
        if (length(missing_cols) > 0) {
          showNotification(
            HTML(paste0(
              '<i class="fas fa-exclamation-triangle"></i> <strong>Data Format Issue</strong><br>',
              'Missing required columns: ', paste(missing_cols, collapse = ", "), '<br>',
              'Please check your data format and reload.'
            )),
            type = "error",
            duration = 10
          )
        }
        
        # Check for minimum data requirements
        if (nrow(data$evaluations) > 0) {
          unique_residents <- length(unique(data$evaluations$Resident_Name))
          unique_periods <- length(unique(data$evaluations$Period))
          unique_subcomp <- length(unique(data$evaluations$Sub_Competency))
          
          # Provide contextual guidance based on data characteristics
          if (unique_residents < 3) {
            showNotification(
              HTML(paste0(
                '<i class="fas fa-info-circle"></i> <strong>Limited Dataset Notice</strong><br>',
                'You have ', unique_residents, ' resident(s) in your dataset.<br>',
                'Program-level analyses work best with 3+ residents.'
              )),
              type = "message",
              duration = 6
            )
          }
          
          if (unique_periods < 2) {
            showNotification(
              HTML(paste0(
                '<i class="fas fa-info-circle"></i> <strong>Single Period Data</strong><br>',
                'Your data contains ', unique_periods, ' evaluation period.<br>',
                'Trend analyses require multiple periods for meaningful insights.'
              )),
              type = "message",
              duration = 6
            )
          }
          
          if (unique_subcomp < 10) {
            showNotification(
              HTML(paste0(
                '<i class="fas fa-info-circle"></i> <strong>Limited Milestone Coverage</strong><br>',
                'Your data includes ', unique_subcomp, ' sub-competencies.<br>',
                'Comprehensive analysis benefits from complete milestone sets.'
              )),
              type = "message",
              duration = 6
            )
          }
        }
        
      }, error = function(e) {
        if (!PRODUCTION_MODE) {
          cat("Data integrity check error:", e$message, "\n")
        }
        
        showNotification(
          HTML(paste0(
            '<i class="fas fa-exclamation-triangle"></i> <strong>System Error</strong><br>',
            'An error occurred while checking your data. Please try refreshing the page.'
          )),
          type = "error",
          duration = 8
        )
      })
    }
  })
  
  # Application health check
  observe({
    # Periodic health monitoring (every 30 seconds in development)
    invalidateLater(30000, session)
    
    if (!PRODUCTION_MODE) {
      # Memory usage check
      mem_usage <- gc()
      total_mem <- sum(mem_usage[,"used"])
      
      if (total_mem > 100) {  # More than 100 MB
        cat("MEMORY WARNING: Application using", round(total_mem, 1), "MB\n")
      }
      
      # Session duration check
      session_start <- session$clientData$singletons
      if (!is.null(session_start)) {
        # Log long-running sessions
        cat("Session health check completed\n")
      }
    }
  })
  
  
  
  # ==========================================================================
  # FINAL SUCCESS MESSAGE
  # ==========================================================================
  
  observe({
    # Show success message when everything is initialized
    if (!is.null(milestone_data()) && !values$processing) {
      data <- milestone_data()
      
      # Only show this once per session
      if (!exists("initialization_complete")) {
        showNotification(
          HTML(paste0(
            '<i class="fas fa-check-circle"></i> <strong>Platform Ready!</strong><br>',
            'All systems initialized. You can now explore your milestone data.'
          )),
          type = "message",
          duration = 4
        )
        
        assign("initialization_complete", TRUE, envir = .GlobalEnv)
      }
    }
  })
  
}