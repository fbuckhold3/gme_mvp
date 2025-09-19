# =============================================================================
# R/server.R - Complete Server Logic
# =============================================================================

server <- function(input, output, session) {
  
  # Store loaded data
  milestone_data <- reactiveVal(NULL)
  
  # Data loaded indicator
  output$data_loaded <- reactive({
    !is.null(milestone_data())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Program subtitle
  output$program_subtitle <- renderUI({
    data <- milestone_data()
    if (is.null(data)) {
      p("Upload CSV data to begin", 
        style = "margin: 5px 0 0 0; opacity: 0.9; font-size: 1.1rem;")
    } else {
      p(paste(data$program_info$specialty_name, "Medicine -", data$program_info$program_name), 
        style = "margin: 5px 0 0 0; opacity: 0.9; font-size: 1.1rem;")
    }
  })
  
  # Enhanced CSV processing to populate period choices
  observeEvent(input$process_csv, {
    req(input$csv_files)
    
    tryCatch({
      # Load the data using existing function
      data <- load_milestone_csv_data(input$csv_files$datapath)
      milestone_data(data)
      
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
      
      # Success notification
      showNotification("Data loaded successfully!", type = "message", duration = 5)
      
      # Update competency choices for other components
      competencies <- unique(data$evaluations$Competency)
      competency_choices <- c("All Competencies" = "all")
      if (length(competencies) > 0) {
        competency_choices <- c(competency_choices, setNames(competencies, competencies))
      }
      updateSelectInput(session, "trend_competency", choices = competency_choices)
      
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error", duration = 10)
      print(paste("CSV processing error:", e$message))
    })
  })
  
  
  
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
  

  # Get current period selection for analysis
  # FIND THIS FUNCTION (around line 124) AND REPLACE IT:
  get_current_period <- reactive({
    req(input$period_selection)
    
    switch(input$period_selection,
           "specific" = input$specific_period,
           "recent_end" = "recent_end",
           "recent_mid" = "recent_mid", 
           "all_periods" = "total",  # CHANGED: from "all_periods" to "total"
           "recent_end")  # default fallback
  })
  
  # Get all available periods from loaded data
  get_available_periods <- reactive({
    req(milestone_data())
    periods <- sort(unique(milestone_data()$evaluations$Period))
    return(periods)
  })
  
  # Enhanced graduation readiness data with period breakdown option
  graduation_readiness_data <- reactive({
    req(milestone_data(), input$milestone_threshold)
    
    calculate_graduation_readiness_enhanced(
      data = milestone_data(),
      threshold = input$milestone_threshold,
      period_filter = input$milestone_period,
      competency_filter = input$milestone_competency,
      show_by_period = isTRUE(input$graduation_by_period)
    )
  })
  
  # Enhanced all levels readiness data with PGY selection
  all_levels_data <- reactive({
    req(milestone_data(), input$milestone_threshold)
    
    # Get selected PGY levels, default to all if none selected
    selected_pgy <- input$all_levels_pgy
    if (is.null(selected_pgy) || length(selected_pgy) == 0) {
      selected_pgy <- sort(unique(milestone_data()$evaluations$PGY_Level))
    }
    
    calculate_all_levels_readiness_enhanced(
      data = milestone_data(),
      threshold = input$milestone_threshold,
      period_filter = input$milestone_period,
      competency_filter = input$milestone_competency,
      selected_pgy_levels = selected_pgy
    )
  })
  
  # Enhanced trend data with period-specific comparisons
  trend_data <- reactive({
    req(milestone_data(), input$trend_subcompetency, input$milestone_threshold)
    
    if (is.null(input$trend_subcompetency) || input$trend_subcompetency == "") {
      return(data.frame())
    }
    
    # Get selected periods for trend analysis
    selected_periods <- NULL
    if (!is.null(input$trend_selected_periods) && length(input$trend_selected_periods) > 0) {
      selected_periods <- input$trend_selected_periods
    }
    
    calculate_subcompetency_trends_enhanced(
      data = milestone_data(),
      sub_competency = input$trend_subcompetency,
      threshold = input$milestone_threshold,
      selected_periods = selected_periods
    )
  })
  
  # 3. ADD: New reactive for available periods for trend selection
  available_trend_periods <- reactive({
    req(milestone_data())
    
    periods <- milestone_data()$evaluations %>%
      pull(Period) %>%
      unique() %>%
      sort()
    
    return(periods)
  })
  
  # 4. ADD: Generate period checkboxes for trend analysis
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
      h5("Select Periods to Include:"),
      div(checkbox_list, style = "max-height: 200px; overflow-y: auto;"),
      br(),
      actionButton("select_all_trend_periods", "Select All", size = "sm", style = "margin-right: 10px;"),
      actionButton("deselect_all_trend_periods", "Deselect All", size = "sm")
    )
  })
  
  # 5. ADD: Observer for select/deselect all trend periods
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
  
  # 6. ADD: Reactive to get selected trend periods
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
  
  
  # Update competency and sub-competency choices when data loads
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
  
  
  # Helper function to parse individual period filter
  parse_individual_period <- function(period_filter) {
    if (period_filter == "total") {
      return(list(period = "total", level = "all"))
    } else if (str_detect(period_filter, "\\|\\|\\|")) {
      # Combined period+level filter: "2021-2022 Mid-Year|||PGY-1"
      parts <- str_split(period_filter, "\\|\\|\\|")[[1]]
      return(list(period = parts[1], level = parts[2]))
    } else {
      # Regular period filter
      return(list(period = period_filter, level = "all"))
    }
  }
  
  # Get selected periods for spider plot (for multi-period comparison)
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
  
  
  # Status text
  output$status_text <- renderText({
    if (is.null(input$csv_files)) {
      "No files selected"
    } else if (is.null(milestone_data())) {
      paste("Ready to process", nrow(input$csv_files), "files:",
            paste(input$csv_files$name, collapse = ", "))
    } else {
      "✓ Data processing completed successfully!"
    }
  })
  
  # Quick summary
  output$quick_summary <- renderUI({
    data <- milestone_data()
    if (is.null(data)) {
      p("No data loaded")
    } else {
      HTML(paste0(
        "<strong>Program:</strong> ", data$program_info$program_name, "<br>",
        "<strong>Specialty:</strong> ", data$program_info$specialty_name, "<br>",
        "<strong>Residents:</strong> ", data$summary$n_residents, "<br>",
        "<strong>Sub-Competencies:</strong> ", data$summary$n_milestones, "<br>",
        "<strong>Evaluation Records:</strong> ", data$summary$n_evaluations, "<br>",
        "<strong>Assessment Periods:</strong> ", data$summary$n_periods, "<br>",
        "<strong>Training Levels:</strong> ", paste(data$summary$training_levels, collapse = ", ")
      ))
    }
  })
  
  # =========================================================================
  # PROGRAM OVERVIEW OUTPUTS
  # =========================================================================
  
  # Areas for improvement table
  output$improvement_areas <- DT::renderDataTable({
    req(milestone_data())
    
    tryCatch({
      if (input$tables_use_filters) {
        improvements <- identify_improvement_areas_filtered(
          milestone_data(), 
          n_items = 5,
          period_selection = get_current_period(),
          selected_pgy_levels = selected_pgy_levels()
        )
      } else {
        improvements <- identify_improvement_areas(
          milestone_data(), 
          n_items = 5, 
          focus_end_year = TRUE
        )
      }
      improvements
    }, error = function(e) {
      data.frame(Error = paste("Error:", e$message))
    })
  }, options = list(pageLength = 5, dom = 't', scrollX = TRUE))
  
  # Areas of strength table  
  output$strength_areas <- DT::renderDataTable({
    req(milestone_data())
    
    tryCatch({
      if (input$tables_use_filters) {
        strengths <- identify_strength_areas_filtered(
          milestone_data(), 
          n_items = 5,
          period_selection = get_current_period(),
          selected_pgy_levels = selected_pgy_levels()
        )
      } else {
        strengths <- identify_strength_areas(
          milestone_data(), 
          n_items = 5, 
          focus_end_year = TRUE
        )
      }
      strengths
    }, error = function(e) {
      data.frame(Error = paste("Error:", e$message))
    })
  }, options = list(pageLength = 5, dom = 't', scrollX = TRUE))
  
  # Dynamic descriptions for the tables
  output$improvement_description <- renderUI({
    if (input$tables_use_filters) {
      period_text <- switch(get_current_period(),
                            "recent_end" = "Most Recent End-Year",
                            "recent_mid" = "Most Recent Mid-Year", 
                            "total" = "All Periods",
                            get_current_period())
      
      pgy_text <- if (length(selected_pgy_levels()) == length(sort(unique(milestone_data()$evaluations$PGY_Level)))) {
        "All PGY Levels"
      } else {
        paste(selected_pgy_levels(), collapse = ", ")
      }
      
      HTML(paste0("<small><strong>Based on:</strong> ", period_text, " • ", pgy_text, "</small>"))
    } else {
      HTML("<small><strong>Based on:</strong> End-Year evaluations across all PGY levels</small>")
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
        "All PGY Levels"
      } else {
        paste(selected_pgy_levels(), collapse = ", ")
      }
      
      HTML(paste0("<small><strong>Based on:</strong> ", period_text, " • ", pgy_text, "</small>"))
    } else {
      HTML("<small><strong>Based on:</strong> End-Year evaluations across all PGY levels</small>")
    }
  })
  
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
      "All PGY Levels"
    } else {
      paste("PGY Levels:", paste(selected_pgy_levels(), collapse = ", "))
    }
    
    HTML(paste0("<small><strong>Analysis Based On:</strong> ", period_text, " • ", pgy_text, "</small>"))
  })
  
  
  # Enhanced program spider plot with multi-period support
  # Enhanced program spider plot with multi-period support
  output$program_spider <- renderPlotly({
    req(milestone_data(), selected_pgy_levels())
    
    spider_periods <- get_spider_periods()
    
    if (length(spider_periods) == 1) {
      # Single period - use existing function
      create_multi_level_spider_plot(  # <-- This function exists
        milestone_data(),
        period_type = spider_periods[1],
        selected_pgy_levels = selected_pgy_levels(),
        show_medians = input$show_program_means
      )
    } else {
      # Multiple periods - use new multi-period function
      create_multi_period_spider_plot(  # <-- This function does NOT exist
        milestone_data(),
        period_types = spider_periods,
        selected_pgy_levels = selected_pgy_levels(),
        show_medians = input$show_program_means
      )
    }
  })
  
  # Milestone reference table
  # Milestone reference table
  output$milestone_reference_table <- DT::renderDataTable({
    req(milestone_data())
    
    tryCatch({
      create_milestone_reference_table(milestone_data())
    }, error = function(e) {
      data.frame(Error = paste("Error:", e$message))
    })
  }, options = list(
    pageLength = 25,
    dom = 'ft',
    scrollX = FALSE,
    autoWidth = FALSE,
    columnDefs = list(
      list(width = '60px', targets = 0, className = 'dt-center'),  # Code column - very narrow
      list(width = 'calc(100% - 60px)', targets = 1)              # Description takes remaining
    ),
    initComplete = JS(
      "function(settings, json) {",
      "  $(this.api().table().container()).find('th:eq(0)').css('width', '60px');",
      "  $(this.api().table().container()).find('th:eq(1)').css('width', 'calc(100% - 60px)');",
      "}"
    ),
    rowCallback = JS("
    function(row, data) {
      $(row).find('td').css({'border': 'none', 'padding': '0px'});
      $(row).find('td:eq(0)').css({'width': '60px', 'max-width': '60px'});
    }")
  ), escape = FALSE)
  
  # =========================================================================
  # MILESTONE ANALYSIS
  # ========================================================================= 
  
  # =========================================================================
  # MILESTONE ANALYSIS OUTPUTS - CHARTS
  # =========================================================================
  
  # Enhanced graduation readiness chart
  output$graduation_readiness_chart <- renderPlotly({
    req(graduation_readiness_data())
    
    tryCatch({
      # Use appropriate chart based on whether breaking down by period
      if (isTRUE(input$graduation_by_period) && input$milestone_period == "all") {
        # Create period-breakdown chart - modify existing chart for period breakdown
        readiness_data <- graduation_readiness_data()
        
        # Create a modified chart that groups by period
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
        
        fig
      } else {
        # Use standard graduation chart
        create_graduation_readiness_chart(graduation_readiness_data())
      }
    }, error = function(e) {
      plot_ly() %>% 
        add_annotations(
          text = paste("Error creating graduation chart:", e$message), 
          x = 0.5, y = 0.5, 
          showarrow = FALSE,
          font = list(size = 16, color = "red")
        )
    })
  })
  
  # Enhanced all levels chart
  output$all_levels_chart <- renderPlotly({
    req(all_levels_data())
    
    tryCatch({
      # Use the enhanced create function (need to create this)
      create_all_levels_chart(all_levels_data())
    }, error = function(e) {
      plot_ly() %>% 
        add_annotations(
          text = paste("Error creating all levels chart:", e$message), 
          x = 0.5, y = 0.5, 
          showarrow = FALSE,
          font = list(size = 16, color = "red")
        )
    })
  })
  
  # Enhanced sub-competency trend chart
  output$subcompetency_trend_chart <- renderPlotly({
    req(trend_data())
    
    if (nrow(trend_data()) == 0) {
      return(plot_ly() %>% 
               add_annotations(
                 text = "No trend data available for selected sub-competency", 
                 x = 0.5, y = 0.5, 
                 showarrow = FALSE,
                 font = list(size = 14, color = "#666")
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
                   text = "No data available for selected periods", 
                   x = 0.5, y = 0.5, 
                   showarrow = FALSE,
                   font = list(size = 14, color = "#666")
                 ))
      }
      
      # Create chart with option to show/hide total average
      show_total <- if(is.null(input$show_total_average)) TRUE else input$show_total_average
      create_trend_chart_enhanced(filtered_trend_data, show_total_average = show_total)
      
    }, error = function(e) {
      plot_ly() %>% 
        add_annotations(
          text = paste("Error creating trend chart:", e$message), 
          x = 0.5, y = 0.5, 
          showarrow = FALSE,
          font = list(size = 16, color = "red")
        )
    })
  })
  
  
  # Update cohort choices when data loads
  observeEvent(milestone_data(), {
    req(milestone_data())
    
    # Update sub-competency choices
    subcompetencies <- sort(unique(milestone_data()$evaluations$Sub_Competency))
    updateSelectInput(session, "trend_subcompetency", 
                      choices = setNames(subcompetencies, subcompetencies),
                      selected = subcompetencies[1])
    
    # Calculate and update cohort choices
    cohort_info <- calculate_cohort_information(milestone_data())
    available_cohorts <- cohort_info %>%
      count(cohort_label, sort = TRUE) %>%
      pull(cohort_label)
    
    updateCheckboxGroupInput(session, "selected_cohorts",
                             choices = setNames(available_cohorts, available_cohorts),
                             selected = head(available_cohorts, 3))  # Select top 3 by default
  })
  
  # Cohort selection buttons
  observeEvent(milestone_data(), {
    req(milestone_data())
    
    # Update sub-competency choices
    subcompetencies <- sort(unique(milestone_data()$evaluations$Sub_Competency))
    updateSelectInput(session, "trend_subcompetency", 
                      choices = setNames(subcompetencies, subcompetencies),
                      selected = subcompetencies[1])
    
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
  })
  
  # Cohort selection buttons
  observeEvent(input$select_recent_cohorts, {
    req(milestone_data())
    cohort_info <- calculate_cohort_information(milestone_data())
    recent_cohorts <- cohort_info %>%
      count(cohort_label, sort = TRUE) %>%
      mutate(graduation_year = as.numeric(str_extract(cohort_label, "\\d{4}"))) %>%
      arrange(desc(graduation_year)) %>%  # Most recent first
      head(2) %>%
      pull(cohort_label)
    updateCheckboxGroupInput(session, "selected_cohorts", selected = recent_cohorts)
  })
  
  observeEvent(input$clear_cohorts, {
    updateCheckboxGroupInput(session, "selected_cohorts", selected = character(0))
  })
  
  # Cohort trend plot - now defaults to program baseline only
  output$cohort_trend_plot <- renderPlotly({
    req(milestone_data(), input$trend_subcompetency)
    
    create_cohort_trend_analysis(
      milestone_data(),
      selected_sub_competency = input$trend_subcompetency,
      selected_cohorts = input$selected_cohorts  # Can be NULL or empty
    )
  })
  
  # =========================================================================
  # MILESTONE ANALYSIS OUTPUTS - TABLES
  # =========================================================================
  
  output$graduation_table <- DT::renderDataTable({
    req(graduation_readiness_data())
    
    tryCatch({
      table_data <- graduation_readiness_data()
      
      if ("Period" %in% names(table_data)) {
        # Period breakdown table
        table_data %>%
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
        table_data %>%
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
    }, error = function(e) {
      data.frame(
        Error = paste("Error creating graduation table:", e$message)
      )
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
      all_levels_data() %>%
        select(
          `Sub-Competency` = Sub_Competency,
          `PGY Level` = PGY_Level,
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
    }, error = function(e) {
      data.frame(
        Error = paste("Error creating all levels table:", e$message)
      )
    })
  }, options = list(
    pageLength = 15,
    scrollX = TRUE,
    order = list(list(1, 'asc'), list(3, 'desc')),
    columnDefs = list(
      list(className = 'dt-center', targets = c(1, 3, 5, 6, 7))
    )
  ))
  
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
      
      filtered_trend_data %>%
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
    }, error = function(e) {
      data.frame(
        Error = paste("Error creating trend table:", e$message)
      )
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
  # INDIVIDUAL ASSESSMENT INTEGRATION
  # =========================================================================
  
  # Update resident choices when data changes
  observeEvent(milestone_data(), {
    if (!is.null(milestone_data()) && !is.null(milestone_data()$evaluations)) {
      residents <- sort(unique(milestone_data()$evaluations$Resident_Name))
      residents <- residents[!is.na(residents) & residents != ""]
      updateSelectInput(session, "individual_resident", 
                        choices = setNames(residents, residents))
    }
  })
  
  # Update level choices when resident changes
  observeEvent(input$individual_resident, {
    req(milestone_data(), input$individual_resident)
    
    resident_data <- milestone_data()$evaluations[milestone_data()$evaluations$Resident_Name == input$individual_resident, ]
    
    if (nrow(resident_data) > 0) {
      level_combinations <- resident_data %>%
        select(Period, PGY_Level) %>%
        distinct() %>%
        arrange(desc(PGY_Level), Period) %>%
        mutate(
          display_name = paste0(PGY_Level, " - ", Period),
          value = paste0(Period, "|||", PGY_Level)
        )
      
      level_choices <- setNames(level_combinations$value, level_combinations$display_name)
      level_choices <- c("All Evaluations" = "all", level_choices)
      
      updateSelectInput(session, "individual_level", 
                        choices = level_choices,
                        selected = level_choices[2])
    }
  })
  
  # Summary statistics reactive
  individual_stats <- reactive({
    req(milestone_data(), input$individual_resident, input$individual_level)
    
    resident_data <- milestone_data()$evaluations[milestone_data()$evaluations$Resident_Name == input$individual_resident, ]
    
    if (input$individual_level != "all") {
      level_parts <- strsplit(input$individual_level, "\\|\\|\\|")[[1]]
      if (length(level_parts) == 2) {
        resident_data <- resident_data[resident_data$Period == level_parts[1] & 
                                         resident_data$PGY_Level == level_parts[2], ]
      }
    }
    
    if (nrow(resident_data) == 0) {
      return(list(total_evaluations = 0, individual_avg = 0, program_avg = 0, percentile = 0))
    }
    
    individual_avg <- round(mean(resident_data$Rating, na.rm = TRUE), 2)
    
    program_data <- milestone_data()$evaluations
    if (input$individual_level != "all") {
      level_parts <- strsplit(input$individual_level, "\\|\\|\\|")[[1]]
      if (length(level_parts) == 2) {
        program_data <- program_data[program_data$Period == level_parts[1] & 
                                       program_data$PGY_Level == level_parts[2], ]
      }
    }
    
    program_avg <- round(mean(program_data$Rating, na.rm = TRUE), 2)
    
    all_residents <- program_data %>%
      group_by(Resident_Name) %>%
      summarise(resident_avg = mean(Rating, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(resident_avg))
    
    percentile <- round(100 * mean(all_residents$resident_avg <= individual_avg, na.rm = TRUE), 0)
    
    return(list(
      total_evaluations = nrow(resident_data),
      individual_avg = individual_avg,
      program_avg = program_avg,
      percentile = percentile
    ))
  })
  
  # Individual assessment outputs
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
  
  # Add this after line 673 in your server.R, in the individual assessment section:
  output$resident_info_display <- renderUI({
    req(milestone_data(), input$individual_resident, input$individual_level)
    
    if (is.null(input$individual_level) || input$individual_level == "all") {
      info_text <- "Showing all evaluations across all periods"
    } else {
      level_parts <- strsplit(input$individual_level, "\\|\\|\\|")[[1]]
      if (length(level_parts) == 2) {
        info_text <- paste("Evaluation Period:", level_parts[1], "<br>",
                           "PGY Level:", level_parts[2])
      } else {
        info_text <- "Selected evaluation level"
      }
    }
    
    HTML(paste0("<small><strong>Current Selection:</strong><br>", info_text, "</small>"))
  })
  
  # Add this too in the individual assessment section:
  output$individual_detail_table <- DT::renderDataTable({
    req(milestone_data(), input$individual_resident, input$individual_level)
    
    # Simple version for now - you can enhance this later
    resident_data <- milestone_data()$evaluations[milestone_data()$evaluations$Resident_Name == input$individual_resident, ]
    
    if (nrow(resident_data) == 0) {
      return(data.frame(Message = "No data available for selected resident"))
    }
    
    # Basic summary by sub-competency
    detail_summary <- resident_data %>%
      group_by(Competency, Sub_Competency) %>%
      summarise(
        Mean_Score = round(mean(Rating, na.rm = TRUE), 2),
        Evaluations = n(),
        Score_Range = paste0(min(Rating, na.rm = TRUE), "-", max(Rating, na.rm = TRUE)),
        .groups = "drop"
      )
    
    detail_summary
  }, options = list(pageLength = 15, scrollX = TRUE, scrollY = "400px"))

  

  # Additional outputs would go here...
  
  # =========================================================================
  # DEBUG OUTPUT (OPTIONAL - REMOVE IN PRODUCTION)
  # =========================================================================
  
  # Add debug output to console to help with troubleshooting
  observe({
    if (!is.null(milestone_data()) && !is.null(input$milestone_threshold)) {
      
      # Find max PGY for graduation analysis
      max_pgy <- milestone_data()$evaluations %>%
        pull(PGY_Level) %>%
        str_extract("\\d+") %>%
        as.numeric() %>%
        max(na.rm = TRUE)
      
      cat("\n=== MILESTONE ANALYSIS DEBUG ===\n")
      cat("Threshold:", input$milestone_threshold, "\n")
      cat("Max PGY Level (Graduating Class):", paste0("PGY-", max_pgy), "\n")
      cat("Available Sub-Competencies:", paste(sort(unique(milestone_data()$evaluations$Sub_Competency)), collapse = ", "), "\n")
      cat("Selected Sub-Competency for Trends:", ifelse(is.null(input$trend_subcompetency) || input$trend_subcompetency == "", "None", input$trend_subcompetency), "\n")
      
      if (!is.null(graduation_readiness_data()) && nrow(graduation_readiness_data()) > 0) {
        cat("Graduation Data Rows:", nrow(graduation_readiness_data()), "\n")
        cat("Risk Distribution:", table(graduation_readiness_data()$readiness_category), "\n")
      }
      
      if (!is.null(all_levels_data()) && nrow(all_levels_data()) > 0) {
        cat("All Levels Data Rows:", nrow(all_levels_data()), "\n")
        cat("PGY Levels in Analysis:", paste(sort(unique(all_levels_data()$PGY_Level)), collapse = ", "), "\n")
      }
      
      cat("================================\n")
    }
  })
  
  # =========================================================================
  # INDIVIDUAL RESIDENT OUTPUTS
  # =========================================================================
  
  # Individual summary table
  output$individual_summary <- DT::renderDataTable({
    req(milestone_data(), input$selected_resident)
    
    tryCatch({
      # Parse the period filter
      period_info <- parse_individual_period(input$individual_period)
      
      # For now, use just the period part for the summary table
      summary_table <- create_individual_summary_table(
        milestone_data(),
        resident_name = input$selected_resident,
        period_filter = period_info$period
      )
      summary_table
    }, error = function(e) {
      data.frame(Error = paste("Error:", e$message))
    })
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Individual spider plot
  output$individual_spider <- renderPlotly({
    req(milestone_data(), input$selected_resident)
    
    tryCatch({
      period_info <- parse_individual_period(input$individual_period)
      
      create_individual_spider_plot(
        milestone_data(),
        resident_name = input$selected_resident,
        period_filter = period_info$period
      )
    }, error = function(e) {
      plot_ly() %>% 
        add_annotations(text = paste("Error:", e$message), 
                        x = 0.5, y = 0.5, showarrow = FALSE)
    })
  })
  
  # Individual trend lines
  output$individual_trends <- renderPlotly({
    req(milestone_data(), input$selected_resident)
    
    tryCatch({
      create_individual_trend_plot(
        milestone_data(),
        resident_name = input$selected_resident,
        competency_filter = input$individual_competency
      )
    }, error = function(e) {
      plot_ly() %>% 
        add_annotations(text = paste("Error:", e$message), 
                        x = 0.5, y = 0.5, showarrow = FALSE)
    })
  })
  
  # Peer comparison
  output$peer_comparison <- renderPlotly({
    req(milestone_data(), input$selected_resident)
    
    tryCatch({
      period_info <- parse_individual_period(input$individual_period)
      
      create_individual_peer_comparison(
        milestone_data(),
        resident_name = input$selected_resident,
        period_filter = period_info$period
      )
    }, error = function(e) {
      plot_ly() %>% 
        add_annotations(text = paste("Error:", e$message), 
                        x = 0.5, y = 0.5, showarrow = FALSE)
    })
  })
  
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
}