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
  
  # Process CSV files
  observeEvent(input$process_csv, {
    req(input$csv_files)
    
    tryCatch({
      # Load the data using your existing function
      data <- load_milestone_csv_data(input$csv_files$datapath)
      milestone_data(data)
      
      # Debug output
      cat("Sample rating values:", head(data$evaluations$Rating, 20), "\n")
      cat("Available PGY levels:", paste(sort(unique(data$evaluations$PGY_Level)), collapse = ", "), "\n")
      
      # Update period choices for specific period selection
      periods <- unique(data$evaluations$Period)
      updateSelectInput(session, "specific_period", choices = setNames(periods, periods))
      
      # Update competency choices
      competencies <- unique(data$evaluations$Competency)
      competency_choices <- c("All Competencies" = "all")
      if (length(competencies) > 0) {
        competency_choices <- c(competency_choices, setNames(competencies, competencies))
      }
      
      updateSelectInput(session, "trend_competency", choices = competency_choices)
      updateSelectInput(session, "individual_competency", choices = competency_choices)
      
      # Update individual period choices
      individual_period_choices <- c("Total (All Periods)" = "total")
      if (length(periods) > 0) {
        individual_period_choices <- c(individual_period_choices, setNames(periods, periods))
      }
      updateSelectInput(session, "individual_period", choices = individual_period_choices)
      
      # Update resident choices
      residents <- sort(unique(data$evaluations$Resident_Name))
      updateSelectInput(session, "selected_resident", choices = residents)
      
      cat("UI updates completed successfully!\n")
      
    }, error = function(e) {
      showNotification(paste("Error processing data:", e$message), type = "error")
      cat("Error:", e$message, "\n")
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
  
  # Helper function to get current period selection
  get_current_period <- reactive({
    if (input$period_selection == "recent_end") {
      return("recent_end")
    } else if (input$period_selection == "recent_mid") {
      return("recent_mid")
    } else if (input$period_selection == "all_periods") {
      return("total")
    } else if (input$period_selection == "specific") {
      return(input$specific_period)
    } else {
      return("recent_end")
    }
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
  
  # Areas for improvement
  output$improvement_areas <- DT::renderDataTable({
    req(milestone_data())
    
    tryCatch({
      if (input$tables_use_filters) {
        # Use the current filter selections
        improvements <- identify_improvement_areas_filtered(
          milestone_data(), 
          n_items = 5,
          period_selection = get_current_period(),
          selected_pgy_levels = selected_pgy_levels()
        )
      } else {
        # Use default end-year behavior
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
  
  # Areas of strength
  output$strength_areas <- DT::renderDataTable({
    req(milestone_data())
    
    tryCatch({
      if (input$tables_use_filters) {
        # Use the current filter selections
        strengths <- identify_strength_areas_filtered(
          milestone_data(), 
          n_items = 5,
          period_selection = get_current_period(),
          selected_pgy_levels = selected_pgy_levels()
        )
      } else {
        # Use default end-year behavior
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
  
  # Program spider plot (updated to use new controls)
  output$program_spider <- renderPlotly({
    req(milestone_data(), selected_pgy_levels())
    
    tryCatch({
      create_multi_level_spider_plot(
        milestone_data(),
        period_type = get_current_period(),
        selected_pgy_levels = selected_pgy_levels(),
        show_medians = input$show_program_means
      )
    }, error = function(e) {
      plot_ly() %>% 
        add_annotations(text = paste("Error:", e$message), 
                        x = 0.5, y = 0.5, showarrow = FALSE)
    })
  })
  
  # Program trend lines
  output$program_trends <- renderPlotly({
    req(milestone_data())
    
    tryCatch({
      create_sequential_trend_plot(
        milestone_data(),
        competency_filter = input$trend_competency
      )
    }, error = function(e) {
      plot_ly() %>% 
        add_annotations(text = paste("Error:", e$message), 
                        x = 0.5, y = 0.5, showarrow = FALSE)
    })
  })
  
  # Program heatmap
  output$program_heatmap <- renderPlotly({
    req(milestone_data())
    
    tryCatch({
      create_performance_heatmap(
        milestone_data(),
        metric = input$heatmap_metric,
        sortable = TRUE
      )
    }, error = function(e) {
      plot_ly() %>% 
        add_annotations(text = paste("Error:", e$message), 
                        x = 0.5, y = 0.5, showarrow = FALSE)
    })
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