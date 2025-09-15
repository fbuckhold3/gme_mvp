# =============================================================================
# GME MILESTONE VISUALIZATION PROJECT - SERVER (CSV ONLY VERSION)
# R/server.R
# =============================================================================

server <- function(input, output, session) {
  
  # ========================================================================
  # REACTIVE VALUES
  # ========================================================================
  
  processed_data <- reactiveVal(NULL)
  median_results <- reactiveVal(NULL)
  residents_data <- reactiveVal(NULL)
  
  # ========================================================================
  # DATA PROCESSING (Using Your Existing Functions)
  # ========================================================================
  
  # CSV upload status
  output$csv_status <- renderUI({
    if (is.null(input$csv_files)) {
      tags$span(icon("info-circle"), " No files uploaded", class = "text-muted")
    } else {
      tags$span(icon("check-circle"), 
                paste(nrow(input$csv_files), "file(s) uploaded"), 
                class = "text-success")
    }
  })
  
  # =============================================================================
  # GENERIC SERVER UPDATE - WORKS WITH ANY ACGME MILESTONE DATA
  # Replace your observeEvent(input$process_csv) section with this
  # =============================================================================
  
  # Process CSV data using GENERIC functions (works for any specialty)
  observeEvent(input$process_csv, {
    req(input$csv_files)
    
    showNotification("Processing milestone CSV data...", 
                     type = "message", duration = NULL, id = "processing")
    
    tryCatch({
      
      # Use the GENERIC processing function
      processed_results <- import_and_process_milestones_generic(input$csv_files$datapath)
      
      # Store the results
      processed_data(processed_results$milestone_data)
      residents_data(processed_results$residents)
      
      # Calculate medians
      medians <- calculate_milestone_medians(processed_results, verbose = TRUE)
      median_results(medians)
      
      # Create summary for display
      summary_info <- create_milestone_summary(processed_results)
      
      # Remove processing notification
      removeNotification("processing")
      
      # Create detailed success message
      success_msg <- paste0(
        "Data loaded successfully! ",
        "Loaded ", summary_info$n_residents, " residents, ",
        summary_info$n_evaluations, " evaluations, ",
        summary_info$n_milestones, " sub-competencies"
      )
      
      showNotification(success_msg, type = "success", duration = 5)
      
      # Log summary to console
      cat("\n=== DATA PROCESSING SUMMARY ===\n")
      cat("Residents:", summary_info$n_residents, "\n")
      cat("Evaluations:", summary_info$n_evaluations, "\n") 
      cat("Sub-competencies:", summary_info$n_milestones, "\n")
      cat("Assessment periods:", summary_info$n_periods, "\n")
      cat("Training levels:", summary_info$n_levels, "\n")
      cat("Competency categories:", paste(summary_info$competency_categories, collapse = ", "), "\n")
      cat("Data completeness:", summary_info$data_completeness, "%\n")
      cat("===============================\n")
      
    }, error = function(e) {
      removeNotification("processing")
      error_msg <- paste("Error processing data:", e$message)
      showNotification(error_msg, type = "error", duration = 10)
      cat("Processing error:", e$message, "\n")
      print(traceback())
    })
  })
  
  # Update the data summary output (generic version)
  output$data_summary <- renderText({
    if (!is.null(processed_data()) && !is.null(residents_data())) {
      processed_results <- list(
        milestone_data = processed_data(),
        residents = residents_data(),
        milestone_columns = grep("^(PC|MK|ICS|SBP|PROF|PBL|PBLI)\\d+", 
                                 names(processed_data()), value = TRUE)
      )
      
      summary_info <- create_milestone_summary(processed_results)
      
      paste0(
        summary_info$n_residents, " residents, ",
        summary_info$n_evaluations, " evaluations, ", 
        summary_info$n_milestones, " sub-competencies"
      )
    } else {
      ""
    }
  })
  
  # Add a reactive to check if data is loaded
  output$data_loaded <- reactive({
    !is.null(processed_data()) && !is.null(residents_data())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Update the data summary output
  output$data_summary <- renderText({
    if (!is.null(processed_data()) && !is.null(residents_data())) {
      processed_results <- list(
        milestone_data = processed_data(),
        residents = residents_data(),
        milestone_columns = grep("^(PC|MK|SBP|PBL|PROF|ICS)\\d+", 
                                 names(processed_data()), value = TRUE)
      )
      
      summary_info <- create_imslu_summary(processed_results)
      
      paste0(
        summary_info$n_residents, " residents, ",
        summary_info$n_evaluations, " evaluations, ", 
        summary_info$n_milestones, " sub-competencies"
      )
    } else {
      ""
    }
  })
  
  # Add a reactive to check if data is loaded (for conditional panels)
  output$data_loaded <- reactive({
    !is.null(processed_data()) && !is.null(residents_data())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Helper function to create residents lookup
  create_residents_lookup <- function(data) {
    if ("Resident.ID" %in% names(data) && "First.Name" %in% names(data)) {
      # Use the structure from your existing functions
      residents <- data %>%
        select(any_of(c("Resident.ID", "First.Name", "Last.Name", "Resident.Year"))) %>%
        distinct() %>%
        filter(!is.na(Resident.ID)) %>%
        mutate(
          name = paste(coalesce(First.Name, ""), coalesce(Last.Name, "")),
          record_id = as.character(Resident.ID),
          Level = as.character(Resident.Year)
        ) %>%
        select(record_id, name, Level) %>%
        filter(name != " ") %>%  # Remove empty names
        arrange(name)
      
      return(residents)
    }
    return(data.frame())
  }
  
  # Helper function to update UI choices
  update_ui_choices <- function(data, medians) {
    # Update period choices from your data structure
    if ("period" %in% names(data)) {
      periods <- unique(data$period)
      period_choices <- c("Total (All Periods)" = "total", setNames(periods, periods))
      
      updateSelectInput(session, "program_period", choices = period_choices)
      updateSelectInput(session, "resident_period", choices = period_choices)
    }
    
    # Update resident choices
    residents <- residents_data()
    if (!is.null(residents) && nrow(residents) > 0) {
      resident_choices <- setNames(residents$record_id, residents$name)
      updateSelectInput(session, "selected_resident", choices = resident_choices)
    }
    
    # Update training year choices
    if ("Resident.Year" %in% names(data)) {
      years <- sort(unique(data$Resident.Year))
      year_choices <- c("All Years" = "all", setNames(paste("PGY", years), years))
      updateSelectInput(session, "program_level", choices = year_choices)
    }
    
    # Update milestone choices from your median results
    if (!is.null(medians) && "overall_medians" %in% names(medians)) {
      milestones <- medians$overall_medians$Sub_Competency
      milestone_choices <- setNames(milestones, milestones)
      updateSelectInput(session, "selected_milestone", choices = milestone_choices)
    }
  }
  
  # Data loaded flag
  output$data_loaded <- reactive({
    !is.null(processed_data()) && !is.null(median_results())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Data summary
  output$data_summary <- renderText({
    req(processed_data(), residents_data())
    
    data <- processed_data()
    residents <- residents_data()
    medians <- median_results()
    
    paste0("Loaded ", nrow(residents), " residents, ",
           nrow(data), " evaluations, ",
           length(medians$milestone_columns), " sub-competencies")
  })
  
  # ========================================================================
  # PROGRAM OVERVIEW OUTPUTS (Using Your Existing Functions)
  # ========================================================================
  
  # Program strengths table
  output$strengths_table <- DT::renderDataTable({
    req(processed_data(), median_results())
    
    medians <- median_results()
    
    # Use your existing median structure to create strengths
    strengths <- medians$overall_medians %>%
      arrange(desc(Overall_Median)) %>%
      head(5) %>%
      mutate(
        `Sub-Competency` = Sub_Competency,
        `Median Score` = round(Overall_Median, 1),
        Category = Category
      ) %>%
      select(`Sub-Competency`, Category, `Median Score`)
    
    return(strengths)
  }, options = list(pageLength = 5, searching = FALSE, dom = 't'))
  
  # Program improvements table
  output$improvements_table <- DT::renderDataTable({
    req(processed_data(), median_results())
    
    medians <- median_results()
    
    # Use your existing median structure to identify areas for improvement
    improvements <- medians$overall_medians %>%
      arrange(Overall_Median) %>%
      head(5) %>%
      mutate(
        `Sub-Competency` = Sub_Competency,
        `Median Score` = round(Overall_Median, 1),
        Category = Category
      ) %>%
      select(`Sub-Competency`, Category, `Median Score`)
    
    return(improvements)
  }, options = list(pageLength = 5, searching = FALSE, dom = 't'))
  
  # Program spider plot using your existing functions
  output$program_spider <- renderPlotly({
    req(median_results())
    
    medians <- median_results()
    
    # Use your existing median calculations for spider plot
    spider_data <- medians$overall_medians %>%
      group_by(Category) %>%
      summarise(avg_median = mean(Overall_Median, na.rm = TRUE), .groups = "drop") %>%
      arrange(Category)
    
    if (nrow(spider_data) == 0) {
      return(plotly::plot_ly() %>% 
               plotly::add_annotations(text = "No data available", 
                                       x = 0.5, y = 0.5, showarrow = FALSE))
    }
    
    # Create spider plot
    plot_ly(
      type = 'scatterpolar',
      r = spider_data$avg_median,
      theta = spider_data$Category,
      fill = 'toself',
      fillcolor = 'rgba(52, 152, 219, 0.3)',
      line = list(color = 'rgb(52, 152, 219)', width = 3),
      marker = list(size = 8, color = 'rgb(52, 152, 219)'),
      hovertemplate = '<b>%{theta}</b><br>Average Median: %{r}<extra></extra>'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(1, 9),
            tickmode = 'linear',
            tick0 = 1,
            dtick = 1
          ),
          angularaxis = list(
            tickfont = list(size = 12)
          )
        ),
        title = "Program Competency Profile",
        showlegend = FALSE
      )
  })
  
  # Competency trends using your existing median calculations
  output$competency_trends <- renderPlotly({
    req(median_results())
    
    medians <- median_results()
    
    # Use your period_year_medians for trends if available
    if ("period_year_medians" %in% names(medians)) {
      trend_data <- medians$period_year_medians %>%
        group_by(Category, Training_Year) %>%
        summarise(avg_median = mean(Period_Year_Median, na.rm = TRUE), .groups = "drop") %>%
        filter(!is.na(avg_median))
      
      if (nrow(trend_data) > 0) {
        plot_ly(trend_data, x = ~Training_Year, y = ~avg_median, 
                color = ~Category, type = 'scatter', mode = 'lines+markers',
                hovertemplate = '<b>%{data.name}</b><br>Year: %{x}<br>Average Median: %{y}<extra></extra>') %>%
          layout(
            title = "Competency Progression by Training Year",
            xaxis = list(title = "Training Year"),
            yaxis = list(title = "Average Median Score", range = c(1, 9)),
            hovermode = 'closest'
          )
      } else {
        plotly::plot_ly() %>% 
          plotly::add_annotations(text = "No trend data available", 
                                  x = 0.5, y = 0.5, showarrow = FALSE)
      }
    } else {
      plotly::plot_ly() %>% 
        plotly::add_annotations(text = "No trend data available", 
                                x = 0.5, y = 0.5, showarrow = FALSE)
    }
  })
  
  # Milestone heatmap using your existing comprehensive medians
  output$milestone_heatmap <- renderPlotly({
    req(median_results())
    
    medians <- median_results()
    metric <- input$heatmap_metric
    
    # Select the appropriate median data based on metric
    heatmap_data <- switch(metric,
                           "overall" = medians$overall_medians,
                           "period" = medians$period_medians,
                           "year" = medians$year_medians,
                           medians$overall_medians
    )
    
    if (is.null(heatmap_data) || nrow(heatmap_data) == 0) {
      return(plotly::plot_ly() %>% 
               plotly::add_annotations(text = "No heatmap data available", 
                                       x = 0.5, y = 0.5, showarrow = FALSE))
    }
    
    # Create heatmap matrix
    if (metric == "overall" && "Sub_Competency" %in% names(heatmap_data)) {
      heatmap_matrix <- heatmap_data %>%
        select(Category, Sub_Competency, Overall_Median) %>%
        mutate(Sub_Comp_Num = as.numeric(gsub("^[A-Z]+", "", Sub_Competency))) %>%
        arrange(Category, Sub_Comp_Num) %>%
        pivot_wider(names_from = Sub_Competency, values_from = Overall_Median)
      
      categories <- heatmap_matrix$Category
      heatmap_matrix <- heatmap_matrix %>% select(-Category)
      milestones <- colnames(heatmap_matrix)
      
      plot_ly(
        z = as.matrix(heatmap_matrix),
        x = milestones,
        y = categories,
        type = "heatmap",
        colorscale = list(
          c(0, "red"),
          c(0.5, "yellow"), 
          c(1, "green")
        ),
        hovertemplate = '<b>%{y}</b><br>%{x}<br>Median: %{z}<extra></extra>'
      ) %>%
        layout(
          title = "Sub-Competency Performance Heatmap",
          xaxis = list(title = "Sub-Competencies", tickangle = -45),
          yaxis = list(title = "Competency Categories")
        )
    } else {
      plotly::plot_ly() %>% 
        plotly::add_annotations(text = "Heatmap not available for this metric", 
                                x = 0.5, y = 0.5, showarrow = FALSE)
    }
  })
  
  # ========================================================================
  # INDIVIDUAL RESIDENT OUTPUTS
  # ========================================================================
  
  # Resident information
  output$resident_info <- renderUI({
    req(residents_data(), input$selected_resident)
    
    residents <- residents_data()
    resident <- residents %>% 
      filter(record_id == input$selected_resident)
    
    if (nrow(resident) == 0) return(p("Resident not found"))
    
    tagList(
      p(tags$strong("Name: "), resident$name),
      p(tags$strong("Training Year: "), paste("PGY-", resident$Level)),
      p(tags$strong("Record ID: "), resident$record_id),
      p(tags$strong("Evaluations: "), get_resident_eval_count())
    )
  })
  
  # Helper function to get evaluation count for selected resident
  get_resident_eval_count <- function() {
    req(processed_data(), input$selected_resident)
    
    data <- processed_data()
    if ("Resident.ID" %in% names(data)) {
      count <- data %>%
        filter(Resident.ID == input$selected_resident) %>%
        nrow()
      return(count)
    }
    return("Unknown")
  }
  
  # Resident spider plot
  output$resident_spider <- renderPlotly({
    req(processed_data(), median_results(), input$selected_resident)
    
    data <- processed_data()
    medians <- median_results()
    residents <- residents_data()
    
    # Get resident data
    resident_data <- data %>%
      filter(Resident.ID == input$selected_resident)
    
    if (nrow(resident_data) == 0) {
      return(plotly::plot_ly() %>% 
               plotly::add_annotations(text = "No data available for this resident", 
                                       x = 0.5, y = 0.5, showarrow = FALSE))
    }
    
    # Get milestone columns from your data structure
    milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)\\d+$", names(resident_data), value = TRUE)
    
    if (length(milestone_cols) == 0) {
      return(plotly::plot_ly() %>% 
               plotly::add_annotations(text = "No milestone columns found", 
                                       x = 0.5, y = 0.5, showarrow = FALSE))
    }
    
    # Calculate resident competency scores
    resident_scores <- resident_data %>%
      select(all_of(milestone_cols)) %>%
      pivot_longer(everything(), names_to = "milestone", values_to = "score") %>%
      filter(!is.na(score)) %>%
      mutate(competency = substr(milestone, 1, 2)) %>%
      group_by(competency) %>%
      summarise(resident_score = median(score, na.rm = TRUE), .groups = "drop")
    
    # Calculate program medians by competency
    program_medians <- medians$overall_medians %>%
      group_by(Category) %>%
      summarise(program_median = mean(Overall_Median, na.rm = TRUE), .groups = "drop") %>%
      rename(competency = Category)
    
    # Combine data
    spider_data <- resident_scores %>%
      left_join(program_medians, by = "competency") %>%
      arrange(competency)
    
    if (nrow(spider_data) == 0) {
      return(plotly::plot_ly() %>% 
               plotly::add_annotations(text = "No competency data", 
                                       x = 0.5, y = 0.5, showarrow = FALSE))
    }
    
    # Create spider plot with both resident and program data
    plot_ly() %>%
      add_trace(
        type = 'scatterpolar',
        r = spider_data$program_median,
        theta = spider_data$competency,
        fill = 'toself',
        fillcolor = 'rgba(169, 169, 169, 0.2)',
        line = list(color = 'rgba(169, 169, 169, 0.8)', width = 2, dash = 'dash'),
        name = 'Program Median',
        hovertemplate = '<b>%{theta}</b><br>Program Median: %{r}<extra></extra>'
      ) %>%
      add_trace(
        type = 'scatterpolar', 
        r = spider_data$resident_score,
        theta = spider_data$competency,
        fill = 'toself',
        fillcolor = 'rgba(52, 152, 219, 0.3)',
        line = list(color = 'rgb(52, 152, 219)', width = 3),
        marker = list(size = 8, color = 'rgb(52, 152, 219)'),
        name = 'Resident',
        hovertemplate = '<b>%{theta}</b><br>Resident Score: %{r}<extra></extra>'
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(1, 9),
            tickmode = 'linear',
            tick0 = 1,
            dtick = 1
          ),
          angularaxis = list(
            tickfont = list(size = 12)
          )
        ),
        title = "Individual vs Program Competency Profile",
        showlegend = TRUE,
        legend = list(x = 0.8, y = 0.1)
      )
  })
  
  # Milestone progression plot
  output$milestone_progression <- renderPlotly({
    req(processed_data(), input$selected_resident, input$selected_milestone)
    
    data <- processed_data()
    
    # Get resident milestone data over time
    resident_data <- data %>%
      filter(Resident.ID == input$selected_resident) %>%
      select(period, all_of(input$selected_milestone)) %>%
      filter(!is.na(.data[[input$selected_milestone]])) %>%
      rename(score = all_of(input$selected_milestone)) %>%
      arrange(period)
    
    if (nrow(resident_data) == 0) {
      return(plotly::plot_ly() %>% 
               plotly::add_annotations(text = "No data available for this milestone", 
                                       x = 0.5, y = 0.5, showarrow = FALSE))
    }
    
    # Get program benchmark for this milestone
    medians <- median_results()
    program_benchmark <- medians$overall_medians %>%
      filter(Sub_Competency == input$selected_milestone) %>%
      pull(Overall_Median)
    
    if (length(program_benchmark) == 0) {
      program_benchmark <- 5  # Default benchmark
    }
    
    # Create progression plot
    plot_ly(resident_data, x = ~period, y = ~score, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'rgb(52, 152, 219)', width = 3),
            marker = list(size = 10, color = 'rgb(52, 152, 219)'),
            hovertemplate = '<b>%{x}</b><br>Score: %{y}<extra></extra>') %>%
      add_hline(y = program_benchmark, line = list(color = 'red', dash = 'dash', width = 2),
                name = paste("Program Median:", round(program_benchmark, 1))) %>%
      add_hline(y = 4, line = list(color = 'orange', dash = 'dot', width = 2),
                name = "Benchmark (4.0)") %>%
      add_hline(y = 6, line = list(color = 'green', dash = 'dot', width = 2),
                name = "Target (6.0)") %>%
      layout(
        title = paste("Milestone Progression:", input$selected_milestone),
        xaxis = list(title = "Assessment Period", tickangle = -45),
        yaxis = list(title = "Score", range = c(1, 9)),
        hovermode = 'x unified'
      )
  })
  
  # Resident scores table
  output$resident_scores_table <- DT::renderDataTable({
    req(processed_data(), input$selected_resident)
    
    data <- processed_data()
    
    # Get resident data
    resident_data <- data %>%
      filter(Resident.ID == input$selected_resident)
    
    if (nrow(resident_data) == 0) {
      return(data.frame(Period = "No data", Milestone = "", Score = "", 
                        Performance = "", check.names = FALSE))
    }
    
    # Get milestone columns
    milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)\\d+$", names(resident_data), value = TRUE)
    
    if (length(milestone_cols) == 0) {
      return(data.frame(Period = "No milestone data", Milestone = "", Score = "", 
                        Performance = "", check.names = FALSE))
    }
    
    # Create detailed scores table
    scores_table <- resident_data %>%
      select(period, all_of(milestone_cols)) %>%
      pivot_longer(cols = all_of(milestone_cols), names_to = "Milestone", values_to = "Score") %>%
      filter(!is.na(Score)) %>%
      mutate(
        Competency = substr(Milestone, 1, 2),
        Performance = case_when(
          Score < 4 ~ "Below Benchmark",
          Score < 6 ~ "At Benchmark", 
          TRUE ~ "Above Target"
        )
      ) %>%
      arrange(period, Competency, Milestone) %>%
      rename(Period = period) %>%
      select(Period, Competency, Milestone, Score, Performance)
    
    return(scores_table)
  }, options = list(pageLength = 10, scrollX = TRUE))
  
}