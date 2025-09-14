# ============================================================================
# DEFINE SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # ========================================================================
  # RDM DATA PROCESSING
  # ========================================================================
  
  # Reactive value to store RDM data
  rdm_data <- reactiveVal(NULL)
  
  # RDM data loading status
  output$rdm_status <- renderUI({
    if (is.null(rdm_data())) {
      tags$span(icon("info-circle"), " No RDM data loaded", class = "text-muted")
    } else {
      tags$span(icon("check-circle"), " RDM data connected", class = "text-success")
    }
  })
  
  # Load RDM data
  observeEvent(input$load_rdm_data, {
    req(input$rdm_token)
    
    showNotification("Loading RDM data...", type = "message", duration = NULL, id = "rdm_loading")
    
    tryCatch({
      # Load RDM data using your existing function
      loaded_data <- load_rdm_simple(rdm_token = input$rdm_token)
      
      # Store data
      rdm_data(loaded_data)
      
      # Success notification
      removeNotification("rdm_loading")
      showNotification(
        paste("Successfully loaded RDM data for", nrow(loaded_data$residents), "residents"),
        type = "success",
        duration = 5
      )
      
    }, error = function(e) {
      removeNotification("rdm_loading")
      showNotification(paste("Error loading RDM data:", e$message), 
                       type = "error", duration = 10)
    })
  })
  
  # RDM data loaded flag
  output$rdm_data_loaded <- reactive({
    !is.null(rdm_data())
  })
  outputOptions(output, "rdm_data_loaded", suspendWhenHidden = FALSE)
  
  # RDM summary
  output$rdm_summary <- renderText({
    req(rdm_data())
    paste("Loaded", nrow(rdm_data()$residents), "residents with", 
          nrow(rdm_data()$assessment_data), "assessment records")
  })
  
  # ========================================================================
  # RDM UI UPDATES
  # ========================================================================
  
  # Update resident choices for RDM
  observe({
    req(rdm_data())
    residents <- rdm_data()$residents
    choices <- setNames(residents$record_id, residents$name)
    updateSelectInput(session, "rdm_resident_select", choices = choices)
  })
  
  # Update period choices for RDM
  observe({
    req(rdm_data())
    # This would depend on your RDM data structure
    # periods <- unique(rdm_data()$some_period_column)
    periods <- c("Current Period")  # Placeholder
    updateSelectInput(session, "rdm_period_select", choices = periods)
  })
  
  # RDM resident info display
  output$rdm_resident_info <- renderUI({
    req(rdm_data(), input$rdm_resident_select)
    
    resident_info <- rdm_data()$residents %>%
      filter(record_id == input$rdm_resident_select) %>%
      slice(1)
    
    if (nrow(resident_info) > 0) {
      tagList(
        div(class = "gmed-resident-panel mt-3",
            tags$p(tags$strong("Name: "), resident_info$name),
            tags$p(tags$strong("Level: "), resident_info$Level %||% "Unknown"),
            tags$p(tags$strong("Access Code: "), resident_info$access_code %||% "N/A")
        )
      )
    }
  })
  
  # ========================================================================
  # RDM MODULE SERVERS
  # ========================================================================
  
  # RDM Milestone dashboard
  observe({
    req(rdm_data(), input$rdm_resident_select, input$rdm_period_select)
    
    # Call your existing milestone dashboard server
    # This would need to be adapted based on your existing module structure
    # milestone_dashboard_server("rdm_milestone_dash", ...)
  })
  
  # RDM Assessment visualization
  observe({
    req(rdm_data(), input$rdm_resident_select)
    
    # Call your existing assessment viz server
    # assessment_viz_server("rdm_assessment_viz", 
    #                       data = reactive(rdm_data()$assessment_data),
    #                       record_id = reactive(input$rdm_resident_select))
  })
  
  # RDM Plus/Delta table
  observe({
    req(rdm_data(), input$rdm_resident_select)
    
    # Call your existing plus/delta server
    # mod_plus_delta_table_server("rdm_plus_delta",
    #                             rdm_data = reactive(rdm_data()$assessment_data),
    #                             record_id = reactive(input$rdm_resident_select))
  })
  
  # ========================================================================
  # ACGME DATA PROCESSING
  # ========================================================================
  
  # Reactive value to store ACGME processed data
  acgme_processed_data <- reactiveVal(NULL)
  
  # ACGME data upload status
  output$acgme_data_status <- renderUI({
    if (is.null(input$acgme_csv_files)) {
      tags$span(icon("info-circle"), " No ACGME files uploaded", class = "text-muted")
    } else {
      tags$span(icon("check-circle"), 
                paste(nrow(input$acgme_csv_files), "ACGME files uploaded"), 
                class = "text-success")
    }
  })
  
  # Process ACGME uploaded data
  observeEvent(input$process_acgme_data, {
    req(input$acgme_csv_files)
    
    # Show processing notification
    showNotification("Processing ACGME data and extracting milestone structure...", 
                     type = "message", duration = NULL, id = "acgme_processing")
    
    tryCatch({
      # Transform ACGME CSV data using dynamic discovery
      acgme_result <- transform_acgme_data_dynamic(input$acgme_csv_files$datapath)
      
      # Store processed data
      acgme_processed_data(acgme_result)
      
      # Extract key info for success message
      structure_info <- acgme_result$structure_info
      n_residents <- n_distinct(acgme_result$milestone_data_long$record_id)
      n_competencies <- nrow(structure_info$competency_counts)
      n_milestones <- structure_info$total_milestones
      
      # Success notification
      removeNotification("acgme_processing")
      showNotification(
        paste("Successfully processed", n_residents, "residents with", 
              n_competencies, "competencies and", n_milestones, "milestones"),
        type = "success",
        duration = 8
      )
      
    }, error = function(e) {
      removeNotification("acgme_processing")
      showNotification(paste("Error processing ACGME data:", e$message), 
                       type = "error", duration = 15)
      print(paste("ACGME Debug error:", e$message))
    })
  })
  
  # ACGME data loaded flag for conditional panels
  output$acgme_data_loaded <- reactive({
    !is.null(acgme_processed_data())
  })
  outputOptions(output, "acgme_data_loaded", suspendWhenHidden = FALSE)
  
  # ACGME specialty detected message
  output$acgme_specialty_detected <- renderText({
    req(acgme_processed_data())
    structure_info <- acgme_processed_data()$structure_info
    paste("Detected:", nrow(structure_info$competency_counts), "competencies,", 
          structure_info$total_milestones, "milestones,", 
          length(acgme_processed_data()$resident_years), "training years")
  })
  
  # ========================================================================
  # ACGME UI UPDATES
  # ========================================================================
  
  # Update ACGME resident choices
  observe({
    req(acgme_processed_data())
    residents <- acgme_processed_data()$milestone_data_long %>%
      distinct(record_id, name) %>%
      arrange(name)
    
    choices <- setNames(residents$record_id, residents$name)
    updateSelectInput(session, "acgme_resident_select", choices = choices)
  })
  
  # Update ACGME period choices
  observe({
    req(acgme_processed_data())
    periods <- c("Total", acgme_processed_data()$periods)
    updateSelectInput(session, "acgme_period_select", choices = periods)
  })
  
  # ========================================================================
  # ACGME MODULE SERVERS
  # ========================================================================
  
  # ACGME program overview module
  mod_acgme_program_overview_server("acgme_program", acgme_processed_data)
  
  # ========================================================================
  # ACGME INDIVIDUAL ANALYSIS (Basic Implementation)
  # ========================================================================
  
  # ACGME resident info display
  output$acgme_resident_info <- renderUI({
    req(acgme_processed_data(), input$acgme_resident_select)
    
    resident_info <- acgme_processed_data()$milestone_data_long %>%
      filter(record_id == input$acgme_resident_select) %>%
      slice(1)
    
    if (nrow(resident_info) > 0) {
      tagList(
        div(class = "gmed-resident-panel mt-3",
            tags$p(tags$strong("Name: "), resident_info$name),
            tags$p(tags$strong("Level: "), paste("PGY-", resident_info$Level)),
            tags$p(tags$strong("Period: "), input$acgme_period_select)
        )
      )
    }
  })
  
  # ACGME resident stats
  output$acgme_resident_stats <- renderTable({
    req(acgme_processed_data(), input$acgme_resident_select, input$acgme_period_select)
    
    resident_data <- acgme_processed_data()$milestone_data_long %>%
      filter(record_id == input$acgme_resident_select)
    
    if (input$acgme_period_select != "Total") {
      resident_data <- resident_data %>% filter(prog_mile_period == input$acgme_period_select)
    }
    
    # Calculate individual competency averages
    individual_comp <- resident_data %>%
      group_by(competency_code, competency_name) %>%
      summarise(
        individual_score = mean(score, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Calculate cohort averages for comparison
    cohort_data <- acgme_processed_data()$milestone_data_long
    if (input$acgme_period_select != "Total") {
      cohort_data <- cohort_data %>% filter(prog_mile_period == input$acgme_period_select)
    }
    
    cohort_comp <- cohort_data %>%
      group_by(competency_code, competency_name) %>%
      summarise(
        cohort_median = median(score, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Combine for spider plot
    spider_data <- individual_comp %>%
      left_join(cohort_comp, by = c("competency_code", "competency_name"))
    
    # Create spider plot
    fig <- plot_ly(type = 'scatterpolar', mode = 'lines+markers')
    
    # Individual scores
    fig <- fig %>%
      add_trace(
        r = spider_data$individual_score,
        theta = spider_data$competency_name,
        name = "Individual",
        line = list(color = "#e74c3c", width = 3),
        marker = list(size = 10, color = "#e74c3c"),
        text = paste("Individual:", round(spider_data$individual_score, 1)),
        hovertemplate = "%{text}<extra></extra>"
      )
    
    # Cohort medians
    fig <- fig %>%
      add_trace(
        r = spider_data$cohort_median,
        theta = spider_data$competency_name,
        name = "Cohort Median",
        line = list(color = "#3498db", width = 2, dash = "dash"),
        marker = list(size = 8, color = "#3498db"),
        text = paste("Cohort:", round(spider_data$cohort_median, 1)),
        hovertemplate = "%{text}<extra></extra>"
      )
    
    # Layout
    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(1, 9),
            tickmode = 'array',
            tickvals = c(1, 3, 5, 7, 9),
            ticktext = c('1', '3', '5', '7', '9'),
            tickfont = list(size = 12),
            gridcolor = 'rgba(0,0,0,0.1)'
          ),
          angularaxis = list(
            tickfont = list(size = 12),
            rotation = 90,
            direction = "clockwise"
          )
        ),
        title = list(text = "Individual vs Cohort Performance", font = list(size = 16)),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1),
        margin = list(t = 60, b = 80, l = 60, r = 60)
      )
    
    return(fig)
  })
  
  # ACGME performance summary
  output$acgme_performance_summary <- renderUI({
    req(acgme_processed_data(), input$acgme_resident_select, input$acgme_period_select)
    
    resident_data <- acgme_processed_data()$milestone_data_long %>%
      filter(record_id == input$acgme_resident_select)
    
    if (input$acgme_period_select != "Total") {
      resident_data <- resident_data %>% filter(prog_mile_period == input$acgme_period_select)
    }
    
    if (nrow(resident_data) > 0) {
      avg_score <- round(mean(resident_data$score, na.rm = TRUE), 1)
      below_benchmark <- sum(resident_data$score < 4, na.rm = TRUE)
      above_target <- sum(resident_data$score >= 7, na.rm = TRUE)
      
      # Get lowest performing competencies
      low_comp <- resident_data %>%
        group_by(competency_name) %>%
        summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
        filter(avg_score < 5) %>%
        arrange(avg_score) %>%
        slice_head(n = 3)
      
      tagList(
        tags$p(tags$strong("Overall Average: "), avg_score),
        tags$p(tags$strong("Below Benchmark: "), below_benchmark, " milestones"),
        tags$p(tags$strong("Above Target: "), above_target, " milestones"),
        
        if(nrow(low_comp) > 0) {
          tagList(
            tags$hr(),
            tags$strong("Focus Areas:"),
            tags$ul(
              lapply(1:nrow(low_comp), function(i) {
                tags$li(paste0(low_comp$competency_name[i], " (", 
                               round(low_comp$avg_score[i], 1), ")"))
              })
            )
          )
        }
      )
    }
  })
  
  # ACGME individual details table
  output$acgme_individual_details <- DT::renderDataTable({
    req(acgme_processed_data(), input$acgme_resident_select, input$acgme_period_select)
    
    resident_data <- acgme_processed_data()$milestone_data_long %>%
      filter(record_id == input$acgme_resident_select)
    
    if (input$acgme_period_select != "Total") {
      resident_data <- resident_data %>% filter(prog_mile_period == input$acgme_period_select)
    }
    
    if (nrow(resident_data) > 0) {
      resident_data %>%
        select(competency_name, milestone_id, sub_competency_title, score) %>%
        mutate(
          status = case_when(
            score < 4 ~ "Needs Improvement",
            score < 6 ~ "Developing", 
            score < 8 ~ "Proficient",
            TRUE ~ "Advanced"
          )
        ) %>%
        arrange(competency_name, milestone_id) %>%
        rename(
          Competency = competency_name,
          Milestone = milestone_id,
          Description = sub_competency_title,
          Score = score,
          Status = status
        )
    }
  }, options = list(pageLength = 20, scrollX = TRUE))
  
  # ACGME cohort comparison
  output$acgme_cohort_comparison <- renderPlotly({
    req(acgme_processed_data(), input$acgme_resident_select, input$acgme_period_select)
    
    # Individual milestone scores
    resident_data <- acgme_processed_data()$milestone_data_long %>%
      filter(record_id == input$acgme_resident_select)
    
    if (input$acgme_period_select != "Total") {
      resident_data <- resident_data %>% filter(prog_mile_period == input$acgme_period_select)
    }
    
    individual_scores <- resident_data %>%
      group_by(competency_name, milestone_id) %>%
      summarise(individual_score = mean(score, na.rm = TRUE), .groups = "drop")
    
    # Cohort milestone medians
    cohort_data <- acgme_processed_data()$milestone_data_long
    if (input$acgme_period_select != "Total") {
      cohort_data <- cohort_data %>% filter(prog_mile_period == input$acgme_period_select)
    }
    
    cohort_scores <- cohort_data %>%
      group_by(competency_name, milestone_id) %>%
      summarise(cohort_median = median(score, na.rm = TRUE), .groups = "drop")
    
    # Combine and create comparison
    comparison_data <- individual_scores %>%
      left_join(cohort_scores, by = c("competency_name", "milestone_id")) %>%
      mutate(
        difference = individual_score - cohort_median,
        milestone_label = paste(competency_name, milestone_id)
      ) %>%
      arrange(difference)
    
    # Create comparison plot
    plot_ly(comparison_data, 
            x = ~milestone_label, 
            y = ~difference,
            type = 'bar',
            marker = list(
              color = ~ifelse(difference >= 0, "#27ae60", "#e74c3c")
            ),
            text = ~paste("Difference:", round(difference, 1)),
            hovertemplate = "%{text}<br>%{x}<extra></extra>") %>%
      layout(
        title = "Performance Difference from Cohort Median",
        xaxis = list(title = "Milestone", tickangle = -45),
        yaxis = list(title = "Score Difference", zeroline = TRUE),
        margin = list(b = 150)
      )
  })
  
  # ACGME comparison summary
  output$acgme_comparison_summary <- renderUI({
    req(acgme_processed_data(), input$acgme_resident_select, input$acgme_period_select)
    
    # Calculate comparison metrics
    resident_data <- acgme_processed_data()$milestone_data_long %>%
      filter(record_id == input$acgme_resident_select)
    
    if (input$acgme_period_select != "Total") {
      resident_data <- resident_data %>% filter(prog_mile_period == input$acgme_period_select)
    }
    
    individual_avg <- mean(resident_data$score, na.rm = TRUE)
    
    cohort_data <- acgme_processed_data()$milestone_data_long
    if (input$acgme_period_select != "Total") {
      cohort_data <- cohort_data %>% filter(prog_mile_period == input$acgme_period_select)
    }
    cohort_avg <- mean(cohort_data$score, na.rm = TRUE)
    
    difference <- individual_avg - cohort_avg
    
    tagList(
      tags$p(tags$strong("Individual Average: "), round(individual_avg, 2)),
      tags$p(tags$strong("Cohort Average: "), round(cohort_avg, 2)),
      tags$p(tags$strong("Difference: "), 
             tags$span(
               paste(ifelse(difference >= 0, "+", ""), round(difference, 2)),
               class = ifelse(difference >= 0, "text-success", "text-danger")
             )),
      tags$p(
        ifelse(difference >= 0.5, 
               "📈 Performing above cohort average",
               ifelse(difference <= -0.5,
                      "📉 Performing below cohort average", 
                      "➡️ Performing near cohort average"))
      )
    )
  })
  
  # ========================================================================
  # ACGME DATA SUMMARY OUTPUTS
  # ========================================================================
  
  # ACGME competency structure table
  output$acgme_competency_structure <- DT::renderDataTable({
    req(acgme_processed_data())
    
    acgme_processed_data()$structure_info$competency_counts %>%
      rename(
        `Code` = competency_code,
        `Competency Name` = competency_name,
        `Sub-competencies` = n_subcompetencies,
        `Range` = subcompetency_range
      )
  }, options = list(dom = 't', pageLength = 10))
  
  # ACGME dataset summary
  output$acgme_dataset_summary <- renderTable({
    req(acgme_processed_data())
    
    data <- acgme_processed_data()$milestone_data_long
    structure_info <- acgme_processed_data()$structure_info
    
    summary_table <- data.frame(
      Metric = c("Total Residents", "Total Evaluations", "Training Years", 
                 "Competencies", "Milestones", "Periods"),
      Value = c(
        n_distinct(data$record_id),
        nrow(data),
        paste(sort(unique(data$Level)), collapse = ", "),
        nrow(structure_info$competency_counts),
        structure_info$total_milestones,
        paste(unique(data$prog_mile_period), collapse = ", ")
      ),
      stringsAsFactors = FALSE
    )
    
    summary_table
  }, bordered = TRUE, striped = TRUE)
  
  # ACGME milestone details table
  output$acgme_milestone_details <- DT::renderDataTable({
    req(acgme_processed_data())
    
    acgme_processed_data()$structure_info$milestone_structure %>%
      select(competency_code, milestone_id, sub_competency_title, question_key) %>%
      rename(
        `Competency` = competency_code,
        `Milestone` = milestone_id,
        `Description` = sub_competency_title,
        `Question Key` = question_key
      ) %>%
      arrange(Competency, Milestone)
  }, options = list(pageLength = 15, scrollX = TRUE))
  
  # ========================================================================
  # SESSION MANAGEMENT
  # ========================================================================
  
  # Clean up on session end
  session$onSessionEnded(function() {
    # Clean up any reactive values or connections
    rdm_data(NULL)
    acgme_processed_data(NULL)
  })
  
  # ========================================================================
  # DEBUG OUTPUTS (Remove in production)
  # ========================================================================
  
  

output$acgme_individual_spider <- renderPlotly({
  req(acgme_processed_data(), input$acgme_resident_select, input$acgme_period_select)
  
  # Get individual resident data
  resident_data <- acgme_processed_data()$milestone_data_long %>%
    filter(record_id == input$acgme_resident_select)
  
  if (input$acgme_period_select != "Total") {
    resident_data <- resident_data %>% filter(prog_mile_period == input$acgme_period_select)
  }
  
  # Calculate individual competency averages
  individual_comp <- resident_data %>%
    group_by(competency_code, competency_name) %>%
    summarise(
      individual_score = mean(score, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate cohort averages for comparison
  cohort_data <- acgme_processed_data()$milestone_data_long
  if (input$acgme_period_select != "Total") {
    cohort_data <- cohort_data %>% filter(prog_mile_period == input$acgme_period_select)
  }
  
  cohort_comp <- cohort_data %>%
    group_by(competency_code, competency_name) %>%
    summarise(
      cohort_median = median(score, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Combine for spider plot
  spider_data <- individual_comp %>%
    left_join(cohort_comp, by = c("competency_code", "competency_name"))
  
  # Create spider plot
  fig <- plot_ly(type = 'scatterpolar', mode = 'lines+markers')
  
  # Individual scores
  fig <- fig %>%
    add_trace(
      r = spider_data$individual_score,
      theta = spider_data$competency_name,
      name = "Individual",
      line = list(color = "#e74c3c", width = 3),
      marker = list(size = 10, color = "#e74c3c"),
      text = paste("Individual:", round(spider_data$individual_score, 1)),
      hovertemplate = "%{text}<extra></extra>"
    )
  
  # Cohort medians
  fig <- fig %>%
    add_trace(
      r = spider_data$cohort_median,
      theta = spider_data$competency_name,
      name = "Cohort Median",
      line = list(color = "#3498db", width = 2, dash = "dash"),
      marker = list(size = 8, color = "#3498db"),
      text = paste("Cohort:", round(spider_data$cohort_median, 1)),
      hovertemplate = "%{text}<extra></extra>"
    )
  
  # Layout
  fig <- fig %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(1, 9),
          tickmode = 'array',
          tickvals = c(1, 3, 5, 7, 9),
          ticktext = c('1', '3', '5', '7', '9'),
          tickfont = list(size = 12),
          gridcolor = 'rgba(0,0,0,0.1)'
        ),
        angularaxis = list(
          tickfont = list(size = 12),
          rotation = 90,
          direction = "clockwise"
        )
      ),
      title = list(text = "Individual vs Cohort Performance", font = list(size = 16)),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1),
      margin = list(t = 60, b = 80, l = 60, r = 60)
    )
  
  return(fig)
})

# ACGME performance summary
output$acgme_performance_summary <- renderUI({
  req(acgme_processed_data(), input$acgme_resident_select, input$acgme_period_select)
  
  resident_data <- acgme_processed_data()$milestone_data_long %>%
    filter(record_id == input$acgme_resident_select)
  
  if (input$acgme_period_select != "Total") {
    resident_data <- resident_data %>% filter(prog_mile_period == input$acgme_period_select)
  }
  
  if (nrow(resident_data) > 0) {
    avg_score <- round(mean(resident_data$score, na.rm = TRUE), 1)
    below_benchmark <- sum(resident_data$score < 4, na.rm = TRUE)
    above_target <- sum(resident_data$score >= 7, na.rm = TRUE)
    
    # Get lowest performing competencies
    low_comp <- resident_data %>%
      group_by(competency_name) %>%
      summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
      filter(avg_score < 5) %>%
      arrange(avg_score) %>%
      slice_head(n = 3)
    
    tagList(
      tags$p(tags$strong("Overall Average: "), avg_score),
      tags$p(tags$strong("Below Benchmark: "), below_benchmark, " milestones"),
      tags$p(tags$strong("Above Target: "), above_target, " milestones"),
      
      if(nrow(low_comp) > 0) {
        tagList(
          tags$hr(),
          tags$strong("Focus Areas:"),
          tags$ul(
            lapply(1:nrow(low_comp), function(i) {
              tags$li(paste0(low_comp$competency_name[i], " (", 
                             round(low_comp$avg_score[i], 1), ")"))
            })
          )
        )
      }
    )
  }
})

# ACGME individual details table
output$acgme_individual_details <- DT::renderDataTable({
  req(acgme_processed_data(), input$acgme_resident_select, input$acgme_period_select)
  
  resident_data <- acgme_processed_data()$milestone_data_long %>%
    filter(record_id == input$acgme_resident_select)
  
  if (input$acgme_period_select != "Total") {
    resident_data <- resident_data %>% filter(prog_mile_period == input$acgme_period_select)
  }
  
  if (nrow(resident_data) > 0) {
    resident_data %>%
      select(competency_name, milestone_id, sub_competency_title, score) %>%
      mutate(
        status = case_when(
          score < 4 ~ "Needs Improvement",
          score < 6 ~ "Developing", 
          score < 8 ~ "Proficient",
          TRUE ~ "Advanced"
        )
      ) %>%
      arrange(competency_name, milestone_id) %>%
      rename(
        Competency = competency_name,
        Milestone = milestone_id,
        Description = sub_competency_title,
        Score = score,
        Status = status
      )
  }
}, options = list(pageLength = 20, scrollX = TRUE))

# ACGME cohort comparison
output$acgme_cohort_comparison <- renderPlotly({
  req(acgme_processed_data(), input$acgme_resident_select, input$acgme_period_select)
  
  # Individual milestone scores
  resident_data <- acgme_processed_data()$milestone_data_long %>%
    filter(record_id == input$acgme_resident_select)
  
  if (input$acgme_period_select != "Total") {
    resident_data <- resident_data %>% filter(prog_mile_period == input$acgme_period_select)
  }
  
  individual_scores <- resident_data %>%
    group_by(competency_name, milestone_id) %>%
    summarise(individual_score = mean(score, na.rm = TRUE), .groups = "drop")
  
  # Cohort milestone medians
  cohort_data <- acgme_processed_data()$milestone_data_long
  if (input$acgme_period_select != "Total") {
    cohort_data <- cohort_data %>% filter(prog_mile_period == input$acgme_period_select)
  }
  
  cohort_scores <- cohort_data %>%
    group_by(competency_name, milestone_id) %>%
    summarise(cohort_median = median(score, na.rm = TRUE), .groups = "drop")
  
  # Combine and create comparison
  comparison_data <- individual_scores %>%
    left_join(cohort_scores, by = c("competency_name", "milestone_id")) %>%
    mutate(
      difference = individual_score - cohort_median,
      milestone_label = paste(competency_name, milestone_id)
    ) %>%
    arrange(difference)
  
  # Create comparison plot
  plot_ly(comparison_data, 
          x = ~milestone_label, 
          y = ~difference,
          type = 'bar',
          marker = list(
            color = ~ifelse(difference >= 0, "#27ae60", "#e74c3c")
          ),
          text = ~paste("Difference:", round(difference, 1)),
          hovertemplate = "%{text}<br>%{x}<extra></extra>") %>%
    layout(
      title = "Performance Difference from Cohort Median",
      xaxis = list(title = "Milestone", tickangle = -45),
      yaxis = list(title = "Score Difference", zeroline = TRUE),
      margin = list(b = 150)
    )
})

# ACGME comparison summary
output$acgme_comparison_summary <- renderUI({
  req(acgme_processed_data(), input$acgme_resident_select, input$acgme_period_select)
  
  # Calculate comparison metrics
  resident_data <- acgme_processed_data()$milestone_data_long %>%
    filter(record_id == input$acgme_resident_select)
  
  if (input$acgme_period_select != "Total") {
    resident_data <- resident_data %>% filter(prog_mile_period == input$acgme_period_select)
  }
  
  individual_avg <- mean(resident_data$score, na.rm = TRUE)
  
  cohort_data <- acgme_processed_data()$milestone_data_long
  if (input$acgme_period_select != "Total") {
    cohort_data <- cohort_data %>% filter(prog_mile_period == input$acgme_period_select)
  }
  cohort_avg <- mean(cohort_data$score, na.rm = TRUE)
  
  difference <- individual_avg - cohort_avg
  
  tagList(
    tags$p(tags$strong("Individual Average: "), round(individual_avg, 2)),
    tags$p(tags$strong("Cohort Average: "), round(cohort_avg, 2)),
    tags$p(tags$strong("Difference: "), 
           tags$span(
             paste(ifelse(difference >= 0, "+", ""), round(difference, 2)),
             class = ifelse(difference >= 0, "text-success", "text-danger")
           )),
    tags$p(
      ifelse(difference >= 0.5, 
             "📈 Performing above cohort average",
             ifelse(difference <= -0.5,
                    "📉 Performing below cohort average", 
                    "➡️ Performing near cohort average"))
    )
  )
})

# ========================================================================
# ACGME DATA SUMMARY OUTPUTS
# ========================================================================

# ACGME competency structure table
output$acgme_competency_structure <- DT::renderDataTable({
  req(acgme_processed_data())
  
  acgme_processed_data()$structure_info$competency_counts %>%
    rename(
      `Code` = competency_code,
      `Competency Name` = competency_name,
      `Sub-competencies` = n_subcompetencies,
      `Range` = subcompetency_range
    )
}, options = list(dom = 't', pageLength = 10))

# ACGME dataset summary
output$acgme_dataset_summary <- renderTable({
  req(acgme_processed_data())
  
  data <- acgme_processed_data()$milestone_data_long
  structure_info <- acgme_processed_data()$structure_info
  
  summary_table <- data.frame(
    Metric = c("Total Residents", "Total Evaluations", "Training Years", 
               "Competencies", "Milestones", "Periods"),
    Value = c(
      n_distinct(data$record_id),
      nrow(data),
      paste(sort(unique(data$Level)), collapse = ", "),
      nrow(structure_info$competency_counts),
      structure_info$total_milestones,
      paste(unique(data$prog_mile_period), collapse = ", ")
    ),
    stringsAsFactors = FALSE
  )
  
  summary_table
}, bordered = TRUE, striped = TRUE)

# ACGME milestone details table
output$acgme_milestone_details <- DT::renderDataTable({
  req(acgme_processed_data())
  
  acgme_processed_data()$structure_info$milestone_structure %>%
    select(competency_code, milestone_id, sub_competency_title, question_key) %>%
    rename(
      `Competency` = competency_code,
      `Milestone` = milestone_id,
      `Description` = sub_competency_title,
      `Question Key` = question_key
    ) %>%
    arrange(Competency, Milestone)
}, options = list(pageLength = 15, scrollX = TRUE))

# ========================================================================
# SESSION MANAGEMENT
# ========================================================================

# Clean up on session end
session$onSessionEnded(function() {
  # Clean up any reactive values or connections
  rdm_data(NULL)
  acgme_processed_data(NULL)
})

# ========================================================================
# DEBUG OUTPUTS (Remove in production)
# ========================================================================

# Uncomment these for debugging
observe({
  if (!is.null(rdm_data())) {
    cat("RDM data loaded with", nrow(rdm_data()$residents), "residents\n")
  }
})

observe({
  if (!is.null(acgme_processed_data())) {
    cat("ACGME data loaded with", 
        dplyr::n_distinct(acgme_processed_data()$milestone_data_long$record_id), 
        "residents\n")
  }
})


}