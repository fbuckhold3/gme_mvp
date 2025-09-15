# ============================================================================
# GME MILESTONE VISUALIZATION PROJECT - SERVER (CSV FILES ONLY)
# R/server.R
# ============================================================================

server <- function(input, output, session) {
  
  # ========================================================================
  # REACTIVE VALUES
  # ========================================================================
  
  # Reactive value to store processed CSV data
  csv_processed_data <- reactiveVal(NULL)
  
  # ========================================================================
  # CSV DATA UPLOAD STATUS
  # ========================================================================
  
  # CSV data upload status display
  output$acgme_data_status <- renderUI({
    if (is.null(input$acgme_csv_files)) {
      tags$span(icon("info-circle"), " No CSV files uploaded", class = "text-muted")
    } else {
      tags$span(icon("check-circle"), 
                paste(nrow(input$acgme_csv_files), "CSV files uploaded"), 
                class = "text-success")
    }
  })
  
  # ========================================================================
  # CSV DATA PROCESSING
  # ========================================================================
  
  # Process uploaded CSV data when button is clicked
  observeEvent(input$process_acgme_data, {
    req(input$acgme_csv_files)
    
    # Show processing notification
    showNotification("Processing CSV data and extracting milestone structure...", 
                     type = "message", duration = NULL, id = "csv_processing")
    
    tryCatch({
      # Process CSV files using function from helpers.R
      csv_result <- import_and_process_milestones(input$acgme_csv_files$datapath)
      
      # Store processed data
      csv_processed_data(csv_result)
      
      # Extract key info for success message
      n_residents <- length(unique(csv_result$Resident.ID))
      n_periods <- length(unique(csv_result$period))
      milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(csv_result), value = TRUE)
      n_milestones <- length(milestone_cols)
      
      success_msg <- paste("Successfully processed", n_residents, "residents with", 
                           n_milestones, "milestones across", n_periods, "periods")
      
      # Success notification
      removeNotification("csv_processing")
      showNotification(success_msg, type = "success", duration = 8)
      
    }, error = function(e) {
      removeNotification("csv_processing")
      showNotification(paste("Error processing CSV data:", e$message), 
                       type = "error", duration = 15)
      print(paste("CSV Debug error:", e$message))
    })
  })
  
  # ========================================================================
  # CONDITIONAL UI CONTROL
  # ========================================================================
  
  # CSV data loaded flag for conditional panels
  output$acgme_data_loaded <- reactive({
    !is.null(csv_processed_data())
  })
  outputOptions(output, "acgme_data_loaded", suspendWhenHidden = FALSE)
  
  # ========================================================================
  # DATA SUMMARY DISPLAYS
  # ========================================================================
  
  # CSV specialty detected message
  output$acgme_specialty_detected <- renderText({
    req(csv_processed_data())
    
    data <- csv_processed_data()
    n_residents <- length(unique(data$Resident.ID))
    milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(data), value = TRUE)
    competencies <- unique(substr(milestone_cols, 1, 2))
    n_periods <- length(unique(data$period))
    
    paste("Detected:", length(competencies), "competencies,", 
          length(milestone_cols), "milestones,", 
          n_periods, "periods for", n_residents, "residents")
  })
  
  # ========================================================================
  # UI UPDATES BASED ON CSV DATA
  # ========================================================================
  
  # Update resident choices based on processed data
  observe({
    req(csv_processed_data())
    
    data <- csv_processed_data()
    
    # Create resident lookup from processed data
    residents <- data %>%
      select(Resident.ID, Resident.Year) %>%
      distinct() %>%
      mutate(
        record_id = as.character(Resident.ID),
        name = paste("Resident", Resident.ID, "(Year", Resident.Year, ")"),
        Level = as.character(Resident.Year)
      )
    
    choices <- setNames(residents$record_id, residents$name)
    updateSelectInput(session, "acgme_resident_select", choices = choices)
  })
  
  # Update period choices based on processed data
  observe({
    req(csv_processed_data())
    
    periods <- unique(csv_processed_data()$period)
    periods <- c("Total", periods[!is.na(periods)])
    
    updateSelectInput(session, "acgme_period_select", choices = periods)
  })
  
  # ========================================================================
  # DATA SUMMARY TABLES
  # ========================================================================
  
  # Competency structure table
  output$acgme_competency_structure <- DT::renderDataTable({
    req(csv_processed_data())
    
    data <- csv_processed_data()
    milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(data), value = TRUE)
    competencies <- unique(substr(milestone_cols, 1, 2))
    
    # Create competency structure table
    competency_table <- data.frame(
      Code = competencies,
      `Competency Name` = case_when(
        competencies == "PC" ~ "Patient Care",
        competencies == "MK" ~ "Medical Knowledge",
        competencies == "SBP" ~ "Systems-Based Practice",
        competencies == "PBL" ~ "Practice-Based Learning and Improvement",
        competencies == "PROF" ~ "Professionalism",
        competencies == "ICS" ~ "Interpersonal Communication Skills",
        TRUE ~ "Unknown"
      ),
      `Sub-competencies` = sapply(competencies, function(comp) {
        comp_cols <- grep(paste0("^", comp), milestone_cols, value = TRUE)
        length(comp_cols)
      }),
      stringsAsFactors = FALSE
    )
    
    competency_table
  }, options = list(dom = 't', pageLength = 10))
  
  # Dataset summary table
  output$acgme_dataset_summary <- renderTable({
    req(csv_processed_data())
    
    data <- csv_processed_data()
    milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(data), value = TRUE)
    competencies <- unique(substr(milestone_cols, 1, 2))
    
    summary_table <- data.frame(
      Metric = c("Total Residents", "Training Years", "Milestones", "Competencies", "Periods"),
      Value = c(
        length(unique(data$Resident.ID)),
        paste(sort(unique(data$Resident.Year)), collapse = ", "),
        length(milestone_cols),
        length(competencies),
        paste(unique(data$period), collapse = ", ")
      ),
      stringsAsFactors = FALSE
    )
    
    summary_table
  }, bordered = TRUE, striped = TRUE)
  
  # Milestone details table
  output$acgme_milestone_details <- DT::renderDataTable({
    req(csv_processed_data())
    
    data <- csv_processed_data()
    milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(data), value = TRUE)
    
    # Create milestone details table
    milestone_details <- data.frame(
      Competency = substr(milestone_cols, 1, 2),
      Milestone = milestone_cols,
      Description = paste("Milestone", milestone_cols),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        `Competency Name` = case_when(
          Competency == "PC" ~ "Patient Care",
          Competency == "MK" ~ "Medical Knowledge",
          Competency == "SBP" ~ "Systems-Based Practice",
          Competency == "PBL" ~ "Practice-Based Learning",
          Competency == "PROF" ~ "Professionalism",
          Competency == "ICS" ~ "Interpersonal Communication",
          TRUE ~ "Unknown"
        )
      ) %>%
      select(Competency, `Competency Name`, Milestone, Description) %>%
      arrange(Competency, Milestone)
    
    milestone_details
  }, options = list(pageLength = 15, scrollX = TRUE))
  
  # ========================================================================
  # INDIVIDUAL VISUALIZATIONS
  # ========================================================================
  
  # Individual spider chart
  output$acgme_individual_spider <- renderPlotly({
    req(csv_processed_data(), input$acgme_resident_select, input$acgme_period_select)
    
    tryCatch({
      data <- csv_processed_data()
      
      # Filter data for selected resident and period
      resident_data <- data %>%
        filter(Resident.ID == input$acgme_resident_select)
      
      if (input$acgme_period_select != "Total") {
        resident_data <- resident_data %>%
          filter(period == input$acgme_period_select)
      }
      
      if (nrow(resident_data) == 0) {
        return(plotly_empty() %>%
                 add_annotations(text = "No data available for selected resident/period", 
                                 showarrow = FALSE))
      }
      
      # Calculate competency averages
      milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(data), value = TRUE)
      competencies <- unique(substr(milestone_cols, 1, 2))
      
      competency_scores <- sapply(competencies, function(comp) {
        comp_cols <- milestone_cols[grepl(paste0("^", comp), milestone_cols)]
        if (length(comp_cols) > 0) {
          mean(as.numeric(resident_data[comp_cols]), na.rm = TRUE)
        } else {
          NA
        }
      })
      
      # Remove NA scores
      competency_scores <- competency_scores[!is.na(competency_scores)]
      
      if (length(competency_scores) == 0) {
        return(plotly_empty() %>%
                 add_annotations(text = "No competency scores available", 
                                 showarrow = FALSE))
      }
      
      # Create competency names
      competency_names <- case_when(
        names(competency_scores) == "PC" ~ "Patient Care",
        names(competency_scores) == "MK" ~ "Medical Knowledge",
        names(competency_scores) == "SBP" ~ "Systems-Based Practice",
        names(competency_scores) == "PBL" ~ "Practice-Based Learning",
        names(competency_scores) == "PROF" ~ "Professionalism",
        names(competency_scores) == "ICS" ~ "Interpersonal Communication",
        TRUE ~ names(competency_scores)
      )
      
      # Create spider chart
      plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        r = as.numeric(competency_scores),
        theta = competency_names,
        fill = 'toself',
        name = 'Individual Performance',
        line = list(color = '#2C3E50'),
        marker = list(color = '#3498DB', size = 8)
      ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(0, 9),
              tickmode = 'linear',
              tick0 = 0,
              dtick = 1
            )
          ),
          title = paste("Competency Performance - Resident", input$acgme_resident_select, 
                        "-", input$acgme_period_select)
        )
      
    }, error = function(e) {
      plotly_empty() %>%
        add_annotations(text = paste("Error creating chart:", e$message), 
                        showarrow = FALSE)
    })
  })
  
  # ========================================================================
  # MODULE SERVERS (IF AVAILABLE)
  # ========================================================================
  
  # Program overview module (if exists)
  observe({
    req(csv_processed_data())
    
    if (exists("mod_acgme_program_overview_server")) {
      tryCatch({
        mod_acgme_program_overview_server(
          "acgme_program_overview",
          processed_data = reactive(csv_processed_data())
        )
      }, error = function(e) {
        message("Program overview server error: ", e$message)
      })
    }
  })
  
  # ========================================================================
  # SESSION MANAGEMENT
  # ========================================================================
  
  # Clean up on session end
  session$onSessionEnded(function() {
    csv_processed_data(NULL)
  })
  
  # ========================================================================
  # DEBUG OUTPUT
  # ========================================================================
  
  observe({
    if (!is.null(csv_processed_data())) {
      cat("CSV data loaded with", csv_processed_data()$n_residents, "residents\n")
    }
  })
}