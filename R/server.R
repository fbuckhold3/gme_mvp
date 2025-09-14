# =============================================================================
# GME MILESTONE VISUALIZATION PROJECT - SERVER (Complete rewrite using working code)
# R/server.R
# =============================================================================

server <- function(input, output, session) {
  
  # =============================================================================
  # REACTIVE DATA PROCESSING (Using exact working functions)
  # =============================================================================
  
  # Load and process uploaded CSV files
  raw_data <- reactive({
    req(input$files)
    file_paths <- input$files$datapath
    import_and_process_milestones(file_paths)
  })
  
  # Program information detection
  program_info <- reactive({
    req(raw_data())
    detect_program_info(raw_data())
  })
  
  # Data loaded indicator
  output$data_loaded <- reactive({
    !is.null(raw_data())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # =============================================================================
  # DYNAMIC UI COMPONENTS
  # =============================================================================
  
  # Period selector
  output$period_selector <- renderUI({
    req(program_info())
    periods <- program_info()$periods
    
    # Default to Year-End if available, otherwise first period
    default_period <- if (any(grepl("Year-End", periods, ignore.case = TRUE))) {
      periods[grepl("Year-End", periods, ignore.case = TRUE)][1]
    } else {
      periods[1]
    }
    
    selectInput("period_filter", "Evaluation Period:",
                choices = c("All Periods" = "All", periods),
                selected = default_period)
  })
  
  # Question index availability
  output$question_index_available <- reactive({
    exists("question_index") && nrow(question_index) > 0
  })
  outputOptions(output, "question_index_available", suspendWhenHidden = FALSE)
  
  # Question index table
  output$question_index_table <- renderDT({
    if (exists("question_index") && nrow(question_index) > 0) {
      datatable(
        question_index,
        options = list(
          pageLength = 50,
          dom = 't',
          scrollY = "180px",
          scrollCollapse = TRUE,
          autoWidth = FALSE
        ),
        rownames = FALSE,
        colnames = c("Sub-Competency", "Description"),
        class = 'cell-border stripe compact'
      )
    }
  })
  
  # =============================================================================
  # DATA PREPARATION FOR VISUALIZATIONS
  # =============================================================================
  
  # Filtered data
  filtered_data <- reactive({
    req(raw_data(), input$period_filter)
    df <- raw_data()
    
    if (input$period_filter != "All") {
      df <- df %>% filter(period == input$period_filter)
    }
    
    df
  })
  
  # Category summary data
  category_data <- reactive({
    req(raw_data())
    prepare_category_summary_data(raw_data(), input$period_filter)
  })
  
  # Individual detail data
  individual_data <- reactive({
    req(raw_data())
    prepare_individual_detail_data(raw_data(), input$period_filter)
  })
  
  # =============================================================================
  # PLOT TITLES
  # =============================================================================
  
  output$main_plot_title <- renderText({
    if (input$view_level == "category") {
      if (input$visualization_type == "heatmap") {
        "Competency Category Overview"
      } else {
        "Category Progression Overview"
      }
    } else {
      if (input$visualization_type == "heatmap") {
        paste0("Individual ", input$selected_category, " Sub-Competencies")
      } else {
        paste0(input$selected_category, " Sub-Competency Progression")
      }
    }
  })
  
  output$secondary_plot_title <- renderText({
    if (input$view_level == "category") {
      if (input$visualization_type == "heatmap") {
        "Category Progression"
      } else {
        "Category Heatmap"
      }
    } else {
      "Category Overview"
    }
  })
  
  # =============================================================================
  # MAIN VISUALIZATIONS
  # =============================================================================
  
  # Main plot
  output$main_plot <- renderPlot({
    if (input$view_level == "category") {
      if (input$visualization_type == "heatmap") {
        create_category_heatmap(category_data())
      } else {
        create_category_progression(category_data())
      }
    } else {
      # Individual detail view
      if (input$visualization_type == "heatmap") {
        # For individual heatmap, show just the selected category's sub-competencies
        individual_filtered <- individual_data() %>% 
          filter(Category == input$selected_category)
        
        if (nrow(individual_filtered) > 0) {
          create_individual_heatmap(individual_filtered, input$selected_category)
        }
      } else {
        create_individual_detail_plot(individual_data(), input$selected_category)
      }
    }
  })
  
  # Secondary plot
  output$secondary_plot <- renderPlot({
    if (input$view_level == "category") {
      if (input$visualization_type == "heatmap") {
        create_category_progression(category_data(), compact = TRUE)
      } else {
        create_category_heatmap(category_data(), compact = TRUE)
      }
    } else {
      # Show category overview when in individual mode
      if (input$visualization_type == "heatmap") {
        create_category_progression(category_data(), compact = TRUE)
      } else {
        create_category_heatmap(category_data(), compact = TRUE)
      }
    }
  })
  
  # =============================================================================
  # SUMMARY TABLE
  # =============================================================================
  
  output$summary_table <- renderDT({
    if (input$view_level == "category") {
      req(category_data())
      
      summary_data <- category_data() %>%
        select(Training_Year, Category, Avg_Score, Competency_Level, Count) %>%
        mutate(Avg_Score = round(Avg_Score, 1)) %>%
        arrange(Training_Year, Category)
      
      datatable(
        summary_data,
        options = list(
          pageLength = 15,
          dom = 't',
          scrollY = "250px",
          scrollCollapse = TRUE,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Training Year", "Category", "Avg Score", "Competency Level", "N"),
        class = 'cell-border stripe compact'
      ) %>%
        formatStyle(
          columns = "Competency_Level",
          backgroundColor = styleEqual(
            c("Beginner", "Advanced Beginner", "Competent", "Proficient", "Expert"),
            c("#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26")
          )
        )
    } else {
      req(individual_data())
      
      # Show individual sub-competencies for selected category
      summary_data <- individual_data() %>%
        filter(Category == input$selected_category) %>%
        select(Training_Year, Milestone, Avg_Score, Competency_Level, Count) %>%
        mutate(Avg_Score = round(Avg_Score, 1)) %>%
        arrange(Training_Year, Milestone)
      
      datatable(
        summary_data,
        options = list(
          pageLength = 15,
          dom = 't',
          scrollY = "250px",
          scrollCollapse = TRUE,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Training Year", "Sub-Competency", "Avg Score", "Competency Level", "N"),
        class = 'cell-border stripe compact'
      ) %>%
        formatStyle(
          columns = "Competency_Level",
          backgroundColor = styleEqual(
            c("Beginner", "Advanced Beginner", "Competent", "Proficient", "Expert"),
            c("#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26")
          )
        )
    }
  })
}