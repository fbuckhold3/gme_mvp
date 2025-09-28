# =============================================================================
# ENHANCED INDIVIDUAL RESIDENT ASSESSMENT WITH CLASS IDENTIFICATION
# individual_assessment_enhanced.R
#
# Enhanced version with graduation class identification for easier comparison
# =============================================================================

# ===== HELPER FUNCTIONS FOR CLASS IDENTIFICATION =====

#' Calculate Graduation Class for Each Resident
#'
#' Determines graduation year based on current PGY level and academic year
#'
#' @param milestone_data Processed data from load_milestone_csv_data()
#' @return Data frame with resident names and their graduation classes
calculate_resident_classes <- function(milestone_data) {
  
  evaluation_data <- milestone_data$evaluations
  
  # Extract program length from data (highest PGY level)
  max_pgy <- evaluation_data %>%
    mutate(pgy_num = as.numeric(str_extract(PGY_Level, "\\d+"))) %>%
    pull(pgy_num) %>%
    max(na.rm = TRUE)
  
  # Calculate graduation classes for each resident
  resident_classes <- evaluation_data %>%
    mutate(
      # Extract academic year and PGY number
      academic_year = str_extract(Period, "^\\d{4}-\\d{4}"),
      pgy_num = as.numeric(str_extract(PGY_Level, "\\d+")),
      start_year = as.numeric(str_extract(academic_year, "^\\d{4}"))
    ) %>%
    filter(!is.na(academic_year), !is.na(pgy_num)) %>%
    group_by(Resident_Name) %>%
    # Use the most recent data for each resident
    arrange(desc(start_year), desc(pgy_num)) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    mutate(
      # Calculate graduation year: start_year + (max_pgy - current_pgy)
      graduation_year = start_year + (max_pgy - pgy_num),
      graduation_class = paste0("Class of ", graduation_year + 1),  # +1 because they graduate in spring
      resident_with_class = paste0(Resident_Name, " (", graduation_class, ")")
    ) %>%
    select(Resident_Name, graduation_year, graduation_class, resident_with_class) %>%
    arrange(desc(graduation_year), Resident_Name)  # Most recent class first, then alphabetical
  
  return(resident_classes)
}

#' Create Resident Choice List with Classes
#'
#' Creates a named list for selectInput with class information
#'
#' @param resident_classes Output from calculate_resident_classes()
#' @return Named list suitable for selectInput choices
create_resident_choices_with_classes <- function(resident_classes) {
  
  if (nrow(resident_classes) == 0) {
    return(NULL)
  }
  
  # Create choices with class information in display names
  choices <- setNames(resident_classes$Resident_Name, 
                      resident_classes$resident_with_class)
  
  return(choices)
}

# ===== UI COMPONENT WITH CLASS IDENTIFICATION =====
create_individual_assessment_ui <- function() {
  nav_panel("Individual Assessment",
            icon = icon("user-check"),
            
            conditionalPanel(
              condition = "output.data_loaded",
              
              # Enhanced tab description
              fluidRow(
                column(12,
                       div(class = "alert alert-info mb-4",
                           h6("Individual Assessment", style = "margin-bottom: 10px; color: #2c3e50;"),
                           p("Focus on individual resident performance with detailed milestone tracking and comparative analysis. Residents are organized by graduation class for easier comparison. Select up to 3 residents to compare their progression over time, performance profiles, and identify specific areas for development or recognition.",
                             style = "margin-bottom: 0; font-size: 0.95em;")
                       )
                )
              ),
              
              # Control Panel and Summary Cards Row
              fluidRow(
                column(3,
                       div(class = "content-card card border-primary h-100",
                           div(class = "card-header bg-primary text-white",
                               h6(icon("sliders-h"), " Controls", class = "mb-0")
                           ),
                           div(class = "card-body",
                               
                               # Comparison Mode Toggle
                               div(class = "form-group",
                                   checkboxInput("comparison_mode", 
                                                 "Multi-Resident Comparison", 
                                                 value = FALSE),
                                   tags$small("Compare up to 3 residents by graduation class", class = "text-muted")
                               ),
                               
                               hr(),
                               
                               # Class Filter (shown in comparison mode)
                               conditionalPanel(
                                 condition = "input.comparison_mode",
                                 div(class = "form-group",
                                     tags$label(class = "form-label",
                                                "Filter by Class:",
                                                style = "font-weight: bold; color: #6c757d;"),
                                     selectInput("class_filter", "",
                                                 choices = NULL,
                                                 width = "100%"),
                                     tags$small("Optional: Filter residents by graduation class", class = "text-muted")
                                 ),
                                 hr()
                               ),
                               
                               # Primary Resident Selection
                               div(class = "form-group",
                                   tags$label(class = "form-label",
                                              "Primary Resident:",
                                              style = "font-weight: bold; color: #2E86AB;"),
                                   selectInput("individual_resident", "",
                                               choices = NULL,
                                               width = "100%")
                               ),
                               
                               # Additional Residents (shown only in comparison mode)
                               conditionalPanel(
                                 condition = "input.comparison_mode",
                                 div(class = "form-group",
                                     tags$label(class = "form-label",
                                                "Compare with:",
                                                style = "font-weight: bold; color: #A23B72;"),
                                     selectInput("comparison_resident_2", "",
                                                 choices = NULL,
                                                 width = "100%")
                                 ),
                                 div(class = "form-group",
                                     tags$label(class = "form-label",
                                                "Third Resident (Optional):",
                                                style = "font-weight: bold; color: #F18F01;"),
                                     selectInput("comparison_resident_3", "",
                                                 choices = NULL,
                                                 width = "100%")
                                 )
                               ),
                               
                               # Evaluation Level/Period Selection
                               div(class = "form-group",
                                   tags$label(class = "form-label",
                                              "Evaluation Level:"),
                                   selectInput("individual_level", "",
                                               choices = NULL,
                                               width = "100%")
                               ),
                               
                               # Additional Info Display
                               div(class = "alert alert-light mt-3",
                                   style = "border-left: 4px solid #007bff; padding: 10px;",
                                   uiOutput("resident_info_display")
                               )
                           )
                       )
                ),
                
                # Summary Cards - Dynamic based on comparison mode
                column(9,
                       conditionalPanel(
                         condition = "!input.comparison_mode",
                         # Single resident cards
                         fluidRow(
                           column(3,
                                  div(class = "content-card card border-info text-center h-100",
                                      div(class = "card-body d-flex flex-column justify-content-center",
                                          div(class = "metric-value",
                                              textOutput("individual_total_evaluations"),
                                              style = "font-size: 2rem; font-weight: bold; color: #17a2b8;"
                                          ),
                                          tags$small("Total Evaluations", class = "text-muted")
                                      )
                                  )
                           ),
                           column(3,
                                  div(class = "content-card card border-success text-center h-100",
                                      div(class = "card-body d-flex flex-column justify-content-center",
                                          div(class = "metric-value",
                                              textOutput("individual_avg_score"),
                                              style = "font-size: 2rem; font-weight: bold; color: #28a745;"
                                          ),
                                          tags$small("Average Score", class = "text-muted")
                                      )
                                  )
                           ),
                           column(3,
                                  div(class = "content-card card border-warning text-center h-100",
                                      div(class = "card-body d-flex flex-column justify-content-center",
                                          div(class = "metric-value",
                                              textOutput("program_avg_score"),
                                              style = "font-size: 2rem; font-weight: bold; color: #ffc107;"
                                          ),
                                          tags$small("Program Average", class = "text-muted")
                                      )
                                  )
                           ),
                           column(3,
                                  div(class = "content-card card border-secondary text-center h-100",
                                      div(class = "card-body d-flex flex-column justify-content-center",
                                          div(class = "metric-value",
                                              textOutput("individual_percentile"),
                                              style = "font-size: 2rem; font-weight: bold; color: #6c757d;"
                                          ),
                                          tags$small("Program Percentile", class = "text-muted")
                                      )
                                  )
                           )
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "input.comparison_mode",
                         # Comparison summary cards
                         div(class = "comparison-metrics",
                             h6("Comparison Metrics by Graduation Class", class = "text-center mb-3 text-muted"),
                             div(id = "comparison_cards_output",
                                 uiOutput("comparison_summary_cards")
                             )
                         )
                       )
                )
              ),
              
              br(),
              
              # Main Visualization Row
              fluidRow(
                # Spider Plot
                column(6,
                       div(class = "content-card card h-100",
                           div(class = "card-header", 
                               h6(icon("chart-area"), 
                                  uiOutput("spider_plot_title"))
                           ),
                           div(class = "card-body position-relative",
                               plotlyOutput("individual_spider_enhanced", height = "550px")
                           )
                       )
                ),
                
                # Trend Chart
                column(6,
                       div(class = "content-card card h-100",
                           div(class = "card-header", 
                               h6(icon("chart-line"), 
                                  uiOutput("trend_plot_title"))
                           ),
                           div(class = "card-body position-relative",
                               plotlyOutput("individual_trend_enhanced", height = "550px")
                           )
                       )
                )
              ),
              
              br(),
              
              # Detailed Performance Table - Only show in single resident mode
              conditionalPanel(
                condition = "!input.comparison_mode",
                fluidRow(
                  column(12,
                         div(class = "content-card card",
                             div(class = "card-header", 
                                 h6(icon("table"), "Detailed Performance Breakdown")
                             ),
                             div(class = "card-body",
                                 div(class = "alert alert-light mb-3",
                                     style = "border-left: 4px solid #28a745;",
                                     icon("info-circle"), " ",
                                     "This table shows detailed performance metrics for the selected resident compared to program averages. Green values indicate above-average performance, red indicates below-average."
                                 ),
                                 DT::dataTableOutput("individual_detail_table")
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
                  p("Please upload milestone data or try the demo from the Get Started tab to begin individual assessment."),
                  actionButton("goto_upload", 
                               HTML('<i class="fas fa-upload"></i> Go to Get Started'),
                               class = "btn btn-primary")
              )
            )
  )
}

# ===== ENHANCED SERVER FUNCTIONS WITH CLASS IDENTIFICATION =====

# Update resident choices for all selectors with class information
update_individual_residents <- function(session, milestone_data) {
  if (is.null(milestone_data) || is.null(milestone_data$evaluations)) {
    updateSelectInput(session, "individual_resident", choices = NULL)
    updateSelectInput(session, "comparison_resident_2", choices = NULL)
    updateSelectInput(session, "comparison_resident_3", choices = NULL)
    updateSelectInput(session, "class_filter", choices = NULL)
    return()
  }
  
  # Calculate resident classes
  resident_classes <- calculate_resident_classes(milestone_data)
  
  if (nrow(resident_classes) == 0) {
    updateSelectInput(session, "individual_resident", choices = NULL)
    updateSelectInput(session, "comparison_resident_2", choices = NULL)
    updateSelectInput(session, "comparison_resident_3", choices = NULL)
    updateSelectInput(session, "class_filter", choices = NULL)
    return()
  }
  
  # Create choices with class information
  resident_choices <- create_resident_choices_with_classes(resident_classes)
  comparison_choices <- c("None" = "", resident_choices)
  
  # Update resident selectors
  updateSelectInput(session, "individual_resident", 
                    choices = resident_choices,
                    selected = NULL)
  updateSelectInput(session, "comparison_resident_2", 
                    choices = comparison_choices,
                    selected = "")
  updateSelectInput(session, "comparison_resident_3", 
                    choices = comparison_choices,
                    selected = "")
  
  # Update class filter
  class_choices <- c("All Classes" = "all", 
                     setNames(unique(resident_classes$graduation_class), 
                              unique(resident_classes$graduation_class)))
  updateSelectInput(session, "class_filter", 
                    choices = class_choices,
                    selected = "all")
}

# Filter resident choices based on selected class
filter_residents_by_class <- function(session, milestone_data, selected_class) {
  if (is.null(milestone_data) || is.null(selected_class) || selected_class == "all") {
    # Show all residents
    update_individual_residents(session, milestone_data)
    return()
  }
  
  # Calculate resident classes
  resident_classes <- calculate_resident_classes(milestone_data)
  
  # Filter by selected class
  filtered_residents <- resident_classes %>%
    filter(graduation_class == selected_class)
  
  if (nrow(filtered_residents) == 0) {
    updateSelectInput(session, "individual_resident", choices = NULL)
    updateSelectInput(session, "comparison_resident_2", choices = c("None" = ""))
    updateSelectInput(session, "comparison_resident_3", choices = c("None" = ""))
    return()
  }
  
  # Create filtered choices
  resident_choices <- create_resident_choices_with_classes(filtered_residents)
  comparison_choices <- c("None" = "", resident_choices)
  
  # Get current selections to preserve them if they're still valid
  current_primary <- session$input$individual_resident
  current_secondary <- session$input$comparison_resident_2
  current_tertiary <- session$input$comparison_resident_3
  
  # Check if current selections are still valid
  valid_residents <- filtered_residents$Resident_Name
  primary_selected <- if (!is.null(current_primary) && current_primary %in% valid_residents) current_primary else NULL
  secondary_selected <- if (!is.null(current_secondary) && current_secondary %in% valid_residents) current_secondary else ""
  tertiary_selected <- if (!is.null(current_tertiary) && current_tertiary %in% valid_residents) current_tertiary else ""
  
  # Update selectors with filtered choices
  updateSelectInput(session, "individual_resident", 
                    choices = resident_choices,
                    selected = primary_selected)
  updateSelectInput(session, "comparison_resident_2", 
                    choices = comparison_choices,
                    selected = secondary_selected)
  updateSelectInput(session, "comparison_resident_3", 
                    choices = comparison_choices,
                    selected = tertiary_selected)
}

# Get graduation class for a resident
get_resident_class <- function(milestone_data, resident_name) {
  if (is.null(milestone_data) || is.null(resident_name) || resident_name == "") {
    return("")
  }
  
  resident_classes <- calculate_resident_classes(milestone_data)
  class_info <- resident_classes %>%
    filter(Resident_Name == resident_name) %>%
    pull(graduation_class)
  
  if (length(class_info) > 0) {
    return(class_info[1])
  } else {
    return("Unknown Class")
  }
}

# Enhanced comparison summary statistics with class information
calculate_comparison_summary_stats <- function(milestone_data, residents, selected_level) {
  # Remove empty residents
  residents <- residents[residents != "" & !is.na(residents)]
  
  if (length(residents) == 0) {
    return(data.frame())
  }
  
  # Get class information for residents
  resident_classes <- calculate_resident_classes(milestone_data)
  
  results <- data.frame(
    Resident = character(),
    Graduation_Class = character(),
    Total_Evaluations = integer(),
    Average_Score = numeric(),
    Percentile = integer(),
    Has_Data = logical(),
    stringsAsFactors = FALSE
  )
  
  for (resident in residents) {
    # Get class info
    class_info <- resident_classes %>%
      filter(Resident_Name == resident) %>%
      pull(graduation_class)
    graduation_class <- if (length(class_info) > 0) class_info[1] else "Unknown"
    
    # Filter data for this resident
    resident_data <- milestone_data$evaluations[milestone_data$evaluations$Resident_Name == resident, ]
    
    # Apply level filter if not "all"
    if (selected_level != "all") {
      level_parts <- strsplit(selected_level, "\\|\\|\\|")[[1]]
      if (length(level_parts) == 2) {
        period_filter <- level_parts[1]
        pgy_filter <- level_parts[2]
        resident_data <- resident_data[resident_data$Period == period_filter & 
                                         resident_data$PGY_Level == pgy_filter, ]
      }
    }
    
    if (nrow(resident_data) == 0) {
      # No data for this resident
      results <- rbind(results, data.frame(
        Resident = resident,
        Graduation_Class = graduation_class,
        Total_Evaluations = 0,
        Average_Score = NA,
        Percentile = NA,
        Has_Data = FALSE,
        stringsAsFactors = FALSE
      ))
    } else {
      # Calculate stats
      individual_avg <- round(mean(resident_data$Rating, na.rm = TRUE), 2)
      total_evaluations <- nrow(resident_data)
      
      # Calculate percentile
      program_data <- milestone_data$evaluations
      if (selected_level != "all") {
        level_parts <- strsplit(selected_level, "\\|\\|\\|")[[1]]
        if (length(level_parts) == 2) {
          period_filter <- level_parts[1]
          pgy_filter <- level_parts[2]
          program_data <- program_data[program_data$Period == period_filter & 
                                         program_data$PGY_Level == pgy_filter, ]
        }
      }
      
      all_residents <- program_data %>%
        group_by(Resident_Name) %>%
        summarise(resident_avg = mean(Rating, na.rm = TRUE), .groups = "drop") %>%
        filter(!is.na(resident_avg))
      
      percentile <- round(100 * mean(all_residents$resident_avg <= individual_avg, na.rm = TRUE), 0)
      
      results <- rbind(results, data.frame(
        Resident = resident,
        Graduation_Class = graduation_class,
        Total_Evaluations = total_evaluations,
        Average_Score = individual_avg,
        Percentile = percentile,
        Has_Data = TRUE,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(results)
}

# ===== SERVER INTEGRATION WITH CLASS IDENTIFICATION =====

# Enhanced server outputs for individual assessment tab
render_individual_assessment_outputs <- function(input, output, session, milestone_data) {
  
  # Update resident choices when data changes
  observeEvent(milestone_data(), {
    update_individual_residents(session, milestone_data())
  })
  
  # Update resident choices when class filter changes
  observeEvent(input$class_filter, {
    req(milestone_data(), input$class_filter)
    filter_residents_by_class(session, milestone_data(), input$class_filter)
  })
  
  # Update level choices when primary resident changes
  observeEvent(input$individual_resident, {
    req(milestone_data(), input$individual_resident)
    update_individual_levels(session, milestone_data(), input$individual_resident)
  })
  
  # Dynamic plot titles
  output$spider_plot_title <- renderUI({
    if (isTRUE(input$comparison_mode)) {
      span("Multi-Resident Performance Comparison by Class")
    } else {
      span("Performance Profile vs Program Mean")
    }
  })
  
  output$trend_plot_title <- renderUI({
    if (isTRUE(input$comparison_mode)) {
      span("Multi-Resident Progression by Class")
    } else {
      span("Progression Over Time vs Program")
    }
  })
  
  # Enhanced resident info display with class information
  output$resident_info_display <- renderUI({
    if (isTRUE(input$comparison_mode)) {
      selected_residents <- c(input$individual_resident, 
                              input$comparison_resident_2, 
                              input$comparison_resident_3)
      selected_residents <- selected_residents[selected_residents != "" & !is.na(selected_residents)]
      
      if (length(selected_residents) == 0) {
        HTML("<small><strong>Select residents to compare</strong></small>")
      } else {
        req(milestone_data())
        
        # Get class information for each resident
        resident_info <- sapply(selected_residents, function(resident) {
          class_info <- get_resident_class(milestone_data(), resident)
          paste0(resident, " (", class_info, ")")
        })
        
        HTML(paste0("<small><strong>Comparing:</strong><br>", 
                    paste(resident_info, collapse = "<br>"), "</small>"))
      }
    } else {
      req(milestone_data(), input$individual_resident, input$individual_level)
      
      # Get class information for primary resident
      class_info <- get_resident_class(milestone_data(), input$individual_resident)
      
      if (input$individual_level == "all") {
        info_text <- paste0("Resident: ", input$individual_resident, " (", class_info, ")<br>",
                            "Showing all evaluations across all periods")
      } else {
        level_parts <- strsplit(input$individual_level, "\\|\\|\\|")[[1]]
        if (length(level_parts) == 2) {
          info_text <- paste0("Resident: ", input$individual_resident, " (", class_info, ")<br>",
                              "Evaluation Period: ", level_parts[1], "<br>",
                              "PGY Level: ", level_parts[2])
        } else {
          info_text <- paste0("Resident: ", input$individual_resident, " (", class_info, ")<br>",
                              "Selected evaluation level")
        }
      }
      
      HTML(paste0("<small><strong>Current Selection:</strong><br>", info_text, "</small>"))
    }
  })
  
  # Enhanced comparison summary cards with class information
  output$comparison_summary_cards <- renderUI({
    req(milestone_data(), input$comparison_mode)
    
    residents <- c(input$individual_resident, 
                   input$comparison_resident_2, 
                   input$comparison_resident_3)
    residents <- residents[residents != "" & !is.na(residents)]
    
    if (length(residents) == 0) {
      return(div(class = "alert alert-light text-center",
                 "Select residents to see comparison metrics"))
    }
    
    comparison_stats <- calculate_comparison_summary_stats(milestone_data(), residents, input$individual_level)
    
    if (nrow(comparison_stats) == 0) {
      return(div(class = "alert alert-warning text-center",
                 "No data available for comparison"))
    }
    
    # Define colors for residents
    resident_colors <- c("#2E86AB", "#A23B72", "#F18F01")
    
    # Create cards for each resident
    cards <- list()
    
    for (i in 1:nrow(comparison_stats)) {
      stat <- comparison_stats[i, ]
      color <- resident_colors[min(i, 3)]
      
      # Determine card styling based on data availability
      if (stat$Has_Data) {
        card_class <- "content-card card h-100"
        text_color <- color
        opacity <- "1"
      } else {
        card_class <- "content-card card h-100 border-secondary"
        text_color <- "#6c757d"
        opacity <- "0.6"
      }
      
      cards[[i]] <- column(4,
                           div(class = card_class, style = paste0("opacity: ", opacity),
                               div(class = "card-header text-center", 
                                   style = paste0("background-color: ", color, "; color: white; border-color: ", color),
                                   h6(stat$Resident, class = "mb-0", style = "font-weight: bold; font-size: 0.9rem;"),
                                   tags$small(stat$Graduation_Class, style = "opacity: 0.9; font-size: 0.8rem;")
                               ),
                               div(class = "card-body text-center p-2",
                                   # Total Evaluations
                                   div(class = "metric-row mb-2",
                                       div(class = "metric-value", 
                                           style = paste0("font-size: 1.4rem; font-weight: bold; color: ", text_color),
                                           stat$Total_Evaluations),
                                       tags$small("Total Evaluations", class = "text-muted d-block")
                                   ),
                                   
                                   # Average Score
                                   div(class = "metric-row mb-2",
                                       div(class = "metric-value", 
                                           style = paste0("font-size: 1.4rem; font-weight: bold; color: ", text_color),
                                           if (stat$Has_Data) stat$Average_Score else "N/A"),
                                       tags$small("Average Score", class = "text-muted d-block")
                                   ),
                                   
                                   # Percentile
                                   div(class = "metric-row",
                                       div(class = "metric-value", 
                                           style = paste0("font-size: 1.4rem; font-weight: bold; color: ", text_color),
                                           if (stat$Has_Data) paste0(stat$Percentile, "%") else "N/A"),
                                       tags$small("Percentile", class = "text-muted d-block")
                                   )
                               )
                           )
      )
    }
    
    # Add program average card if we have space
    if (length(cards) < 3) {
      program_data <- milestone_data()$evaluations
      if (input$individual_level != "all") {
        level_parts <- strsplit(input$individual_level, "\\|\\|\\|")[[1]]
        if (length(level_parts) == 2) {
          period_filter <- level_parts[1]
          pgy_filter <- level_parts[2]
          program_data <- program_data[program_data$Period == period_filter & 
                                         program_data$PGY_Level == pgy_filter, ]
        }
      }
      
      program_avg <- round(mean(program_data$Rating, na.rm = TRUE), 2)
      
      cards[[length(cards) + 1]] <- column(4,
                                           div(class = "content-card card h-100 border-info",
                                               div(class = "card-header text-center bg-info text-white",
                                                   h6("Program Average", class = "mb-0", style = "font-weight: bold; font-size: 0.9rem;"),
                                                   tags$small("All Classes", style = "opacity: 0.9; font-size: 0.8rem;")
                                               ),
                                               div(class = "card-body text-center p-2",
                                                   div(class = "metric-row mb-2",
                                                       div(class = "metric-value", 
                                                           style = "font-size: 1.4rem; font-weight: bold; color: #17a2b8",
                                                           n_distinct(program_data$Resident_Name)),
                                                       tags$small("Total Residents", class = "text-muted d-block")
                                                   ),
                                                   
                                                   div(class = "metric-row mb-2",
                                                       div(class = "metric-value", 
                                                           style = "font-size: 1.4rem; font-weight: bold; color: #17a2b8",
                                                           program_avg),
                                                       tags$small("Average Score", class = "text-muted d-block")
                                                   ),
                                                   
                                                   div(class = "metric-row",
                                                       div(class = "metric-value", 
                                                           style = "font-size: 1.4rem; font-weight: bold; color: #17a2b8",
                                                           "50%"),
                                                       tags$small("Median Percentile", class = "text-muted d-block")
                                                   )
                                               )
                                           )
      )
    }
    
    return(fluidRow(cards))
  })
  
  # Summary statistics for cards (single resident mode only)
  individual_stats <- reactive({
    req(milestone_data(), input$individual_resident, input$individual_level)
    if (isTRUE(input$comparison_mode)) return(NULL)
    calculate_individual_summary_stats(milestone_data(), input$individual_resident, input$individual_level)
  })
  
  # Single resident summary cards
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
  
  # Enhanced spider plot with class information in legends
  output$individual_spider_enhanced <- renderPlotly({
    req(milestone_data(), input$individual_resident)
    
    tryCatch({
      if (isTRUE(input$comparison_mode)) {
        # Multi-resident comparison with class information
        residents <- c(input$individual_resident, 
                       input$comparison_resident_2, 
                       input$comparison_resident_3)
        create_individual_spider_comparison_with_classes(
          milestone_data(), 
          residents, 
          input$individual_level
        )
      } else {
        # Single resident with program comparison
        create_individual_spider_enhanced(
          milestone_data(), 
          input$individual_resident, 
          input$individual_level
        )
      }
    }, error = function(e) {
      plot_ly() %>% 
        add_annotations(text = paste("Error creating spider plot:", e$message), 
                        x = 0.5, y = 0.5, showarrow = FALSE,
                        font = list(color = "red"))
    })
  })
  
  # Enhanced trend chart with class information in legends
  output$individual_trend_enhanced <- renderPlotly({
    req(milestone_data(), input$individual_resident)
    
    tryCatch({
      if (isTRUE(input$comparison_mode)) {
        # Multi-resident comparison with class information
        residents <- c(input$individual_resident, 
                       input$comparison_resident_2, 
                       input$comparison_resident_3)
        create_individual_trend_comparison_with_classes(
          milestone_data(), 
          residents
        )
      } else {
        # Single resident with program comparison
        create_individual_trend_enhanced(
          milestone_data(), 
          input$individual_resident
        )
      }
    }, error = function(e) {
      plot_ly() %>% 
        add_annotations(text = paste("Error creating trend chart:", e$message), 
                        x = 0.5, y = 0.5, showarrow = FALSE,
                        font = list(color = "red"))
    })
  })
  
  # Detailed performance table (single resident mode only)
  output$individual_detail_table <- DT::renderDataTable({
    req(milestone_data(), input$individual_resident, input$individual_level)
    if (isTRUE(input$comparison_mode)) return(data.frame())
    
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
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel'),
    columnDefs = list(
      list(targets = c(2, 3, 4, 8), className = 'dt-center'),
      list(targets = 5, className = 'dt-center'),
      list(targets = 4, 
           createdCell = JS(
             "function (td, cellData, rowData, row, col) {",
             "  if (cellData > 0) {",
             "    $(td).css('color', 'green');",
             "    $(td).css('font-weight', 'bold');",
             "  } else if (cellData < 0) {",
             "    $(td).css('color', 'red');", 
             "    $(td).css('font-weight', 'bold');",
             "  }",
             "}"
           ))
    )
  ), server = TRUE)
}

# ===== ENHANCED VISUALIZATION FUNCTIONS WITH CLASS INFORMATION =====

# Enhanced spider plot for multi-resident comparison with class info
create_individual_spider_comparison_with_classes <- function(milestone_data, residents, selected_level) {
  # Remove empty/null residents
  residents <- residents[residents != "" & !is.na(residents)]
  
  if (length(residents) == 0) {
    return(plot_ly() %>% 
             add_annotations(text = "Please select at least one resident", 
                             x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Get class information for residents
  resident_classes <- calculate_resident_classes(milestone_data)
  
  # Define colors for up to 3 residents
  resident_colors <- c("#2E86AB", "#A23B72", "#F18F01")
  names(resident_colors) <- c("Primary", "Secondary", "Tertiary")
  
  fig <- plot_ly(type = 'scatterpolar')
  
  all_scores <- data.frame()
  
  for (i in seq_along(residents)) {
    resident <- residents[i]
    
    # Get class information
    class_info <- resident_classes %>%
      filter(Resident_Name == resident) %>%
      pull(graduation_class)
    class_label <- if (length(class_info) > 0) class_info[1] else "Unknown Class"
    
    # Filter data for this resident
    resident_data <- milestone_data$evaluations[milestone_data$evaluations$Resident_Name == resident, ]
    
    # Apply level filter if not "all"
    if (selected_level != "all") {
      level_parts <- strsplit(selected_level, "\\|\\|\\|")[[1]]
      if (length(level_parts) == 2) {
        period_filter <- level_parts[1]
        pgy_filter <- level_parts[2]
        resident_data <- resident_data[resident_data$Period == period_filter & 
                                         resident_data$PGY_Level == pgy_filter, ]
      }
    }
    
    if (nrow(resident_data) == 0) next
    
    # Calculate individual scores by sub-competency
    individual_scores <- resident_data %>%
      group_by(Sub_Competency, Competency) %>%
      summarise(
        score = median(Rating, na.rm = TRUE),
        n_evaluations = n(),
        .groups = "drop"
      ) %>%
      filter(n_evaluations >= 1) %>%
      mutate(resident = resident)
    
    all_scores <- rbind(all_scores, individual_scores)
    
    # Create legend name with class information
    legend_name <- paste0(resident, " (", class_label, ")")
    
    # Add trace for this resident
    fig <- fig %>% add_trace(
      r = individual_scores$score,
      theta = individual_scores$Sub_Competency,
      name = legend_name,
      type = 'scatterpolar',
      mode = 'lines+markers',
      line = list(color = resident_colors[i], width = 3),
      marker = list(color = resident_colors[i], size = 8),
      fill = ifelse(i == 1, 'toself', 'none'),
      fillcolor = if (i == 1) paste0(substr(resident_colors[i], 1, 7), "20") else NULL,
      hovertemplate = paste0(
        '<b>%{theta}</b><br>',
        resident, ' (', class_label, '): %{r}<br>',
        'Competency: ', individual_scores$Competency, '<br>',
        'Evaluations: ', individual_scores$n_evaluations,
        '<extra></extra>'
      )
    )
  }
  
  # Calculate program means for comparison if we have multiple residents
  program_data <- milestone_data$evaluations
  if (selected_level != "all") {
    level_parts <- strsplit(selected_level, "\\|\\|\\|")[[1]]
    if (length(level_parts) == 2) {
      period_filter <- level_parts[1]
      pgy_filter <- level_parts[2]
      program_data <- program_data[program_data$Period == period_filter & 
                                     program_data$PGY_Level == pgy_filter, ]
    }
  }
  
  program_scores <- program_data %>%
    group_by(Sub_Competency, Competency) %>%
    summarise(
      program_mean = mean(Rating, na.rm = TRUE),
      program_n = n(),
      unique_residents = n_distinct(Resident_Name),
      .groups = "drop"
    ) %>%
    filter(program_n >= 5 & unique_residents >= 3)
  
  # Add program mean if we have sufficient data
  if (nrow(program_scores) > 0 && nrow(all_scores) > 0) {
    # Get common sub-competencies
    common_subcomp <- intersect(unique(all_scores$Sub_Competency), 
                                unique(program_scores$Sub_Competency))
    
    if (length(common_subcomp) > 0) {
      program_filtered <- program_scores %>% 
        filter(Sub_Competency %in% common_subcomp) %>%
        arrange(match(Sub_Competency, unique(all_scores$Sub_Competency)))
      
      fig <- fig %>% add_trace(
        r = program_filtered$program_mean,
        theta = program_filtered$Sub_Competency,
        name = "Program Mean (All Classes)",
        type = 'scatterpolar',
        mode = 'lines+markers',
        line = list(color = '#E0E0E0', width = 2, dash = 'dash'),
        marker = list(color = '#E0E0E0', size = 6, symbol = 'diamond'),
        hovertemplate = paste0(
          '<b>%{theta}</b><br>',
          'Program Mean: %{r}<br>',
          'Based on ', program_filtered$unique_residents, ' residents (all classes)',
          '<extra></extra>'
        )
      )
    }
  }
  
  # Layout configuration
  level_text <- if (selected_level == "all") "All Evaluations" else {
    level_parts <- strsplit(selected_level, "\\|\\|\\|")[[1]]
    if (length(level_parts) == 2) paste(level_parts[2], "-", level_parts[1]) else selected_level
  }
  
  fig <- fig %>% layout(
    polar = list(
      radialaxis = list(
        visible = TRUE,
        range = c(1, 9),
        tickmode = 'linear',
        tick0 = 1,
        dtick = 1,
        showticklabels = TRUE,
        tickfont = list(size = 10),
        gridcolor = 'rgba(128, 128, 128, 0.3)'
      ),
      angularaxis = list(
        tickfont = list(size = 11),
        rotation = 90,
        direction = "clockwise"
      )
    ),
    title = list(
      text = paste("Resident Comparison by Class -", level_text),
      font = list(size = 14),
      y = 0.95
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.15,
      font = list(size = 10)
    ),
    showlegend = TRUE,
    margin = list(t = 80, b = 80, l = 60, r = 60)
  )
  
  return(fig)
}

# Enhanced trend chart for multi-resident comparison with class info
create_individual_trend_comparison_with_classes <- function(milestone_data, residents) {
  # Remove empty/null residents
  residents <- residents[residents != "" & !is.na(residents)]
  
  if (length(residents) == 0) {
    return(plot_ly() %>% 
             add_annotations(text = "Please select at least one resident", 
                             x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Get class information for residents
  resident_classes <- calculate_resident_classes(milestone_data)
  
  # Determine the program length from the overall data
  max_pgy <- milestone_data$evaluations %>%
    pull(PGY_Level) %>%
    str_extract("\\d+") %>%
    as.numeric() %>%
    max(na.rm = TRUE)
  
  min_pgy <- milestone_data$evaluations %>%
    pull(PGY_Level) %>%
    str_extract("\\d+") %>%
    as.numeric() %>%
    min(na.rm = TRUE)
  
  # Create complete timeline
  complete_timeline <- data.frame(
    PGY_Year = rep(min_pgy:max_pgy, each = 2),
    Period_Type = rep(c("Mid-Year", "Year-End"), times = (max_pgy - min_pgy + 1))
  ) %>%
    mutate(
      Period_Order = case_when(
        Period_Type == "Mid-Year" ~ (PGY_Year - 1) * 2 + 1,
        Period_Type == "Year-End" ~ (PGY_Year - 1) * 2 + 2
      ),
      Period_Label = paste0(Period_Type, " PGY-", PGY_Year)
    ) %>%
    arrange(Period_Order)
  
  # Define colors
  resident_colors <- c("#2E86AB", "#A23B72", "#F18F01")
  
  fig <- plot_ly()
  
  all_data_available <- FALSE
  
  for (i in seq_along(residents)) {
    resident <- residents[i]
    
    # Get class information
    class_info <- resident_classes %>%
      filter(Resident_Name == resident) %>%
      pull(graduation_class)
    class_label <- if (length(class_info) > 0) class_info[1] else "Unknown Class"
    
    # Get resident data
    resident_data <- milestone_data$evaluations[milestone_data$evaluations$Resident_Name == resident, ]
    
    if (nrow(resident_data) == 0) next
    
    # Process resident trend data
    resident_trend <- resident_data %>%
      mutate(
        PGY_Year = as.numeric(str_extract(PGY_Level, "\\d+")),
        Period_Type = case_when(
          str_detect(Period, "Mid-Year|Mid Year") ~ "Mid-Year",
          str_detect(Period, "Year-End|End Year") ~ "Year-End",
          TRUE ~ "Other"
        )
      ) %>%
      filter(Period_Type %in% c("Mid-Year", "Year-End")) %>%
      mutate(
        Period_Order = case_when(
          Period_Type == "Mid-Year" ~ (PGY_Year - 1) * 2 + 1,
          Period_Type == "Year-End" ~ (PGY_Year - 1) * 2 + 2,
          TRUE ~ 999
        ),
        Period_Label = paste0(Period_Type, " PGY-", PGY_Year)
      ) %>%
      filter(Period_Order < 999) %>%
      group_by(Period_Order, Period_Label) %>%
      summarise(
        individual_score = mean(Rating, na.rm = TRUE),
        individual_n = n(),
        .groups = "drop"
      ) %>%
      arrange(Period_Order)
    
    if (nrow(resident_trend) > 0) {
      all_data_available <- TRUE
      
      # Create legend name with class information
      legend_name <- paste0(resident, " (", class_label, ")")
      
      fig <- fig %>% add_trace(
        data = resident_trend,
        x = ~Period_Order,
        y = ~individual_score,
        type = 'scatter',
        mode = 'lines+markers',
        name = legend_name,
        line = list(color = resident_colors[i], width = 3),
        marker = list(color = resident_colors[i], size = 10, symbol = 'circle'),
        hovertemplate = paste0(
          '<b>', resident, ' (', class_label, ')</b><br>',
          'Period: ', resident_trend$Period_Label, '<br>',
          'Score: %{y:.2f}<br>',
          'Evaluations: ', resident_trend$individual_n,
          '<extra></extra>'
        )
      )
    }
  }
  
  if (!all_data_available) {
    return(plot_ly() %>% 
             add_annotations(text = "No trend data available for selected residents", 
                             x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Layout configuration
  fig <- fig %>% layout(
    title = list(
      text = paste("Training Progression Comparison by Class"),
      font = list(size = 14),
      y = 0.95
    ),
    xaxis = list(
      title = "Training Period",
      tickmode = 'array',
      tickvals = complete_timeline$Period_Order,
      ticktext = complete_timeline$Period_Label,
      tickangle = -45,
      tickfont = list(size = 10)
    ),
    yaxis = list(
      title = "Average Score",
      range = c(1, 9),
      tickmode = 'linear',
      tick0 = 1,
      dtick = 1,
      tickfont = list(size = 10)
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.25,
      font = list(size = 10)
    ),
    hovermode = 'x unified',
    margin = list(t = 80, b = 100, l = 60, r = 40)
  )
  
  return(fig)
}

# ===== HELPER FUNCTIONS (EXISTING FUNCTIONS) =====

# Update evaluation level choices based on selected resident
update_individual_levels <- function(session, milestone_data, selected_resident) {
  if (is.null(milestone_data) || is.null(selected_resident) || selected_resident == "") {
    updateSelectInput(session, "individual_level", choices = NULL)
    return()
  }
  
  # Get available periods and levels for this resident
  resident_data <- milestone_data$evaluations[milestone_data$evaluations$Resident_Name == selected_resident, ]
  
  if (nrow(resident_data) == 0) {
    updateSelectInput(session, "individual_level", choices = NULL)
    return()
  }
  
  # Create comprehensive level combinations
  level_combinations <- resident_data %>%
    select(Period, PGY_Level) %>%
    distinct() %>%
    arrange(desc(PGY_Level), Period) %>%
    mutate(
      display_name = paste0(PGY_Level, " - ", Period),
      value = paste0(Period, "|||", PGY_Level)  # Use separator for parsing
    )
  
  # Create choices list with most recent/highest as first option
  level_choices <- setNames(level_combinations$value, level_combinations$display_name)
  
  # Add an "All Evaluations" option at the beginning
  level_choices <- c("All Evaluations" = "all", level_choices)
  
  updateSelectInput(session, "individual_level", 
                    choices = level_choices,
                    selected = level_choices[2])  # Select the highest/most recent by default
}

# Calculate summary statistics for single resident
calculate_individual_summary_stats <- function(milestone_data, selected_resident, selected_level) {
  if (is.null(milestone_data) || is.null(selected_resident) || selected_resident == "") {
    return(list(
      total_evaluations = 0,
      individual_avg = 0,
      program_avg = 0,
      percentile = 0
    ))
  }
  
  # Filter data for selected resident
  resident_data <- milestone_data$evaluations[milestone_data$evaluations$Resident_Name == selected_resident, ]
  
  # Apply level filter if not "all"
  if (selected_level != "all") {
    level_parts <- strsplit(selected_level, "\\|\\|\\|")[[1]]
    if (length(level_parts) == 2) {
      period_filter <- level_parts[1]
      pgy_filter <- level_parts[2]
      resident_data <- resident_data[resident_data$Period == period_filter & 
                                       resident_data$PGY_Level == pgy_filter, ]
    }
  }
  
  if (nrow(resident_data) == 0) {
    return(list(
      total_evaluations = 0,
      individual_avg = 0,
      program_avg = 0,
      percentile = 0
    ))
  }
  
  # Calculate individual statistics
  individual_avg <- round(mean(resident_data$Rating, na.rm = TRUE), 2)
  total_evaluations <- nrow(resident_data)
  
  # Calculate program average for comparison
  program_data <- milestone_data$evaluations
  if (selected_level != "all") {
    level_parts <- strsplit(selected_level, "\\|\\|\\|")[[1]]
    if (length(level_parts) == 2) {
      period_filter <- level_parts[1]
      pgy_filter <- level_parts[2]
      program_data <- program_data[program_data$Period == period_filter & 
                                     program_data$PGY_Level == pgy_filter, ]
    }
  }
  
  program_avg <- round(mean(program_data$Rating, na.rm = TRUE), 2)
  
  # Estimate percentile (simplified calculation)
  all_residents <- program_data %>%
    group_by(Resident_Name) %>%
    summarise(resident_avg = mean(Rating, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(resident_avg))
  
  percentile <- round(100 * mean(all_residents$resident_avg <= individual_avg, na.rm = TRUE), 0)
  
  return(list(
    total_evaluations = total_evaluations,
    individual_avg = individual_avg,
    program_avg = program_avg,
    percentile = percentile
  ))
}

# Enhanced spider plot (single resident, existing function with minor enhancements)
create_individual_spider_enhanced <- function(milestone_data, selected_resident, selected_level) {
  # [Existing function code remains the same - no changes needed for single resident mode]
  # This function already works perfectly for single resident analysis
  
  if (is.null(milestone_data) || is.null(selected_resident) || selected_resident == "") {
    return(plot_ly() %>% 
             add_annotations(text = "Please select a resident", 
                             x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Filter data for selected resident
  resident_data <- milestone_data$evaluations[milestone_data$evaluations$Resident_Name == selected_resident, ]
  
  # Apply level filter if not "all"
  if (selected_level != "all") {
    level_parts <- strsplit(selected_level, "\\|\\|\\|")[[1]]
    if (length(level_parts) == 2) {
      period_filter <- level_parts[1]
      pgy_filter <- level_parts[2]
      resident_data <- resident_data[resident_data$Period == period_filter & 
                                       resident_data$PGY_Level == pgy_filter, ]
    }
  }
  
  if (nrow(resident_data) == 0) {
    return(plot_ly() %>% 
             add_annotations(text = "No data available for selected criteria", 
                             x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Calculate individual scores by sub-competency
  individual_scores <- resident_data %>%
    group_by(Sub_Competency, Competency) %>%
    summarise(
      individual_score = median(Rating, na.rm = TRUE),
      individual_n = n(),
      .groups = "drop"
    ) %>%
    filter(individual_n >= 1)
  
  # Calculate program means for comparison (using same level criteria if specified)
  program_data <- milestone_data$evaluations
  if (selected_level != "all") {
    level_parts <- strsplit(selected_level, "\\|\\|\\|")[[1]]
    if (length(level_parts) == 2) {
      period_filter <- level_parts[1]
      pgy_filter <- level_parts[2]
      program_data <- program_data[program_data$Period == period_filter & 
                                     program_data$PGY_Level == pgy_filter, ]
    }
  }
  
  # Check for sufficient program data for comparison
  program_scores <- program_data %>%
    group_by(Sub_Competency, Competency) %>%
    summarise(
      program_mean = mean(Rating, na.rm = TRUE),
      program_n = n(),
      unique_residents = n_distinct(Resident_Name),
      .groups = "drop"
    ) %>%
    filter(program_n >= 5 & unique_residents >= 3)  # Need multiple residents for meaningful comparison
  
  # Determine if we have sufficient data for comparison
  has_comparison_data <- nrow(program_scores) > 0
  
  if (!has_comparison_data) {
    # FALLBACK: Show individual data only with informative message
    fig <- plot_ly(type = 'scatterpolar', fill = 'toself')
    
    # Individual performance only
    fig <- fig %>% add_trace(
      r = individual_scores$individual_score,
      theta = individual_scores$Sub_Competency,
      name = selected_resident,
      line = list(color = '#2E86AB', width = 4),
      marker = list(color = '#2E86AB', size = 10),
      fillcolor = 'rgba(46, 134, 171, 0.2)',
      hovertemplate = paste0(
        '<b>%{theta}</b><br>',
        selected_resident, ': %{r}<br>',
        'Competency: ', individual_scores$Competency,
        '<extra></extra>'
      )
    )
    
    # Layout for individual-only display
    level_text <- if (selected_level == "all") "All Evaluations" else {
      level_parts <- strsplit(selected_level, "\\|\\|\\|")[[1]]
      if (length(level_parts) == 2) paste(level_parts[2], "-", level_parts[1]) else selected_level
    }
    
    fig <- fig %>% layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(1, 9),
          tickmode = 'linear',
          tick0 = 1,
          dtick = 1,
          showticklabels = TRUE,
          tickfont = list(size = 10),
          gridcolor = 'rgba(128, 128, 128, 0.3)'
        ),
        angularaxis = list(
          tickfont = list(size = 11),
          rotation = 90,
          direction = "clockwise"
        )
      ),
      title = list(
        text = paste("Individual Performance:", selected_resident, "<br><sub>", level_text, "</sub><br>",
                     "<span style='font-size: 12px; color: #666;'>Insufficient program data for comparison</span>"),
        font = list(size = 14),
        y = 0.95
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.1,
        font = list(size = 11)
      ),
      showlegend = TRUE,
      margin = list(t = 100, b = 60, l = 60, r = 60),
      annotations = list(
        list(
          text = "Tip: For program comparisons, visit the Program Overview tab",
          showarrow = FALSE,
          x = 0.5, y = -0.15,
          xref = "paper", yref = "paper",
          font = list(size = 11, color = "#666")
        )
      )
    )
    
    return(fig)
  }
  
  # NORMAL PATH: Show comparison when sufficient data exists
  combined_scores <- individual_scores %>%
    inner_join(program_scores, by = c("Sub_Competency", "Competency")) %>%
    arrange(Competency, Sub_Competency)
  
  if (nrow(combined_scores) == 0) {
    return(plot_ly() %>% 
             add_annotations(text = "No matching data for comparison", 
                             x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Create comparison spider plot
  fig <- plot_ly(type = 'scatterpolar', fill = 'toself')
  
  # Individual performance
  fig <- fig %>% add_trace(
    r = combined_scores$individual_score,
    theta = combined_scores$Sub_Competency,
    name = selected_resident,
    line = list(color = '#2E86AB', width = 3),
    marker = list(color = '#2E86AB', size = 8),
    fillcolor = 'rgba(46, 134, 171, 0.1)',
    hovertemplate = paste0(
      '<b>%{theta}</b><br>',
      selected_resident, ': %{r}<br>',
      'Program Mean: ', round(combined_scores$program_mean, 2), '<br>',
      'Difference: ', ifelse(combined_scores$individual_score >= combined_scores$program_mean, '+', ''),
      round(combined_scores$individual_score - combined_scores$program_mean, 2),
      '<extra></extra>'
    )
  )
  
  # Program mean
  fig <- fig %>% add_trace(
    r = combined_scores$program_mean,
    theta = combined_scores$Sub_Competency,
    name = "Program Mean",
    line = list(color = '#FF6B35', width = 2, dash = 'dash'),
    marker = list(color = '#FF6B35', size = 6),
    fillcolor = 'rgba(255, 107, 53, 0.05)',
    hovertemplate = paste0(
      '<b>%{theta}</b><br>',
      'Program Mean: %{r}<br>',
      selected_resident, ': ', round(combined_scores$individual_score, 2), '<br>',
      'Difference: ', ifelse(combined_scores$individual_score >= combined_scores$program_mean, '+', ''),
      round(combined_scores$individual_score - combined_scores$program_mean, 2),
      '<extra></extra>'
    )
  )
  
  # Layout configuration for comparison
  level_text <- if (selected_level == "all") "All Evaluations" else {
    level_parts <- strsplit(selected_level, "\\|\\|\\|")[[1]]
    if (length(level_parts) == 2) paste(level_parts[2], "-", level_parts[1]) else selected_level
  }
  
  fig <- fig %>% layout(
    polar = list(
      radialaxis = list(
        visible = TRUE,
        range = c(1, 9),
        tickmode = 'linear',
        tick0 = 1,
        dtick = 1,
        showticklabels = TRUE,
        tickfont = list(size = 10),
        gridcolor = 'rgba(128, 128, 128, 0.3)'
      ),
      angularaxis = list(
        tickfont = list(size = 11),
        rotation = 90,
        direction = "clockwise"
      )
    ),
    title = list(
      text = paste("Performance Profile:", selected_resident, "<br><sub>", level_text, "</sub>"),
      font = list(size = 14),
      y = 0.95
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1,
      font = list(size = 11)
    ),
    showlegend = TRUE,
    margin = list(t = 80, b = 60, l = 60, r = 60)
  )
  
  return(fig)
}

# Enhanced trend chart (single resident, existing function)
create_individual_trend_enhanced <- function(milestone_data, selected_resident) {
  # [Existing function code remains the same - no changes needed for single resident mode]
  # This function already works perfectly for single resident analysis
  
  if (is.null(milestone_data) || is.null(selected_resident) || selected_resident == "") {
    return(plot_ly() %>% 
             add_annotations(text = "Please select a resident", 
                             x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Get resident data
  resident_data <- milestone_data$evaluations[milestone_data$evaluations$Resident_Name == selected_resident, ]
  
  if (nrow(resident_data) == 0) {
    return(plot_ly() %>% 
             add_annotations(text = "No data available for selected resident", 
                             x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Determine the program length from the overall data
  max_pgy <- milestone_data$evaluations %>%
    pull(PGY_Level) %>%
    str_extract("\\d+") %>%
    as.numeric() %>%
    max(na.rm = TRUE)
  
  min_pgy <- milestone_data$evaluations %>%
    pull(PGY_Level) %>%
    str_extract("\\d+") %>%
    as.numeric() %>%
    min(na.rm = TRUE)
  
  # Create complete timeline of all possible periods
  complete_timeline <- data.frame(
    PGY_Year = rep(min_pgy:max_pgy, each = 2),
    Period_Type = rep(c("Mid-Year", "Year-End"), times = (max_pgy - min_pgy + 1))
  ) %>%
    mutate(
      Period_Order = case_when(
        Period_Type == "Mid-Year" ~ (PGY_Year - 1) * 2 + 1,
        Period_Type == "Year-End" ~ (PGY_Year - 1) * 2 + 2
      ),
      Period_Label = paste0(Period_Type, " PGY-", PGY_Year),
      PGY_Level = paste0("PGY-", PGY_Year)
    ) %>%
    arrange(Period_Order)
  
  # Process individual resident data
  resident_trend <- resident_data %>%
    mutate(
      PGY_Year = as.numeric(str_extract(PGY_Level, "\\d+")),
      Period_Type = case_when(
        str_detect(Period, "Mid-Year|Mid Year") ~ "Mid-Year",
        str_detect(Period, "Year-End|End Year") ~ "Year-End",
        TRUE ~ "Other"
      )
    ) %>%
    filter(Period_Type %in% c("Mid-Year", "Year-End")) %>%
    mutate(
      Period_Order = case_when(
        Period_Type == "Mid-Year" ~ (PGY_Year - 1) * 2 + 1,
        Period_Type == "Year-End" ~ (PGY_Year - 1) * 2 + 2,
        TRUE ~ 999
      ),
      Period_Label = paste0(Period_Type, " PGY-", PGY_Year),
      Full_Period = paste(Period, "-", PGY_Level)
    ) %>%
    filter(Period_Order < 999) %>%
    group_by(Period_Order, Period_Label, PGY_Level, Period, Period_Type, PGY_Year) %>%
    summarise(
      individual_score = mean(Rating, na.rm = TRUE),
      individual_n = n(),
      .groups = "drop"
    ) %>%
    arrange(Period_Order)
  
  # Calculate program means for ALL possible periods
  program_trend <- milestone_data$evaluations %>%
    mutate(
      PGY_Year = as.numeric(str_extract(PGY_Level, "\\d+")),
      Period_Type = case_when(
        str_detect(Period, "Mid-Year|Mid Year") ~ "Mid-Year",
        str_detect(Period, "Year-End|End Year") ~ "Year-End",
        TRUE ~ "Other"
      )
    ) %>%
    filter(Period_Type %in% c("Mid-Year", "Year-End")) %>%
    mutate(
      Period_Order = case_when(
        Period_Type == "Mid-Year" ~ (PGY_Year - 1) * 2 + 1,
        Period_Type == "Year-End" ~ (PGY_Year - 1) * 2 + 2,
        TRUE ~ 999
      ),
      Period_Label = paste0(Period_Type, " PGY-", PGY_Year)
    ) %>%
    filter(Period_Order < 999) %>%
    group_by(Period_Order, Period_Label, PGY_Level, Period_Type, PGY_Year) %>%
    summarise(
      program_mean = mean(Rating, na.rm = TRUE),
      program_n = n(),
      unique_residents = n_distinct(Resident_Name),
      .groups = "drop"
    ) %>%
    filter(unique_residents >= 3) %>%
    arrange(Period_Order)
  
  # Merge with complete timeline
  complete_data <- complete_timeline %>%
    left_join(resident_trend %>% select(Period_Order, individual_score, individual_n), 
              by = "Period_Order") %>%
    left_join(program_trend %>% select(Period_Order, program_mean, program_n, unique_residents), 
              by = "Period_Order")
  
  # Check if we have any data at all
  if (all(is.na(complete_data$individual_score))) {
    return(plot_ly() %>% 
             add_annotations(text = "No trend data available for this resident", 
                             x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Check if we have only one data point
  individual_data_points <- sum(!is.na(complete_data$individual_score))
  if (individual_data_points == 1) {
    single_point <- complete_data[!is.na(complete_data$individual_score), ][1, ]
    return(plot_ly() %>% 
             add_annotations(
               text = paste("Only one evaluation period available for", selected_resident, 
                            "<br>Trend analysis requires multiple time points<br><br>",
                            "Current score:", round(single_point$individual_score, 2),
                            "for", single_point$Period_Label),
               x = 0.5, y = 0.5, showarrow = FALSE,
               font = list(size = 14)
             ))
  }
  
  # Determine if we have program comparison data
  has_program_comparison <- any(!is.na(complete_data$program_mean))
  
  # Create trend plot
  fig <- plot_ly()
  
  # Add program mean line if available
  if (has_program_comparison) {
    program_data_available <- complete_data[!is.na(complete_data$program_mean), ]
    
    if (nrow(program_data_available) > 0) {
      fig <- fig %>% add_trace(
        data = program_data_available,
        x = ~Period_Order,
        y = ~program_mean,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Program Mean',
        line = list(color = '#E0E0E0', width = 2, dash = 'dash'),
        marker = list(color = '#E0E0E0', size = 6, symbol = 'diamond'),
        opacity = 0.7,
        hovertemplate = paste0(
          '<b>Program Mean</b><br>',
          'Period: ', program_data_available$Period_Label, '<br>',
          'Program Mean: %{y:.2f}<br>',
          'Based on ', program_data_available$unique_residents, ' residents',
          '<extra></extra>'
        )
      )
    }
  }
  
  # Add individual trend line
  individual_data_available <- complete_data[!is.na(complete_data$individual_score), ]
  
  if (nrow(individual_data_available) > 0) {
    individual_hover <- if (has_program_comparison) {
      hover_data <- individual_data_available %>%
        left_join(complete_data %>% select(Period_Order, program_mean), by = "Period_Order")
      
      paste0(
        '<b>', selected_resident, '</b><br>',
        'Period: ', individual_data_available$Period_Label, '<br>',
        'Individual Score: %{y:.2f}<br>',
        ifelse(!is.na(hover_data$program_mean), 
               paste0('Program Mean: ', round(hover_data$program_mean, 2), '<br>',
                      'Difference: ', ifelse(individual_data_available$individual_score >= hover_data$program_mean, '+', ''),
                      round(individual_data_available$individual_score - hover_data$program_mean, 2)), 
               'No program comparison available'),
        '<extra></extra>'
      )
    } else {
      paste0(
        '<b>', selected_resident, '</b><br>',
        'Period: ', individual_data_available$Period_Label, '<br>',
        'Score: %{y:.2f}<br>',
        'Evaluations: ', individual_data_available$individual_n,
        '<extra></extra>'
      )
    }
    
    fig <- fig %>% add_trace(
      data = individual_data_available,
      x = ~Period_Order,
      y = ~individual_score,
      type = 'scatter',
      mode = 'lines+markers',
      name = selected_resident,
      line = list(color = '#2E86AB', width = 4),
      marker = list(color = '#2E86AB', size = 12, symbol = 'circle'),
      hovertemplate = individual_hover
    )
  }
  
  # Layout configuration
  title_text <- if (has_program_comparison) {
    paste("Training Progression:", selected_resident, "vs Program")
  } else {
    paste("Individual Training Progression:", selected_resident)
  }
  
  # Calculate y-axis range
  all_scores <- c(complete_data$individual_score, complete_data$program_mean)
  all_scores <- all_scores[!is.na(all_scores)]
  
  y_range <- if (length(all_scores) > 0) {
    c(max(1, min(all_scores) - 0.5), min(9, max(all_scores) + 0.5))
  } else {
    c(1, 9)
  }
  
  fig <- fig %>% layout(
    title = list(
      text = title_text,
      font = list(size = 14),
      y = 0.95
    ),
    xaxis = list(
      title = "Training Period",
      tickmode = 'array',
      tickvals = complete_timeline$Period_Order,
      ticktext = complete_timeline$Period_Label,
      tickangle = -45,
      tickfont = list(size = 10),
      range = c(min(complete_timeline$Period_Order) - 0.5, max(complete_timeline$Period_Order) + 0.5)
    ),
    yaxis = list(
      title = "Average Score",
      range = y_range,
      tickmode = 'linear',
      tick0 = 1,
      dtick = 1,
      tickfont = list(size = 10)
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.25,
      font = list(size = 11)
    ),
    hovermode = 'x unified',
    margin = list(t = 80, b = 100, l = 60, r = 40)
  )
  
  return(fig)
}

# Create detailed performance table (existing function - no changes needed)
create_individual_detail_table <- function(milestone_data, selected_resident, selected_level) {
  if (is.null(milestone_data) || is.null(selected_resident) || selected_resident == "") {
    return(data.frame())
  }
  
  # Filter data for selected resident
  resident_data <- milestone_data$evaluations[milestone_data$evaluations$Resident_Name == selected_resident, ]
  
  # Apply level filter if not "all"
  if (selected_level != "all") {
    level_parts <- strsplit(selected_level, "\\|\\|\\|")[[1]]
    if (length(level_parts) == 2) {
      period_filter <- level_parts[1]
      pgy_filter <- level_parts[2]
      resident_data <- resident_data[resident_data$Period == period_filter & 
                                       resident_data$PGY_Level == pgy_filter, ]
    }
  }
  
  if (nrow(resident_data) == 0) {
    return(data.frame())
  }
  
  # Calculate individual performance by sub-competency
  individual_summary <- resident_data %>%
    group_by(Competency, Sub_Competency) %>%
    summarise(
      Individual_Mean = round(mean(Rating, na.rm = TRUE), 2),
      Individual_Median = round(median(Rating, na.rm = TRUE), 1),
      Individual_Evaluations = n(),
      Individual_Range = paste0(min(Rating, na.rm = TRUE), "-", max(Rating, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Calculate program benchmarks for comparison
  program_data <- milestone_data$evaluations
  if (selected_level != "all") {
    level_parts <- strsplit(selected_level, "\\|\\|\\|")[[1]]
    if (length(level_parts) == 2) {
      period_filter <- level_parts[1]
      pgy_filter <- level_parts[2]
      program_data <- program_data[program_data$Period == period_filter & 
                                     program_data$PGY_Level == pgy_filter, ]
    }
  }
  
  program_summary <- program_data %>%
    group_by(Competency, Sub_Competency) %>%
    summarise(
      Program_Mean = round(mean(Rating, na.rm = TRUE), 2),
      Program_Median = round(median(Rating, na.rm = TRUE), 1),
      Program_N = n(),
      .groups = "drop"
    ) %>%
    filter(Program_N >= 5)  # Minimum sample size
  
  # Combine individual and program data
  detail_table <- individual_summary %>%
    left_join(program_summary, by = c("Competency", "Sub_Competency")) %>%
    mutate(
      Difference_from_Mean = round(Individual_Mean - Program_Mean, 2),
      Percentile_Estimate = round(100 * pnorm((Individual_Mean - Program_Mean) / 
                                                pmax(0.5, Program_Mean * 0.2)), 0),
      Performance_Level = case_when(
        Individual_Mean >= Program_Mean + 1 ~ "Above Average",
        Individual_Mean >= Program_Mean + 0.5 ~ "Slightly Above Average",
        Individual_Mean >= Program_Mean - 0.5 ~ "Average",
        Individual_Mean >= Program_Mean - 1 ~ "Slightly Below Average",
        TRUE ~ "Below Average"
      )
    ) %>%
    select(
      Competency,
      Sub_Competency,
      Individual_Mean,
      Program_Mean,
      Difference_from_Mean,
      Performance_Level,
      Individual_Evaluations,
      Individual_Range,
      Percentile_Estimate
    ) %>%
    arrange(Competency, Sub_Competency)
  
  return(detail_table)
}

# ===== INTEGRATION NOTES =====
# 
# This enhanced version provides:
# 
# 1. **Class Identification**: Residents are automatically organized by graduation class
# 2. **Class Filtering**: Optional filter to compare residents within the same class
# 3. **Enhanced UI**: Class information shown in resident selectors and legends
# 4. **Visual Improvements**: Class info in card headers and plot legends
# 5. **Maintains All Functionality**: All existing features preserved
# 
# Key Features Added:
# - Automatic graduation class calculation based on PGY level and academic year
# - Class filter dropdown in comparison mode
# - Class information in all resident displays
# - Enhanced legends showing "Resident Name (Class of YYYY)"
# - Comparison cards with class headers
# - Improved organization for easier resident comparison
# 
# Usage Instructions:
# 1. Replace your existing individual_assessment_enhanced.R with this version
# 2. The module will automatically calculate graduation classes from your data
# 3. Residents will be sorted by class (most recent first) then alphabetically
# 4. In comparison mode, use the class filter to narrow down resident choices
# 5. All plots and tables will show class information for better context
# 
# Integration:
# - Drop-in replacement for existing module
# - No changes needed to server.R or other files
# - Fully backward compatible with existing functionality
# - Works with all existing helper functions
# 
# ===== END =====