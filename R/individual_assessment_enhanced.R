# =============================================================================
# ENHANCED INDIVIDUAL RESIDENT ASSESSMENT TAB
# individual_assessment_enhanced.R
#
# Creates individual resident assessment with resident selector, level selector,
# spider plot comparison with program mean, and trend chart with program overlay
# =============================================================================

# ===== UI COMPONENT =====
create_individual_assessment_ui <- function() {
  nav_panel("Individual Assessment",
            icon = icon("user-check"),
            
            conditionalPanel(
              condition = "output.data_loaded",
              
              # Enhanced tab description
              fluidRow(
                column(12,
                       div(class = "alert alert-info mb-4",
                           section_header("Individual Assessment", 
                                          "Focus on individual resident performance and progression",
                                          "user-check"),
                           p(HTML(paste("Focus on individual resident performance with detailed", 
                                        create_tooltip("milestone tracking", 
                                                       "Track individual resident progress across<br/>all milestone evaluations over time"),
                                        "and comparative analysis. Select a resident to view their progression over time, compare their performance to",
                                        create_tooltip("program averages", 
                                                       "See how individual performance compares<br/>to program-wide averages for context"),
                                        "and identify specific areas for development or recognition.")),
                             style = "margin-bottom: 0; font-size: 0.95em;")
                       )
                )
              ),
              
              # Control Panel and Summary Cards Row
              fluidRow(
                column(3,
                       div(class = "content-card card border-primary h-100",
                           div(class = "card-header bg-primary text-white",
                               h6(icon("sliders-h"), " Controls", 
                                  class = "mb-0",
                                  help_icon("Select resident and evaluation period"))
                           ),
                           div(class = "card-body",
                               
                               # Resident Selection
                               div(class = "form-group",
                                   tags$label(class = "form-label",
                                              "Select Resident:",
                                              help_icon("Choose individual resident for analysis")),
                                   selectInput("individual_resident", "",
                                               choices = NULL,
                                               width = "100%")
                               ),
                               
                               # Evaluation Level/Period Selection
                               div(class = "form-group",
                                   tags$label(class = "form-label",
                                              create_tooltip("Evaluation Level", 
                                                             "Choose specific evaluation period<br/>or view all evaluations combined"),
                                              help_icon("Select evaluation timeframe")),
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
                
                # Summary Cards
                column(9,
                       fluidRow(
                         column(3,
                                div(class = "content-card card border-info text-center h-100",
                                    div(class = "card-body d-flex flex-column justify-content-center",
                                        div(class = "metric-value",
                                            textOutput("individual_total_evaluations"),
                                            style = "font-size: 2rem; font-weight: bold; color: #17a2b8;"
                                        ),
                                        tags$small("Total Evaluations", 
                                                   class = "text-muted",
                                                   help_icon("Number of milestone evaluations for this resident"))
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
                                        tags$small("Average Score", 
                                                   class = "text-muted",
                                                   help_icon("Mean milestone score for selected criteria"))
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
                                        tags$small("Program Average", 
                                                   class = "text-muted",
                                                   help_icon("Program-wide average for comparison"))
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
                                        tags$small("Program Percentile", 
                                                   class = "text-muted",
                                                   help_icon("Percentile rank within program<br/>Higher values indicate above-average performance"))
                                    )
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
                                  "Performance Profile vs Program Mean",
                                  help_icon("Radar chart comparing individual performance<br/>to program averages across all sub-competencies<br/>Points further from center indicate higher scores"))
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
                                  "Progression Over Time vs Program",
                                  help_icon("Shows individual progression through training<br/>compared to program averages over time<br/>Tracks development across evaluation periods"))
                           ),
                           div(class = "card-body position-relative",
                               plotlyOutput("individual_trend_enhanced", height = "550px")
                           )
                       )
                )
              ),
              
              br(),
              
              # Detailed Performance Table
              fluidRow(
                column(12,
                       div(class = "content-card card",
                           div(class = "card-header", 
                               h6(icon("table"), 
                                  "Detailed Performance Breakdown",
                                  help_icon("Comprehensive table showing individual performance<br/>compared to program benchmarks by sub-competency<br/>includes percentile estimates and performance levels"))
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

# ===== SERVER FUNCTIONS =====

# Update resident choices based on loaded data
update_individual_residents <- function(session, milestone_data) {
  if (is.null(milestone_data) || is.null(milestone_data$evaluations)) {
    updateSelectInput(session, "individual_resident", choices = NULL)
    return()
  }
  
  # Get unique residents and sort alphabetically
  residents <- sort(unique(milestone_data$evaluations$Resident_Name))
  residents <- residents[!is.na(residents) & residents != ""]
  
  updateSelectInput(session, "individual_resident", 
                    choices = setNames(residents, residents),
                    selected = NULL)
}

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

# ===== ENHANCED FUNCTIONS WITH DATA SUFFICIENCY FALLBACKS =====

# Enhanced spider plot with fallback for insufficient comparison data
create_individual_spider_enhanced <- function(milestone_data, selected_resident, selected_level) {
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

# Enhanced trend chart with fallback for insufficient program data
# Enhanced trend chart with complete training timeline
create_individual_trend_enhanced <- function(milestone_data, selected_resident) {
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
  
  # Calculate program means for ALL possible periods (not just resident's periods)
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
    filter(unique_residents >= 3) %>%  # Need multiple residents for meaningful comparison
    arrange(Period_Order)
  
  # Merge with complete timeline to ensure all periods are represented
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
  
  # Add program mean line FIRST (as background reference) if available
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
        line = list(color = '#E0E0E0', width = 2, dash = 'dash'),  # Lighter color for background
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
  
  # Add individual trend line (always show, but only for periods with data)
  individual_data_available <- complete_data[!is.na(complete_data$individual_score), ]
  
  if (nrow(individual_data_available) > 0) {
    # Create hover template
    individual_hover <- if (has_program_comparison) {
      # Join to get program mean for hover
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
  
  # Add markers for periods without data (ghost points)
  periods_without_data <- complete_data[is.na(complete_data$individual_score), ]
  
  if (nrow(periods_without_data) > 0) {
    fig <- fig %>% add_trace(
      data = periods_without_data,
      x = ~Period_Order,
      y = rep(NA, nrow(periods_without_data)),
      type = 'scatter',
      mode = 'markers',
      name = 'No Data Available',
      marker = list(color = 'lightgray', size = 8, symbol = 'circle-open'),
      showlegend = FALSE,
      hovertemplate = paste0(
        '<b>No Data</b><br>',
        'Period: ', periods_without_data$Period_Label, '<br>',
        'No evaluations for ', selected_resident,
        '<extra></extra>'
      )
    )
  }
  
  # Layout configuration
  title_text <- if (has_program_comparison) {
    paste("Training Progression:", selected_resident, "vs Program")
  } else {
    paste("Individual Training Progression:", selected_resident, "<br>",
          "<span style='font-size: 12px; color: #666;'>Insufficient program data for comparison</span>")
  }
  
  # Calculate y-axis range to accommodate all data
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
    margin = list(t = 80, b = 100, l = 60, r = 40),
    annotations = if (!has_program_comparison) {
      list(
        list(
          text = "For program comparisons, visit the Program Overview tab",
          showarrow = FALSE,
          x = 0.5, y = -0.3,
          xref = "paper", yref = "paper",
          font = list(size = 11, color = "#666")
        )
      )
    } else NULL
  )
  
  return(fig)
}

# Create detailed performance table
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

# Calculate summary statistics for the cards
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

# ===== SERVER INTEGRATION FUNCTIONS =====

# Main server outputs for individual assessment tab
render_individual_assessment_outputs <- function(output, session, milestone_data) {
  
  # Update resident choices when data changes
  observeEvent(milestone_data(), {
    update_individual_residents(session, milestone_data())
  })
  
  # Update level choices when resident changes
  observeEvent(input$individual_resident, {
    req(milestone_data(), input$individual_resident)
    update_individual_levels(session, milestone_data(), input$individual_resident)
  })
  
  # Resident info display
  output$resident_info_display <- renderUI({
    req(milestone_data(), input$individual_resident, input$individual_level)
    
    if (input$individual_level == "all") {
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
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel'),
    columnDefs = list(
      list(targets = c(2, 3, 4, 8), className = 'dt-center'),  # Center numeric columns
      list(targets = 5, className = 'dt-center'),  # Center performance level
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

# ===== INTEGRATION NOTES =====
# 
# To integrate this enhanced individual assessment tab:
# 
# 1. In ui.R, replace the existing "Individual Residents" nav_panel with:
#    create_individual_assessment_ui()
# 
# 2. In server.R, add the server integration:
#    render_individual_assessment_outputs(output, session, milestone_data)
# 
# 3. Ensure required libraries are loaded:
#    library(dplyr)
#    library(plotly)
#    library(DT)
#    library(stringr)
# 
# 4. The code assumes milestone_data() is a reactive containing:
#    - $evaluations: data frame with columns Resident_Name, Period, PGY_Level, 
#      Sub_Competency, Competency, Rating
# 
# 5. Key features:
#    - Resident selector with alphabetical ordering
#    - Level selector defaulting to highest/most recent evaluation
#    - Spider plot comparing individual vs program means
#    - Trend chart showing progression over time vs program
#    - Summary cards with key statistics
#    - Detailed performance breakdown table with color coding
# 
# ===== END =====