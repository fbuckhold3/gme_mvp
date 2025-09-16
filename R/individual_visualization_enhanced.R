# =============================================================================
# ENHANCED INDIVIDUAL RESIDENT VISUALIZATION FUNCTIONS
# R/individual_visualization_enhanced.R
# 
# Implements individual resident views with spider plots and trend lines
# =============================================================================

# Note: Libraries should be loaded in app.R, but adding here for safety
if (!require(plotly)) library(plotly)
if (!require(dplyr)) library(dplyr)
if (!require(tidyr)) library(tidyr)
if (!require(stringr)) library(stringr)

#' Create Individual Resident Spider Plot
#'
#' Shows individual resident performance vs program medians across all sub-competencies
#'
#' @param data Processed data from load_milestone_csv_data()
#' @param resident_name Selected resident name
#' @param period_filter Selected period ("total" for all)
#' @return Plotly spider plot
create_individual_spider_plot <- function(data, resident_name, period_filter = "total") {
  
  # Get individual resident data
  resident_data <- data$evaluations %>%
    filter(Resident_Name == resident_name)
  
  if (period_filter != "total") {
    resident_data <- resident_data %>%
      filter(Period == period_filter)
  }
  
  if (nrow(resident_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = paste("No data available for", resident_name),
               x = 0.5, y = 0.5, showarrow = FALSE
             ))
  }
  
  # Calculate individual resident medians by sub-competency
  individual_scores <- resident_data %>%
    group_by(Sub_Competency, Competency) %>%
    summarise(
      individual_score = median(Rating, na.rm = TRUE),
      n_evaluations = n(),
      .groups = "drop"
    ) %>%
    filter(n_evaluations >= 1)  # At least one evaluation
  
  # Calculate program medians for comparison
  program_data <- data$evaluations
  if (period_filter != "total") {
    program_data <- program_data %>%
      filter(Period == period_filter)
  }
  
  program_medians <- program_data %>%
    group_by(Sub_Competency, Competency) %>%
    summarise(
      program_median = median(Rating, na.rm = TRUE),
      program_n = n(),
      .groups = "drop"
    ) %>%
    filter(program_n >= 3)  # Minimum program data
  
  # Combine individual and program data
  spider_data <- individual_scores %>%
    inner_join(program_medians, by = c("Sub_Competency", "Competency")) %>%
    arrange(Competency, Sub_Competency)
  
  if (nrow(spider_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = "Insufficient data for comparison",
               x = 0.5, y = 0.5, showarrow = FALSE
             ))
  }
  
  # Create hover text
  individual_hover <- paste0(
    '<b>', spider_data$Sub_Competency, '</b><br>',
    '<b>', resident_name, ':</b> ', round(spider_data$individual_score, 1), '<br>',
    'Program Median: ', round(spider_data$program_median, 1), '<br>',
    'Competency: ', spider_data$Competency,
    '<extra></extra>'
  )
  
  program_hover <- paste0(
    '<b>', spider_data$Sub_Competency, '</b><br>',
    '<b>Program Median:</b> ', round(spider_data$program_median, 1), '<br>',
    resident_name, ': ', round(spider_data$individual_score, 1), '<br>',
    'Competency: ', spider_data$Competency,
    '<extra></extra>'
  )
  
  # Create the spider plot
  fig <- plot_ly(type = 'scatterpolar')
  
  # Add individual resident scores
  fig <- fig %>% add_trace(
    r = spider_data$individual_score,
    theta = spider_data$Sub_Competency,
    name = resident_name,
    line = list(color = '#2E86AB', width = 4),
    marker = list(color = '#2E86AB', size = 10),
    fill = 'toself',
    fillcolor = 'rgba(46, 134, 171, 0.15)',
    hovertemplate = individual_hover
  )
  
  # Add program median overlay
  fig <- fig %>% add_trace(
    r = spider_data$program_median,
    theta = spider_data$Sub_Competency,
    name = "Program Median",
    line = list(color = '#A23B72', width = 3, dash = 'dash'),
    marker = list(color = '#A23B72', size = 8, symbol = 'diamond'),
    hovertemplate = program_hover
  )
  
  # Configure layout
  fig <- fig %>% layout(
    polar = list(
      radialaxis = list(
        visible = TRUE,
        range = c(1, 9),
        tickmode = 'linear',
        tick0 = 1,
        dtick = 1
      ),
      angularaxis = list(
        tickfont = list(size = 10),
        rotation = 90,
        direction = "clockwise"
      )
    ),
    title = list(
      text = paste("Individual Performance Profile -", resident_name),
      font = list(size = 16)
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1
    )
  )
  
  return(fig)
}

#' Create Individual Resident Trend Lines
#'
#' Shows individual resident progression across periods by sub-competency
#'
#' @param data Processed data from load_milestone_csv_data()
#' @param resident_name Selected resident name
#' @param competency_filter Specific competency to show ("all" for all)
#' @return Plotly line plot
create_individual_trend_plot <- function(data, resident_name, competency_filter = "all") {
  
  # Get individual resident data
  resident_data <- data$evaluations %>%
    filter(Resident_Name == resident_name)
  
  if (competency_filter != "all") {
    resident_data <- resident_data %>%
      filter(Competency == competency_filter)
  }
  
  if (nrow(resident_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = paste("No data available for", resident_name),
               x = 0.5, y = 0.5, showarrow = FALSE
             ))
  }
  
  # Create sequential period ordering
  trend_data <- resident_data %>%
    mutate(
      PGY_Year = as.numeric(str_extract(PGY_Level, "\\d+")),
      Period_Type = case_when(
        str_detect(Period, "Mid-Year") ~ "Mid-Year",
        str_detect(Period, "Year-End|End-Year") ~ "Year-End",
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
      Period_Label = paste(Period_Type, paste0("PGY-", PGY_Year))
    ) %>%
    filter(Period_Order < 999) %>%
    # Calculate individual scores by sub-competency and period
    group_by(Sub_Competency, Competency, Period_Order, Period_Label, PGY_Level) %>%
    summarise(
      individual_score = median(Rating, na.rm = TRUE),
      n_evaluations = n(),
      .groups = "drop"
    ) %>%
    arrange(Sub_Competency, Period_Order)
  
  if (nrow(trend_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = "No trend data available for this resident",
               x = 0.5, y = 0.5, showarrow = FALSE
             ))
  }
  
  # Create the trend plot
  fig <- plot_ly(trend_data, 
                 x = ~Period_Order, 
                 y = ~individual_score,
                 color = ~Sub_Competency,
                 type = 'scatter', 
                 mode = 'lines+markers',
                 line = list(width = 3),
                 marker = list(size = 8),
                 hovertemplate = paste0(
                   '<b>%{data.name}</b><br>',
                   'Period: ', trend_data$Period_Label, '<br>',
                   'Score: %{y}<br>',
                   'Level: ', trend_data$PGY_Level, '<br>',
                   'Competency: ', trend_data$Competency,
                   '<extra></extra>'
                 ))
  
  # Configure layout
  fig <- fig %>% layout(
    title = list(
      text = paste("Individual Progression -", resident_name, 
                   ifelse(competency_filter == "all", "(All Competencies)", 
                          paste("(", competency_filter, ")"))),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Training Period",
      tickmode = 'array',
      tickvals = unique(trend_data$Period_Order),
      ticktext = unique(trend_data$Period_Label[order(trend_data$Period_Order)]),
      tickangle = -45
    ),
    yaxis = list(
      title = "Score",
      range = c(1, 9),
      tickmode = 'linear',
      tick0 = 1,
      dtick = 1
    ),
    legend = list(
      title = list(text = "Sub-Competency"),
      orientation = "v",
      x = 1.02,
      y = 1
    ),
    hovermode = 'closest'
  )
  
  return(fig)
}

#' Create Individual Performance Summary Table
#'
#' Shows individual resident's current performance across all sub-competencies
#'
#' @param data Processed data from load_milestone_csv_data()
#' @param resident_name Selected resident name
#' @param period_filter Selected period ("total" for all)
#' @return Data frame for display
create_individual_summary_table <- function(data, resident_name, period_filter = "total") {
  
  # Get individual resident data
  resident_data <- data$evaluations %>%
    filter(Resident_Name == resident_name)
  
  if (period_filter != "total") {
    resident_data <- resident_data %>%
      filter(Period == period_filter)
  }
  
  if (nrow(resident_data) == 0) {
    return(data.frame(
      Sub_Competency = "No data",
      Competency = "",
      Individual_Score = "",
      Performance_Level = "",
      Program_Median = "",
      Comparison = ""
    ))
  }
  
  # Calculate individual scores
  individual_scores <- resident_data %>%
    group_by(Sub_Competency, Competency) %>%
    summarise(
      individual_score = median(Rating, na.rm = TRUE),
      individual_n = n(),
      .groups = "drop"
    )
  
  # Calculate program medians for comparison
  program_data <- data$evaluations
  if (period_filter != "total") {
    program_data <- program_data %>%
      filter(Period == period_filter)
  }
  
  program_medians <- program_data %>%
    group_by(Sub_Competency, Competency) %>%
    summarise(
      program_median = median(Rating, na.rm = TRUE),
      program_n = n(),
      .groups = "drop"
    ) %>%
    filter(program_n >= 3)
  
  # Combine and create summary
  summary_table <- individual_scores %>%
    left_join(program_medians, by = c("Sub_Competency", "Competency")) %>%
    filter(!is.na(program_median)) %>%
    mutate(
      performance_level = get_performance_category(individual_score),
      difference = individual_score - program_median,
      comparison = case_when(
        difference > 0.5 ~ "Above Program",
        difference < -0.5 ~ "Below Program", 
        TRUE ~ "At Program Level"
      ),
      Individual_Score = round(individual_score, 1),
      Program_Median = round(program_median, 1),
      Performance_Level = performance_level,
      Comparison = comparison
    ) %>%
    arrange(Competency, Sub_Competency) %>%
    select(Sub_Competency, Competency, Individual_Score, Performance_Level, 
           Program_Median, Comparison)
  
  return(summary_table)
}

#' Create Individual vs Peers Comparison
#'
#' Shows how individual resident compares to peers at same PGY level
#'
#' @param data Processed data from load_milestone_csv_data()
#' @param resident_name Selected resident name
#' @param period_filter Selected period ("total" for all)
#' @return Plotly bar chart
create_individual_peer_comparison <- function(data, resident_name, period_filter = "total") {
  
  # Get individual resident data and their PGY level
  resident_data <- data$evaluations %>%
    filter(Resident_Name == resident_name)
  
  if (period_filter != "total") {
    resident_data <- resident_data %>%
      filter(Period == period_filter)
  }
  
  if (nrow(resident_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = paste("No data available for", resident_name),
               x = 0.5, y = 0.5, showarrow = FALSE
             ))
  }
  
  # Get resident's PGY level (use most recent)
  resident_pgy <- resident_data %>%
    pull(PGY_Level) %>%
    unique() %>%
    sort() %>%
    tail(1)
  
  # Calculate individual resident scores
  individual_scores <- resident_data %>%
    group_by(Sub_Competency, Competency) %>%
    summarise(
      individual_score = median(Rating, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate peer medians (same PGY level, excluding this resident)
  peer_data <- data$evaluations %>%
    filter(
      PGY_Level == resident_pgy,
      Resident_Name != resident_name
    )
  
  if (period_filter != "total") {
    peer_data <- peer_data %>%
      filter(Period == period_filter)
  }
  
  if (nrow(peer_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = "No peer data available for comparison",
               x = 0.5, y = 0.5, showarrow = FALSE
             ))
  }
  
  peer_medians <- peer_data %>%
    group_by(Sub_Competency, Competency) %>%
    summarise(
      peer_median = median(Rating, na.rm = TRUE),
      peer_n = n(),
      .groups = "drop"
    ) %>%
    filter(peer_n >= 3)  # Minimum peer data
  
  # Combine data
  comparison_data <- individual_scores %>%
    inner_join(peer_medians, by = c("Sub_Competency", "Competency")) %>%
    arrange(Competency, Sub_Competency) %>%
    mutate(
      difference = individual_score - peer_median,
      color = case_when(
        difference > 0.5 ~ "Above Peers",
        difference < -0.5 ~ "Below Peers",
        TRUE ~ "Similar to Peers"
      )
    )
  
  if (nrow(comparison_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = "Insufficient data for peer comparison",
               x = 0.5, y = 0.5, showarrow = FALSE
             ))
  }
  
  # Create bar chart
  fig <- plot_ly(comparison_data, 
                 x = ~Sub_Competency,
                 y = ~individual_score,
                 type = 'bar',
                 name = resident_name,
                 marker = list(color = '#2E86AB'),
                 hovertemplate = paste0(
                   '<b>%{x}</b><br>',
                   resident_name, ': %{y}<br>',
                   'Peer Median: ', round(comparison_data$peer_median, 1), '<br>',
                   'Difference: ', round(comparison_data$difference, 1),
                   '<extra></extra>'
                 ))
  
  # Add peer median line
  fig <- fig %>% add_trace(
    x = comparison_data$Sub_Competency,
    y = comparison_data$peer_median,
    type = 'scatter',
    mode = 'markers',
    name = paste("Peer Median (", resident_pgy, ")"),
    marker = list(color = '#A23B72', size = 10, symbol = 'diamond'),
    hovertemplate = paste0(
      '<b>%{x}</b><br>',
      'Peer Median: %{y}<br>',
      resident_name, ': ', round(comparison_data$individual_score, 1),
      '<extra></extra>'
    )
  )
  
  # Configure layout
  fig <- fig %>% layout(
    title = list(
      text = paste("Performance vs Peers -", resident_name, "(", resident_pgy, ")"),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Sub-Competency",
      tickangle = -45
    ),
    yaxis = list(
      title = "Score",
      range = c(1, 9),
      tickmode = 'linear',
      tick0 = 1,
      dtick = 1
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.2
    ),
    hovermode = 'x unified'
  )
  
  return(fig)
}