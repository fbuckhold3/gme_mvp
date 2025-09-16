# =============================================================================
# ENHANCED PROGRAM VISUALIZATION FUNCTIONS
# R/program_visualization_enhanced.R
# 
# Implements the specific requirements for your GME milestone visualization app
# =============================================================================

# Note: Libraries should be loaded in app.R, but adding here for safety
if (!require(plotly)) library(plotly)
if (!require(dplyr)) library(dplyr)
if (!require(tidyr)) library(tidyr)
if (!require(stringr)) library(stringr)

#' Create Performance Categories Based on Your Scoring System
#'
#' Converts numeric scores to performance categories
#'
#' @param scores Numeric vector of scores (1-9)
#' @return Character vector of performance categories
get_performance_category <- function(scores) {
  case_when(
    scores < 3 ~ "Beginner",
    scores >= 3 & scores < 5 ~ "Advanced Beginner", 
    scores >= 5 & scores < 7 ~ "Competent",
    scores >= 7 & scores < 9 ~ "Proficient",
    scores == 9 ~ "Expert",
    TRUE ~ "Unknown"
  )
}

#' Get Performance Context
#'
#' Provides contextual interpretation of scores
#'
#' @param scores Numeric vector of scores (1-9)
#' @return Character vector with contextual descriptions
get_performance_context <- function(scores) {
  case_when(
    scores < 2 ~ "Low Beginner",
    scores >= 2 & scores < 3 ~ "High Beginner",
    scores >= 3 & scores < 4 ~ "Low Advanced Beginner", 
    scores >= 4 & scores < 5 ~ "High Advanced Beginner",
    scores >= 5 & scores < 6 ~ "Low Competent",
    scores >= 6 & scores < 7 ~ "High Competent",
    scores >= 7 & scores < 8 ~ "Low Proficient",
    scores >= 8 & scores < 9 ~ "High Proficient",
    scores == 9 ~ "Expert",
    TRUE ~ "Unknown"
  )
}

#' Calculate Period-Specific Means
#'
#' Calculates means for each period + PGY level combination
#'
#' @param data Processed data from load_milestone_csv_data()
#' @return Data frame with means by period, level, and sub-competency
calculate_period_level_means <- function(data) {
  evaluation_data <- data$evaluations
  
  # Calculate means for each period + level + sub-competency combination
  period_means <- evaluation_data %>%
    mutate(
      Period_Type = case_when(
        str_detect(Period, "Mid-Year") ~ "Mid-Year",
        str_detect(Period, "Year-End|End-Year") ~ "Year-End",
        TRUE ~ "Other"
      )
    ) %>%
    filter(Period_Type %in% c("Mid-Year", "Year-End")) %>%
    group_by(Period_Type, PGY_Level, Sub_Competency, Competency) %>%
    summarise(
      mean_score = mean(Rating, na.rm = TRUE),
      n_evaluations = n(),
      n_residents = n_distinct(Resident_Name),
      .groups = "drop"
    ) %>%
    filter(n_evaluations >= 3) %>%  # Minimum data requirement
    mutate(
      period_level = paste(Period_Type, PGY_Level),
      performance_category = get_performance_category(mean_score),
      performance_context = get_performance_context(mean_score)
    )
  
  return(period_means)
}

#' Create Multi-Level Spider Plot (UPDATED VERSION)
#'
#' Creates spider plot allowing multiple PGY level comparisons
#'
#' @param data Processed data from load_milestone_csv_data()
#' @param period_type Period type ("recent_end", "recent_mid", "all_periods", or specific period)
#' @param selected_pgy_levels Vector of PGY levels to show
#' @param show_medians Whether to show overall program means
#' @return Plotly spider plot
create_multi_level_spider_plot <- function(data, period_type = "recent_end", 
                                           selected_pgy_levels = NULL, show_medians = TRUE) {
  evaluation_data <- data$evaluations
  
  # Handle period selection
  if (period_type == "recent_end") {
    recent_period <- evaluation_data %>%
      filter(str_detect(Period, "Year-End|End-Year")) %>%
      arrange(desc(Period)) %>%
      pull(Period) %>%
      head(1)
    
    if (length(recent_period) > 0) {
      evaluation_data <- evaluation_data %>% filter(Period == recent_period)
      period_title <- paste("Most Recent End-Year (", recent_period, ")")
    } else {
      period_title <- "No End-Year Data Available"
    }
  } else if (period_type == "recent_mid") {
    recent_period <- evaluation_data %>%
      filter(str_detect(Period, "Mid-Year")) %>%
      arrange(desc(Period)) %>%
      pull(Period) %>%
      head(1)
    
    if (length(recent_period) > 0) {
      evaluation_data <- evaluation_data %>% filter(Period == recent_period)
      period_title <- paste("Most Recent Mid-Year (", recent_period, ")")
    } else {
      period_title <- "No Mid-Year Data Available"
    }
  } else if (period_type == "all_periods") {
    # Don't filter by period - use all data
    period_title <- "All Periods Combined"
  } else {
    # Specific period
    evaluation_data <- evaluation_data %>% filter(Period == period_type)
    period_title <- period_type
  }
  
  # Filter by selected PGY levels
  if (!is.null(selected_pgy_levels) && length(selected_pgy_levels) > 0) {
    evaluation_data <- evaluation_data %>% filter(PGY_Level %in% selected_pgy_levels)
  }
  
  if (nrow(evaluation_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "No data available for selected filters", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Calculate means by PGY level and sub-competency
  spider_data <- evaluation_data %>%
    group_by(PGY_Level, Sub_Competency, Competency) %>%
    summarise(
      mean_score = mean(Rating, na.rm = TRUE),
      n_evaluations = n(),
      .groups = "drop"
    ) %>%
    filter(n_evaluations >= 3) %>%
    arrange(Competency, Sub_Competency)
  
  if (nrow(spider_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "Insufficient data for visualization", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Create the spider plot
  fig <- plot_ly(type = 'scatterpolar')
  
  # Add traces for each PGY level
  pgy_levels <- sort(unique(spider_data$PGY_Level))
  colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#592E83", "#048A81")
  
  for (i in seq_along(pgy_levels)) {
    level_data <- spider_data %>% filter(PGY_Level == pgy_levels[i])
    
    # Create hover text
    hover_text <- paste0(
      '<b>', level_data$Sub_Competency, '</b><br>',
      'PGY Level: ', pgy_levels[i], '<br>',
      'Mean Score: ', round(level_data$mean_score, 2), '<br>',
      'Competency: ', level_data$Competency,
      '<extra></extra>'
    )
    
    fig <- fig %>% add_trace(
      r = level_data$mean_score,
      theta = level_data$Sub_Competency,
      name = pgy_levels[i],
      line = list(color = colors[((i-1) %% length(colors)) + 1], width = 3),
      marker = list(color = colors[((i-1) %% length(colors)) + 1], size = 8),
      fill = ifelse(i == 1, 'toself', 'none'),  # Only fill first trace to avoid overlap
      fillcolor = paste0('rgba(', paste(col2rgb(colors[((i-1) %% length(colors)) + 1]), collapse = ', '), ', 0.1)'),
      hovertemplate = hover_text
    )
  }
  
  # Add program means if requested
  if (show_medians) {
    program_means <- spider_data %>%
      group_by(Sub_Competency, Competency) %>%
      summarise(
        overall_mean = mean(mean_score, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Competency, Sub_Competency)
    
    if (nrow(program_means) > 0) {
      fig <- fig %>% add_trace(
        r = program_means$overall_mean,
        theta = program_means$Sub_Competency,
        name = "Program Mean",
        line = list(color = '#FF6B35', width = 4, dash = 'dot'),
        marker = list(color = '#FF6B35', size = 10, symbol = 'diamond'),
        hovertemplate = paste0(
          '<b>', program_means$Sub_Competency, '</b><br>',
          'Program Mean: ', round(program_means$overall_mean, 2), '<br>',
          'Competency: ', program_means$Competency,
          '<extra></extra>'
        )
      )
    }
  }
  
  # Configure layout
  fig <- fig %>% layout(
    title = list(
      text = period_title,
      font = list(size = 16, family = "Arial, sans-serif", color = "#2c3e50"),
      x = 0.5
    ),
    polar = list(
      radialaxis = list(
        visible = TRUE,
        range = c(1, 9),
        tickmode = 'array',
        tickvals = c(1, 3, 5, 7, 9),
        ticktext = c('<b>1</b><br>Novice', '<b>3</b><br>Beginner', 
                     '<b>5</b><br>Competent', '<b>7</b><br>Proficient', '<b>9</b><br>Expert'),
        tickfont = list(size = 11, family = "Arial, sans-serif", color = "#2c3e50"),
        gridcolor = 'rgba(52, 73, 94, 0.2)',
        linecolor = 'rgba(52, 73, 94, 0.3)'
      ),
      angularaxis = list(
        tickfont = list(size = 11, family = "Arial, sans-serif", color = "#2c3e50"),
        rotation = 90,
        direction = "clockwise"
      )
    ),
    margin = list(l = 60, r = 60, t = 80, b = 60),
    paper_bgcolor = 'white',
    plot_bgcolor = 'rgba(248, 249, 250, 0.5)',
    showlegend = TRUE,
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1,
      font = list(size = 12, family = "Arial, sans-serif", color = "#2c3e50")
    )
  ) %>%
    plotly::config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c('pan2d', 'select2d', 'lasso2d', 'autoScale2d')
    )
  
  return(fig)
}

#' Create Sequential Trend Line Plot
#'
#' Shows progression across periods: Mid-Year PGY1, Year-End PGY1, Mid-Year PGY2, etc.
#'
#' @param data Processed data from load_milestone_csv_data()
#' @param competency_filter Specific competency to show ("all" for all)
#' @return Plotly line plot
create_sequential_trend_plot <- function(data, competency_filter = "all") {
  evaluation_data <- data$evaluations
  
  if (competency_filter != "all") {
    evaluation_data <- evaluation_data %>% filter(Competency == competency_filter)
  }
  
  if (nrow(evaluation_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "No data available", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Create sequential period ordering
  trend_data <- evaluation_data %>%
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
    group_by(Sub_Competency, Competency, Period_Order, Period_Label) %>%
    summarise(
      mean_score = mean(Rating, na.rm = TRUE),
      n_evaluations = n(),
      .groups = "drop"
    ) %>%
    filter(n_evaluations >= 3) %>%
    arrange(Sub_Competency, Period_Order)
  
  if (nrow(trend_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "Insufficient data for trends", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Create the plot
  fig <- plot_ly(trend_data, 
                 x = ~Period_Order, 
                 y = ~mean_score,
                 color = ~Sub_Competency,
                 type = 'scatter', 
                 mode = 'lines+markers',
                 line = list(width = 3),
                 marker = list(size = 6),
                 hovertemplate = paste0(
                   '<b>%{data.name}</b><br>',
                   'Period: ', trend_data$Period_Label, '<br>',
                   'Mean Score: %{y}<br>',
                   'Competency: ', trend_data$Competency,
                   '<extra></extra>'
                 ))
  
  # Configure layout
  fig <- fig %>% layout(
    title = list(
      text = ifelse(competency_filter == "all", 
                    "Sub-Competency Progression Across Training Periods",
                    paste("Sub-Competency Progression -", competency_filter)),
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
      title = "Mean Score",
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

#' Create Enhanced Performance Heatmap with Period-Level Data
#'
#' Shows sub-competencies (rows) by period+level combinations (columns)
#'
#' @param data Processed data from load_milestone_csv_data()
#' @param metric Metric to display ("median", "performance_category")
#' @param sortable Whether to make rows sortable
#' @return Plotly heatmap
create_performance_heatmap <- function(data, metric = "median", sortable = TRUE) {
  period_means <- calculate_period_level_means(data)
  
  if (nrow(period_means) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "No data available", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Prepare data based on selected metric
  if (metric == "median") {
    heatmap_data <- period_means %>%
      select(Sub_Competency, period_level, mean_score) %>%
      pivot_wider(names_from = period_level, values_from = mean_score)
    
    color_scale = "Viridis"
    color_title = "Mean Score"
    
  } else if (metric == "performance_category") {
    heatmap_data <- period_means %>%
      mutate(
        value = case_when(
          performance_category == "Beginner" ~ 1,
          performance_category == "Advanced Beginner" ~ 2,
          performance_category == "Competent" ~ 3,
          performance_category == "Proficient" ~ 4,
          performance_category == "Expert" ~ 5,
          TRUE ~ 0
        )
      ) %>%
      select(Sub_Competency, period_level, value) %>%
      pivot_wider(names_from = period_level, values_from = value)
    
    color_scale = list(c(0, "#d32f2f"), c(0.25, "#ff9800"), c(0.5, "#ffeb3b"), 
                       c(0.75, "#4caf50"), c(1, "#2e7d32"))
    color_title = "Performance Level"
    
  } else {
    # Default to mean if other metrics requested
    heatmap_data <- period_means %>%
      select(Sub_Competency, period_level, mean_score) %>%
      pivot_wider(names_from = period_level, values_from = mean_score)
    
    color_scale = "Viridis" 
    color_title = "Mean Score"
  }
  
  # Convert to matrix for heatmap
  heatmap_matrix <- heatmap_data %>%
    column_to_rownames("Sub_Competency") %>%
    as.matrix()
  
  # Sort rows if requested
  if (sortable) {
    row_means <- rowMeans(heatmap_matrix, na.rm = TRUE)
    heatmap_matrix <- heatmap_matrix[order(row_means, decreasing = TRUE), ]
  }
  
  # Create heatmap
  fig <- plot_ly(
    z = heatmap_matrix,
    x = colnames(heatmap_matrix),
    y = rownames(heatmap_matrix),
    type = "heatmap",
    colorscale = color_scale,
    hovertemplate = paste0(
      '<b>Sub-competency:</b> %{y}<br>',
      '<b>Period & Level:</b> %{x}<br>',
      '<b>', color_title, ':</b> %{z}<br>',
      '<extra></extra>'
    )
  )
  
  fig <- fig %>% layout(
    title = list(
      text = paste("Sub-Competency Performance by Period & Level -", color_title),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Period & Training Level",
      tickangle = -45
    ),
    yaxis = list(
      title = "Sub-Competency",
      autorange = "reversed"
    )
  )
  
  return(fig)
}

#' Identify Areas for Improvement - Period Specific (Using Means)
#'
#' Creates a table showing lowest performing sub-competencies for specific periods
#'
#' @param data Processed data from load_milestone_csv_data()
#' @param n_items Number of items to show
#' @param focus_end_year Whether to focus on end-year evaluations only
#' @return Data frame with improvement areas
identify_improvement_areas <- function(data, n_items = 5, focus_end_year = TRUE) {
  period_means <- calculate_period_level_means(data)
  
  # Focus on End-Year evaluations if requested
  if (focus_end_year) {
    period_means <- period_means %>% filter(Period_Type == "Year-End")
  }
  
  if (nrow(period_means) == 0) {
    return(data.frame(
      Sub_Competency = "No data",
      Period_Level = "",
      Mean_Score = "",
      Performance_Context = "",
      Residents = ""
    ))
  }
  
  # For each PGY level, find the lowest performing sub-competencies
  improvement_areas <- period_means %>%
    group_by(PGY_Level) %>%
    slice_min(mean_score, n = ceiling(n_items/length(unique(period_means$PGY_Level)))) %>%
    ungroup() %>%
    head(n_items) %>%
    mutate(
      Mean_Score = sprintf("%.2f", mean_score),
      Performance_Context = performance_context,
      Residents = n_residents
    ) %>%
    arrange(PGY_Level, mean_score) %>%
    select(Sub_Competency, period_level, Mean_Score, Performance_Context, Residents) %>%
    rename(Period_Level = period_level)
  
  return(improvement_areas)
}

#' Identify Areas of Strength - Period Specific (Using Means)
#'
#' Creates a table showing highest performing sub-competencies for specific periods
#'
#' @param data Processed data from load_milestone_csv_data()
#' @param n_items Number of items to show
#' @param focus_end_year Whether to focus on end-year evaluations only
#' @return Data frame with strength areas
identify_strength_areas <- function(data, n_items = 5, focus_end_year = TRUE) {
  period_means <- calculate_period_level_means(data)
  
  # Focus on End-Year evaluations if requested
  if (focus_end_year) {
    period_means <- period_means %>% filter(Period_Type == "Year-End")
  }
  
  if (nrow(period_means) == 0) {
    return(data.frame(
      Sub_Competency = "No data",
      Period_Level = "",
      Mean_Score = "",
      Performance_Context = "",
      Residents = ""
    ))
  }
  
  # For each PGY level, find the highest performing sub-competencies
  strength_areas <- period_means %>%
    group_by(PGY_Level) %>%
    slice_max(mean_score, n = ceiling(n_items/length(unique(period_means$PGY_Level)))) %>%
    ungroup() %>%
    head(n_items) %>%
    mutate(
      Mean_Score = sprintf("%.2f", mean_score),
      Performance_Context = performance_context,
      Residents = n_residents
    ) %>%
    arrange(PGY_Level, desc(mean_score)) %>%
    select(Sub_Competency, period_level, Mean_Score, Performance_Context, Residents) %>%
    rename(Period_Level = period_level)
  
  return(strength_areas)
}

#' Identify Areas for Improvement - With Custom Filters
#'
#' Creates a table showing lowest performing sub-competencies with custom period/level filters
#'
#' @param data Processed data from load_milestone_csv_data()
#' @param n_items Number of items to show
#' @param period_selection Period selection ("recent_end", "recent_mid", "total", or specific period)
#' @param selected_pgy_levels Vector of PGY levels to include
#' @return Data frame with improvement areas
identify_improvement_areas_filtered <- function(data, n_items = 5, period_selection = "recent_end", selected_pgy_levels = NULL) {
  evaluation_data <- data$evaluations
  
  # Filter by period
  if (period_selection == "recent_end") {
    recent_period <- evaluation_data %>%
      filter(str_detect(Period, "Year-End|End-Year")) %>%
      arrange(desc(Period)) %>%
      pull(Period) %>%
      head(1)
    
    if (length(recent_period) > 0) {
      evaluation_data <- evaluation_data %>% filter(Period == recent_period)
    }
  } else if (period_selection == "recent_mid") {
    recent_period <- evaluation_data %>%
      filter(str_detect(Period, "Mid-Year")) %>%
      arrange(desc(Period)) %>%
      pull(Period) %>%
      head(1)
    
    if (length(recent_period) > 0) {
      evaluation_data <- evaluation_data %>% filter(Period == recent_period)
    }
  } else if (period_selection != "total") {
    evaluation_data <- evaluation_data %>% filter(Period == period_selection)
  }
  
  # Filter by PGY levels
  if (!is.null(selected_pgy_levels) && length(selected_pgy_levels) > 0) {
    evaluation_data <- evaluation_data %>% filter(PGY_Level %in% selected_pgy_levels)
  }
  
  if (nrow(evaluation_data) == 0) {
    return(data.frame(
      Sub_Competency = "No data",
      Period_Level = "",
      Mean_Score = "",
      Performance_Context = "",
      Residents = ""
    ))
  }
  
  # Calculate means and find lowest performers
  improvement_areas <- evaluation_data %>%
    group_by(PGY_Level, Sub_Competency, Competency) %>%
    summarise(
      mean_score = mean(Rating, na.rm = TRUE),
      n_residents = n_distinct(Resident_Name),
      n_evaluations = n(),
      .groups = "drop"
    ) %>%
    filter(n_evaluations >= 3) %>%
    group_by(PGY_Level) %>%
    slice_min(mean_score, n = ceiling(n_items/length(unique(.$PGY_Level)))) %>%
    ungroup() %>%
    head(n_items) %>%
    mutate(
      Period_Level = paste("Filtered", PGY_Level),
      Mean_Score = sprintf("%.2f", mean_score),
      Performance_Context = get_performance_context(mean_score),
      Residents = n_residents
    ) %>%
    arrange(PGY_Level, mean_score) %>%
    select(Sub_Competency, Period_Level, Mean_Score, Performance_Context, Residents)
  
  return(improvement_areas)
}

#' Identify Areas of Strength - With Custom Filters
#'
#' Creates a table showing highest performing sub-competencies with custom period/level filters
#'
#' @param data Processed data from load_milestone_csv_data()
#' @param n_items Number of items to show
#' @param period_selection Period selection ("recent_end", "recent_mid", "total", or specific period)
#' @param selected_pgy_levels Vector of PGY levels to include
#' @return Data frame with strength areas
identify_strength_areas_filtered <- function(data, n_items = 5, period_selection = "recent_end", selected_pgy_levels = NULL) {
  evaluation_data <- data$evaluations
  
  # Filter by period
  if (period_selection == "recent_end") {
    recent_period <- evaluation_data %>%
      filter(str_detect(Period, "Year-End|End-Year")) %>%
      arrange(desc(Period)) %>%
      pull(Period) %>%
      head(1)
    
    if (length(recent_period) > 0) {
      evaluation_data <- evaluation_data %>% filter(Period == recent_period)
    }
  } else if (period_selection == "recent_mid") {
    recent_period <- evaluation_data %>%
      filter(str_detect(Period, "Mid-Year")) %>%
      arrange(desc(Period)) %>%
      pull(Period) %>%
      head(1)
    
    if (length(recent_period) > 0) {
      evaluation_data <- evaluation_data %>% filter(Period == recent_period)
    }
  } else if (period_selection != "total") {
    evaluation_data <- evaluation_data %>% filter(Period == period_selection)
  }
  
  # Filter by PGY levels
  if (!is.null(selected_pgy_levels) && length(selected_pgy_levels) > 0) {
    evaluation_data <- evaluation_data %>% filter(PGY_Level %in% selected_pgy_levels)
  }
  
  if (nrow(evaluation_data) == 0) {
    return(data.frame(
      Sub_Competency = "No data",
      Period_Level = "",
      Mean_Score = "",
      Performance_Context = "",
      Residents = ""
    ))
  }
  
  # Calculate means and find highest performers
  strength_areas <- evaluation_data %>%
    group_by(PGY_Level, Sub_Competency, Competency) %>%
    summarise(
      mean_score = mean(Rating, na.rm = TRUE),
      n_residents = n_distinct(Resident_Name),
      n_evaluations = n(),
      .groups = "drop"
    ) %>%
    filter(n_evaluations >= 3) %>%
    group_by(PGY_Level) %>%
    slice_max(mean_score, n = ceiling(n_items/length(unique(.$PGY_Level)))) %>%
    ungroup() %>%
    head(n_items) %>%
    mutate(
      Period_Level = paste("Filtered", PGY_Level),
      Mean_Score = sprintf("%.2f", mean_score),
      Performance_Context = get_performance_context(mean_score),
      Residents = n_residents
    ) %>%
    arrange(PGY_Level, desc(mean_score)) %>%
    select(Sub_Competency, Period_Level, Mean_Score, Performance_Context, Residents)
  
  return(strength_areas)
}

# ===================================================================
# COMPLETE create_multi_level_spider_plot Function
# Add this to your R/program_visualization_enhanced.R file
# ===================================================================

#' Create Multi-Level Spider Plot
#'
#' Creates spider plot allowing multiple PGY level comparisons
#'
#' @param data Processed data from load_milestone_csv_data()
#' @param period_type Period type ("recent_end", "recent_mid", "all_periods", or specific period)
#' @param selected_pgy_levels Vector of PGY levels to show
#' @param show_medians Whether to show overall program means
#' @return Plotly spider plot
create_multi_level_spider_plot <- function(data, period_type = "recent_end", 
                                           selected_pgy_levels = NULL, show_medians = TRUE) {
  evaluation_data <- data$evaluations
  
  # Handle period selection
  if (period_type == "recent_end") {
    recent_period <- evaluation_data %>%
      filter(str_detect(Period, "Year-End|End-Year")) %>%
      arrange(desc(Period)) %>%
      pull(Period) %>%
      head(1)
    
    if (length(recent_period) > 0) {
      evaluation_data <- evaluation_data %>% filter(Period == recent_period)
      period_title <- paste("Most Recent End-Year (", recent_period, ")")
    } else {
      period_title <- "No End-Year Data Available"
    }
  } else if (period_type == "recent_mid") {
    recent_period <- evaluation_data %>%
      filter(str_detect(Period, "Mid-Year")) %>%
      arrange(desc(Period)) %>%
      pull(Period) %>%
      head(1)
    
    if (length(recent_period) > 0) {
      evaluation_data <- evaluation_data %>% filter(Period == recent_period)
      period_title <- paste("Most Recent Mid-Year (", recent_period, ")")
    } else {
      period_title <- "No Mid-Year Data Available"
    }
  } else if (period_type == "all_periods") {
    period_title <- "All Periods Combined"
  } else {
    # Specific period
    evaluation_data <- evaluation_data %>% filter(Period == period_type)
    period_title <- period_type
  }
  
  # Filter by selected PGY levels
  if (!is.null(selected_pgy_levels) && length(selected_pgy_levels) > 0) {
    evaluation_data <- evaluation_data %>% filter(PGY_Level %in% selected_pgy_levels)
  }
  
  if (nrow(evaluation_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "No data available for selected filters", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Calculate means by PGY level and sub-competency
  spider_data <- evaluation_data %>%
    group_by(PGY_Level, Sub_Competency, Competency) %>%
    summarise(
      mean_score = mean(Rating, na.rm = TRUE),
      n_evaluations = n(),
      .groups = "drop"
    ) %>%
    filter(n_evaluations >= 3) %>%
    arrange(Competency, Sub_Competency)
  
  if (nrow(spider_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "Insufficient data for visualization", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Create the spider plot
  fig <- plot_ly(type = 'scatterpolar')
  
  # Add traces for each PGY level
  pgy_levels <- unique(spider_data$PGY_Level)
  colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#592E83", "#048A81")
  
  for (i in seq_along(pgy_levels)) {
    level_data <- spider_data %>% filter(PGY_Level == pgy_levels[i])
    
    # Create hover text
    hover_text <- paste0(
      '<b>', level_data$Sub_Competency, '</b><br>',
      '<b>PGY Level:</b> ', pgy_levels[i], '<br>',
      '<b>Mean Score:</b> ', round(level_data$mean_score, 2), '<br>',
      'Competency: ', level_data$Competency, '<br>',
      'N Evaluations: ', level_data$n_evaluations,
      '<extra></extra>'
    )
    
    fig <- fig %>% add_trace(
      r = level_data$mean_score,
      theta = level_data$Sub_Competency,
      name = pgy_levels[i],
      type = 'scatterpolar',
      mode = 'lines+markers',  # EXPLICITLY SET MODE
      line = list(color = colors[i %% length(colors) + 1], width = 3),
      marker = list(color = colors[i %% length(colors) + 1], size = 6),
      fill = ifelse(i == 1, 'toself', 'none'),
      fillcolor = if (i == 1) paste0(substr(colors[i %% length(colors) + 1], 1, 7), "20") else NULL,
      hovertemplate = hover_text
    )
  }
  
  # Configure the plot layout
  fig <- fig %>% layout(
    title = list(
      text = paste0("<b>", period_title, "</b>"),
      font = list(size = 18, family = "Arial, sans-serif", color = "#2c3e50"),
      x = 0.5,
      y = 0.95
    ),
    polar = list(
      radialaxis = list(
        visible = TRUE,
        range = c(0, 9),
        tickmode = 'array',
        tickvals = c(1, 3, 5, 7, 9),
        ticktext = c('<b>1</b><br>Beginner', '<b>3</b><br>Low Advanced Beginner', 
                     '<b>5</b><br>Competent', '<b>7</b><br>Proficient', '<b>9</b><br>Expert'),
        tickfont = list(size = 12, color = "#34495e", family = "Arial, sans-serif"),
        gridcolor = 'rgba(52, 73, 94, 0.15)',
        linecolor = 'rgba(52, 73, 94, 0.2)',
        zeroline = FALSE
      ),
      angularaxis = list(
        tickfont = list(size = 11, family = "Arial, sans-serif", color = "#2c3e50"),
        linecolor = 'rgba(52, 73, 94, 0.3)',
        gridcolor = 'rgba(52, 73, 94, 0.15)'
      )
    ),
    showlegend = TRUE,
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.25,
      font = list(size = 13, family = "Arial, sans-serif", color = "#2c3e50"),
      bgcolor = 'rgba(255, 255, 255, 0.9)',
      bordercolor = 'rgba(52, 73, 94, 0.2)',
      borderwidth = 1
    ),
    paper_bgcolor = 'white',
    plot_bgcolor = 'rgba(248, 249, 250, 0.8)',
    margin = list(t = 120, b = 120, l = 80, r = 80),
    hovermode = 'x unified'
  ) %>%
    # Add config for better interactivity
    plotly::config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c('pan2d', 'select2d', 'lasso2d', 'autoScale2d')
    )
  
  return(fig)
}
