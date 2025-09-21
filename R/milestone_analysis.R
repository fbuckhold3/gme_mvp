# ===== ENHANCED MILESTONE ANALYSIS FUNCTIONS WITH DATA SUFFICIENCY CHECKS =====

# Enhanced Graduation Readiness Analysis with Data Checks
calculate_graduation_readiness_enhanced <- function(data, threshold = 7, period_filter = "latest_year_end", 
                                                    competency_filter = "all", show_by_period = FALSE) {
  
  evaluation_data <- data$evaluations
  
  # Check for minimum program-level data requirements FIRST
  unique_residents <- n_distinct(evaluation_data$Resident_Name)
  
  if (unique_residents < 3) {
    # Return a special data frame that signals insufficient data
    return(data.frame(
      Sub_Competency = "INSUFFICIENT_DATA",
      message = paste0(
        "Insufficient data for graduation readiness analysis. ",
        "Current: ", unique_residents, " residents. ",
        "Required: Minimum 3 residents for meaningful analysis. ",
        "Please use the Individual Assessment tab for resident-specific insights."
      ),
      unique_residents = unique_residents,
      stringsAsFactors = FALSE
    ))
  }
  
  # Find the highest PGY level (graduating class)
  max_pgy <- evaluation_data %>%
    pull(PGY_Level) %>%
    str_extract("\\d+") %>%
    as.numeric() %>%
    max(na.rm = TRUE)
  
  graduating_level <- paste0("PGY-", max_pgy)
  
  # Filter to graduating residents only
  graduating_data <- evaluation_data %>%
    filter(PGY_Level == graduating_level)
  
  if (nrow(graduating_data) == 0) {
    return(data.frame(
      Sub_Competency = "NO_GRADUATING_DATA",
      message = paste0(
        "No data found for graduating class (", graduating_level, "). ",
        "Please verify PGY level coding or use All Levels Analysis instead."
      ),
      graduating_level = graduating_level,
      stringsAsFactors = FALSE
    ))
  }
  
  graduating_residents <- n_distinct(graduating_data$Resident_Name)
  
  if (graduating_residents < 2) {
    return(data.frame(
      Sub_Competency = "INSUFFICIENT_GRADUATING_DATA",
      message = paste0(
        "Only ", graduating_residents, " resident in graduating class (", graduating_level, "). ",
        "Readiness analysis requires multiple residents for meaningful comparison. ",
        "Consider using Individual Assessment or All Levels Analysis instead."
      ),
      graduating_residents = graduating_residents,
      graduating_level = graduating_level,
      stringsAsFactors = FALSE
    ))
  }
  
  # For "all" periods, use the specialized total calculation
  if (period_filter == "all" && !show_by_period) {
    return(calculate_total_graduation_readiness_with_checks(data, threshold, competency_filter))
  }
  
  # Apply period filter
  if (period_filter == "latest_year_end") {
    graduating_data <- graduating_data %>%
      filter(str_detect(Period, "Year-End|End-Year")) %>%
      group_by(Resident_Name, Sub_Competency) %>%
      arrange(desc(Period)) %>%
      slice_head(n = 1) %>%
      ungroup()
  } else if (period_filter != "all") {
    graduating_data <- graduating_data %>% filter(Period == period_filter)
  }
  
  # Apply competency filter
  if (competency_filter != "all") {
    graduating_data <- graduating_data %>% filter(Competency == competency_filter)
  }
  
  # Check if we still have data after filtering
  if (nrow(graduating_data) == 0) {
    return(data.frame(
      Sub_Competency = "NO_DATA_AFTER_FILTERING",
      message = paste0(
        "No data available after applying filters. ",
        "Period: ", period_filter, ", Competency: ", competency_filter, ". ",
        "Try different filter combinations or use Individual Assessment."
      ),
      stringsAsFactors = FALSE
    ))
  }
  
  # Continue with your existing calculation logic...
  # (Rest of your existing function code here)
  
  # Calculate metrics - either by period or overall
  if (show_by_period && period_filter == "all") {
    readiness_metrics <- graduating_data %>%
      group_by(Sub_Competency, Competency, Period) %>%
      summarise(
        total_residents = n_distinct(Resident_Name),
        total_evaluations = n(),
        below_threshold = sum(Rating < threshold, na.rm = TRUE),
        at_or_above_threshold = sum(Rating >= threshold, na.rm = TRUE),
        mean_score = mean(Rating, na.rm = TRUE),
        median_score = median(Rating, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(total_evaluations >= 3) %>%
      mutate(
        percent_below_threshold = (below_threshold / total_evaluations) * 100,
        percent_ready = ((total_evaluations - below_threshold) / total_evaluations) * 100,
        readiness_category = case_when(
          percent_below_threshold < 2.5 ~ "Excellent (<2.5%)",
          percent_below_threshold < 5.0 ~ "Good (2.5-5%)",
          percent_below_threshold < 7.5 ~ "Concerning (5-7.5%)",
          TRUE ~ "High Risk (>7.5%)"
        ),
        graduating_class = graduating_level,
        threshold_used = threshold,
        competency_period = paste0(Sub_Competency, " - ", Period)
      ) %>%
      arrange(desc(percent_below_threshold))
  } else {
    readiness_metrics <- graduating_data %>%
      group_by(Sub_Competency, Competency) %>%
      summarise(
        total_residents = n_distinct(Resident_Name),
        total_evaluations = n(),
        below_threshold = sum(Rating < threshold, na.rm = TRUE),
        at_or_above_threshold = sum(Rating >= threshold, na.rm = TRUE),
        mean_score = mean(Rating, na.rm = TRUE),
        median_score = median(Rating, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(total_evaluations >= 3) %>%
      mutate(
        percent_below_threshold = (below_threshold / total_evaluations) * 100,
        percent_ready = ((total_evaluations - below_threshold) / total_evaluations) * 100,
        readiness_category = case_when(
          percent_below_threshold < 2.5 ~ "Excellent (<2.5%)",
          percent_below_threshold < 5.0 ~ "Good (2.5-5%)",
          percent_below_threshold < 7.5 ~ "Concerning (5-7.5%)",
          TRUE ~ "High Risk (>7.5%)"
        ),
        graduating_class = graduating_level,
        threshold_used = threshold
      ) %>%
      arrange(desc(percent_below_threshold))
  }
  
  return(readiness_metrics)
}

# Enhanced Total Graduation Readiness with Checks
calculate_total_graduation_readiness_with_checks <- function(data, threshold = 7, competency_filter = "all") {
  
  evaluation_data <- data$evaluations
  
  # Check for minimum data
  unique_residents <- n_distinct(evaluation_data$Resident_Name)
  
  if (unique_residents < 3) {
    return(data.frame(
      Sub_Competency = "INSUFFICIENT_DATA",
      message = paste0(
        "Insufficient data for total graduation readiness analysis. ",
        "Current: ", unique_residents, " residents. Required: Minimum 3."
      ),
      stringsAsFactors = FALSE
    ))
  }
  
  # Continue with your existing logic...
  max_pgy <- evaluation_data %>%
    pull(PGY_Level) %>%
    str_extract("\\d+") %>%
    as.numeric() %>%
    max(na.rm = TRUE)
  
  graduating_level <- paste0("PGY-", max_pgy)
  
  final_assessments <- evaluation_data %>%
    filter(
      PGY_Level == graduating_level,
      str_detect(Period, "Year-End|End-Year")
    ) %>%
    group_by(Resident_Name, Sub_Competency) %>%
    arrange(desc(Period)) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  if (competency_filter != "all") {
    final_assessments <- final_assessments %>% filter(Competency == competency_filter)
  }
  
  # Rest of your existing calculation...
  readiness_metrics <- final_assessments %>%
    group_by(Sub_Competency, Competency) %>%
    summarise(
      total_residents = n_distinct(Resident_Name),
      total_evaluations = n(),
      below_threshold = sum(Rating < threshold, na.rm = TRUE),
      at_or_above_threshold = sum(Rating >= threshold, na.rm = TRUE),
      mean_score = mean(Rating, na.rm = TRUE),
      median_score = median(Rating, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(total_evaluations >= 3) %>%
    mutate(
      percent_below_threshold = (below_threshold / total_evaluations) * 100,
      percent_ready = ((total_evaluations - below_threshold) / total_evaluations) * 100,
      readiness_category = case_when(
        percent_below_threshold < 2.5 ~ "Excellent (<2.5%)",
        percent_below_threshold < 5.0 ~ "Good (2.5-5%)",
        percent_below_threshold < 7.5 ~ "Concerning (5-7.5%)",
        TRUE ~ "High Risk (>7.5%)"
      ),
      graduating_class = graduating_level,
      threshold_used = threshold
    ) %>%
    arrange(desc(percent_below_threshold))
  
  return(readiness_metrics)
}

# Enhanced All Levels Analysis with Checks
calculate_all_levels_readiness_enhanced <- function(data, threshold = 7, period_filter = "latest_year_end", 
                                                    competency_filter = "all", selected_pgy_levels = NULL) {
  
  evaluation_data <- data$evaluations
  
  # Check for minimum program-level data
  unique_residents <- n_distinct(evaluation_data$Resident_Name)
  
  if (unique_residents < 3) {
    return(data.frame(
      PGY_Level = "INSUFFICIENT_DATA",
      Sub_Competency = "INSUFFICIENT_DATA",
      message = paste0(
        "Insufficient data for all levels analysis. ",
        "Current: ", unique_residents, " residents. Required: Minimum 3. ",
        "Please use Individual Assessment for resident-specific insights."
      ),
      stringsAsFactors = FALSE
    ))
  }
  
  # Filter by selected PGY levels
  if (!is.null(selected_pgy_levels) && length(selected_pgy_levels) > 0) {
    evaluation_data <- evaluation_data %>% filter(PGY_Level %in% selected_pgy_levels)
  }
  
  # Check if we have data after PGY filtering
  if (nrow(evaluation_data) == 0) {
    return(data.frame(
      PGY_Level = "NO_DATA_SELECTED_LEVELS",
      Sub_Competency = "NO_DATA_SELECTED_LEVELS",
      message = "No data available for selected PGY levels. Try different level selections.",
      stringsAsFactors = FALSE
    ))
  }
  
  # Continue with your existing logic...
  # (Rest of your existing function here)
  
  # Apply period filter
  if (period_filter == "latest_year_end") {
    latest_periods <- evaluation_data %>%
      filter(str_detect(Period, "Year-End|End-Year")) %>%
      group_by(PGY_Level) %>%
      arrange(desc(Period)) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      select(PGY_Level, Period) %>%
      distinct()
    
    evaluation_data <- evaluation_data %>%
      inner_join(latest_periods, by = c("PGY_Level", "Period"))
    
  } else if (period_filter != "all") {
    evaluation_data <- evaluation_data %>% filter(Period == period_filter)
  }
  
  if (competency_filter != "all") {
    evaluation_data <- evaluation_data %>% filter(Competency == competency_filter)
  }
  
  readiness_metrics <- evaluation_data %>%
    group_by(PGY_Level, Sub_Competency, Competency) %>%
    summarise(
      total_residents = n_distinct(Resident_Name),
      total_evaluations = n(),
      below_threshold = sum(Rating < threshold, na.rm = TRUE),
      at_or_above_threshold = sum(Rating >= threshold, na.rm = TRUE),
      mean_score = mean(Rating, na.rm = TRUE),
      median_score = median(Rating, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(total_evaluations >= 3) %>%
    mutate(
      percent_below_threshold = (below_threshold / total_evaluations) * 100,
      percent_ready = ((total_evaluations - below_threshold) / total_evaluations) * 100,
      readiness_category = case_when(
        percent_below_threshold < 2.5 ~ "Excellent (<2.5%)",
        percent_below_threshold < 5.0 ~ "Good (2.5-5%)",
        percent_below_threshold < 7.5 ~ "Concerning (5-7.5%)",
        TRUE ~ "High Risk (>7.5%)"
      ),
      threshold_used = threshold,
      competency_pgy = paste0(Sub_Competency, " - ", PGY_Level)
    ) %>%
    arrange(PGY_Level, desc(percent_below_threshold))
  
  return(readiness_metrics)
}

# Enhanced Sub-Competency Trends with Checks
calculate_subcompetency_trends_enhanced <- function(data, sub_competency, threshold = 7, selected_periods = NULL) {
  
  evaluation_data <- data$evaluations
  
  # Check for minimum data
  unique_residents <- n_distinct(evaluation_data$Resident_Name)
  
  if (unique_residents < 3) {
    return(data.frame(
      sub_competency = sub_competency,
      message = paste0(
        "Insufficient data for trend analysis. ",
        "Current: ", unique_residents, " residents. Required: Minimum 3. ",
        "Consider using Individual Assessment for progression tracking."
      ),
      unique_residents = unique_residents,
      stringsAsFactors = FALSE
    ))
  }
  
  # Filter to specific sub-competency
  filtered_data <- evaluation_data %>%
    filter(Sub_Competency == sub_competency)
  
  if (nrow(filtered_data) == 0) {
    return(data.frame(
      sub_competency = sub_competency,
      message = paste0(
        "No data available for sub-competency '", sub_competency, "'. ",
        "Please verify the sub-competency name or select a different one."
      ),
      stringsAsFactors = FALSE
    ))
  }
  
  # Apply period selection if specified
  if (!is.null(selected_periods) && length(selected_periods) > 0) {
    filtered_data <- filtered_data %>%
      filter(Period %in% selected_periods)
  }
  
  # Check for multiple time points
  unique_periods <- n_distinct(filtered_data$Period)
  
  if (unique_periods < 2) {
    return(data.frame(
      sub_competency = sub_competency,
      message = paste0(
        "Insufficient time points for trend analysis. ",
        "Sub-competency '", sub_competency, "' has data from only ", unique_periods, " period. ",
        "Trend analysis requires multiple evaluation periods."
      ),
      unique_periods = unique_periods,
      stringsAsFactors = FALSE
    ))
  }
  
  # Continue with your existing trend calculation logic...
  trend_data <- filtered_data %>%
    mutate(
      PGY_Number = as.numeric(str_extract(PGY_Level, "\\d+")),
      Period_Type = case_when(
        str_detect(Period, "Mid-Year") ~ "Mid-Year",
        str_detect(Period, "Year-End|End-Year") ~ "Year-End",
        TRUE ~ "Other"
      ),
      Academic_Year = str_extract(Period, "^\\d{4}-\\d{4}"),
      Period_Order = case_when(
        Period_Type == "Mid-Year" ~ (PGY_Number - 1) * 2 + 1,
        Period_Type == "Year-End" ~ (PGY_Number - 1) * 2 + 2,
        TRUE ~ 999
      ),
      Period_Label = paste0(Period_Type, " ", PGY_Level),
      Full_Period_Label = Period
    ) %>%
    filter(Period_Order < 999) %>%
    group_by(Period_Order, Period_Label, Full_Period_Label, Period_Type, Academic_Year, Period) %>%
    summarise(
      total_residents = n_distinct(Resident_Name),
      total_evaluations = n(),
      mean_score = mean(Rating, na.rm = TRUE),
      median_score = median(Rating, na.rm = TRUE),
      min_score = min(Rating, na.rm = TRUE),
      max_score = max(Rating, na.rm = TRUE),
      percent_below_threshold = (sum(Rating < threshold, na.rm = TRUE) / n()) * 100,
      .groups = "drop"
    ) %>%
    filter(total_evaluations >= 3) %>%
    arrange(Period_Order)
  
  # Rest of your existing calculation...
  period_specific_means <- evaluation_data %>%
    mutate(
      PGY_Number = as.numeric(str_extract(PGY_Level, "\\d+")),
      Period_Type = case_when(
        str_detect(Period, "Mid-Year") ~ "Mid-Year",
        str_detect(Period, "Year-End|End-Year") ~ "Year-End",
        TRUE ~ "Other"
      ),
      Period_Order = case_when(
        Period_Type == "Mid-Year" ~ (PGY_Number - 1) * 2 + 1,
        Period_Type == "Year-End" ~ (PGY_Number - 1) * 2 + 2,
        TRUE ~ 999
      )
    ) %>%
    filter(Period_Order < 999, Period %in% trend_data$Period) %>%
    group_by(Period) %>%
    summarise(
      period_specific_mean = mean(Rating, na.rm = TRUE),
      total_evaluations_period = n(),
      .groups = "drop"
    ) %>%
    filter(total_evaluations_period >= 5)
  
  final_trend_data <- trend_data %>%
    left_join(
      period_specific_means %>% select(Period, period_specific_mean),
      by = "Period"
    ) %>%
    mutate(
      sub_competency = sub_competency,
      threshold_used = threshold,
      performance_vs_period = mean_score - period_specific_mean
    ) %>%
    arrange(Period_Order)
  
  return(final_trend_data)
}

# Enhanced Graduation Readiness Chart with Data Check
create_graduation_readiness_chart <- function(readiness_data) {
  
  # Check if this is an error/insufficient data response
  if (nrow(readiness_data) == 1 && "message" %in% names(readiness_data)) {
    return(plot_ly() %>% 
             add_annotations(
               text = paste0(
                 "<b>Insufficient Data for Graduation Readiness Analysis</b><br><br>",
                 readiness_data$message[1], "<br><br>",
                 "📊 <b>Alternative Approaches:</b><br>",
                 "• Use the <b>Individual Assessment</b> tab for resident-specific insights<br>",
                 "• Consider longitudinal tracking as more residents complete evaluations<br>",
                 "• Focus on competency-specific progress rather than comparative analysis"
               ),
               x = 0.5, y = 0.5, 
               showarrow = FALSE,
               font = list(size = 14, color = "#2c3e50"),
               bgcolor = "rgba(255, 248, 230, 0.9)",
               bordercolor = "#ffc107",
               borderwidth = 2
             ) %>%
             layout(
               title = list(
                 text = "Graduation Readiness Analysis",
                 font = list(size = 16, color = "#495057")
               ),
               margin = list(t = 60, b = 40, l = 40, r = 40)
             ))
  }
  
  if (nrow(readiness_data) == 0) {
    return(plot_ly() %>% 
             add_annotations(text = "No graduation data available", 
                             x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Continue with your existing chart creation logic...
  colors <- c(
    "Excellent (<2.5%)" = "#4CAF50",
    "Good (2.5-5%)" = "#8BC34A",  
    "Concerning (5-7.5%)" = "#FF9800",
    "High Risk (>7.5%)" = "#F44336"
  )
  
  hover_text <- paste0(
    '<b>', readiness_data$Sub_Competency, '</b><br>',
    'Competency: ', readiness_data$Competency, '<br>',
    '<b>Below Threshold (', readiness_data$threshold_used, '):</b> ', 
    round(readiness_data$percent_below_threshold, 1), '%<br>',
    'Evaluations Below: ', readiness_data$below_threshold, ' / ', readiness_data$total_evaluations, '<br>',
    'Mean Score: ', round(readiness_data$mean_score, 2), '<br>',
    'Residents: ', readiness_data$total_residents,
    '<extra></extra>'
  )
  
  fig <- plot_ly(
    data = readiness_data,
    y = ~reorder(Sub_Competency, percent_below_threshold),
    x = ~percent_below_threshold,
    type = 'bar',
    orientation = 'h',
    color = ~readiness_category,
    colors = colors,
    hovertemplate = hover_text,
    text = ~paste0(round(percent_below_threshold, 1), "%"),
    textposition = "outside",
    textfont = list(size = 12, color = "black")
  )
  
  fig <- fig %>% layout(
    title = list(
      text = paste0("<b>Graduation Readiness - ", readiness_data$graduating_class[1], 
                    " (Threshold: ", readiness_data$threshold_used[1], ")</b>"),
      font = list(size = 18, family = "Arial", color = "#2c3e50")
    ),
    xaxis = list(
      title = "Percentage Below Threshold (%)",
      range = c(0, max(10, max(readiness_data$percent_below_threshold) * 1.1))
    ),
    yaxis = list(
      title = "Sub-Competency",
      tickfont = list(size = 11)
    ),
    legend = list(
      title = list(text = "Risk Level"),
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.3
    ),
    margin = list(l = 100, r = 50, t = 80, b = 100)
  )
  
  return(fig)
}

# Enhanced Trend Chart with Data Check
create_trend_chart_enhanced <- function(trend_data, show_total_average = TRUE) {
  
  # Check if this is an error/insufficient data response
  if (nrow(trend_data) == 1 && "message" %in% names(trend_data)) {
    return(plot_ly() %>% 
             add_annotations(
               text = paste0(
                 "<b>Insufficient Data for Trend Analysis</b><br><br>",
                 trend_data$message[1], "<br><br>",
                 "📊 <b>Alternative Approaches:</b><br>",
                 "• Use the <b>Individual Assessment</b> tab for resident progression<br>",
                 "• Focus on individual milestone tracking<br>",
                 "• Consider cross-sectional analysis instead of trends"
               ),
               x = 0.5, y = 0.5, 
               showarrow = FALSE,
               font = list(size = 14, color = "#2c3e50"),
               bgcolor = "rgba(248, 249, 250, 0.9)",
               bordercolor = "#dee2e6",
               borderwidth = 2
             ) %>%
             layout(
               title = list(
                 text = "Sub-Competency Trends Analysis",
                 font = list(size = 16, color = "#495057")
               ),
               margin = list(t = 60, b = 40, l = 40, r = 40)
             ))
  }
  
  if (nrow(trend_data) == 0) {
    return(plot_ly() %>% 
             add_annotations(text = "No trend data available", x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Continue with your existing chart creation logic...
  total_mean <- mean(trend_data$mean_score, na.rm = TRUE)
  
  hover_text <- paste0(
    '<b>', trend_data$Full_Period_Label, '</b><br>',
    'Training Stage: ', trend_data$Period_Label, '<br>',
    'Mean Score: ', round(trend_data$mean_score, 2), '<br>',
    'Period Average: ', round(trend_data$period_specific_mean, 2), '<br>',
    'Difference: ', 
    ifelse(trend_data$mean_score >= trend_data$period_specific_mean, '+', ''), 
    round(trend_data$mean_score - trend_data$period_specific_mean, 2), '<br>',
    'Residents: ', trend_data$total_residents,
    '<extra></extra>'
  )
  
  # Rest of your existing chart code...
  fig <- plot_ly(
    data = trend_data,
    x = ~Period_Order,
    y = ~mean_score,
    type = 'scatter',
    mode = 'lines+markers',
    name = paste(trend_data$sub_competency[1], 'Mean Score'),
    line = list(color = '#2E86AB', width = 4),
    marker = list(color = '#2E86AB', size = 12, symbol = 'circle'),
    hovertemplate = hover_text
  )
  
  # Add period-specific comparison line
  fig <- fig %>% add_trace(
    x = ~Period_Order,
    y = ~period_specific_mean,
    type = 'scatter',
    mode = 'lines+markers',
    name = 'Period Average (All Sub-Competencies)',
    line = list(color = '#A23B72', width = 3, dash = 'dash'),
    marker = list(color = '#A23B72', size = 10, symbol = 'diamond'),
    hovertemplate = paste0(
      '<b>Period Average</b><br>',
      'Period: ', trend_data$Full_Period_Label, '<br>',
      'Average Score: ', round(trend_data$period_specific_mean, 2), '<br>',
      'Across all sub-competencies',
      '<extra></extra>'
    )
  )
  
  if (show_total_average) {
    x_range <- range(trend_data$Period_Order)
    horizontal_line_data <- data.frame(
      x = c(x_range[1], x_range[2]),
      y = c(total_mean, total_mean)
    )
    
    fig <- fig %>% add_trace(
      data = horizontal_line_data,
      x = ~x,
      y = ~y,
      type = 'scatter',
      mode = 'lines',
      name = paste0("Total Average (", round(total_mean, 2), ")"),
      line = list(color = '#FF6B35', width = 2, dash = 'dot'),
      hovertemplate = paste0(
        '<b>Total Average</b><br>',
        'Mean across all periods: ', round(total_mean, 2),
        '<extra></extra>'
      )
    )
  }
  
  fig <- fig %>% layout(
    title = list(
      text = paste("Progression Trends:", trend_data$sub_competency[1]),
      font = list(size = 18, family = "Arial, sans-serif", color = "#2c3e50"),
      x = 0.5
    ),
    xaxis = list(
      title = list(
        text = "Training Period",
        font = list(size = 14, family = "Arial, sans-serif", color = "#34495e")
      ),
      tickvals = trend_data$Period_Order,
      ticktext = trend_data$Period_Label,
      tickangle = -45,
      tickfont = list(size = 12, color = "#34495e", family = "Arial, sans-serif"),
      gridcolor = 'rgba(52, 73, 94, 0.15)',
      linecolor = 'rgba(52, 73, 94, 0.2)',
      zeroline = FALSE
    ),
    yaxis = list(
      title = list(
        text = "Mean Milestone Score",
        font = list(size = 14, family = "Arial, sans-serif", color = "#34495e")
      ),
      range = c(1, 9),
      tickmode = 'linear',
      tick0 = 1,
      dtick = 1,
      tickfont = list(size = 12, color = "#34495e", family = "Arial, sans-serif"),
      gridcolor = 'rgba(52, 73, 94, 0.15)',
      linecolor = 'rgba(52, 73, 94, 0.2)',
      zeroline = FALSE
    ),
    showlegend = TRUE,
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.25,
      font = list(size = 13, family = "Arial, sans-serif", color = "#2c3e50")
    ),
    paper_bgcolor = 'white',
    plot_bgcolor = 'rgba(248, 249, 250, 0.8)',
    margin = list(t = 120, b = 120, l = 80, r = 80),
    hovermode = 'x unified'
  ) %>%
    plotly::config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c('pan2d', 'select2d', 'lasso2d', 'autoScale2d')
    )
  
  return(fig)
}

# Enhanced All Levels Chart with Data Check
create_all_levels_chart <- function(readiness_data) {
  
  # Check if this is an error/insufficient data response
  if (nrow(readiness_data) == 1 && "message" %in% names(readiness_data)) {
    return(plot_ly() %>% 
             add_annotations(
               text = paste0(
                 "<b>Insufficient Data for All Levels Analysis</b><br><br>",
                 readiness_data$message[1], "<br><br>",
                 "📊 <b>Alternative Approaches:</b><br>",
                 "• Use the <b>Individual Assessment</b> tab for resident-specific insights<br>",
                 "• Focus on individual milestone tracking<br>",
                 "• Consider program-wide analysis when more data is available"
               ),
               x = 0.5, y = 0.5, 
               showarrow = FALSE,
               font = list(size = 14, color = "#2c3e50"),
               bgcolor = "rgba(248, 249, 250, 0.9)",
               bordercolor = "#dee2e6",
               borderwidth = 2
             ) %>%
             layout(
               title = list(
                 text = "All Levels Analysis",
                 font = list(size = 16, color = "#495057")
               ),
               margin = list(t = 60, b = 40, l = 40, r = 40)
             ))
  }
  
  if (nrow(readiness_data) == 0) {
    return(plot_ly() %>% 
             add_annotations(text = "No data available", x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Continue with your existing chart creation logic...
  colors <- c(
    "Excellent (<2.5%)" = "#4CAF50",
    "Good (2.5-5%)" = "#8BC34A",  
    "Concerning (5-7.5%)" = "#FF9800",
    "High Risk (>7.5%)" = "#F44336"
  )
  
  fig <- plot_ly(readiness_data)
  
  pgy_levels <- unique(readiness_data$PGY_Level)
  
  for (i in seq_along(pgy_levels)) {
    level_data <- readiness_data %>% filter(PGY_Level == pgy_levels[i])
    
    fig <- fig %>% add_trace(
      x = ~competency_pgy,
      y = ~percent_below_threshold,
      name = pgy_levels[i],
      type = 'bar',
      data = level_data,
      marker = list(
        color = colors[level_data$readiness_category],
        line = list(color = 'white', width = 1)
      ),
      hovertemplate = paste0(
        '<b>', level_data$Sub_Competency, ' - ', level_data$PGY_Level, '</b><br>',
        'Below Threshold: ', round(level_data$percent_below_threshold, 1), '%<br>',
        'Mean Score: ', round(level_data$mean_score, 2), '<br>',
        'Risk Level: ', level_data$readiness_category,
        '<extra></extra>'
      )
    )
  }
  
  fig <- fig %>% layout(
    title = list(
      text = paste0("<b>All Training Levels - Readiness Analysis</b><br>",
                    "<i>Threshold: ", readiness_data$threshold_used[1], "</i>"),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Sub-Competency by PGY Level",
      tickangle = -45
    ),
    yaxis = list(
      title = "Percentage Below Threshold (%)"
    ),
    barmode = 'group',
    legend = list(
      title = list(text = "PGY Level")
    ),
    margin = list(b = 150)
  )
  
  return(fig)
}

# Enhanced Cohort Trend Analysis with Data Check
create_cohort_trend_analysis <- function(data, selected_sub_competency, selected_cohorts = NULL) {
  
  # Check for minimum data
  unique_residents <- n_distinct(data$evaluations$Resident_Name)
  
  if (unique_residents < 3) {
    return(plot_ly() %>% 
             add_annotations(
               text = paste0(
                 "<b>Insufficient Data for Cohort Trend Analysis</b><br><br>",
                 "Current: ", unique_residents, " residents<br>",
                 "Required: Minimum 3 residents for meaningful cohort comparisons<br><br>",
                 "📊 <b>Alternative Approaches:</b><br>",
                 "• Use the <b>Individual Assessment</b> tab for resident progression<br>",
                 "• Focus on individual milestone tracking<br>",
                 "• Consider longitudinal analysis as more data becomes available"
               ),
               x = 0.5, y = 0.5, 
               showarrow = FALSE,
               font = list(size = 14, color = "#2c3e50"),
               bgcolor = "rgba(248, 249, 250, 0.9)",
               bordercolor = "#dee2e6",
               borderwidth = 2
             ) %>%
             layout(
               title = list(
                 text = "Cohort Trend Analysis",
                 font = list(size = 16, color = "#495057")
               ),
               margin = list(t = 60, b = 40, l = 40, r = 40)
             ))
  }
  
  # Calculate cohort information for all data
  cohort_data <- calculate_cohort_information(data)
  
  # Filter for selected sub-competency
  cohort_data <- cohort_data %>%
    filter(Sub_Competency == selected_sub_competency)
  
  if (nrow(cohort_data) == 0) {
    return(plot_ly() %>%
             add_annotations(
               text = paste0(
                 "<b>No Data for ", selected_sub_competency, "</b><br><br>",
                 "This sub-competency has no evaluation data<br><br>",
                 "📊 <b>Try:</b><br>",
                 "• Selecting a different sub-competency<br>",
                 "• Checking data upload completeness<br>",
                 "• Verifying sub-competency naming"
               ),
               x = 0.5, y = 0.5, showarrow = FALSE,
               font = list(size = 14, color = "#2c3e50")
             ))
  }
  
  # Continue with your existing cohort analysis logic...
  all_program_data <- cohort_data %>%
    group_by(period_level, period_order, PGY_Level) %>%
    summarise(
      program_mean = mean(Rating, na.rm = TRUE),
      program_n = n(),
      .groups = "drop"
    ) %>%
    filter(program_n >= 3) %>%
    arrange(period_order)
  
  ordered_periods <- all_program_data$period_level[order(all_program_data$period_order)]
  all_program_data$period_level <- factor(all_program_data$period_level, levels = ordered_periods, ordered = TRUE)
  
  fig <- plot_ly()
  
  fig <- fig %>%
    add_trace(
      data = all_program_data,
      x = ~period_level,
      y = ~program_mean,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Program Average (All Years)',
      line = list(color = '#2C3E50', width = 3),
      marker = list(color = '#2C3E50', size = 8),
      hovertemplate = paste0(
        '<b>Program Average</b><br>',
        'Period: %{x}<br>',
        'Mean Score: %{y:.2f}<br>',
        'Total Evaluations: ', all_program_data$program_n, '<br>',
        '<extra></extra>'
      )
    )
  
  # Add cohort lines only if cohorts are selected
  if (!is.null(selected_cohorts) && length(selected_cohorts) > 0) {
    
    cohort_subset <- cohort_data %>%
      filter(cohort_label %in% selected_cohorts)
    
    cohort_means <- cohort_subset %>%
      group_by(cohort_label, period_level, period_order, PGY_Level) %>%
      summarise(
        cohort_mean = mean(Rating, na.rm = TRUE),
        cohort_n = n(),
        .groups = "drop"
      ) %>%
      filter(cohort_n >= 2) %>%
      arrange(period_order)
    
    cohort_means$period_level <- factor(cohort_means$period_level, levels = ordered_periods, ordered = TRUE)
    
    cohort_colors <- c('#E74C3C', '#3498DB', '#2ECC71', '#F39C12', '#9B59B6', '#E67E22')
    
    for (i in seq_along(selected_cohorts)) {
      cohort <- selected_cohorts[i]
      cohort_data_subset <- cohort_means %>% filter(cohort_label == cohort)
      
      if (nrow(cohort_data_subset) > 0) {
        fig <- fig %>%
          add_trace(
            data = cohort_data_subset,
            x = ~period_level,
            y = ~cohort_mean,
            type = 'scatter',
            mode = 'lines+markers',
            name = cohort,
            line = list(color = cohort_colors[i], width = 2.5),
            marker = list(color = cohort_colors[i], size = 7),
            hovertemplate = paste0(
              '<b>', cohort, '</b><br>',
              'Period: %{x}<br>',
              'Mean Score: %{y:.2f}<br>',
              'N: ', cohort_data_subset$cohort_n, '<br>',
              '<extra></extra>'
            )
          )
      }
    }
  }
  
  fig <- fig %>%
    layout(
      title = list(
        text = paste0("<b>Training Progression: ", selected_sub_competency, "</b>"),
        font = list(size = 16)
      ),
      xaxis = list(
        title = "Training Period & Level",
        tickangle = -45,
        categoryorder = "array",
        categoryarray = ordered_periods
      ),
      yaxis = list(
        title = "Mean Milestone Score",
        range = c(1, 9)
      ),
      hovermode = 'closest',
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = 'center',
        y = -0.3
      )
    )
  
  return(fig)
}

#' Calculate Cohort Information
#' 
#' Determines graduation years and cohorts based on current PGY level and academic year
#' 
#' @param data Processed data from load_milestone_csv_data()
#' @return Data frame with cohort information
calculate_cohort_information <- function(data) {
  
  evaluation_data <- data$evaluations
  
  # Extract program length from data (highest PGY level)
  max_pgy <- evaluation_data %>%
    mutate(pgy_num = as.numeric(str_extract(PGY_Level, "\\d+"))) %>%
    pull(pgy_num) %>%
    max(na.rm = TRUE)
  
  # Calculate cohorts
  cohort_data <- evaluation_data %>%
    mutate(
      # Extract academic year and PGY number
      academic_year = str_extract(Period, "^\\d{4}-\\d{4}"),
      pgy_num = as.numeric(str_extract(PGY_Level, "\\d+")),
      start_year = as.numeric(str_extract(academic_year, "^\\d{4}"))
    ) %>%
    filter(!is.na(academic_year), !is.na(pgy_num)) %>%
    mutate(
      # Calculate graduation year: start_year + (max_pgy - current_pgy)
      graduation_year = start_year + (max_pgy - pgy_num),
      cohort_label = paste0("Class of ", graduation_year + 1),  # +1 because they graduate in spring
      
      # Create period-level identifier for tracking progression
      period_level = paste(str_extract(Period, "(Mid-Year|Year-End)"), PGY_Level),
      
      # Calculate what "year" this is in their training (1st year, 2nd year, etc.)
      training_year = pgy_num,
      
      # Create a standardized period order for plotting
      period_order = case_when(
        str_detect(Period, "Mid-Year") ~ (pgy_num - 1) * 2 + 1,
        str_detect(Period, "Year-End") ~ (pgy_num - 1) * 2 + 2,
        TRUE ~ 999
      )
    ) %>%
    select(Resident_Name, academic_year, PGY_Level, pgy_num, graduation_year, 
           cohort_label, period_level, training_year, period_order, Period, 
           Sub_Competency, Rating, Competency)
  
  return(cohort_data)
}