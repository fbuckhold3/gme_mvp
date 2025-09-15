# =============================================================================
# PROGRAM-LEVEL VISUALIZATION FUNCTIONS
# R/program_visualization.R
# =============================================================================

#' Create Strengths Table
#'
#' Identifies and displays top-performing competencies
#'
#' @param data Processed data list
#' @param period_filter Selected period
#' @param level_filter Selected level
#' @return Data table showing strengths
create_strengths_table <- function(data, period_filter = "total", level_filter = "all") {
  
  filtered_data <- filter_data(data$milestone_data, period_filter, level_filter)
  
  if (nrow(filtered_data) == 0) {
    return(data.frame(Competency = "No data", `Median Score` = "", 
                      `Above Target %` = "", check.names = FALSE))
  }
  
  strengths <- filtered_data %>%
    group_by(Competency) %>%
    summarise(
      median_score = median(Score, na.rm = TRUE),
      above_target_pct = mean(Score >= 6, na.rm = TRUE) * 100,
      n_evaluations = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(median_score), desc(above_target_pct)) %>%
    head(5) %>%
    mutate(
      `Median Score` = round(median_score, 1),
      `Above Target %` = paste0(round(above_target_pct, 1), "%"),
      Evaluations = n_evaluations
    ) %>%
    select(Competency, `Median Score`, `Above Target %`, Evaluations)
  
  return(strengths)
}

#' Create Improvements Table
#'
#' Identifies competencies needing improvement
#'
#' @param data Processed data list
#' @param period_filter Selected period
#' @param level_filter Selected level
#' @return Data table showing areas for improvement
create_improvements_table <- function(data, period_filter = "total", level_filter = "all") {
  
  filtered_data <- filter_data(data$milestone_data, period_filter, level_filter)
  
  if (nrow(filtered_data) == 0) {
    return(data.frame(Competency = "No data", `Median Score` = "", 
                      `Below Benchmark %` = "", check.names = FALSE))
  }
  
  improvements <- filtered_data %>%
    group_by(Competency) %>%
    summarise(
      median_score = median(Score, na.rm = TRUE),
      below_benchmark_pct = mean(Score < 4, na.rm = TRUE) * 100,
      n_evaluations = n(),
      .groups = "drop"
    ) %>%
    arrange(median_score, desc(below_benchmark_pct)) %>%
    head(5) %>%
    mutate(
      `Median Score` = round(median_score, 1),
      `Below Benchmark %` = paste0(round(below_benchmark_pct, 1), "%"),
      Evaluations = n_evaluations
    ) %>%
    select(Competency, `Median Score`, `Below Benchmark %`, Evaluations)
  
  return(improvements)
}

#' Create Program Spider Plot
#'
#' Creates a spider/radar plot showing competency performance
#'
#' @param data Processed data list
#' @param period_filter Selected period
#' @param level_filter Selected level
#' @return Plotly spider plot
create_program_spider_plot <- function(data, period_filter = "total", level_filter = "all") {
  
  filtered_data <- filter_data(data$milestone_data, period_filter, level_filter)
  
  if (nrow(filtered_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "No data available", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Calculate competency medians
  spider_data <- filtered_data %>%
    group_by(Competency) %>%
    summarise(median_score = median(Score, na.rm = TRUE), .groups = "drop") %>%
    arrange(Competency)
  
  if (nrow(spider_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "No competency data", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Prepare data for fmsb radar chart
  max_score <- 9
  min_score <- 1
  
  # Create matrix for fmsb
  spider_matrix <- rbind(
    rep(max_score, nrow(spider_data)),  # Max values
    rep(min_score, nrow(spider_data)),  # Min values
    spider_data$median_score            # Actual values
  )
  colnames(spider_matrix) <- spider_data$Competency
  
  # Convert to data frame
  spider_df <- as.data.frame(spider_matrix)
  
  # Create radar chart using plotly
  theta <- seq(0, 2*pi, length.out = nrow(spider_data) + 1)[1:nrow(spider_data)]
  
  plot_ly(
    type = 'scatterpolar',
    r = spider_data$median_score,
    theta = spider_data$Competency,
    fill = 'toself',
    fillcolor = 'rgba(52, 152, 219, 0.3)',
    line = list(color = 'rgb(52, 152, 219)', width = 3),
    marker = list(size = 8, color = 'rgb(52, 152, 219)'),
    hovertemplate = '<b>%{theta}</b><br>Median Score: %{r}<extra></extra>'
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
      title = paste("Program Competency Profile -", 
                    ifelse(period_filter == "total", "All Periods", period_filter)),
      showlegend = FALSE
    )
}

#' Create Competency Trends Plot
#'
#' Shows trends across resident levels or time periods
#'
#' @param data Processed data list
#' @param level_filter Selected level
#' @return Plotly line plot
create_competency_trends <- function(data, level_filter = "all") {
  
  milestone_data <- data$milestone_data
  
  if (level_filter != "all") {
    milestone_data <- milestone_data %>%
      filter(Resident_Year == level_filter)
  }
  
  if (nrow(milestone_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "No data available", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Calculate trends by resident year
  trend_data <- milestone_data %>%
    group_by(Competency, Resident_Year) %>%
    summarise(
      median_score = median(Score, na.rm = TRUE),
      n_evaluations = n(),
      .groups = "drop"
    ) %>%
    filter(n_evaluations >= 3)  # Only include groups with sufficient data
  
  if (nrow(trend_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "Insufficient data for trends", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Create line plot
  plot_ly(trend_data, x = ~Resident_Year, y = ~median_score, 
          color = ~Competency, type = 'scatter', mode = 'lines+markers',
          hovertemplate = '<b>%{data.name}</b><br>Year: %{x}<br>Median Score: %{y}<extra></extra>') %>%
    layout(
      title = "Competency Progression by Resident Year",
      xaxis = list(title = "Resident Year"),
      yaxis = list(title = "Median Score", range = c(1, 9)),
      hovermode = 'closest'
    )
}

#' Create Milestone Heatmap
#'
#' Creates a heatmap showing performance across milestones
#'
#' @param data Processed data list
#' @param period_filter Selected period
#' @param level_filter Selected level  
#' @param metric Metric to display
#' @return Plotly heatmap
create_milestone_heatmap <- function(data, period_filter = "total", level_filter = "all", metric = "median") {
  
  filtered_data <- filter_data(data$milestone_data, period_filter, level_filter)
  
  if (nrow(filtered_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "No data available", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Calculate metrics by competency and milestone
  heatmap_data <- filtered_data %>%
    group_by(Competency, Milestone_ID) %>%
    summarise(
      median_score = median(Score, na.rm = TRUE),
      mean_score = mean(Score, na.rm = TRUE),
      below_benchmark = mean(Score < 4, na.rm = TRUE) * 100,
      above_target = mean(Score >= 6, na.rm = TRUE) * 100,
      n_evaluations = n(),
      .groups = "drop"
    ) %>%
    filter(n_evaluations >= 3)  # Filter out sparse data
  
  if (nrow(heatmap_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "Insufficient data for heatmap", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Select metric
  metric_col <- switch(metric,
                       "median" = "median_score",
                       "below_benchmark" = "below_benchmark", 
                       "above_target" = "above_target",
                       "median_score"
  )
  
  # Create matrix for heatmap
  heatmap_matrix <- heatmap_data %>%
    select(Competency, Milestone_ID, all_of(metric_col)) %>%
    pivot_wider(names_from = Milestone_ID, values_from = all_of(metric_col), values_fill = NA)
  
  competencies <- heatmap_matrix$Competency
  heatmap_matrix <- heatmap_matrix %>% select(-Competency)
  milestones <- colnames(heatmap_matrix)
  
  # Create heatmap
  plot_ly(
    z = as.matrix(heatmap_matrix),
    x = milestones,
    y = competencies,
    type = "heatmap",
    colorscale = list(
      c(0, "red"),
      c(0.5, "yellow"), 
      c(1, "green")
    ),
    hovertemplate = '<b>%{y}</b><br>%{x}<br>Value: %{z}<extra></extra>'
  ) %>%
    layout(
      title = paste("Milestone Performance Heatmap -", 
                    switch(metric,
                           "median" = "Median Scores",
                           "below_benchmark" = "Below Benchmark %",
                           "above_target" = "Above Target %")),
      xaxis = list(title = "Milestones", tickangle = -45),
      yaxis = list(title = "Competencies")
    )
}