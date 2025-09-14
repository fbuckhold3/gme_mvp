# ============================================================================
# R/acgme_visualization_functions.R
# ACGME visualization functions for gmed-mvp
# ============================================================================

#' Create Dynamic Spider Plot
#'
#' Creates spider plot using the dynamically discovered competency structure
#'
#' @param processed_data Output from transform_acgme_data_dynamic()
#' @param period_filter Selected period
#' @param selected_years Vector of years to include
#' @param show_national Whether to show national medians (if available)
#' @return Plotly spider plot
#' @export
create_dynamic_spider_plot <- function(processed_data, period_filter = "Total", 
                                       selected_years = NULL, show_national = FALSE) {
  
  library(plotly)
  library(dplyr)
  library(stringr)
  
  milestone_data <- processed_data$milestone_data_long
  structure_info <- processed_data$structure_info
  
  # Use all available years if none specified
  if (is.null(selected_years)) {
    selected_years <- as.character(structure_info$resident_years)
  }
  
  # Filter data by period and years
  filtered_data <- milestone_data
  if (period_filter != "Total") {
    filtered_data <- filtered_data %>% 
      filter(prog_mile_period == period_filter)
  }
  filtered_data <- filtered_data %>%
    filter(Level %in% selected_years)
  
  # Calculate competency averages by resident year
  competency_averages <- filtered_data %>%
    group_by(Level, competency_code, competency_name) %>%
    summarise(
      median_score = median(score, na.rm = TRUE),
      mean_score = mean(score, na.rm = TRUE),
      n_residents = n_distinct(record_id),
      .groups = "drop"
    ) %>%
    # Create display-friendly competency names
    mutate(
      display_name = case_when(
        competency_code == "PC" ~ "Patient Care",
        competency_code == "MK" ~ "Medical Knowledge", 
        competency_code == "SBP" ~ "Systems-Based Practice",
        competency_code == "PBLI" ~ "Practice-Based Learning",
        competency_code == "PROF" ~ "Professionalism",
        competency_code == "ICS" ~ "Communication Skills",
        TRUE ~ competency_name
      )
    )
  
  # Create spider plot
  fig <- plot_ly(type = 'scatterpolar', mode = 'lines+markers')
  
  # Define colors for each year
  year_colors <- c("#e74c3c", "#f39c12", "#27ae60", "#9b59b6", "#3498db", "#1abc9c")
  names(year_colors) <- sort(unique(filtered_data$Level))
  
  # Add trace for each resident year
  for (year in selected_years) {
    year_data <- competency_averages %>% filter(Level == year)
    
    if (nrow(year_data) > 0) {
      fig <- fig %>%
        add_trace(
          r = year_data$median_score,
          theta = year_data$display_name,
          name = paste("PGY", year),
          line = list(color = year_colors[[year]], width = 3),
          marker = list(size = 8, color = year_colors[[year]]),
          text = paste("PGY", year, "<br>",
                       year_data$display_name, "<br>",
                       "Median:", round(year_data$median_score, 1), "<br>",
                       "Residents:", year_data$n_residents),
          hovertemplate = "%{text}<extra></extra>"
        )
    }
  }
  
  # Configure layout
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
      title = list(
        text = paste("Program Performance Spider Plot -", period_filter),
        font = list(size = 16)
      ),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
      margin = list(t = 80, b = 120, l = 80, r = 80)
    )
  
  return(fig)
}

#' Create Dynamic Performance Tables
#'
#' Creates performance tables using the discovered competency structure
#'
#' @param processed_data Output from transform_acgme_data_dynamic()
#' @param period_filter Selected period
#' @return List with strengths and improvement tables
#' @export
create_dynamic_performance_tables <- function(processed_data, period_filter = "Total") {
  
  library(dplyr)
  
  milestone_data <- processed_data$milestone_data_long
  
  # Filter by period
  if (period_filter != "Total") {
    milestone_data <- milestone_data %>% 
      filter(prog_mile_period == period_filter)
  }
  
  # Calculate competency-level statistics
  competency_stats <- milestone_data %>%
    group_by(competency_code, competency_name) %>%
    summarise(
      median_score = median(score, na.rm = TRUE),
      mean_score = mean(score, na.rm = TRUE),
      n_evaluations = n(),
      n_residents = n_distinct(record_id),
      below_4 = sum(score < 4, na.rm = TRUE),
      above_6 = sum(score >= 6, na.rm = TRUE),
      below_4_pct = round(mean(score < 4, na.rm = TRUE) * 100, 1),
      above_6_pct = round(mean(score >= 6, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(median_score))
  
  # Create strengths table (median >= 5.5)
  strengths <- competency_stats %>%
    filter(median_score >= 5.5) %>%
    select(competency_name, median_score, above_6_pct, n_residents) %>%
    rename(
      Competency = competency_name,
      `Median Score` = median_score,
      `Above Target %` = above_6_pct,
      `Residents` = n_residents
    ) %>%
    mutate(`Median Score` = round(`Median Score`, 1))
  
  # Create improvement areas table (median < 4.5)
  improvements <- competency_stats %>%
    filter(median_score < 4.5) %>%
    select(competency_name, median_score, below_4_pct, n_residents) %>%
    rename(
      Competency = competency_name,
      `Median Score` = median_score,
      `Below Benchmark %` = below_4_pct,
      `Residents` = n_residents
    ) %>%
    mutate(`Median Score` = round(`Median Score`, 1))
  
  # Sub-competency details
  subcompetency_details <- milestone_data %>%
    group_by(competency_name, milestone_id, sub_competency_title) %>%
    summarise(
      median_score = median(score, na.rm = TRUE),
      n_evaluations = n(),
      .groups = "drop"
    ) %>%
    arrange(competency_name, milestone_id) %>%
    mutate(median_score = round(median_score, 1)) %>%
    rename(
      Competency = competency_name,
      `Sub-competency` = milestone_id,
      Description = sub_competency_title,
      `Median Score` = median_score,
      `Evaluations` = n_evaluations
    )
  
  return(list(
    strengths = strengths,
    improvements = improvements,
    all_stats = competency_stats,
    subcompetency_details = subcompetency_details
  ))
}

#' Create Dynamic Heat Map
#'
#' Creates heat map using the discovered milestone structure
#'
#' @param processed_data Output from transform_acgme_data_dynamic()
#' @param period_filter Selected period
#' @param metric Metric to display ("median", "mean", "below_benchmark", "above_target")
#' @return Plotly heat map
#' @export
create_dynamic_heatmap <- function(processed_data, period_filter = "Total", metric = "median") {
  
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  milestone_data <- processed_data$milestone_data_long
  
  # Filter by period
  if (period_filter != "Total") {
    milestone_data <- milestone_data %>% 
      filter(prog_mile_period == period_filter)
  }
  
  # Calculate metrics by milestone and year
  heatmap_data <- milestone_data %>%
    group_by(Level, competency_name, milestone_id, sub_competency_title) %>%
    summarise(
      median_score = median(score, na.rm = TRUE),
      mean_score = mean(score, na.rm = TRUE),
      below_benchmark = mean(score < 4, na.rm = TRUE) * 100,
      above_target = mean(score >= 6, na.rm = TRUE) * 100,
      n_evaluations = n(),
      .groups = "drop"
    ) %>%
    # Create display names
    mutate(
      milestone_display = paste0(competency_name, " - ", milestone_id),
      year_display = paste0("PGY-", Level)
    )
  
  # Select the metric to display
  metric_col <- paste0(metric, "_score")
  if (!metric_col %in% names(heatmap_data)) {
    metric_col <- metric
  }
  
  # Create matrix for heatmap
  heatmap_matrix <- heatmap_data %>%
    select(milestone_display, year_display, all_of(metric_col)) %>%
    pivot_wider(
      names_from = year_display, 
      values_from = all_of(metric_col), 
      values_fill = NA
    ) %>%
    column_to_rownames("milestone_display") %>%
    as.matrix()
  
  # Determine color scale based on metric
  if (metric %in% c("median", "mean")) {
    color_scale <- list(
      c(0, "#d32f2f"),     # Red for low scores
      c(0.4, "#fff3e0"),   # Light orange
      c(0.6, "#e8f5e8"),   # Light green  
      c(1, "#2e7d32")      # Dark green for high scores
    )
    zmin <- 1
    zmax <- 9
  } else {
    color_scale <- list(
      c(0, "#2e7d32"),     # Green for low percentages (good)
      c(0.5, "#fff3e0"),   # Light orange
      c(1, "#d32f2f")      # Red for high percentages (concerning)
    )
    zmin <- 0
    zmax <- 100
  }
  
  # Create plotly heatmap
  fig <- plot_ly(
    z = heatmap_matrix,
    x = colnames(heatmap_matrix),
    y = rownames(heatmap_matrix),
    type = "heatmap",
    colorscale = color_scale,
    zmin = zmin, zmax = zmax,
    hoverongaps = FALSE,
    text = round(heatmap_matrix, 1),
    texttemplate = "%{text}",
    textfont = list(size = 10, color = "black")
  ) %>%
    layout(
      title = paste("Milestone Performance Heat Map -", str_to_title(gsub("_", " ", metric))),
      xaxis = list(title = "Resident Year", tickfont = list(size = 12)),
      yaxis = list(title = "Competency - Milestone", tickfont = list(size = 9)),
      margin = list(l = 300, r = 50, t = 80, b = 50)
    )
  
  return(fig)
}