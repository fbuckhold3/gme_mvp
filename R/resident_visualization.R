# =============================================================================
# INDIVIDUAL RESIDENT VISUALIZATION FUNCTIONS
# R/resident_visualization.R
# =============================================================================

#' Create Resident Spider Plot
#'
#' Creates a spider plot comparing individual resident to program medians
#'
#' @param data Processed data list
#' @param resident_id Selected resident ID
#' @param period_filter Selected period
#' @return Plotly spider plot
create_resident_spider_plot <- function(data, resident_id, period_filter = "total") {
  
  # Get resident data
  resident_data <- data$milestone_data %>%
    filter(Resident_ID == resident_id)
  
  if (period_filter != "total") {
    resident_data <- resident_data %>%
      filter(Period == period_filter)
  }
  
  if (nrow(resident_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "No data available for this resident", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Calculate resident competency scores
  resident_scores <- resident_data %>%
    group_by(Competency) %>%
    summarise(resident_score = median(Score, na.rm = TRUE), .groups = "drop")
  
  # Calculate program medians (for comparison)
  program_data <- data$milestone_data
  if (period_filter != "total") {
    program_data <- program_data %>%
      filter(Period == period_filter)
  }
  
  program_medians <- program_data %>%
    group_by(Competency) %>%
    summarise(program_median = median(Score, na.rm = TRUE), .groups = "drop")
  
  # Combine data
  spider_data <- resident_scores %>%
    left_join(program_medians, by = "Competency") %>%
    arrange(Competency)
  
  if (nrow(spider_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "No competency data", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Create spider plot with both resident and program data
  plot_ly() %>%
    add_trace(
      type = 'scatterpolar',
      r = spider_data$program_median,
      theta = spider_data$Competency,
      fill = 'toself',
      fillcolor = 'rgba(169, 169, 169, 0.2)',
      line = list(color = 'rgba(169, 169, 169, 0.8)', width = 2, dash = 'dash'),
      name = 'Program Median',
      hovertemplate = '<b>%{theta}</b><br>Program Median: %{r}<extra></extra>'
    ) %>%
    add_trace(
      type = 'scatterpolar', 
      r = spider_data$resident_score,
      theta = spider_data$Competency,
      fill = 'toself',
      fillcolor = 'rgba(52, 152, 219, 0.3)',
      line = list(color = 'rgb(52, 152, 219)', width = 3),
      marker = list(size = 8, color = 'rgb(52, 152, 219)'),
      name = 'Resident',
      hovertemplate = '<b>%{theta}</b><br>Resident Score: %{r}<extra></extra>'
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
      title = "Individual vs Program Competency Profile",
      showlegend = TRUE,
      legend = list(x = 0.8, y = 0.1)
    )
}

#' Create Milestone Progression Plot
#'
#' Shows progression over time for a specific milestone
#'
#' @param data Processed data list
#' @param resident_id Selected resident ID
#' @param milestone_id Selected milestone
#' @return Plotly line plot
create_milestone_progression_plot <- function(data, resident_id, milestone_id) {
  
  # Get resident milestone data over time
  resident_data <- data$milestone_data %>%
    filter(Resident_ID == resident_id, Milestone_ID == milestone_id) %>%
    arrange(Period)
  
  if (nrow(resident_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(text = "No data available for this milestone", 
                                     x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Get program benchmark for this milestone
  program_benchmark <- data$milestone_data %>%
    filter(Milestone_ID == milestone_id) %>%
    summarise(benchmark = median(Score, na.rm = TRUE)) %>%
    pull(benchmark)
  
  # Create progression plot
  plot_ly(resident_data, x = ~Period, y = ~Score, type = 'scatter', mode = 'lines+markers',
          line = list(color = 'rgb(52, 152, 219)', width = 3),
          marker = list(size = 10, color = 'rgb(52, 152, 219)'),
          hovertemplate = '<b>%{x}</b><br>Score: %{y}<extra></extra>') %>%
    add_hline(y = program_benchmark, line = list(color = 'red', dash = 'dash', width = 2),
              name = paste("Program Median:", round(program_benchmark, 1))) %>%
    add_hline(y = 4, line = list(color = 'orange', dash = 'dot', width = 2),
              name = "Benchmark (4.0)") %>%
    add_hline(y = 6, line = list(color = 'green', dash = 'dot', width = 2),
              name = "Target (6.0)") %>%
    layout(
      title = paste("Milestone Progression:", milestone_id),
      xaxis = list(title = "Assessment Period", tickangle = -45),
      yaxis = list(title = "Score", range = c(1, 9)),
      hovermode = 'x unified'
    )
}

#' Create Resident Scores Table
#'
#' Creates a detailed table of all milestone scores for a resident
#'
#' @param data Processed data list
#' @param resident_id Selected resident ID
#' @param period_filter Selected period
#' @return Data frame for display in DataTable
create_resident_scores_table <- function(data, resident_id, period_filter = "total") {
  
  resident_data <- data$milestone_data %>%
    filter(Resident_ID == resident_id)
  
  if (period_filter != "total") {
    resident_data <- resident_data %>%
      filter(Period == period_filter)
  }
  
  if (nrow(resident_data) == 0) {
    return(data.frame(Competency = "No data", Milestone = "", Score = "", 
                      Period = "", Description = "", check.names = FALSE))
  }
  
  # Create detailed scores table
  scores_table <- resident_data %>%
    select(Competency, Milestone_ID, Score, Period, Question_Text) %>%
    arrange(Competency, Milestone_ID, Period) %>%
    mutate(
      Performance = case_when(
        Score < 4 ~ "Below Benchmark",
        Score < 6 ~ "At Benchmark", 
        TRUE ~ "Above Target"
      )
    ) %>%
    rename(
      Milestone = Milestone_ID,
      Description = Question_Text
    ) %>%
    select(Competency, Milestone, Score, Performance, Period, Description)
  
  return(scores_table)
}

#' Create Resident Summary Stats
#'
#' Calculates summary statistics for a resident
#'
#' @param data Processed data list
#' @param resident_id Selected resident ID
#' @param period_filter Selected period
#' @return List with summary statistics
create_resident_summary <- function(data, resident_id, period_filter = "total") {
  
  resident_data <- data$milestone_data %>%
    filter(Resident_ID == resident_id)
  
  if (period_filter != "total") {
    resident_data <- resident_data %>%
      filter(Period == period_filter)
  }
  
  if (nrow(resident_data) == 0) {
    return(list(
      total_evaluations = 0,
      overall_median = NA,
      below_benchmark_pct = 0,
      above_target_pct = 0,
      competencies_evaluated = 0
    ))
  }
  
  list(
    total_evaluations = nrow(resident_data),
    overall_median = median(resident_data$Score, na.rm = TRUE),
    below_benchmark_pct = mean(resident_data$Score < 4, na.rm = TRUE) * 100,
    at_benchmark_pct = mean(resident_data$Score >= 4 & resident_data$Score < 6, na.rm = TRUE) * 100,
    above_target_pct = mean(resident_data$Score >= 6, na.rm = TRUE) * 100,
    competencies_evaluated = length(unique(resident_data$Competency)),
    milestones_evaluated = length(unique(resident_data$Milestone_ID)),
    assessment_periods = length(unique(resident_data$Period))
  )
}

#' Compare Resident to Cohort
#'
#' Compares resident performance to their training level cohort
#'
#' @param data Processed data list
#' @param resident_id Selected resident ID
#' @return Data frame with comparison statistics
compare_resident_to_cohort <- function(data, resident_id) {
  
  # Get resident info
  resident_info <- data$residents %>%
    filter(Resident_ID == resident_id)
  
  if (nrow(resident_info) == 0) {
    return(data.frame(Competency = "No data", check.names = FALSE))
  }
  
  resident_year <- resident_info$Resident_Year[1]
  
  # Get resident scores
  resident_scores <- data$milestone_data %>%
    filter(Resident_ID == resident_id) %>%
    group_by(Competency) %>%
    summarise(resident_median = median(Score, na.rm = TRUE), .groups = "drop")
  
  # Get cohort scores (same training year)
  cohort_scores <- data$milestone_data %>%
    filter(Resident_Year == resident_year, Resident_ID != resident_id) %>%
    group_by(Competency) %>%
    summarise(
      cohort_median = median(Score, na.rm = TRUE),
      cohort_n = n_distinct(Resident_ID),
      .groups = "drop"
    )
  
  # Combine and calculate differences
  comparison <- resident_scores %>%
    left_join(cohort_scores, by = "Competency") %>%
    mutate(
      difference = resident_median - cohort_median,
      performance = case_when(
        difference > 0.5 ~ "Above Cohort",
        difference < -0.5 ~ "Below Cohort",
        TRUE ~ "At Cohort Level"
      )
    ) %>%
    arrange(desc(difference)) %>%
    mutate(
      resident_median = round(resident_median, 1),
      cohort_median = round(cohort_median, 1),
      difference = round(difference, 1)
    ) %>%
    rename(
      `Resident Score` = resident_median,
      `Cohort Median` = cohort_median,
      Difference = difference,
      Performance = performance
    ) %>%
    select(Competency, `Resident Score`, `Cohort Median`, Difference, Performance)
  
  return(comparison)
}