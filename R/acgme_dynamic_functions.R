# ============================================================================
# R/acgme_dynamic_functions.R
# Core ACGME data processing functions for gmed-mvp
# ============================================================================

#' Extract Milestone Structure from ACGME Data
#'
#' Dynamically discovers the milestone structure for any specialty
#' by analyzing the Question Key and Report Category columns
#'
#' @param milestone_data Raw ACGME CSV data
#' @return List containing milestone structure information
#' @export
extract_milestone_structure <- function(milestone_data) {
  
  library(dplyr)
  library(stringr)
  
  # Clean and standardize column names
  names(milestone_data) <- tolower(gsub("\\.", "_", names(milestone_data)))
  
  # Filter out followup questions and missing data
  clean_data <- milestone_data %>%
    filter(
      !grepl("followup", question_key, ignore.case = TRUE),
      !is.na(int_response_value),
      int_response_value > 0,
      !is.na(question_key),
      !is.na(report_category)
    )
  
  # Extract milestone structure
  milestone_structure <- clean_data %>%
    distinct(question_key, report_category, question_text) %>%
    mutate(
      # Extract competency code (e.g., "PC", "MK", "SBP")
      competency_code = str_extract(question_key, "(?<=Comp\\d_)[A-Z]+"),
      
      # Extract sub-competency number (e.g., "1", "2", "3")
      sub_competency = str_extract(question_key, "(?<=Q)\\d+"),
      
      # Create standardized milestone ID (e.g., "pc1", "mk2")
      milestone_id = paste0(tolower(competency_code), sub_competency),
      
      # Clean competency names
      competency_name = str_trim(report_category),
      
      # Extract just the sub-competency title from question text
      sub_competency_title = str_trim(question_text)
    ) %>%
    filter(!is.na(competency_code), !is.na(sub_competency)) %>%
    arrange(competency_code, as.numeric(sub_competency))
  
  # Create competency mapping
  competency_mapping <- milestone_structure %>%
    distinct(competency_code, competency_name) %>%
    arrange(competency_code)
  
  # Count sub-competencies per competency
  competency_counts <- milestone_structure %>%
    group_by(competency_code, competency_name) %>%
    summarise(
      n_subcompetencies = n(),
      subcompetency_range = paste0("1-", max(as.numeric(sub_competency))),
      .groups = "drop"
    )
  
  # Get unique resident years
  resident_years <- sort(unique(clean_data$resident_year))
  
  # Get unique periods  
  periods <- unique(clean_data$schedule_window_description)
  
  # Create milestone column mapping (for compatibility with gmed functions)
  milestone_columns <- milestone_structure %>%
    mutate(
      # Create acgme-style column names for compatibility
      acgme_column = paste0("acgme_", milestone_id)
    ) %>%
    select(question_key, milestone_id, acgme_column, competency_code, 
           competency_name, sub_competency, sub_competency_title)
  
  return(list(
    milestone_structure = milestone_structure,
    competency_mapping = competency_mapping,
    competency_counts = competency_counts,
    milestone_columns = milestone_columns,
    resident_years = resident_years,
    periods = periods,
    total_milestones = nrow(milestone_structure)
  ))
}

#' Transform ACGME Data with Dynamic Structure
#'
#' Converts ACGME CSV data to gmed-compatible format using discovered structure
#'
#' @param csv_files Vector of file paths to ACGME CSV files
#' @return List containing processed data and structure information
#' @export
transform_acgme_data_dynamic <- function(csv_files) {
  
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(stringr)
  
  # Read and combine all CSV files
  combined_data <- map_dfr(csv_files, ~ {
    read.csv(.x, stringsAsFactors = FALSE) %>%
      mutate(source_file = basename(.x))
  })
  
  # Clean column names
  names(combined_data) <- tolower(gsub("\\.", "_", names(combined_data)))
  
  # Extract the milestone structure from the data
  structure_info <- extract_milestone_structure(combined_data)
  
  # Clean and standardize the data
  milestone_long <- combined_data %>%
    # Rename to match gmed expected columns
    rename(
      record_id = resident_id,
      Level = resident_year,
      prog_mile_period = schedule_window_description,
      category = report_category,
      score = int_response_value
    ) %>%
    # Create resident name
    mutate(
      name = paste(first_name, last_name),
      Level = as.character(Level),
      prog_mile_period = case_when(
        grepl("Mid", prog_mile_period, ignore.case = TRUE) ~ "Mid-Year",
        grepl("End", prog_mile_period, ignore.case = TRUE) ~ "End-Year",
        TRUE ~ "Total"
      )
    ) %>%
    # Filter out followup questions and missing scores
    filter(
      !grepl("followup", question_key, ignore.case = TRUE),
      !is.na(score), score > 0
    ) %>%
    # Add milestone mapping using the discovered structure
    left_join(
      structure_info$milestone_columns %>% 
        select(question_key, milestone_id, acgme_column, competency_code),
      by = "question_key"
    ) %>%
    filter(!is.na(milestone_id))
  
  # Reshape to wide format for gmed compatibility
  milestone_wide <- milestone_long %>%
    select(record_id, name, Level, prog_mile_period, acgme_column, score) %>%
    pivot_wider(
      names_from = acgme_column,
      values_from = score,
      values_fn = mean  # In case of duplicates, take mean
    ) %>%
    # Add period_name for compatibility
    mutate(period_name = prog_mile_period)
  
  # Also keep the long format for detailed analysis
  milestone_detailed <- milestone_long %>%
    select(record_id, name, Level, prog_mile_period, question_key, 
           milestone_id, competency_code, category, score, question_text) %>%
    rename(
      competency_name = category,
      sub_competency_title = question_text
    )
  
  return(list(
    milestone_data_wide = milestone_wide,
    milestone_data_long = milestone_detailed,
    structure_info = structure_info,
    competency_mapping = structure_info$competency_mapping,
    resident_years = structure_info$resident_years,
    periods = structure_info$periods
  ))
}

#' Create Program Summary Statistics (Dynamic)
#'
#' Calculates summary stats using discovered structure
#'
#' @param processed_data Output from transform_acgme_data_dynamic()
#' @param period_filter Selected period
#' @return List of summary statistics
#' @export
create_dynamic_summary_stats <- function(processed_data, period_filter = "Total") {
  
  library(dplyr)
  
  milestone_data <- processed_data$milestone_data_long
  structure_info <- processed_data$structure_info
  
  # Filter by period
  if (period_filter != "Total") {
    milestone_data <- milestone_data %>% 
      filter(prog_mile_period == period_filter)
  }
  
  # Calculate overall statistics
  summary_stats <- list(
    total_residents = n_distinct(milestone_data$record_id),
    total_evaluations = nrow(milestone_data),
    total_competencies = length(unique(milestone_data$competency_code)),
    total_milestones = structure_info$total_milestones,
    resident_years = paste(sort(unique(milestone_data$Level)), collapse = ", "),
    overall_median = round(median(milestone_data$score, na.rm = TRUE), 1),
    overall_mean = round(mean(milestone_data$score, na.rm = TRUE), 1),
    below_benchmark = sum(milestone_data$score < 4, na.rm = TRUE),
    below_benchmark_pct = round(mean(milestone_data$score < 4, na.rm = TRUE) * 100, 1),
    above_target = sum(milestone_data$score >= 7, na.rm = TRUE),
    above_target_pct = round(mean(milestone_data$score >= 7, na.rm = TRUE) * 100, 1),
    period = period_filter,
    competency_breakdown = structure_info$competency_counts
  )
  
  return(summary_stats)
}

#' Get Dynamic Milestone Columns
#'
#' Returns milestone column names for compatibility with existing gmed functions
#'
#' @param processed_data Output from transform_acgme_data_dynamic()
#' @return Vector of milestone column names
#' @export
get_dynamic_milestone_columns <- function(processed_data) {
  processed_data$structure_info$milestone_columns$acgme_column
}