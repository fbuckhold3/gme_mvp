# =============================================================================
# MEDIAN CALCULATION FUNCTIONS FOR GME MILESTONE PROJECT
# R/median_calculations.R
# =============================================================================

#' Calculate comprehensive milestone medians
#'
#' Calculates medians for milestone sub-competencies in multiple ways:
#' - Overall program medians (across all data)
#' - Period medians (Mid-Year vs Year-End)
#' - Training year medians (PGY-1, PGY-2, PGY-3)
#' - Period x Year combination medians
#'
#' @param df Processed milestone data in wide format (from import_and_process_milestones)
#' @param verbose Logical. Print detailed progress messages (default: TRUE)
#' @return List with comprehensive median calculations and summary statistics
#' @export
#'
#' @examples
#' \dontrun{
#' # Load and process your MILES data first
#' processed_data <- import_and_process_milestones(file_paths)
#' 
#' # Calculate comprehensive medians
#' median_results <- calculate_comprehensive_medians(processed_data)
#' 
#' # Access specific median types
#' overall <- median_results$overall_medians
#' by_period <- median_results$period_medians
#' by_year <- median_results$year_medians
#' combinations <- median_results$period_year_medians
#' master_table <- median_results$master_summary
#' }
calculate_comprehensive_medians <- function(df, verbose = TRUE) {
  
  # Get milestone columns (sub-competencies)
  milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)\\d+$", names(df), value = TRUE)
  
  if (length(milestone_cols) == 0) {
    stop("No milestone columns found in the data")
  }
  
  if (verbose) {
    cat("\n=== MILESTONE MEDIAN CALCULATIONS ===\n")
    cat("Sub-competencies found:", length(milestone_cols), "\n")
    cat("Sample sub-competencies:", paste(head(milestone_cols, 10), collapse = ", "), "\n")
    cat("Academic years included: Multi-year dataset\n")
    cat("Total data points available:", sum(!is.na(df[milestone_cols])), "\n")
  }
  
  # 1. OVERALL PROGRAM MEDIANS (across all periods and years)
  if (verbose) cat("\n1. Calculating overall program medians...\n")
  
  overall_medians <- df %>%
    select(all_of(milestone_cols)) %>%
    summarise(across(everything(), ~ median(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Sub_Competency", values_to = "Overall_Median") %>%
    mutate(
      Category = sub("\\d+$", "", Sub_Competency),
      Sub_Comp_Number = as.numeric(gsub("^[A-Z]+", "", Sub_Competency))
    )
  
  # 2. MEDIANS BY PERIOD (Mid-Year vs Year-End)
  if (verbose) cat("2. Calculating medians by period...\n")
  
  period_medians <- df %>%
    select(period, all_of(milestone_cols)) %>%
    filter(!is.na(period)) %>%
    group_by(period) %>%
    summarise(across(all_of(milestone_cols), ~ median(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(cols = all_of(milestone_cols), names_to = "Sub_Competency", values_to = "Period_Median") %>%
    mutate(
      Category = sub("\\d+$", "", Sub_Competency),
      Sub_Comp_Number = as.numeric(gsub("^[A-Z]+", "", Sub_Competency))
    )
  
  # 3. MEDIANS BY TRAINING YEAR
  if (verbose) cat("3. Calculating medians by training year...\n")
  
  year_medians <- df %>%
    select(Resident.Year, all_of(milestone_cols)) %>%
    filter(!is.na(Resident.Year)) %>%
    group_by(Resident.Year) %>%
    summarise(across(all_of(milestone_cols), ~ median(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(cols = all_of(milestone_cols), names_to = "Sub_Competency", values_to = "Year_Median") %>%
    mutate(
      Category = sub("\\d+$", "", Sub_Competency),
      Sub_Comp_Number = as.numeric(gsub("^[A-Z]+", "", Sub_Competency)),
      Training_Year = paste0("PGY-", Resident.Year)
    )
  
  # 4. MEDIANS BY PERIOD AND YEAR COMBINATION
  if (verbose) cat("4. Calculating medians by period and year combination...\n")
  
  period_year_medians <- df %>%
    select(period, Resident.Year, all_of(milestone_cols)) %>%
    filter(!is.na(period), !is.na(Resident.Year)) %>%
    group_by(period, Resident.Year) %>%
    summarise(across(all_of(milestone_cols), ~ median(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(cols = all_of(milestone_cols), names_to = "Sub_Competency", values_to = "Period_Year_Median") %>%
    mutate(
      Category = sub("\\d+$", "", Sub_Competency),
      Sub_Comp_Number = as.numeric(gsub("^[A-Z]+", "", Sub_Competency)),
      Training_Year = paste0("PGY-", Resident.Year)
    )
  
  # 5. CREATE COMPREHENSIVE SUMMARY TABLE
  if (verbose) cat("5. Creating comprehensive summary...\n")
  
  # Create a complete summary with all combinations
  comprehensive_summary <- period_year_medians %>%
    left_join(
      period_medians %>% select(period, Sub_Competency, Period_Median),
      by = c("period", "Sub_Competency")
    ) %>%
    left_join(
      year_medians %>% select(Resident.Year, Sub_Competency, Year_Median),
      by = c("Resident.Year", "Sub_Competency")
    ) %>%
    left_join(
      overall_medians %>% select(Sub_Competency, Overall_Median),
      by = "Sub_Competency"
    ) %>%
    arrange(Category, Sub_Comp_Number, Training_Year, period)
  
  # 6. SUMMARY STATISTICS
  summary_stats <- list(
    total_sub_competencies = length(milestone_cols),
    categories = sort(unique(overall_medians$Category)),
    periods = sort(unique(df$period[!is.na(df$period)])),
    training_years = sort(unique(df$Resident.Year[!is.na(df$Resident.Year)])),
    total_combinations = nrow(period_year_medians),
    data_points_analyzed = sum(!is.na(df[milestone_cols])),
    milestone_columns = milestone_cols
  )
  
  if (verbose) {
    cat("\n=== SUMMARY STATISTICS ===\n")
    cat("Total sub-competencies:", summary_stats$total_sub_competencies, "\n")
    cat("Categories:", paste(summary_stats$categories, collapse = ", "), "\n")
    cat("Periods:", paste(summary_stats$periods, collapse = ", "), "\n")
    cat("Training years:", paste(summary_stats$training_years, collapse = ", "), "\n")
    cat("Period x Year combinations:", summary_stats$total_combinations, "\n")
    cat("Total data points analyzed:", summary_stats$data_points_analyzed, "\n")
  }
  
  return(list(
    overall_medians = overall_medians,
    period_medians = period_medians,
    year_medians = year_medians,
    period_year_medians = period_year_medians,
    comprehensive_summary = comprehensive_summary,
    summary_stats = summary_stats,
    milestone_columns = milestone_cols
  ))
}

#' Get Medians for Specific Sub-Competency
#'
#' Extract all median values for a specific sub-competency across all calculation types
#'
#' @param median_results Results from calculate_comprehensive_medians()
#' @param sub_competency Character. Sub-competency code (e.g., "PC1", "MK2")
#' @return Data frame with all median types for the specified sub-competency
#' @export
#'
#' @examples
#' \dontrun{
#' median_results <- calculate_comprehensive_medians(processed_data)
#' pc1_medians <- get_subcompetency_medians(median_results, "PC1")
#' }
get_subcompetency_medians <- function(median_results, sub_competency) {
  
  if (!sub_competency %in% median_results$milestone_columns) {
    stop("Sub-competency '", sub_competency, "' not found in milestone data")
  }
  
  result <- median_results$comprehensive_summary %>%
    filter(Sub_Competency == sub_competency) %>%
    select(period, Training_Year, Period_Year_Median, Period_Median, Year_Median, Overall_Median) %>%
    arrange(Training_Year, period)
  
  return(result)
}

#' Get Category Summary Medians
#'
#' Summarize median performance by competency category
#'
#' @param median_results Results from calculate_comprehensive_medians()
#' @return Data frame with category-level median summaries
#' @export
#'
#' @examples
#' \dontrun{
#' median_results <- calculate_comprehensive_medians(processed_data)
#' category_summary <- get_category_summary_medians(median_results)
#' }
get_category_summary_medians <- function(median_results) {
  
  overall_by_category <- median_results$overall_medians %>%
    group_by(Category) %>%
    summarise(
      Count = n(),
      Min_Median = round(min(Overall_Median, na.rm = TRUE), 2),
      Max_Median = round(max(Overall_Median, na.rm = TRUE), 2),
      Avg_Median = round(mean(Overall_Median, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(Category)
  
  return(overall_by_category)
}

#' Load Multi-Year MILES Data and Calculate Medians
#'
#' Convenience function that loads multiple years of MILES data and calculates
#' comprehensive medians in one step. Useful for app initialization.
#'
#' @param data_folder Character. Path to folder containing MILES CSV files
#' @param file_patterns Character vector. File patterns to look for (optional)
#' @param verbose Logical. Print progress messages (default: TRUE)
#' @return List containing both processed_data and median_results
#' @export
#'
#' @examples
#' \dontrun{
#' data_folder <- "/Users/fredbuckhold/Documents/Milestone Data"
#' results <- load_miles_data_and_calculate_medians(data_folder)
#' 
#' # Access processed data
#' processed_data <- results$processed_data
#' 
#' # Access median calculations
#' median_results <- results$median_results
#' }
load_miles_data_and_calculate_medians <- function(data_folder, 
                                                  file_patterns = NULL, 
                                                  verbose = TRUE) {
  
  # Default file patterns for MILES data
  if (is.null(file_patterns)) {
    file_patterns <- c(
      "imslu_miles_data_end_2122.csv",
      "imslu_miles_data_mid_2122.csv",
      "imslu_miles_data_end_2223.csv", 
      "imslu_miles_data_mid_2223.csv",
      "imslu_miles_data_end_2324.csv",
      "imslu_miles_data_mid_2324.csv",
      "imslu_miles_data_end_2425.csv",
      "imslu_miles_data_mid_2425.csv"
    )
  }
  
  # Find existing files
  full_file_paths <- file.path(data_folder, file_patterns)
  existing_files <- full_file_paths[file.exists(full_file_paths)]
  
  if (length(existing_files) == 0) {
    stop("No MILES data files found in folder: ", data_folder)
  }
  
  if (verbose) {
    cat("Loading", length(existing_files), "MILES data files from:", data_folder, "\n")
  }
  
  # Process the data
  processed_data <- import_and_process_milestones(existing_files)
  
  # Calculate comprehensive medians
  median_results <- calculate_comprehensive_medians(processed_data, verbose = verbose)
  
  if (verbose) {
    cat("\nData loading and median calculation completed successfully!\n")
  }
  
  return(list(
    processed_data = processed_data,
    median_results = median_results,
    files_processed = existing_files
  ))
}

#' Validate Median Calculations
#'
#' Perform validation checks on median calculation results
#'
#' @param median_results Results from calculate_comprehensive_medians()
#' @param verbose Logical. Print validation details (default: TRUE)
#' @return List with validation results
#' @export
validate_median_calculations <- function(median_results, verbose = TRUE) {
  
  validation_results <- list()
  
  # Check for missing values
  validation_results$missing_values <- list(
    overall = sum(is.na(median_results$overall_medians$Overall_Median)),
    period = sum(is.na(median_results$period_medians$Period_Median)),
    year = sum(is.na(median_results$year_medians$Year_Median)),
    combinations = sum(is.na(median_results$period_year_medians$Period_Year_Median))
  )
  
  # Check value ranges
  validation_results$value_ranges <- list(
    overall_range = range(median_results$overall_medians$Overall_Median, na.rm = TRUE),
    period_range = range(median_results$period_medians$Period_Median, na.rm = TRUE),
    year_range = range(median_results$year_medians$Year_Median, na.rm = TRUE)
  )
  
  # Check data completeness
  validation_results$completeness <- list(
    total_subcompetencies = length(median_results$milestone_columns),
    categories_covered = length(median_results$summary_stats$categories),
    periods_covered = length(median_results$summary_stats$periods),
    years_covered = length(median_results$summary_stats$training_years)
  )
  
  if (verbose) {
    cat("\n=== VALIDATION RESULTS ===\n")
    cat("Missing values:\n")
    cat("  Overall medians:", validation_results$missing_values$overall, "\n")
    cat("  Period medians:", validation_results$missing_values$period, "\n")
    cat("  Year medians:", validation_results$missing_values$year, "\n")
    cat("  Combination medians:", validation_results$missing_values$combinations, "\n")
    
    cat("\nValue ranges:\n")
    cat("  Overall:", paste(round(validation_results$value_ranges$overall_range, 2), collapse = " to "), "\n")
    cat("  Period:", paste(round(validation_results$value_ranges$period_range, 2), collapse = " to "), "\n")
    cat("  Year:", paste(round(validation_results$value_ranges$year_range, 2), collapse = " to "), "\n")
    
    cat("\nData completeness:\n")
    cat("  Sub-competencies:", validation_results$completeness$total_subcompetencies, "\n")
    cat("  Categories:", validation_results$completeness$categories_covered, "\n")
    cat("  Periods:", validation_results$completeness$periods_covered, "\n")
    cat("  Training years:", validation_results$completeness$years_covered, "\n")
  }
  
  return(validation_results)
}
