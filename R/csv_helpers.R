# =============================================================================
# CSV PROCESSING HELPER FUNCTIONS
# R/csv_helpers.R
# =============================================================================

#' Process ACGME Milestone Data from CSV
#'
#' Takes raw CSV data and processes it into a structured format for visualization
#'
#' @param raw_data Combined data frame from CSV files
#' @return List with processed milestone data, residents info, and metadata
process_milestone_data <- function(raw_data) {
  
  # Standardize column names to handle variations
  std_names <- standardize_column_names(raw_data)
  data <- raw_data
  names(data) <- std_names
  
  # Extract resident information
  residents <- data %>%
    select(any_of(c("Resident_ID", "First_Name", "Last_Name", "Program_Name", 
                    "Specialty_Name", "Resident_Year", "Sponsor_Name"))) %>%
    distinct() %>%
    filter(!is.na(Resident_ID))
  
  # Extract milestone evaluations
  milestone_data <- data %>%
    filter(!is.na(Int_Response_Value), !is.na(Question_Key)) %>%
    select(any_of(c("Resident_ID", "First_Name", "Last_Name", "Program_Name",
                    "Resident_Year", "Question_Key", "Report_Category", 
                    "Question_Text", "Int_Response_Value", "Schedule_Window_Description",
                    "Academic_Year"))) %>%
    mutate(
      Score = as.numeric(Int_Response_Value),
      Competency = Report_Category,
      Milestone_ID = Question_Key,
      Period = Schedule_Window_Description %||% "Unknown"
    ) %>%
    filter(!is.na(Score), Score >= 1, Score <= 9)
  
  # Extract unique periods
  periods <- milestone_data %>%
    pull(Period) %>%
    unique() %>%
    sort()
  
  # Extract competency structure
  competencies <- milestone_data %>%
    select(Competency, Milestone_ID, Question_Text) %>%
    distinct() %>%
    arrange(Competency, Milestone_ID)
  
  # Calculate summary statistics
  competency_stats <- calculate_competency_stats(milestone_data)
  
  return(list(
    residents = residents,
    milestone_data = milestone_data,
    competencies = competencies,
    periods = periods,
    competency_stats = competency_stats,
    data_summary = create_data_summary(residents, milestone_data)
  ))
}

#' Standardize Column Names
#'
#' Handles common variations in ACGME CSV column names
#'
#' @param data Data frame with raw column names
#' @return Vector of standardized column names
standardize_column_names <- function(data) {
  names_map <- c(
    # Resident identification
    "Resident.ID" = "Resident_ID",
    "Resident_ID" = "Resident_ID", 
    "ResidentID" = "Resident_ID",
    "Record.ID" = "Resident_ID",
    
    # Names
    "First.Name" = "First_Name",
    "First_Name" = "First_Name",
    "FirstName" = "First_Name",
    "Last.Name" = "Last_Name", 
    "Last_Name" = "Last_Name",
    "LastName" = "Last_Name",
    
    # Program info
    "Program.Name" = "Program_Name",
    "Program_Name" = "Program_Name",
    "ProgramName" = "Program_Name",
    "Specialty.Name" = "Specialty_Name",
    "Specialty_Name" = "Specialty_Name",
    "SpecialtyName" = "Specialty_Name",
    "Sponsor.Name" = "Sponsor_Name",
    "Sponsor_Name" = "Sponsor_Name",
    
    # Training level
    "Resident.Year" = "Resident_Year",
    "Resident_Year" = "Resident_Year",
    "ResidentYear" = "Resident_Year",
    "Year" = "Resident_Year",
    "Level" = "Resident_Year",
    
    # Milestone info
    "Question.Key" = "Question_Key",
    "Question_Key" = "Question_Key",
    "QuestionKey" = "Question_Key",
    "Milestone.ID" = "Question_Key",
    "Question.ID" = "Question_Key",
    
    "Report.Category" = "Report_Category",
    "Report_Category" = "Report_Category", 
    "ReportCategory" = "Report_Category",
    "Competency" = "Report_Category",
    
    "Question.Text" = "Question_Text",
    "Question_Text" = "Question_Text",
    "QuestionText" = "Question_Text",
    "Milestone.Text" = "Question_Text",
    
    # Scores
    "Int.Response.Value" = "Int_Response_Value",
    "Int_Response_Value" = "Int_Response_Value",
    "IntResponseValue" = "Int_Response_Value",
    "Score" = "Int_Response_Value",
    "Response.Value" = "Int_Response_Value",
    
    # Time periods
    "Schedule.Window.Description" = "Schedule_Window_Description",
    "Schedule_Window_Description" = "Schedule_Window_Description",
    "Period" = "Schedule_Window_Description",
    "Assessment.Period" = "Schedule_Window_Description",
    
    # Academic year
    "Academic.Year" = "Academic_Year",
    "Academic_Year" = "Academic_Year",
    "AcademicYear" = "Academic_Year"
  )
  
  current_names <- names(data)
  standardized_names <- ifelse(current_names %in% names(names_map), 
                               names_map[current_names], 
                               current_names)
  
  return(standardized_names)
}

#' Calculate Competency Statistics
#'
#' Calculates summary statistics for each competency
#'
#' @param milestone_data Processed milestone data
#' @return Data frame with competency statistics
calculate_competency_stats <- function(milestone_data) {
  
  milestone_data %>%
    group_by(Competency) %>%
    summarise(
      n_evaluations = n(),
      n_residents = n_distinct(Resident_ID),
      median_score = median(Score, na.rm = TRUE),
      mean_score = mean(Score, na.rm = TRUE),
      sd_score = sd(Score, na.rm = TRUE),
      min_score = min(Score, na.rm = TRUE),
      max_score = max(Score, na.rm = TRUE),
      below_benchmark_pct = mean(Score < 4, na.rm = TRUE) * 100,
      at_benchmark_pct = mean(Score >= 4 & Score < 6, na.rm = TRUE) * 100,
      above_target_pct = mean(Score >= 6, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    mutate(
      median_score = round(median_score, 1),
      mean_score = round(mean_score, 1),
      sd_score = round(sd_score, 1),
      below_benchmark_pct = round(below_benchmark_pct, 1),
      at_benchmark_pct = round(at_benchmark_pct, 1),
      above_target_pct = round(above_target_pct, 1)
    )
}

#' Create Data Summary
#'
#' Creates a summary of the loaded data
#'
#' @param residents Residents data
#' @param milestone_data Milestone data
#' @return List with summary statistics
create_data_summary <- function(residents, milestone_data) {
  
  list(
    total_residents = nrow(residents),
    total_evaluations = nrow(milestone_data),
    unique_competencies = length(unique(milestone_data$Competency)),
    unique_milestones = length(unique(milestone_data$Milestone_ID)),
    programs = unique(residents$Program_Name),
    resident_years = sort(unique(residents$Resident_Year)),
    score_range = range(milestone_data$Score, na.rm = TRUE),
    assessment_periods = unique(milestone_data$Period)
  )
}

#' Get Resident Evaluation Count
#'
#' Returns the number of evaluations for a specific resident
#'
#' @param data Processed data list
#' @param resident_id Resident ID
#' @return Number of evaluations
get_resident_eval_count <- function(data, resident_id) {
  data$milestone_data %>%
    filter(Resident_ID == resident_id) %>%
    nrow()
}

#' Filter Data by Period and Level
#'
#' Filters milestone data based on selected period and resident level
#'
#' @param milestone_data Milestone data
#' @param period_filter Selected period ("total" for all)
#' @param level_filter Selected level ("all" for all levels)
#' @return Filtered milestone data
filter_data <- function(milestone_data, period_filter = "total", level_filter = "all") {
  
  filtered_data <- milestone_data
  
  # Filter by period
  if (period_filter != "total") {
    filtered_data <- filtered_data %>%
      filter(Period == period_filter)
  }
  
  # Filter by level
  if (level_filter != "all") {
    filtered_data <- filtered_data %>%
      filter(Resident_Year == level_filter)
  }
  
  return(filtered_data)
}