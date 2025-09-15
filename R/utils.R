# =============================================================================
# UTILITY FUNCTIONS FOR GME MILESTONE PROJECT
# R/utils.R
# =============================================================================

#' Reorder Milestone Columns
#'
#' Reorders columns in milestone data frames to group them logically by competency.
#' Places non-milestone columns first, then milestone columns organized by competency.
#'
#' @param df Data frame with milestone columns
#' @return Data frame with reordered columns
#' @export
reorder_milestones <- function(df) {
  milestone_groups <- c("PC", "MK", "SBP", "PBL", "PROF", "ICS")
  cols <- names(df)
  non_milestone_cols <- cols[!grepl("^(PC|MK|SBP|PBL|PROF|ICS)", cols)]
  milestone_cols <- character(0)
  
  for(group in milestone_groups) {
    pattern <- paste0("^", group)
    group_cols <- sort(cols[grepl(pattern, cols)])
    milestone_cols <- c(milestone_cols, group_cols)
  }
  
  df <- df[, c(non_milestone_cols, milestone_cols)]
  return(df)
}

#' Import and Select Columns (Basic Version)
#'
#' Basic CSV import function with column validation and cleaning.
#' This is a simpler version compared to the main import_and_process_milestones function.
#'
#' @param file_paths Vector of file paths to CSV files
#' @return Data frame with processed milestone data
#' @export
import_and_select_columns <- function(file_paths) {
  # Add error handling for file reading
  combined_data <- do.call(rbind, lapply(file_paths, function(x) {
    tryCatch({
      df <- read.csv(x, stringsAsFactors = FALSE)
      
      # Verify required columns exist
      required_cols <- c("Schedule.Window.Description", "Resident.ID",
                         "Resident.Year", "Question.Key", 
                         "Int.Response.Value")
      
      missing_cols <- setdiff(required_cols, names(df))
      if (length(missing_cols) > 0) {
        stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
      }
      
      # Select only needed columns
      df <- df[, required_cols]
      
      # Clean column names
      colnames(df) <- gsub("\\s+", ".", colnames(df))
      
      return(df)
    }, error = function(e) {
      message("Error reading file ", x, ": ", e$message)
      return(NULL)
    })
  }))
  
  # Remove any NULL entries from failed reads
  combined_data <- combined_data[!sapply(combined_data, is.null)]
  
  if (nrow(combined_data) == 0) {
    stop("No valid data found in the uploaded files")
  }
  
  # Process the data
  processed_data <- combined_data %>%
    mutate(
      Resident.Year = as.character(Resident.Year),
      Int.Response.Value = as.numeric(Int.Response.Value)
    ) %>%
    filter(!is.na(Int.Response.Value)) %>%  # Remove NA values
    filter(!grepl("_followup", Question.Key)) %>%
    mutate(
      Question.Key = case_when(
        grepl("[Cc]omp[1-9]_PC_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_PC_Q(\\d+)", "PC\\1", Question.Key),
        grepl("[Cc]omp[1-9]_MK_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_MK_Q(\\d+)", "MK\\1", Question.Key),
        grepl("[Cc]omp[1-9]_ICS_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_ICS_Q(\\d+)", "ICS\\1", Question.Key),
        grepl("[Cc]omp[1-9]_SBP_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_SBP_Q(\\d+)", "SBP\\1", Question.Key),
        grepl("[Cc]omp[1-9]_(PROF|PR)_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_(PROF|PR)_Q(\\d+)", "PROF\\2", Question.Key),
        grepl("[Cc]omp[1-9]_(PBL|PBLI)_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_(PBL|PBLI)_Q(\\d+)", "PBL\\2", Question.Key),
        TRUE ~ Question.Key
      )
    ) %>%
    rename(period = Schedule.Window.Description) %>%
    select(Resident.ID, Resident.Year, period, Question.Key, Int.Response.Value) %>%
    group_by(Resident.ID, Resident.Year, period, Question.Key) %>%
    summarise(Int.Response.Value = mean(Int.Response.Value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = Question.Key,
      values_from = Int.Response.Value
    ) %>%
    arrange(Resident.ID, Resident.Year)
  
  # Verify that we have milestone columns
  milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(processed_data), value = TRUE)
  if (length(milestone_cols) == 0) {
    stop("No milestone columns found in the data")
  }
  
  return(processed_data)
}

#' Import and Select Columns (Legacy Version with Names)
#'
#' Legacy version that includes resident names in processing.
#' Kept for backward compatibility.
#'
#' @param file_paths Vector of file paths to CSV files
#' @return Data frame with processed milestone data including names
#' @export
import_and_select_columns_old <- function(file_paths) {
  columns_of_interest <- c("Schedule.Window.Description", "First.Name", 
                           "Last.Name", "Resident.Year", "Question.Key", 
                           "Int.Response.Value")
  
  combined_data <- do.call(rbind, lapply(file_paths, function(x) {
    df <- read.csv(x)
    colnames(df) <- gsub("\\.", ".", colnames(df))
    df[, columns_of_interest]
  }))
  
  combined_data %>%
    mutate(
      name = paste(First.Name, Last.Name),
      Resident.Year = as.character(Resident.Year)
    ) %>%
    filter(!grepl("_followup", Question.Key)) %>%
    mutate(
      Question.Key = gsub("[Cc]omp[1-9]_PC_Q(\\d+)", "PC\\1", Question.Key),
      Question.Key = gsub("[Cc]omp[1-9]_MK_Q(\\d+)", "MK\\1", Question.Key),
      Question.Key = gsub("[Cc]omp[1-9]_ICS_Q(\\d+)", "ICS\\1", Question.Key),
      Question.Key = gsub("[Cc]omp[1-9]_SBP_Q(\\d+)", "SBP\\1", Question.Key),
      Question.Key = gsub("[Cc]omp[1-9]_(PROF|PR)_Q(\\d+)", "PROF\\2", Question.Key),
      Question.Key = gsub("[Cc]omp[1-9]_(PBL|PBLI)_Q(\\d+)", "PBL\\2", Question.Key)
    ) %>%
    rename(period = Schedule.Window.Description) %>%
    select(name, Resident.Year, period, Question.Key, Int.Response.Value) %>%
    pivot_wider(
      names_from = Question.Key,
      values_from = Int.Response.Value,
      values_fn = mean
    ) %>%
    arrange(name, Resident.Year)
}

#' Clean Column Names
#'
#' Standardizes column names by removing spaces and special characters
#'
#' @param df Data frame with columns to clean
#' @return Data frame with cleaned column names
#' @export
clean_column_names <- function(df) {
  names(df) <- gsub("\\s+", ".", names(df))
  names(df) <- gsub("\\.+", ".", names(df))
  names(df) <- gsub("\\.$", "", names(df))
  return(df)
}

#' Standardize Question Keys
#'
#' Converts various ACGME question key formats to standardized format
#'
#' @param question_keys Vector of question key strings
#' @return Vector of standardized question keys
#' @export
standardize_question_keys <- function(question_keys) {
  standardized <- question_keys %>%
    # Patient Care
    gsub("[Cc]omp[1-9]_PC_Q(\\d+)", "PC\\1", .) %>%
    # Medical Knowledge
    gsub("[Cc]omp[1-9]_MK_Q(\\d+)", "MK\\1", .) %>%
    # Interpersonal Communication Skills
    gsub("[Cc]omp[1-9]_ICS_Q(\\d+)", "ICS\\1", .) %>%
    # Systems-Based Practice
    gsub("[Cc]omp[1-9]_SBP_Q(\\d+)", "SBP\\1", .) %>%
    # Professionalism
    gsub("[Cc]omp[1-9]_(PROF|PR)_Q(\\d+)", "PROF\\2", .) %>%
    # Practice-Based Learning
    gsub("[Cc]omp[1-9]_(PBL|PBLI)_Q(\\d+)", "PBL\\2", .)
  
  return(standardized)
}

#' Validate CSV Structure
#'
#' Checks if uploaded CSV files have the required structure for milestone processing
#'
#' @param file_path Path to CSV file
#' @return List with validation results
#' @export
validate_csv_structure <- function(file_path) {
  tryCatch({
    df <- read.csv(file_path, nrows = 5)  # Read just first few rows for validation
    
    required_cols <- c("Schedule.Window.Description", "Resident.ID", 
                       "Resident.Year", "Question.Key", "Int.Response.Value")
    
    missing_cols <- setdiff(required_cols, names(df))
    extra_cols <- setdiff(names(df), required_cols)
    
    # Check for milestone questions
    has_milestones <- any(grepl("[Cc]omp[1-9]_(PC|MK|ICS|SBP|PROF|PBL)_Q\\d+", df$Question.Key))
    
    list(
      valid = length(missing_cols) == 0,
      missing_columns = missing_cols,
      extra_columns = extra_cols,
      has_milestone_questions = has_milestones,
      total_columns = ncol(df),
      sample_question_keys = head(df$Question.Key, 5)
    )
  }, error = function(e) {
    list(
      valid = FALSE,
      error = e$message,
      missing_columns = "Cannot read file",
      extra_columns = character(0),
      has_milestone_questions = FALSE,
      total_columns = 0,
      sample_question_keys = character(0)
    )
  })
}

#' Filter Followup Questions
#'
#' Removes followup questions from milestone data
#'
#' @param df Data frame with Question.Key column
#' @return Data frame with followup questions removed
#' @export
filter_followup_questions <- function(df) {
  df %>%
    filter(!grepl("_followup", Question.Key, ignore.case = TRUE))
}

#' Get Competency from Milestone
#'
#' Extracts competency category from milestone identifier
#'
#' @param milestone_id Milestone identifier (e.g., "PC1", "MK3")
#' @return Competency category (e.g., "PC", "MK")
#' @export
get_competency_from_milestone <- function(milestone_id) {
  substr(milestone_id, 1, 2)
}

#' Get Milestone Number
#'
#' Extracts milestone number from milestone identifier
#'
#' @param milestone_id Milestone identifier (e.g., "PC1", "MK3")
#' @return Milestone number as integer
#' @export
get_milestone_number <- function(milestone_id) {
  as.integer(gsub("^[A-Z]+", "", milestone_id))
}

#' Check Required Packages
#'
#' Checks if required packages are installed and loads them
#'
#' @param packages Vector of package names
#' @return Logical indicating if all packages are available
#' @export
check_required_packages <- function(packages = c("dplyr", "tidyr", "ggplot2", "plotly", "DT")) {
  missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    message("Missing required packages: ", paste(missing_packages, collapse = ", "))
    message("Install with: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))")
    return(FALSE)
  }
  
  # Load packages
  sapply(packages, library, character.only = TRUE, quietly = TRUE)
  return(TRUE)
}