# =============================================================================
# CLEAN DATA-DRIVEN CSV LOADER
# Extracts all structure and definitions from the CSV data itself
# =============================================================================

#' Load and Process ACGME Milestone CSV Data - Generalized Version
#'
#' Enhanced version that handles various Question Key patterns with full case-insensitivity.
#' Drop-in replacement for the original load_milestone_csv_data function.
#'
#' @param file_paths Vector of file paths to CSV files
#' @return List containing all processed data and metadata (same structure as original)
#' @export
load_milestone_csv_data <- function(file_paths) {
  
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  cat("Loading", length(file_paths), "CSV files...\n")
  
  # ========================================================================
  # READ AND COMBINE CSV FILES
  # ========================================================================
  
  data_list <- lapply(file_paths, function(x) {
    tryCatch({
      cat("Reading:", basename(x), "\n")
      
      # Use data.table::fread for faster reading if available
      if (requireNamespace("data.table", quietly = TRUE)) {
        df <- data.table::fread(x, data.table = FALSE)
      } else {
        df <- read.csv(x, stringsAsFactors = FALSE)
      }
      
      # Clean column names
      names(df) <- gsub("\\s+", ".", names(df))
      names(df) <- gsub("\\.+", ".", names(df))
      names(df) <- gsub("\\.$", "", names(df))
      
      cat("  Rows:", nrow(df), "Columns:", ncol(df), "\n")
      
      return(df)
    }, error = function(e) {
      cat("Error reading file", x, ":", e$message, "\n")
      return(NULL)
    })
  })
  
  # Remove any NULL entries from failed reads
  data_list <- data_list[!sapply(data_list, is.null)]
  
  if (length(data_list) == 0) {
    stop("No valid data found in the uploaded files")
  }
  
  # Combine all dataframes
  combined_data <- do.call(rbind, data_list)
  
  cat("Combined data:", nrow(combined_data), "total rows\n")
  cat("Column names:", paste(head(names(combined_data), 5), collapse = ", "), "...\n")
  
  # ========================================================================
  # EXTRACT PROGRAM INFORMATION
  # ========================================================================
  
  program_info <- list(
    program_name = if("Program.Name" %in% names(combined_data)) unique(combined_data$Program.Name)[1] else "Unknown Program",
    specialty_name = if("Specialty.Name" %in% names(combined_data)) unique(combined_data$Specialty.Name)[1] else "Unknown Specialty", 
    academic_year = if("Academic.Year" %in% names(combined_data)) unique(combined_data$Academic.Year)[1] else "Unknown Year",
    survey_name = if("Survey.Name" %in% names(combined_data)) unique(combined_data$Survey.Name)[1] else "Unknown Survey"
  )
  
  cat("\nProgram Info:\n")
  cat("  Program:", program_info$program_name, "\n")
  cat("  Specialty:", program_info$specialty_name, "\n") 
  cat("  Academic Year:", program_info$academic_year, "\n")
  
  # ========================================================================
  # CREATE MILESTONE DEFINITIONS (FULLY CASE-INSENSITIVE)
  # ========================================================================
  
  milestone_definitions <- combined_data %>%
    filter(!is.na(Question.Key), !is.na(Question.Text)) %>%
    select(Question.Key, Question.Text, Report.Category) %>%
    distinct() %>%
    mutate(
      # Extract sub-competency from Question Key - FULLY CASE-INSENSITIVE
      # Handles: comp/Comp/COMP, pc/PC/Pc, mk/MK/Mk, etc.
      Sub_Competency = case_when(
        str_detect(Question.Key, regex("[Cc][Oo][Mm][Pp][0-9]+_[Pp][Cc]_[Qq][0-9]+")) ~ 
          paste0("PC", str_extract(Question.Key, "(?i)(?<=_[Pp][Cc]_[Qq])[0-9]+")),
        str_detect(Question.Key, regex("[Cc][Oo][Mm][Pp][0-9]+_[Mm][Kk]_[Qq][0-9]+")) ~ 
          paste0("MK", str_extract(Question.Key, "(?i)(?<=_[Mm][Kk]_[Qq])[0-9]+")),
        str_detect(Question.Key, regex("[Cc][Oo][Mm][Pp][0-9]+_[Ii][Cc][Ss]_[Qq][0-9]+")) ~ 
          paste0("ICS", str_extract(Question.Key, "(?i)(?<=_[Ii][Cc][Ss]_[Qq])[0-9]+")),
        str_detect(Question.Key, regex("[Cc][Oo][Mm][Pp][0-9]+_[Ss][Bb][Pp]_[Qq][0-9]+")) ~ 
          paste0("SBP", str_extract(Question.Key, "(?i)(?<=_[Ss][Bb][Pp]_[Qq])[0-9]+")),
        str_detect(Question.Key, regex("[Cc][Oo][Mm][Pp][0-9]+_[Pp][Rr][Oo][Ff]_[Qq][0-9]+")) ~ 
          paste0("PROF", str_extract(Question.Key, "(?i)(?<=_[Pp][Rr][Oo][Ff]_[Qq])[0-9]+")),
        str_detect(Question.Key, regex("[Cc][Oo][Mm][Pp][0-9]+_[Pp][Bb][Ll][Ii]?_[Qq][0-9]+")) ~ 
          paste0("PBLI", str_extract(Question.Key, "(?i)(?<=_[Pp][Bb][Ll][Ii]?_[Qq])[0-9]+")),
        # Handle PBL without I
        str_detect(Question.Key, regex("[Cc][Oo][Mm][Pp][0-9]+_[Pp][Bb][Ll]_[Qq][0-9]+")) ~ 
          paste0("PBL", str_extract(Question.Key, "(?i)(?<=_[Pp][Bb][Ll]_[Qq])[0-9]+")),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Sub_Competency)) %>%
    rename(
      Competency = Report.Category,
      Milestone_Description = Question.Text,
      Original_Question_Key = Question.Key
    ) %>%
    arrange(Competency, Sub_Competency)
  
  cat("Found", nrow(milestone_definitions), "unique milestone definitions\n")
  
  # Show sample milestone definitions
  cat("Sample milestone definitions:\n")
  print(head(milestone_definitions$Sub_Competency, 10))
  
  # Extract competency categories from the data
  competency_categories <- milestone_definitions %>%
    select(Competency) %>%
    distinct() %>%
    pull(Competency) %>%
    sort()
  
  cat("Competency categories:", paste(competency_categories, collapse = ", "), "\n")
  
  # ========================================================================
  # PROCESS MILESTONE EVALUATION DATA (FULLY CASE-INSENSITIVE)
  # ========================================================================
  
  cat("Starting evaluation data processing...\n")
  
  tryCatch({
    # Filter and clean evaluation data
    evaluation_data <- combined_data %>%
      filter(
        !is.na(Int.Response.Value),
        Int.Response.Value >= 1,
        Int.Response.Value <= 9,
        !grepl("_followup", Question.Key, ignore.case = TRUE),
        !is.na(Question.Key),
        !is.na(First.Name),
        !is.na(Last.Name)
      ) %>%
      mutate(
        # Create resident name
        Resident_Name = paste(First.Name, Last.Name),
        
        # Extract period from Schedule Window Description
        Period = case_when(
          grepl("Mid-Year", Schedule.Window.Description, ignore.case = TRUE) ~ 
            str_replace(Schedule.Window.Description, "^([0-9]{4}-[0-9]{4}).*[Mm]id-?[Yy]ear.*", "\\1 Mid-Year"),
          grepl("Year-End", Schedule.Window.Description, ignore.case = TRUE) ~ 
            str_replace(Schedule.Window.Description, "^([0-9]{4}-[0-9]{4}).*[Yy]ear-?[Ee]nd.*", "\\1 Year-End"),
          TRUE ~ Schedule.Window.Description
        ),
        
        # Keep PGY level flexible
        PGY_Level = case_when(
          grepl("PGY", Resident.Year, ignore.case = TRUE) ~ as.character(Resident.Year),
          is.numeric(as.numeric(Resident.Year)) ~ paste0("PGY-", Resident.Year),
          TRUE ~ as.character(Resident.Year)
        ),
        
        # Transform Question.Key - FULLY CASE-INSENSITIVE
        Sub_Competency = case_when(
          str_detect(Question.Key, regex("[Cc][Oo][Mm][Pp][0-9]+_[Pp][Cc]_[Qq][0-9]+")) ~ 
            paste0("PC", str_extract(Question.Key, "(?i)(?<=_[Pp][Cc]_[Qq])[0-9]+")),
          str_detect(Question.Key, regex("[Cc][Oo][Mm][Pp][0-9]+_[Mm][Kk]_[Qq][0-9]+")) ~ 
            paste0("MK", str_extract(Question.Key, "(?i)(?<=_[Mm][Kk]_[Qq])[0-9]+")),
          str_detect(Question.Key, regex("[Cc][Oo][Mm][Pp][0-9]+_[Ii][Cc][Ss]_[Qq][0-9]+")) ~ 
            paste0("ICS", str_extract(Question.Key, "(?i)(?<=_[Ii][Cc][Ss]_[Qq])[0-9]+")),
          str_detect(Question.Key, regex("[Cc][Oo][Mm][Pp][0-9]+_[Ss][Bb][Pp]_[Qq][0-9]+")) ~ 
            paste0("SBP", str_extract(Question.Key, "(?i)(?<=_[Ss][Bb][Pp]_[Qq])[0-9]+")),
          str_detect(Question.Key, regex("[Cc][Oo][Mm][Pp][0-9]+_[Pp][Rr][Oo][Ff]_[Qq][0-9]+")) ~ 
            paste0("PROF", str_extract(Question.Key, "(?i)(?<=_[Pp][Rr][Oo][Ff]_[Qq])[0-9]+")),
          str_detect(Question.Key, regex("[Cc][Oo][Mm][Pp][0-9]+_[Pp][Bb][Ll][Ii]?_[Qq][0-9]+")) ~ 
            paste0("PBLI", str_extract(Question.Key, "(?i)(?<=_[Pp][Bb][Ll][Ii]?_[Qq])[0-9]+")),
          # Handle PBL without I  
          str_detect(Question.Key, regex("[Cc][Oo][Mm][Pp][0-9]+_[Pp][Bb][Ll]_[Qq][0-9]+")) ~ 
            paste0("PBL", str_extract(Question.Key, "(?i)(?<=_[Pp][Bb][Ll]_[Qq])[0-9]+")),
          TRUE ~ NA_character_
        ),
        
        # Rename score column
        Rating = as.numeric(Int.Response.Value),
        
        # Add competency category
        Competency = Report.Category
      ) %>%
      # Keep only successfully transformed sub-competencies
      filter(!is.na(Sub_Competency)) %>%
      select(Resident_Name, PGY_Level, Period, Competency, Sub_Competency, Rating, Question.Key)
    
    cat("Created evaluation_data with", nrow(evaluation_data), "rows\n")
    
  }, error = function(e) {
    cat("Error in evaluation data processing:", e$message, "\n")
    stop("Failed to create evaluation_data: ", e$message)
  })
  
  cat("Processed", nrow(evaluation_data), "milestone evaluations\n")
  
  # Check if evaluation_data was created properly
  if (nrow(evaluation_data) == 0) {
    stop("No evaluation data was processed. Check Question.Key patterns.")
  }
  
  # ========================================================================
  # CREATE SUMMARY STATISTICS
  # ========================================================================
  
  # Get unique evaluations (combinations of resident, period, competency)
  unique_evaluations <- evaluation_data %>%
    select(Resident_Name, Period, PGY_Level) %>%
    distinct()
  
  summary_stats <- list(
    total_residents = length(unique(evaluation_data$Resident_Name)),
    total_evaluations = nrow(evaluation_data),
    periods = unique(evaluation_data$Period),
    training_levels = unique(evaluation_data$PGY_Level),
    competencies = unique(evaluation_data$Competency),
    data_completeness = round((nrow(evaluation_data) / nrow(combined_data)) * 100, 1)
  )
  
  cat("\n=== PROCESSING SUMMARY ===\n")
  cat("Total residents:", summary_stats$total_residents, "\n")
  cat("Total evaluations:", summary_stats$total_evaluations, "\n")
  cat("Periods (", length(summary_stats$periods), "):", 
      paste(summary_stats$periods, collapse = ", "), "\n")
  cat("Training levels:", paste(summary_stats$training_levels, collapse = ", "), "\n")
  cat("Unique evaluation sessions:", nrow(unique_evaluations), "\n")
  cat("Data completeness:", summary_stats$data_completeness, "%\n")
  cat("============================\n")
  
  # ========================================================================
  # RETURN STRUCTURED RESULTS (MATCHING EXPECTED APP STRUCTURE)
  # ========================================================================
  
  return(list(
    raw_data = combined_data,
    evaluations = evaluation_data,
    evaluation_data = evaluation_data,
    milestone_definitions = milestone_definitions,
    milestone_structure = list(definitions = milestone_definitions),  # ADD THIS LINE
    program_info = program_info,
    summary = summary_stats
  ))
}

#' Create Dynamic App Header
#'
#' Creates a dynamic header showing the program and specialty info
#'
#' @param program_info Program information from load_milestone_csv_data
#' @return HTML div for the header
create_dynamic_header <- function(program_info) {
  
  title <- "GME Milestone Visualization Platform"
  subtitle <- paste0(program_info$specialty_name, " - ", program_info$program_name)
  
  div(class = "gmed-header",
      style = "background: linear-gradient(90deg, #2C3E50 0%, #3498DB 100%); color: white; padding: 20px; margin-bottom: 20px;",
      h1(title, style = "margin: 0; font-weight: 300; font-size: 2.5rem;"),
      p(subtitle, style = "margin: 5px 0 0 0; opacity: 0.9; font-size: 1.1rem;")
  )
}

#' Get Milestone Definition by Question Key
#'
#' Retrieves the full milestone definition text for a given question key
#'
#' @param question_key The question key (e.g., "Comp1_PC_Q1")
#' @param milestone_definitions The milestone definitions from load_milestone_csv_data
#' @return The question text or "Unknown milestone"
get_milestone_text <- function(question_key, milestone_definitions) {
  
  definition <- milestone_definitions %>%
    filter(Question.Key == question_key) %>%
    pull(Question.Text)
  
  if (length(definition) > 0) {
    return(definition[1])
  } else {
    return("Unknown milestone")
  }
}