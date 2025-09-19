# =============================================================================
# CLEAN DATA-DRIVEN CSV LOADER
# Extracts all structure and definitions from the CSV data itself
# =============================================================================

#' Load and Process ACGME Milestone CSV Data
#'
#' Processes ACGME milestone CSV files and extracts all milestone definitions,
#' program info, and structure directly from the data. Works with any specialty.
#'
#' @param file_paths Vector of file paths to CSV files
#' @return List containing all processed data and metadata
#' @export
load_milestone_csv_data <- function(file_paths) {
  
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  cat("Loading", length(file_paths), "CSV files...\n")
  
  # Read and combine all CSV files
  # In your load_milestone_csv_data function, replace the file reading section:
  data_list <- lapply(file_paths, function(x) {
    tryCatch({
      cat("Reading:", basename(x), "\n")
      
      # Use data.table::fread for faster reading
      if (requireNamespace("data.table", quietly = TRUE)) {
        df <- data.table::fread(x, data.table = FALSE)
      } else {
        df <- read.csv(x, stringsAsFactors = FALSE)
      }
      
      # Clean column names (your existing code)
      names(df) <- gsub("\\s+", ".", names(df))
      names(df) <- gsub("\\.+", ".", names(df))
      names(df) <- gsub("\\.$", "", names(df))
      
      cat("  Rows:", nrow(df), "Columns:", ncol(df), "\n")
      return(df)
      
    }, error = function(e) {
      cat("ERROR reading", basename(x), ":", e$message, "\n")
      return(NULL)
    })
  })
  
  # Remove NULL entries
  data_list <- data_list[!sapply(data_list, is.null)]
  
  # Combine data frames properly
  if (length(data_list) == 0) {
    stop("No valid CSV files could be read")
  }
  
  combined_data <- do.call(rbind, data_list)
  
  # Ensure we have a proper data frame
  if (!is.data.frame(combined_data)) {
    combined_data <- as.data.frame(combined_data, stringsAsFactors = FALSE)
  }
  
  # Validate combined data
  if (nrow(combined_data) == 0) {
    stop("No valid data found in uploaded files")
  }
  
  cat("Combined data:", nrow(combined_data), "total rows\n")
  cat("Column names:", paste(names(combined_data)[1:5], collapse = ", "), "...\n")
  
  # ========================================================================
  # EXTRACT PROGRAM/SPECIALTY INFORMATION (for headers and comparisons)
  # ========================================================================
  
  program_info <- combined_data %>%
    select(Program.Name, Specialty.Name, Academic.Year) %>%
    distinct() %>%
    filter(!is.na(Program.Name), !is.na(Specialty.Name))
  
  # Get primary program info (most common if multiple)
  primary_program <- program_info %>%
    count(Program.Name, Specialty.Name, Academic.Year) %>%
    slice_max(n, n = 1) %>%
    select(-n) %>%
    slice(1)  # Take only the first row if there are ties
  
  # Clean up the program info - take first values only
  program_name <- strsplit(as.character(primary_program$Program.Name), " ")[[1]][1:4] %>% 
    paste(collapse = " ")
  specialty_name <- strsplit(as.character(primary_program$Specialty.Name), " ")[[1]][1] %>% 
    paste(collapse = " ")
  academic_year <- strsplit(as.character(primary_program$Academic.Year), " ")[[1]][1]
  
  cat("Program Info:\n")
  cat("  Program:", program_name, "\n")
  cat("  Specialty:", specialty_name, "\n")
  cat("  Academic Year:", academic_year, "\n")
  
  # ========================================================================
  # EXTRACT MILESTONE DEFINITIONS (data-driven from Question.Text)
  # ========================================================================
  
  milestone_definitions <- combined_data %>%
    select(Question.Key, Report.Category, Question.Text) %>%
    filter(
      !is.na(Question.Key), 
      !is.na(Report.Category), 
      !is.na(Question.Text),
      !grepl("_followup", Question.Key, ignore.case = TRUE)  # Remove followup questions
    ) %>%
    distinct() %>%
    # Transform Question.Key: "Comp1_PC_Q2" -> "PC2"
    mutate(
      Sub_Competency = case_when(
        str_detect(Question.Key, "Comp[0-9]+_PC_Q[0-9]+") ~ 
          paste0("PC", str_extract(Question.Key, "(?<=_PC_Q)[0-9]+")),
        str_detect(Question.Key, "Comp[0-9]+_MK_Q[0-9]+") ~ 
          paste0("MK", str_extract(Question.Key, "(?<=_MK_Q)[0-9]+")),
        str_detect(Question.Key, "Comp[0-9]+_ICS_Q[0-9]+") ~ 
          paste0("ICS", str_extract(Question.Key, "(?<=_ICS_Q)[0-9]+")),
        str_detect(Question.Key, "Comp[0-9]+_SBP_Q[0-9]+") ~ 
          paste0("SBP", str_extract(Question.Key, "(?<=_SBP_Q)[0-9]+")),
        str_detect(Question.Key, "Comp[0-9]+_PROF_Q[0-9]+") ~ 
          paste0("PROF", str_extract(Question.Key, "(?<=_PROF_Q)[0-9]+")),
        str_detect(Question.Key, "Comp[0-9]+_PBL_Q[0-9]+") ~ 
          paste0("PBL", str_extract(Question.Key, "(?<=_PBL_Q)[0-9]+")),
        str_detect(Question.Key, "Comp[0-9]+_PBLI_Q[0-9]+") ~ 
          paste0("PBLI", str_extract(Question.Key, "(?<=_PBLI_Q)[0-9]+")),
        TRUE ~ NA_character_  # Mark as NA if pattern doesn't match
      )
    ) %>%
    # Rename columns as requested
    select(
      Competency = Report.Category,
      Sub_Competency = Sub_Competency,
      Milestone_Description = Question.Text,
      Original_Question_Key = Question.Key
    ) %>%
    arrange(Competency, Sub_Competency)
  
  cat("Found", nrow(milestone_definitions), "unique milestone definitions\n")
  
  # Debug: Show sample milestone definitions
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
  # PROCESS MILESTONE EVALUATION DATA
  # ========================================================================
  
  cat("Starting evaluation data processing...\n")
  
  tryCatch({
    # Filter and clean evaluation data
    evaluation_data <- combined_data %>%
      filter(
        !is.na(Int.Response.Value),
        Int.Response.Value >= 1,
        Int.Response.Value <= 9,
        !grepl("_followup", Question.Key, ignore.case = TRUE),  # Remove followup questions
        !is.na(Question.Key),
        !is.na(First.Name),
        !is.na(Last.Name)
      ) %>%
      mutate(
        # Create resident name
        Resident_Name = paste(First.Name, Last.Name),
        
        # Extract period from Schedule Window Description
        # "2024-2025 ACGME Mid-Year Milestone Evaluations" -> "2024-2025 Mid-Year"
        Period = case_when(
          grepl("Mid-Year", Schedule.Window.Description) ~ str_replace(Schedule.Window.Description, "^([0-9]{4}-[0-9]{4}).*Mid-Year.*", "\\1 Mid-Year"),
          grepl("Year-End", Schedule.Window.Description) ~ str_replace(Schedule.Window.Description, "^([0-9]{4}-[0-9]{4}).*Year-End.*", "\\1 Year-End"),
          TRUE ~ Schedule.Window.Description
        ),
        
        # Keep PGY level as is (flexible for PGY-1 through PGY-5+)
        PGY_Level = case_when(
          grepl("PGY", Resident.Year, ignore.case = TRUE) ~ as.character(Resident.Year),
          is.numeric(as.numeric(Resident.Year)) ~ paste0("PGY-", Resident.Year),
          TRUE ~ as.character(Resident.Year)
        ),
        
        # Transform Question.Key: "Comp1_PC_Q2" -> "PC2"
        Sub_Competency = case_when(
          str_detect(Question.Key, "Comp[0-9]+_PC_Q[0-9]+") ~ 
            paste0("PC", str_extract(Question.Key, "(?<=_PC_Q)[0-9]+")),
          str_detect(Question.Key, "Comp[0-9]+_MK_Q[0-9]+") ~ 
            paste0("MK", str_extract(Question.Key, "(?<=_MK_Q)[0-9]+")),
          str_detect(Question.Key, "Comp[0-9]+_ICS_Q[0-9]+") ~ 
            paste0("ICS", str_extract(Question.Key, "(?<=_ICS_Q)[0-9]+")),
          str_detect(Question.Key, "Comp[0-9]+_SBP_Q[0-9]+") ~ 
            paste0("SBP", str_extract(Question.Key, "(?<=_SBP_Q)[0-9]+")),
          str_detect(Question.Key, "Comp[0-9]+_PROF_Q[0-9]+") ~ 
            paste0("PROF", str_extract(Question.Key, "(?<=_PROF_Q)[0-9]+")),
          str_detect(Question.Key, "Comp[0-9]+_PBL_Q[0-9]+") ~ 
            paste0("PBL", str_extract(Question.Key, "(?<=_PBL_Q)[0-9]+")),
          str_detect(Question.Key, "Comp[0-9]+_PBLI_Q[0-9]+") ~ 
            paste0("PBLI", str_extract(Question.Key, "(?<=_PBLI_Q)[0-9]+")),
          TRUE ~ NA_character_  # Mark as NA if pattern doesn't match
        ),
        
        # Rename score column
        Rating = as.numeric(Int.Response.Value),
        
        # Add competency category
        Competency = Report.Category
      ) %>%
      # Debug: Check transformations
      mutate(
        debug_before = Question.Key,
        debug_after = Sub_Competency
      ) %>%
      # Keep only successfully transformed sub-competencies
      filter(!is.na(Sub_Competency)) %>%
      select(Resident_Name, PGY_Level, Period, Competency, Sub_Competency, Rating, debug_before, debug_after)
    
    cat("Created evaluation_data with", nrow(evaluation_data), "rows\n")
    
    # Debug output
    cat("Sample transformations:\n")
    sample_transforms <- evaluation_data %>%
      select(debug_before, debug_after) %>%
      distinct() %>%
      head(10)
    print(sample_transforms)
    
    # Remove debug columns for final data
    evaluation_data <- evaluation_data %>%
      select(-debug_before, -debug_after)
    
  }, error = function(e) {
    cat("Error in evaluation data processing:", e$message, "\n")
    stop("Failed to create evaluation_data: ", e$message)
  })
  
  cat("Processed", nrow(evaluation_data), "milestone evaluations\n")
  
  # Debug: Check if evaluation_data was created properly
  if (nrow(evaluation_data) == 0) {
    stop("No evaluation data was processed. Check Question.Key patterns.")
  }
  
  # ========================================================================
  # CREATE RESIDENTS DATA (unique names for dropdown)
  # ========================================================================
  
  residents <- evaluation_data %>%
    select(Resident_Name) %>%
    distinct() %>%
    arrange(Resident_Name)
  
  cat("Found", nrow(residents), "unique residents\n")
  
  # ========================================================================
  # CREATE SUMMARY STATISTICS
  # ========================================================================
  
  # Assessment periods (unique periods from evaluation data)
  periods <- unique(evaluation_data$Period) %>% sort()
  
  # Training levels (unique PGY levels)
  training_levels <- unique(evaluation_data$PGY_Level) %>% sort()
  
  # Sub-competencies per category
  milestones_per_category <- milestone_definitions %>%
    count(Competency, name = "n_milestones") %>%
    arrange(desc(n_milestones))
  
  # Unique evaluation combinations (Resident x Period x PGY)
  unique_evaluations <- evaluation_data %>%
    select(Resident_Name, PGY_Level, Period) %>%
    distinct()
  
  # Data completeness estimate
  total_possible <- nrow(residents) * nrow(milestone_definitions) * length(periods)
  total_actual <- nrow(evaluation_data)
  completeness <- round(total_actual / total_possible * 100, 1)
  
  # ========================================================================
  # COMPILE RESULTS
  # ========================================================================
  
  results <- list(
    # Program/Specialty Info
    program_info = list(
      program_name = program_name,
      specialty_name = specialty_name,
      academic_year = academic_year,
      all_programs = program_info  # For future comparisons
    ),
    
    # Milestone Structure (extracted from data)
    milestone_structure = list(
      definitions = milestone_definitions,
      competency_categories = competency_categories,
      milestones_per_category = milestones_per_category,
      total_milestones = nrow(milestone_definitions)
    ),
    
    # Resident Information
    residents = residents,
    
    # Evaluation Data
    evaluations = evaluation_data,
    
    # Summary Statistics
    summary = list(
      n_residents = nrow(residents),
      n_evaluations = nrow(evaluation_data),
      n_milestones = nrow(milestone_definitions),
      n_periods = length(periods),
      n_training_levels = length(training_levels),
      periods = periods,
      training_levels = training_levels,
      data_completeness = completeness
    ),
    
    # Metadata
    metadata = list(
      files_processed = basename(file_paths),
      processing_date = Sys.time(),
      total_csv_rows = nrow(combined_data)
    )
  )
  
  # Print summary
  cat("\n=== DATA LOADING SUMMARY ===\n")
  cat("Program:", results$program_info$program_name, "\n")
  cat("Specialty:", results$program_info$specialty_name, "\n")
  cat("Residents:", results$summary$n_residents, "\n")
  cat("Milestone evaluations:", results$summary$n_evaluations, "\n")
  cat("Unique sub-competencies:", results$summary$n_milestones, "\n")
  cat("Competency categories:", length(results$milestone_structure$competency_categories), 
      "(", paste(results$milestone_structure$competency_categories, collapse = ", "), ")\n")
  cat("Assessment periods:", length(results$summary$periods), 
      "(", paste(results$summary$periods, collapse = ", "), ")\n")
  cat("Training levels:", paste(results$summary$training_levels, collapse = ", "), "\n")
  cat("Unique evaluation sessions:", nrow(unique_evaluations), "\n")
  cat("Data completeness:", results$summary$data_completeness, "%\n")
  cat("============================\n")
  
  return(results)
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