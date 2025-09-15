# =============================================================================
# GME MILESTONE VISUALIZATION PROJECT - HELPER FUNCTIONS (Complete rewrite using working code)
# R/helpers.R
# =============================================================================

# =============================================================================
# DATA PROCESSING FUNCTIONS (Exact working functions from original app)
# =============================================================================

#' Function to map scores to competency levels (strict criteria)
get_competency_level <- function(score) {
  case_when(
    is.na(score) ~ "Unknown",
    score < 2 ~ "Beginner",
    score < 4 ~ "Advanced Beginner", 
    score < 6 ~ "Competent",
    score > 8 ~ "Expert",      # >8 for Expert
    score > 7 ~ "Proficient",  # >7 for Proficient  
    TRUE ~ "Competent"         # Default for scores 6-7
  )
}

# =============================================================================
# GENERIC ACGME MILESTONE DATA PROCESSING FUNCTIONS
# Works with any specialty's milestone data structure
# =============================================================================

#' Import and Process Generic ACGME Milestone Data
#'
#' Processes ACGME milestone data from uploaded CSV files with flexible
#' column name handling for any specialty program.
#'
#' @param file_paths Vector of file paths to uploaded CSV files
#' @return List containing processed milestone data
#' @export
import_and_process_milestones_generic <- function(file_paths) {
  
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  cat("Processing", length(file_paths), "milestone CSV files...\n")
  
  # Read and combine all files with error handling
  combined_data <- do.call(rbind, lapply(file_paths, function(x) {
    tryCatch({
      cat("Reading:", basename(x), "\n")
      df <- read.csv(x, stringsAsFactors = FALSE)
      
      # Clean column names - handle spaces, periods, and special characters
      names(df) <- gsub("\\s+", ".", names(df))
      names(df) <- gsub("\\.+", ".", names(df))
      names(df) <- gsub("\\.$", "", names(df))
      
      cat("  Columns found:", ncol(df), "\n")
      cat("  Rows found:", nrow(df), "\n")
      
      return(df)
    }, error = function(e) {
      cat("  ERROR reading file:", basename(x), "-", e$message, "\n")
      return(NULL)
    })
  }))
  
  # Remove NULL entries and combine data
  if (is.list(combined_data)) {
    combined_data <- do.call(rbind, combined_data[!sapply(combined_data, is.null)])
  }
  
  if (is.null(combined_data) || nrow(combined_data) == 0) {
    stop("No valid data found in the uploaded files")
  }
  
  cat("Combined data:", nrow(combined_data), "rows\n")
  
  # Flexible column identification
  col_names <- names(combined_data)
  
  # Find key columns with flexible naming
  period_col <- find_column(col_names, c("Schedule.Window.Description", "Period", "Assessment.Period"))
  resident_id_col <- find_column(col_names, c("Resident.ID", "ID", "ResidentID", "Record.ID"))
  year_col <- find_column(col_names, c("Resident.Year", "Year", "Level", "Training.Year"))
  question_col <- find_column(col_names, c("Question.Key", "QuestionKey", "Milestone.Key", "Question.ID"))
  score_col <- find_column(col_names, c("Int.Response.Value", "Score", "Response.Value", "Value"))
  
  # Optional name columns
  first_name_col <- find_column(col_names, c("First.Name", "FirstName", "First"), required = FALSE)
  last_name_col <- find_column(col_names, c("Last.Name", "LastName", "Last"), required = FALSE)
  
  # Verify required columns exist
  required_cols <- list(
    Period = period_col,
    Resident_ID = resident_id_col,
    Year = year_col,
    Question = question_col,
    Score = score_col
  )
  
  missing <- names(required_cols)[sapply(required_cols, is.null)]
  if (length(missing) > 0) {
    stop("Could not identify required columns: ", paste(missing, collapse = ", "),
         "\nAvailable columns: ", paste(col_names, collapse = ", "))
  }
  
  # Create residents lookup table
  residents_cols <- c(resident_id_col, year_col)
  if (!is.null(first_name_col)) residents_cols <- c(residents_cols, first_name_col)
  if (!is.null(last_name_col)) residents_cols <- c(residents_cols, last_name_col)
  
  residents <- combined_data %>%
    select(all_of(residents_cols)) %>%
    distinct() %>%
    rename(
      record_id = !!sym(resident_id_col),
      Level = !!sym(year_col)
    )
  
  # Add name column if name columns exist
  if (!is.null(first_name_col) && !is.null(last_name_col)) {
    residents <- residents %>%
      rename(
        First.Name = !!sym(first_name_col),
        Last.Name = !!sym(last_name_col)
      ) %>%
      mutate(name = paste(First.Name, Last.Name))
  } else {
    residents <- residents %>%
      mutate(name = paste("Resident", record_id))
  }
  
  # Standardize Level format
  residents <- residents %>%
    mutate(
      record_id = as.character(record_id),
      Level = case_when(
        grepl("PGY", Level, ignore.case = TRUE) ~ as.character(Level),
        is.numeric(as.numeric(Level)) ~ paste0("PGY-", Level),
        TRUE ~ as.character(Level)
      )
    ) %>%
    select(record_id, name, Level) %>%
    arrange(name)
  
  cat("Found", nrow(residents), "unique residents\n")
  
  # Process milestone data
  milestone_data <- combined_data %>%
    # Select and rename key columns
    select(
      record_id = !!sym(resident_id_col),
      period = !!sym(period_col),
      Level = !!sym(year_col),
      Question.Key = !!sym(question_col),
      Score = !!sym(score_col)
    ) %>%
    # Filter valid data
    filter(
      !is.na(Score),
      Score > 0,
      Score <= 9,  # Standard milestone scale
      !grepl("_followup", Question.Key, ignore.case = TRUE)
    ) %>%
    # Convert data types
    mutate(
      record_id = as.character(record_id),
      Score = as.numeric(Score),
      Level = case_when(
        grepl("PGY", Level, ignore.case = TRUE) ~ as.character(Level),
        is.numeric(as.numeric(Level)) ~ paste0("PGY-", Level),
        TRUE ~ as.character(Level)
      )
    ) %>%
    # Standardize milestone question keys
    mutate(
      Question.Key = standardize_milestone_keys(Question.Key)
    ) %>%
    # Keep only recognized milestone questions
    filter(grepl("^(PC|MK|ICS|SBP|PROF|PBL|PBLI)\\d+", Question.Key)) %>%
    # Aggregate multiple responses
    group_by(record_id, Level, period, Question.Key) %>%
    summarise(Score = mean(Score, na.rm = TRUE), .groups = "drop") %>%
    # Pivot to wide format
    pivot_wider(
      names_from = Question.Key,
      values_from = Score
    ) %>%
    arrange(record_id, period)
  
  # Get milestone columns
  milestone_cols <- grep("^(PC|MK|ICS|SBP|PROF|PBL|PBLI)\\d+", names(milestone_data), value = TRUE)
  
  if (length(milestone_cols) == 0) {
    stop("No milestone columns found. Check Question.Key format in your data.")
  }
  
  cat("Found", length(milestone_cols), "milestone sub-competencies\n")
  cat("Sample milestones:", paste(head(milestone_cols, 5), collapse = ", "), "\n")
  
  # Determine competency categories
  categories <- unique(substr(milestone_cols, 1, regexpr("\\d", milestone_cols) - 1))
  cat("Competency categories:", paste(categories, collapse = ", "), "\n")
  
  return(list(
    milestone_data = milestone_data,
    residents = residents,
    milestone_columns = milestone_cols,
    competency_categories = categories,
    n_residents = nrow(residents),
    n_evaluations = nrow(milestone_data),
    n_milestones = length(milestone_cols)
  ))
}

#' Find Column Name Flexibly
#'
#' Helper function to find column names with various possible formats
#'
#' @param col_names Vector of available column names
#' @param possible_names Vector of possible column names to look for
#' @param required Whether the column is required (default TRUE)
#' @return Column name if found, NULL if not found and not required
find_column <- function(col_names, possible_names, required = TRUE) {
  
  # Try exact matches first
  for (name in possible_names) {
    if (name %in% col_names) {
      return(name)
    }
  }
  
  # Try case-insensitive matches
  for (name in possible_names) {
    match_idx <- which(tolower(col_names) == tolower(name))
    if (length(match_idx) > 0) {
      return(col_names[match_idx[1]])
    }
  }
  
  # Try partial matches
  for (name in possible_names) {
    match_idx <- grep(gsub("\\.", ".*", name), col_names, ignore.case = TRUE)
    if (length(match_idx) > 0) {
      return(col_names[match_idx[1]])
    }
  }
  
  if (required) {
    return(NULL)  # Will trigger error in calling function
  } else {
    return(NULL)
  }
}

#' Standardize Milestone Keys
#'
#' Converts various milestone question key formats to standard format
#'
#' @param keys Vector of question keys
#' @return Standardized milestone keys
standardize_milestone_keys <- function(keys) {
  
  standardized <- keys %>%
    # Patient Care: Various formats to PC#
    str_replace_all("[Cc]omp[1-9]_PC_Q(\\d+)", "PC\\1") %>%
    str_replace_all("PC[_\\.]?(\\d+)", "PC\\1") %>%
    
    # Medical Knowledge: Various formats to MK#
    str_replace_all("[Cc]omp[1-9]_MK_Q(\\d+)", "MK\\1") %>%
    str_replace_all("MK[_\\.]?(\\d+)", "MK\\1") %>%
    
    # Interpersonal Communication: Various formats to ICS#
    str_replace_all("[Cc]omp[1-9]_ICS_Q(\\d+)", "ICS\\1") %>%
    str_replace_all("ICS[_\\.]?(\\d+)", "ICS\\1") %>%
    
    # Systems-Based Practice: Various formats to SBP#
    str_replace_all("[Cc]omp[1-9]_SBP_Q(\\d+)", "SBP\\1") %>%
    str_replace_all("SBP[_\\.]?(\\d+)", "SBP\\1") %>%
    
    # Professionalism: Various formats to PROF#
    str_replace_all("[Cc]omp[1-9]_(PROF|PR)_Q(\\d+)", "PROF\\2") %>%
    str_replace_all("PROF[_\\.]?(\\d+)", "PROF\\1") %>%
    str_replace_all("PR[_\\.]?(\\d+)", "PROF\\1") %>%
    
    # Practice-Based Learning: Various formats to PBL#
    str_replace_all("[Cc]omp[1-9]_(PBL|PBLI)_Q(\\d+)", "PBL\\2") %>%
    str_replace_all("PBLI?[_\\.]?(\\d+)", "PBL\\1")
  
  return(standardized)
}

#' Calculate Medians from Generic Milestone Data
#'
#' @param processed_data Output from import_and_process_milestones_generic
#' @param verbose Print progress messages
#' @return Data frame with median calculations
calculate_milestone_medians <- function(processed_data, verbose = TRUE) {
  
  milestone_data <- processed_data$milestone_data
  milestone_cols <- processed_data$milestone_columns
  
  if (verbose) {
    cat("Calculating medians for", length(milestone_cols), "milestones\n")
  }
  
  medians <- milestone_data %>%
    group_by(period, Level) %>%
    summarise(
      across(all_of(milestone_cols), ~ median(.x, na.rm = TRUE)),
      n_residents = n(),
      .groups = "drop"
    ) %>%
    arrange(period, Level)
  
  if (verbose) {
    cat("Calculated medians for", nrow(medians), "period/level combinations\n")
  }
  
  return(medians)
}

#' Create Summary Statistics
#'
#' @param processed_data Output from import_and_process_milestones_generic
#' @return List of summary information
create_milestone_summary <- function(processed_data) {
  
  milestone_data <- processed_data$milestone_data
  residents <- processed_data$residents
  
  # Basic counts
  periods <- unique(milestone_data$period)
  levels <- unique(milestone_data$Level)
  milestone_cols <- processed_data$milestone_columns
  categories <- processed_data$competency_categories
  
  # Data completeness
  total_possible <- nrow(milestone_data) * length(milestone_cols)
  total_actual <- sum(!is.na(milestone_data[milestone_cols]))
  completeness <- round(total_actual / total_possible * 100, 1)
  
  return(list(
    n_residents = nrow(residents),
    n_evaluations = nrow(milestone_data),
    n_milestones = length(milestone_cols),
    n_periods = length(periods),
    n_levels = length(levels),
    periods = periods,
    levels = levels,
    competency_categories = categories,
    data_completeness = completeness
  ))
}

#' Function to detect program characteristics
detect_program_info <- function(df) {
  info <- list()
  
  # Detect training length
  info$max_year <- max(as.numeric(df$Resident.Year), na.rm = TRUE)
  info$min_year <- min(as.numeric(df$Resident.Year), na.rm = TRUE)
  info$training_years <- sort(unique(as.numeric(df$Resident.Year)))
  
  # Detect available periods
  info$periods <- sort(unique(df$period))
  info$has_year_end <- any(grepl("Year-End", info$periods, ignore.case = TRUE))
  info$has_mid_year <- any(grepl("Mid-Year|Midyear", info$periods, ignore.case = TRUE))
  
  # Detect program info if available
  if ("Program.Name" %in% names(df)) {
    info$programs <- unique(df$Program.Name[!is.na(df$Program.Name)])
  }
  if ("Specialty.Name" %in% names(df)) {
    info$specialties <- unique(df$Specialty.Name[!is.na(df$Specialty.Name)])
  }
  
  # Detect milestone categories
  milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(df), value = TRUE)
  info$milestone_categories <- unique(sub("\\d+$", "", milestone_cols))
  info$individual_milestones <- milestone_cols
  
  return(info)
}

#' Function to prepare CATEGORY AVERAGE data (clean view)
prepare_category_summary_data <- function(df, period_filter = "All") {
  if (period_filter != "All") {
    df <- df %>% filter(period == period_filter)
  }
  
  # Get individual milestone columns
  milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(df), value = TRUE)
  
  # Calculate averages by training year and CATEGORY (clean view)
  category_data <- df %>%
    select(Resident.Year, all_of(milestone_cols)) %>%
    pivot_longer(cols = all_of(milestone_cols), 
                 names_to = "Milestone", 
                 values_to = "Score") %>%
    filter(!is.na(Score)) %>%
    mutate(
      Category = sub("\\d+$", "", Milestone),
      Training_Year = paste0("PGY-", Resident.Year)
    ) %>%
    group_by(Training_Year, Category) %>%  # GROUP BY CATEGORY for clean view
    summarise(
      Avg_Score = mean(Score, na.rm = TRUE),
      SE = sd(Score, na.rm = TRUE) / sqrt(n()),
      Count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      Training_Year_Num = as.numeric(gsub("PGY-", "", Training_Year)),
      Competency_Level = get_competency_level(Avg_Score)
    ) %>%
    filter(!is.na(Avg_Score))
  
  return(category_data)
}

#' Function to prepare INDIVIDUAL milestone data (detail view)
prepare_individual_detail_data <- function(df, period_filter = "All") {
  if (period_filter != "All") {
    df <- df %>% filter(period == period_filter)
  }
  
  # Get individual milestone columns
  milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(df), value = TRUE)
  
  # Calculate averages by training year and INDIVIDUAL milestone
  individual_data <- df %>%
    select(Resident.Year, all_of(milestone_cols)) %>%
    pivot_longer(cols = all_of(milestone_cols), 
                 names_to = "Milestone", 
                 values_to = "Score") %>%
    filter(!is.na(Score)) %>%
    mutate(
      Category = sub("\\d+$", "", Milestone),
      Training_Year = paste0("PGY-", Resident.Year)
    ) %>%
    group_by(Training_Year, Milestone) %>%  # GROUP BY MILESTONE for detail
    summarise(
      Avg_Score = mean(Score, na.rm = TRUE),
      SE = sd(Score, na.rm = TRUE) / sqrt(n()),
      Count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      Training_Year_Num = as.numeric(gsub("PGY-", "", Training_Year)),
      Competency_Level = get_competency_level(Avg_Score),
      Category = sub("\\d+$", "", Milestone)
    ) %>%
    filter(!is.na(Avg_Score))
  
  return(individual_data)
}

# =============================================================================
# VISUALIZATION FUNCTIONS (Exact working functions from original app)
# =============================================================================

#' Clean category heatmap
create_category_heatmap <- function(data, compact = FALSE) {
  req(data)
  
  # Order categories properly
  data <- data %>%
    mutate(
      Category = factor(Category, levels = c("PC", "MK", "SBP", "PBL", "PROF", "ICS"))
    )
  
  # Create clean category heatmap
  p <- ggplot(data, aes(x = Training_Year, y = Category, fill = Avg_Score)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = paste0(round(Avg_Score, 1), "\n(", Competency_Level, ")")), 
              color = "white", size = if(compact) 4 else 6, fontface = "bold") +
    scale_fill_viridis_c(
      name = "Average\nScore",
      option = "plasma",
      begin = 0.1,
      end = 0.9,
      limits = c(1, 9),
      breaks = c(1, 3, 5, 7, 9),
      labels = c("1\n(Beginner)", "3\n(Adv. Beginner)", "5\n(Competent)", 
                 "7\n(Proficient)", "9\n(Expert)")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = if(compact) 10 else 12, angle = 0),
      axis.text.y = element_text(size = if(compact) 10 else 12),
      axis.title = element_text(size = if(compact) 12 else 14, color = "#2C3E50"),
      plot.title = element_text(size = if(compact) 14 else 16, face = "bold", color = "#2C3E50"),
      legend.text = element_text(size = if(compact) 8 else 10),
      legend.title = element_text(size = if(compact) 10 else 12),
      panel.grid = element_blank()
    ) +
    labs(
      x = "Training Year",
      y = "Competency Categories",
      title = if(!compact) "Milestone Scores by Category and Training Year" else NULL
    )
  
  return(p)
}

#' Category progression plot
create_category_progression <- function(data, compact = FALSE) {
  req(data)
  
  p <- ggplot(data, aes(x = Training_Year_Num, y = Avg_Score, color = Category, group = Category)) +
    geom_line(size = if(compact) 1 else 1.5, alpha = 0.8) +
    geom_point(size = if(compact) 2 else 3, alpha = 0.9) +
    scale_color_viridis_d(name = "Category", option = "plasma", begin = 0.1, end = 0.9) +
    scale_x_continuous(
      breaks = unique(data$Training_Year_Num),
      labels = paste0("PGY-", unique(data$Training_Year_Num))
    ) +
    scale_y_continuous(limits = c(1, 9), breaks = seq(1, 9, 2)) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = if(compact) 10 else 12),
      axis.title = element_text(size = if(compact) 12 else 14, color = "#2C3E50"),
      plot.title = element_text(size = if(compact) 14 else 16, face = "bold", color = "#2C3E50"),
      legend.text = element_text(size = if(compact) 8 else 10),
      legend.title = element_text(size = if(compact) 10 else 12),
      panel.grid.minor = element_blank()
    ) +
    labs(
      x = "Training Year",
      y = "Average Milestone Score",
      title = if(!compact) "Milestone Progression by Category" else NULL
    )
  
  return(p)
}

#' Individual milestone heatmap
create_individual_heatmap <- function(individual_filtered, selected_category) {
  ggplot(individual_filtered, aes(x = Training_Year, y = Milestone, fill = Avg_Score)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = paste0(round(Avg_Score, 1), "\n(", Competency_Level, ")")), 
              color = "white", size = 4.5, fontface = "bold") +
    scale_fill_viridis_c(
      name = "Average\nScore",
      option = "plasma",
      begin = 0.1,
      end = 0.9,
      limits = c(1, 9),
      breaks = c(1, 3, 5, 7, 9),
      labels = c("1\n(Beginner)", "3\n(Adv. Beginner)", "5\n(Competent)", 
                 "7\n(Proficient)", "9\n(Expert)")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12, angle = 0),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14, color = "#2C3E50"),
      plot.title = element_text(size = 16, face = "bold", color = "#2C3E50"),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12),
      panel.grid = element_blank()
    ) +
    labs(
      x = "Training Year",
      y = "Sub-Competencies",
      title = paste0(selected_category, " Individual Sub-Competencies")
    )
}

#' Individual detail progression plot
create_individual_detail_plot <- function(individual_data, selected_category) {
  filtered_data <- individual_data %>%
    filter(Category == selected_category)
  
  if (nrow(filtered_data) == 0) {
    return(ggplot() + 
             geom_text(aes(x = 0.5, y = 0.5, label = "No data available"), 
                       size = 6, color = "#666") +
             theme_void())
  }
  
  ggplot(filtered_data, aes(x = Training_Year_Num, y = Avg_Score, color = Milestone, group = Milestone)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(size = 3, alpha = 0.9) +
    scale_color_viridis_d(name = "Sub-Competency", option = "plasma", begin = 0.1, end = 0.9) +
    scale_x_continuous(
      breaks = unique(filtered_data$Training_Year_Num),
      labels = paste0("PGY-", unique(filtered_data$Training_Year_Num))
    ) +
    scale_y_continuous(limits = c(1, 9), breaks = seq(1, 9, 2)) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14, color = "#2C3E50"),
      plot.title = element_text(size = 16, face = "bold", color = "#2C3E50"),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12),
      panel.grid.minor = element_blank()
    ) +
    labs(
      x = "Training Year",
      y = "Average Milestone Score",
      title = paste0(selected_category, " Sub-Competency Progression")
    )
}

# ============================================================================
# IMSLU MILESTONE DATA PROCESSING FUNCTION
# Add this to your helpers.R file or create as a separate function
# ============================================================================

#' Process IMSLU Milestone Data from Uploaded CSV Files
#'
#' Processes the actual IMSLU milestone data structure from uploaded CSV files
#' and creates both resident lookup and milestone data in the format expected
#' by the GME visualization modules.
#'
#' @param file_paths Vector of file paths to uploaded CSV files
#' @return List containing residents data frame and milestone data frame
#' @export
import_and_process_milestones_with_names <- function(file_paths) {
  
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  # Read and combine all CSV files
  combined_data <- do.call(rbind, lapply(file_paths, function(x) {
    tryCatch({
      df <- read.csv(x, stringsAsFactors = FALSE)
      
      # Clean column names - handle spaces and periods
      names(df) <- gsub("\\s+", ".", names(df))
      names(df) <- gsub("\\.+", ".", names(df))
      
      # Verify required columns exist
      required_cols <- c("Schedule.Window.Description", "Resident.ID", "First.Name", 
                         "Last.Name", "Resident.Year", "Question.Key", "Int.Response.Value")
      
      missing_cols <- setdiff(required_cols, names(df))
      if (length(missing_cols) > 0) {
        warning("Missing columns in file ", basename(x), ": ", paste(missing_cols, collapse = ", "))
        # Try alternative column names
        if ("First.Name" %in% missing_cols && "First Name" %in% names(df)) {
          df$First.Name <- df$`First Name`
        }
        if ("Last.Name" %in% missing_cols && "Last Name" %in% names(df)) {
          df$Last.Name <- df$`Last Name`
        }
        # Add other column name alternatives as needed
      }
      
      return(df)
    }, error = function(e) {
      message("Error reading file ", basename(x), ": ", e$message)
      return(NULL)
    })
  }))
  
  # Remove any NULL entries from failed reads
  combined_data <- combined_data[!sapply(combined_data, is.null)]
  
  if (nrow(combined_data) == 0) {
    stop("No valid data found in the uploaded files")
  }
  
  # Create residents lookup table
  residents <- combined_data %>%
    select(Resident.ID, First.Name, Last.Name, Resident.Year) %>%
    distinct() %>%
    mutate(
      name = paste(First.Name, Last.Name),
      record_id = as.character(Resident.ID),
      Level = as.character(Resident.Year)
    ) %>%
    select(record_id, name, Level) %>%
    arrange(name)
  
  # Process milestone data
  milestone_data <- combined_data %>%
    # Filter out missing values and followup questions
    filter(
      !is.na(Int.Response.Value),
      Int.Response.Value > 0,
      !grepl("_followup", Question.Key, ignore.case = TRUE)
    ) %>%
    # Clean and standardize milestone question keys
    mutate(
      # Convert question keys to standard format (PC1, MK2, etc.)
      Question.Key = case_when(
        grepl("[Cc]omp[1-9]_PC_Q\\d+", Question.Key) ~ 
          gsub("[Cc]omp[1-9]_PC_Q(\\d+)", "PC\\1", Question.Key),
        grepl("[Cc]omp[1-9]_MK_Q\\d+", Question.Key) ~ 
          gsub("[Cc]omp[1-9]_MK_Q(\\d+)", "MK\\1", Question.Key),
        grepl("[Cc]omp[1-9]_ICS_Q\\d+", Question.Key) ~ 
          gsub("[Cc]omp[1-9]_ICS_Q(\\d+)", "ICS\\1", Question.Key),
        grepl("[Cc]omp[1-9]_SBP_Q\\d+", Question.Key) ~ 
          gsub("[Cc]omp[1-9]_SBP_Q(\\d+)", "SBP\\1", Question.Key),
        grepl("[Cc]omp[1-9]_(PROF|PR)_Q\\d+", Question.Key) ~ 
          gsub("[Cc]omp[1-9]_(PROF|PR)_Q(\\d+)", "PROF\\2", Question.Key),
        grepl("[Cc]omp[1-9]_(PBL|PBLI)_Q\\d+", Question.Key) ~ 
          gsub("[Cc]omp[1-9]_(PBL|PBLI)_Q(\\d+)", "PBL\\2", Question.Key),
        TRUE ~ Question.Key
      ),
      # Standardize period names
      period = case_when(
        grepl("Mid", Schedule.Window.Description, ignore.case = TRUE) ~ "Mid-Year",
        grepl("End", Schedule.Window.Description, ignore.case = TRUE) ~ "End-Year",
        TRUE ~ Schedule.Window.Description
      ),
      # Create record_id and Level for consistency
      record_id = as.character(Resident.ID),
      Level = as.character(Resident.Year)
    ) %>%
    # Select and rename columns
    select(record_id, Level, period, Question.Key, Int.Response.Value) %>%
    # Average multiple responses for same resident/period/question
    group_by(record_id, Level, period, Question.Key) %>%
    summarise(score = mean(Int.Response.Value, na.rm = TRUE), .groups = "drop") %>%
    # Pivot to wide format with milestones as columns
    pivot_wider(
      names_from = Question.Key,
      values_from = score,
      values_fill = NA
    ) %>%
    arrange(record_id, Level, period)
  
  # Verify that we have milestone columns
  milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(milestone_data), value = TRUE)
  if (length(milestone_cols) == 0) {
    stop("No milestone columns found in the processed data")
  }
  
  # Create summary information
  competencies <- unique(substr(milestone_cols, 1, 2))
  n_milestones <- length(milestone_cols)
  periods <- unique(milestone_data$period)
  
  message("Successfully processed IMSLU data:")
  message("- ", nrow(residents), " residents")
  message("- ", length(competencies), " competencies: ", paste(competencies, collapse = ", "))
  message("- ", n_milestones, " milestones")
  message("- ", length(periods), " periods: ", paste(periods, collapse = ", "))
  
  return(list(
    residents = residents,
    milestone_data = milestone_data,
    milestone_columns = milestone_cols,
    competencies = competencies,
    periods = periods,
    n_residents = nrow(residents),
    n_milestones = n_milestones
  ))
}