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

#' Enhanced import and processing function
import_and_process_milestones <- function(file_paths) {
  # Read and combine all files
  combined_data <- do.call(rbind, lapply(file_paths, function(x) {
    tryCatch({
      df <- read.csv(x, stringsAsFactors = FALSE)
      
      # Clean column names
      colnames(df) <- gsub("\\s+", ".", colnames(df))
      
      # Verify required columns exist
      required_cols <- c("Schedule.Window.Description", "Resident.ID",
                         "Resident.Year", "Question.Key", 
                         "Int.Response.Value")
      
      missing_cols <- setdiff(required_cols, names(df))
      if (length(missing_cols) > 0) {
        stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
      }
      
      # Select needed columns, add optional ones if available
      available_cols <- intersect(c(required_cols, "Program.Name", "Specialty.Name", 
                                    "Question.Text", "Response.Text"), names(df))
      df <- df[, available_cols]
      
      return(df)
    }, error = function(e) {
      message("Error reading file ", x, ": ", e$message)
      return(NULL)
    })
  }))
  
  # Remove any NULL entries from failed reads
  combined_data <- combined_data[!sapply(combined_data, is.null)]
  
  if (length(combined_data) == 0 || nrow(combined_data) == 0) {
    stop("No valid data found in the uploaded files")
  }
  
  # Create question text index if available
  if ("Question.Text" %in% names(combined_data)) {
    question_index <<- combined_data %>%
      filter(!is.na(Question.Key), !is.na(Question.Text)) %>%
      filter(!grepl("_followup", Question.Key)) %>%
      select(Question.Key, Question.Text) %>%
      distinct() %>%
      mutate(
        Milestone_Key = case_when(
          grepl("[Cc]omp[1-9]_PC_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_PC_Q(\\d+)", "PC\\1", Question.Key),
          grepl("[Cc]omp[1-9]_MK_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_MK_Q(\\d+)", "MK\\1", Question.Key),
          grepl("[Cc]omp[1-9]_ICS_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_ICS_Q(\\d+)", "ICS\\1", Question.Key),
          grepl("[Cc]omp[1-9]_SBP_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_SBP_Q(\\d+)", "SBP\\1", Question.Key),
          grepl("[Cc]omp[1-9]_(PROF|PR)_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_(PROF|PR)_Q(\\d+)", "PROF\\2", Question.Key),
          grepl("[Cc]omp[1-9]_(PBL|PBLI)_Q\\d+", Question.Key) ~ gsub("[Cc]omp[1-9]_(PBL|PBLI)_Q(\\d+)", "PBL\\2", Question.Key),
          TRUE ~ Question.Key
        )
      ) %>%
      select(Milestone_Key, Question.Text) %>%
      distinct()
  } else {
    question_index <<- data.frame(Milestone_Key = character(0), Question.Text = character(0))
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
      # Transform Question.Key to individual milestone codes
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
    select(any_of(c("Resident.ID", "Resident.Year", "period", "Question.Key", 
                    "Int.Response.Value", "Program.Name", "Specialty.Name"))) %>%
    group_by(across(c(-Int.Response.Value))) %>%
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
  
  cat("Data processing complete!\n")
  cat("Milestone columns found:", paste(milestone_cols, collapse = ", "), "\n")
  cat("Processed", nrow(processed_data), "rows\n")
  
  return(processed_data)
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