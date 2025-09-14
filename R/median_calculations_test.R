# =============================================================================
# STANDALONE MEDIAN CALCULATION TESTING SCRIPT
# median_calculations_test.R - Complete rewrite
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)

# Source the helper functions from your app
source("R/helpers.R")

# =============================================================================
# SET UP FILE PATHS FOR ALL YOUR MILES DATA
# =============================================================================

# Your data folder
data_folder <- "/Users/fredbuckhold/Documents/Milestone Data"

# All possible MILES files for your 4 academic years
miles_files <- c(
  "imslu_miles_data_end_2122.csv",
  "imslu_miles_data_mid_2122.csv",
  "imslu_miles_data_end_2223.csv", 
  "imslu_miles_data_mid_2223.csv",
  "imslu_miles_data_end_2324.csv",
  "imslu_miles_data_mid_2324.csv",
  "imslu_miles_data_end_2425.csv",
  "imslu_miles_data_mid_2425.csv"
)

# Create full file paths
full_file_paths <- file.path(data_folder, miles_files)

# Check which files actually exist
existing_files <- full_file_paths[file.exists(full_file_paths)]

# Report what we found
cat("=== FILE DISCOVERY ===\n")
cat("Looking in folder:", data_folder, "\n")
cat("Found", length(existing_files), "out of", length(miles_files), "possible files:\n")

if (length(existing_files) == 0) {
  stop("ERROR: No MILES data files found! Check your folder path and file names.")
}

for (i in seq_along(existing_files)) {
  cat("  ", i, ".", basename(existing_files[i]), "\n")
}

# =============================================================================
# LOAD AND PROCESS ALL MILES DATA
# =============================================================================

cat("\n=== DATA LOADING ===\n")
cat("Processing", length(existing_files), "MILES data files...\n")

# Use your working function to process all files
processed_data <- import_and_process_milestones(existing_files)

cat("SUCCESS: Data loaded and processed!\n")
cat("Final dataset dimensions:", nrow(processed_data), "rows x", ncol(processed_data), "columns\n")

# Check what we got
unique_years <- sort(unique(processed_data$Resident.Year))
unique_periods <- sort(unique(processed_data$period))

cat("Training years found:", paste(unique_years, collapse = ", "), "\n")
cat("Evaluation periods found:", paste(unique_periods, collapse = ", "), "\n")

# =============================================================================
# COMPREHENSIVE MEDIAN CALCULATIONS
# =============================================================================

cat("\n=== MEDIAN CALCULATIONS ===\n")

# Find all milestone columns (sub-competencies)
milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)\\d+$", names(processed_data), value = TRUE)
cat("Sub-competencies found:", length(milestone_cols), "\n")
cat("Examples:", paste(head(milestone_cols, 8), collapse = ", "), "...\n")

if (length(milestone_cols) == 0) {
  stop("ERROR: No milestone columns found in processed data!")
}

# 1. OVERALL PROGRAM MEDIANS (across everything)
cat("\n1. Calculating overall program medians...\n")
overall_medians <- processed_data %>%
  select(all_of(milestone_cols)) %>%
  summarise(across(everything(), ~ median(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Sub_Competency", values_to = "Overall_Median") %>%
  mutate(
    Category = sub("\\d+$", "", Sub_Competency),
    Number = as.numeric(sub("^[A-Z]+", "", Sub_Competency))
  ) %>%
  arrange(Category, Number)

cat("   Overall medians calculated for", nrow(overall_medians), "sub-competencies\n")

# 2. PERIOD MEDIANS (Mid-Year vs Year-End)
cat("2. Calculating period medians...\n")
period_medians <- processed_data %>%
  group_by(period) %>%
  summarise(across(all_of(milestone_cols), ~ median(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = all_of(milestone_cols), names_to = "Sub_Competency", values_to = "Period_Median") %>%
  mutate(
    Category = sub("\\d+$", "", Sub_Competency),
    Number = as.numeric(sub("^[A-Z]+", "", Sub_Competency))
  ) %>%
  arrange(period, Category, Number)

cat("   Period medians calculated for", length(unique(period_medians$period)), "periods\n")

# 3. TRAINING YEAR MEDIANS
cat("3. Calculating training year medians...\n")
year_medians <- processed_data %>%
  group_by(Resident.Year) %>%
  summarise(across(all_of(milestone_cols), ~ median(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = all_of(milestone_cols), names_to = "Sub_Competency", values_to = "Year_Median") %>%
  mutate(
    Category = sub("\\d+$", "", Sub_Competency),
    Number = as.numeric(sub("^[A-Z]+", "", Sub_Competency)),
    Training_Year = paste0("PGY-", Resident.Year)
  ) %>%
  arrange(Resident.Year, Category, Number)

cat("   Year medians calculated for", length(unique(year_medians$Resident.Year)), "training years\n")

# 4. PERIOD x YEAR COMBINATION MEDIANS
cat("4. Calculating period x year combination medians...\n")
combo_medians <- processed_data %>%
  group_by(period, Resident.Year) %>%
  summarise(across(all_of(milestone_cols), ~ median(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = all_of(milestone_cols), names_to = "Sub_Competency", values_to = "Combo_Median") %>%
  mutate(
    Category = sub("\\d+$", "", Sub_Competency),
    Number = as.numeric(sub("^[A-Z]+", "", Sub_Competency)),
    Training_Year = paste0("PGY-", Resident.Year),
    Period_Year = paste(period, Training_Year)
  ) %>%
  arrange(period, Resident.Year, Category, Number)

cat("   Combination medians calculated for", length(unique(combo_medians$Period_Year)), "period-year combinations\n")

# 5. CREATE MASTER SUMMARY TABLE
cat("5. Creating master summary table...\n")
master_summary <- combo_medians %>%
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
  select(Sub_Competency, Category, Number, period, Training_Year, Period_Year,
         Combo_Median, Period_Median, Year_Median, Overall_Median) %>%
  arrange(Category, Number, Training_Year, period)

cat("   Master summary created with", nrow(master_summary), "rows\n")

# =============================================================================
# DISPLAY RESULTS
# =============================================================================

cat("\n=== SAMPLE RESULTS ===\n")

# Show overall program medians by category
cat("\n1. Overall Program Medians by Category:\n")
overall_by_category <- overall_medians %>%
  group_by(Category) %>%
  summarise(
    Count = n(),
    Min_Median = round(min(Overall_Median, na.rm = TRUE), 2),
    Max_Median = round(max(Overall_Median, na.rm = TRUE), 2),
    Avg_Median = round(mean(Overall_Median, na.rm = TRUE), 2),
    .groups = "drop"
  )
print(overall_by_category)

# Show first few sub-competencies in detail
cat("\n2. First 10 Sub-Competencies - All Median Types:\n")
sample_detail <- master_summary %>%
  head(10) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(sample_detail)

# Show one specific sub-competency across all combinations
first_sub <- milestone_cols[1]
cat("\n3. Example:", first_sub, "across all period-year combinations:\n")
example_sub <- master_summary %>%
  filter(Sub_Competency == first_sub) %>%
  select(Period_Year, Combo_Median, Period_Median, Year_Median, Overall_Median) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(example_sub)

# =============================================================================
# VALIDATION CHECKS
# =============================================================================

cat("\n=== VALIDATION ===\n")

# Check for missing values
missing_combo <- sum(is.na(master_summary$Combo_Median))
missing_period <- sum(is.na(master_summary$Period_Median))
missing_year <- sum(is.na(master_summary$Year_Median))
missing_overall <- sum(is.na(master_summary$Overall_Median))

cat("Missing values check:\n")
cat("  Combo medians:", missing_combo, "\n")
cat("  Period medians:", missing_period, "\n")
cat("  Year medians:", missing_year, "\n")
cat("  Overall medians:", missing_overall, "\n")

# Check value ranges
overall_range <- range(master_summary$Overall_Median, na.rm = TRUE)
cat("Overall median range:", round(overall_range[1], 2), "to", round(overall_range[2], 2), "\n")

# Summary stats
cat("\nFinal Summary:\n")
cat("  Total sub-competencies:", length(milestone_cols), "\n")
cat("  Categories:", paste(sort(unique(master_summary$Category)), collapse = ", "), "\n")
cat("  Training years:", paste(sort(unique(master_summary$Training_Year)), collapse = ", "), "\n")
cat("  Evaluation periods:", paste(sort(unique(master_summary$period)), collapse = ", "), "\n")
cat("  Total combinations:", nrow(master_summary), "\n")

cat("\n=== COMPLETED SUCCESSFULLY! ===\n")
cat("Results are stored in these variables:\n")
cat("  - overall_medians: Program-wide medians\n")
cat("  - period_medians: By evaluation period\n")
cat("  - year_medians: By training year\n")
cat("  - combo_medians: By period-year combination\n")
cat("  - master_summary: Everything combined\n")