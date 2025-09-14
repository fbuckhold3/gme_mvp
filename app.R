library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(viridis)

# =============================================================================
# DATA PROCESSING FUNCTIONS
# =============================================================================

# Function to map scores to competency levels (strict criteria)
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

# Enhanced import and processing function
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
  
  if (nrow(combined_data) == 0) {
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
  
  return(processed_data)
}

# Function to detect program characteristics
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

# Function to prepare CATEGORY AVERAGE data (clean view)
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

# Function to prepare INDIVIDUAL milestone data (detail view)
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
# VISUALIZATION FUNCTIONS
# =============================================================================

# Clean category heatmap
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
      axis.text.x = element_text(size = if(compact) 12 else 14, angle = 0),
      axis.text.y = element_text(size = if(compact) 12 else 14),
      axis.title = element_text(size = if(compact) 14 else 16, color = "#2C3E50"),
      plot.title = element_text(size = if(compact) 16 else 18, face = "bold", color = "#2C3E50"),
      legend.text = element_text(size = if(compact) 10 else 12),
      legend.title = element_text(size = if(compact) 12 else 14),
      panel.grid = element_blank()
    ) +
    labs(
      x = "Training Year",
      y = "Competency Category",
      title = if(!compact) "Competency Category Averages by Training Year" else NULL
    )
  
  return(p)
}

# Clean category progression
create_category_progression <- function(data, compact = FALSE) {
  req(data)
  
  # Color palette for categories
  category_colors <- c(
    "PC" = "#1f77b4", "MK" = "#ff7f0e", "SBP" = "#2ca02c", 
    "PBL" = "#d62728", "PROF" = "#9467bd", "ICS" = "#8c564b"
  )
  
  p <- ggplot(data, aes(x = Training_Year_Num, y = Avg_Score, color = Category)) +
    geom_line(size = 2, alpha = 0.8) +
    geom_point(size = 4, alpha = 0.9) +
    geom_ribbon(aes(ymin = Avg_Score - SE, ymax = Avg_Score + SE, fill = Category), 
                alpha = 0.2, color = NA) +
    scale_color_manual(values = category_colors) +
    scale_fill_manual(values = category_colors) +
    scale_x_continuous(breaks = unique(data$Training_Year_Num), 
                       labels = paste0("PGY-", unique(data$Training_Year_Num))) +
    scale_y_continuous(limits = c(1, 9), breaks = c(1, 3, 5, 7, 9)) +
    geom_hline(yintercept = c(1, 3, 5, 7, 9), 
               linetype = "dashed", alpha = 0.3, color = "gray") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = if(compact) 11 else 13),
      axis.title = element_text(size = if(compact) 13 else 15, color = "#2C3E50"),
      plot.title = element_text(size = if(compact) 15 else 17, face = "bold", color = "#2C3E50"),
      legend.text = element_text(size = if(compact) 11 else 13),
      legend.title = element_text(size = if(compact) 12 else 14),
      panel.grid.major = element_line(color = "#E1E5EA"),
      panel.grid.minor = element_line(color = "#F1F4F8"),
      legend.position = "right"
    ) +
    labs(
      x = "Training Year",
      y = "Average Category Score",
      color = "Competency\nCategory",
      fill = "Competency\nCategory",
      title = if(!compact) "Category Progression Across Training Years" else NULL
    )
  
  # Add competency level annotations 
  if (!compact) {
    p <- p + annotate("text", x = max(data$Training_Year_Num) + 0.4, y = c(1, 3, 5, 7, 9),
                      label = c("Beginner", "Adv. Beginner", "Competent", "Proficient", "Expert"),
                      hjust = 0, size = 3.5, color = "gray50")
  }
  
  return(p)
}

# Individual milestone detail plots
create_individual_detail_plot <- function(data, selected_category, compact = FALSE) {
  req(data, selected_category)
  
  # Filter to selected category
  category_data <- data %>% filter(Category == selected_category)
  
  if (nrow(category_data) == 0) return(NULL)
  
  # Color for the selected category
  category_colors <- c(
    "PC" = "#1f77b4", "MK" = "#ff7f0e", "SBP" = "#2ca02c", 
    "PBL" = "#d62728", "PROF" = "#9467bd", "ICS" = "#8c564b"
  )
  
  color <- category_colors[[selected_category]]
  
  p <- ggplot(category_data, aes(x = Training_Year_Num, y = Avg_Score)) +
    geom_line(aes(group = Milestone, linetype = Milestone), 
              color = color, size = 1.2, alpha = 0.8) +
    geom_point(aes(shape = Milestone), 
               color = color, size = 3, alpha = 0.9) +
    scale_x_continuous(breaks = unique(category_data$Training_Year_Num), 
                       labels = paste0("PGY-", unique(category_data$Training_Year_Num))) +
    scale_y_continuous(limits = c(1, 9), breaks = c(1, 3, 5, 7, 9)) +
    geom_hline(yintercept = c(1, 3, 5, 7, 9), 
               linetype = "dashed", alpha = 0.3, color = "gray") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = if(compact) 10 else 12),
      axis.title = element_text(size = if(compact) 12 else 14, color = "#2C3E50"),
      plot.title = element_text(size = if(compact) 14 else 16, face = "bold", color = "#2C3E50"),
      legend.text = element_text(size = if(compact) 9 else 11),
      legend.title = element_text(size = if(compact) 10 else 12),
      panel.grid.major = element_line(color = "#E1E5EA"),
      panel.grid.minor = element_line(color = "#F1F4F8")
    ) +
    labs(
      x = "Training Year",
      y = "Average Sub-Competency Score",
      linetype = "Sub-Competency",
      shape = "Sub-Competency",
      title = if(!compact) paste0(selected_category, " Individual Sub-Competencies") else NULL
    )
  
  return(p)
}

# =============================================================================
# SHINY APP
# =============================================================================

milestones_app <- function(...) {
  
  my_theme <- bs_theme(
    version = 5,
    primary = "#2C3E50",
    "bg-body" = "#F8F9FA",
    "card-bg" = "#FFFFFF",
    "font-size-base" = "0.95rem"
  )
  
  ui <- page_fluid(
    theme = my_theme,
    title = "Competency Analysis 2.0",
    style = "min-height: 100vh;",
    
    card(
      height = "80px",
      class = "mb-3",
      style = "background: linear-gradient(90deg, #2C3E50 0%, #3498DB 100%);",
      div(
        style = "color: white; padding: 15px;",
        h2("Residency/Fellowship Competency Analysis 2.0",
           style = "margin: 0; font-weight: 300;")
      )
    ),
    
    layout_columns(
      col_widths = c(3, 9),
      
      # Sidebar Panel
      card(
        full_screen = TRUE,
        class = "shadow-sm",
        card_header(
          class = "bg-light",
          "Analysis Controls"
        ),
        fileInput("files", "Upload CSV Files", 
                  multiple = TRUE,
                  accept = c(".csv")),
        
        conditionalPanel(
          condition = "output.data_loaded",
          br(),
          h5("Program Information", style = "color: #2C3E50;"),
          verbatimTextOutput("program_info", placeholder = TRUE),
          br(),
          uiOutput("period_selector"),
          br(),
          radioButtons("view_level", "Analysis Level:",
                       choices = c("Category Overview" = "category",
                                   "Individual Detail" = "individual"),
                       selected = "category"),
          
          # Category selection for individual view
          conditionalPanel(
            condition = "input.view_level == 'individual'",
            br(),
            uiOutput("category_selector")
          ),
          
          br(),
          radioButtons("visualization_type", "Primary Visualization:",
                       choices = c("Heatmap" = "heatmap",
                                   "Progression Lines" = "progression"),
                       selected = "heatmap"),
          br(),
          checkboxInput("show_summary", "Show Summary Table", value = TRUE),
          br(),
          conditionalPanel(
            condition = "output.question_index_available",
            checkboxInput("show_question_index", "Show Sub-Competency Index", value = FALSE),
            conditionalPanel(
              condition = "input.show_question_index",
              br(),
              h6("Sub-Competency Index", style = "color: #2C3E50;"),
              div(
                style = "height: 200px; overflow-y: auto; font-size: 0.85em; background-color: #f8f9fa; padding: 8px; border-radius: 4px;",
                DTOutput("question_index_table")
              )
            )
          )
        )
      ),
      
      # Main Panel
      layout_columns(
        col_widths = c(12),
        
        # Primary Visualization
        card(
          full_screen = TRUE,
          height = "500px",
          class = "shadow-sm",
          card_header(
            class = "bg-light",
            textOutput("main_plot_title")
          ),
          plotOutput("main_plot", height = "430px")
        ),
        
        # Secondary Visualizations
        layout_columns(
          col_widths = c(6, 6),
          
          # Secondary plot
          card(
            height = "400px",
            class = "shadow-sm",
            card_header(
              class = "bg-light",
              textOutput("secondary_plot_title")
            ),
            plotOutput("secondary_plot", height = "330px")
          ),
          
          # Summary Table
          conditionalPanel(
            condition = "input.show_summary",
            card(
              height = "400px",
              class = "shadow-sm",
              card_header(
                class = "bg-light",
                "Score Summary"
              ),
              div(
                style = "height: 330px; overflow-y: auto;",
                DTOutput("summary_table")
              )
            )
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    # Reactive data processing
    raw_data <- reactive({
      req(input$files)
      file_paths <- input$files$datapath
      import_and_process_milestones(file_paths)
    })
    
    # Program information detection
    program_info <- reactive({
      req(raw_data())
      detect_program_info(raw_data())
    })
    
    # Data loaded indicator
    output$data_loaded <- reactive({
      !is.null(raw_data())
    })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    # Program information display
    output$program_info <- renderText({
      req(program_info())
      info <- program_info()
      
      text <- paste0("Training Years: ", min(info$training_years), "-", max(info$training_years), 
                     " (", length(info$training_years), " levels)\n")
      text <- paste0(text, "Competency Categories: ", paste(info$milestone_categories, collapse = ", "), "\n")
      text <- paste0(text, "Individual Sub-Competencies: ", length(info$individual_milestones), "\n")
      text <- paste0(text, "Available Periods: ", length(info$periods))
      
      if (!is.null(info$programs)) {
        text <- paste0(text, "\nPrograms: ", paste(info$programs, collapse = ", "))
      }
      if (!is.null(info$specialties)) {
        text <- paste0(text, "\nSpecialties: ", paste(info$specialties, collapse = ", "))
      }
      
      text
    })
    
    # Period selector
    output$period_selector <- renderUI({
      req(program_info())
      periods <- program_info()$periods
      
      # Default to Year-End if available, otherwise first period
      default_period <- if (any(grepl("Year-End", periods, ignore.case = TRUE))) {
        periods[grepl("Year-End", periods, ignore.case = TRUE)][1]
      } else {
        periods[1]
      }
      
      selectInput("period_filter", "Evaluation Period:",
                  choices = c("All Periods" = "All", periods),
                  selected = default_period)
    })
    
    # Category selector for individual view
    output$category_selector <- renderUI({
      req(program_info())
      categories <- program_info()$milestone_categories
      
      selectInput("selected_category", "Select Category for Detail:",
                  choices = categories,
                  selected = categories[1])
    })
    
    # Filtered data
    filtered_data <- reactive({
      req(raw_data(), input$period_filter)
      df <- raw_data()
      
      if (input$period_filter != "All") {
        df <- df %>% filter(period == input$period_filter)
      }
      
      df
    })
    
    # Category summary data
    category_data <- reactive({
      req(filtered_data())
      prepare_category_summary_data(raw_data(), input$period_filter)
    })
    
    # Individual detail data
    individual_data <- reactive({
      req(filtered_data())
      prepare_individual_detail_data(raw_data(), input$period_filter)
    })
    
    # Question index availability
    output$question_index_available <- reactive({
      exists("question_index") && nrow(question_index) > 0
    })
    outputOptions(output, "question_index_available", suspendWhenHidden = FALSE)
    
    # Question index table
    output$question_index_table <- renderDT({
      if (exists("question_index") && nrow(question_index) > 0) {
        datatable(
          question_index,
          options = list(
            pageLength = 50,
            dom = 't',
            scrollY = "180px",
            scrollCollapse = TRUE,
            autoWidth = FALSE
          ),
          rownames = FALSE,
          colnames = c("Sub-Competency", "Description"),
          class = 'cell-border stripe compact'
        )
      }
    })
    
    # Plot titles
    output$main_plot_title <- renderText({
      if (input$view_level == "category") {
        if (input$visualization_type == "heatmap") {
          "Competency Category Overview"
        } else {
          "Category Progression Overview"
        }
      } else {
        if (input$visualization_type == "heatmap") {
          paste0("Individual ", input$selected_category, " Sub-Competencies")
        } else {
          paste0(input$selected_category, " Sub-Competency Progression")
        }
      }
    })
    
    output$secondary_plot_title <- renderText({
      if (input$view_level == "category") {
        if (input$visualization_type == "heatmap") {
          "Category Progression"
        } else {
          "Category Heatmap"
        }
      } else {
        "Category Overview"
      }
    })
    
    # Main plot
    output$main_plot <- renderPlot({
      if (input$view_level == "category") {
        if (input$visualization_type == "heatmap") {
          create_category_heatmap(category_data())
        } else {
          create_category_progression(category_data())
        }
      } else {
        # Individual detail view
        if (input$visualization_type == "heatmap") {
          # For individual heatmap, show just the selected category's sub-competencies
          individual_filtered <- individual_data() %>% 
            filter(Category == input$selected_category)
          
          if (nrow(individual_filtered) > 0) {
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
                title = paste0(input$selected_category, " Individual Sub-Competencies")
              )
          }
        } else {
          create_individual_detail_plot(individual_data(), input$selected_category)
        }
      }
    })
    
    # Secondary plot
    output$secondary_plot <- renderPlot({
      if (input$view_level == "category") {
        if (input$visualization_type == "heatmap") {
          create_category_progression(category_data(), compact = TRUE)
        } else {
          create_category_heatmap(category_data(), compact = TRUE)
        }
      } else {
        # Show category overview when in individual mode
        if (input$visualization_type == "heatmap") {
          create_category_progression(category_data(), compact = TRUE)
        } else {
          create_category_heatmap(category_data(), compact = TRUE)
        }
      }
    })
    
    # Summary table
    output$summary_table <- renderDT({
      if (input$view_level == "category") {
        req(category_data())
        
        summary_data <- category_data() %>%
          select(Training_Year, Category, Avg_Score, Competency_Level, Count) %>%
          mutate(Avg_Score = round(Avg_Score, 1)) %>%
          arrange(Training_Year, Category)
        
        datatable(
          summary_data,
          options = list(
            pageLength = 15,
            dom = 't',
            scrollY = "250px",
            scrollCollapse = TRUE,
            autoWidth = TRUE
          ),
          rownames = FALSE,
          colnames = c("Training Year", "Category", "Avg Score", "Competency Level", "N"),
          class = 'cell-border stripe compact'
        ) %>%
          formatStyle(
            columns = "Competency_Level",
            backgroundColor = styleEqual(
              c("Beginner", "Advanced Beginner", "Competent", "Proficient", "Expert"),
              c("#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26")
            )
          )
      } else {
        req(individual_data())
        
        # Show individual sub-competencies for selected category
        summary_data <- individual_data() %>%
          filter(Category == input$selected_category) %>%
          select(Training_Year, Milestone, Avg_Score, Competency_Level, Count) %>%
          mutate(Avg_Score = round(Avg_Score, 1)) %>%
          arrange(Training_Year, Milestone)
        
        datatable(
          summary_data,
          options = list(
            pageLength = 15,
            dom = 't',
            scrollY = "250px",
            scrollCollapse = TRUE,
            autoWidth = TRUE
          ),
          rownames = FALSE,
          colnames = c("Training Year", "Sub-Competency", "Avg Score", "Competency Level", "N"),
          class = 'cell-border stripe compact'
        ) %>%
          formatStyle(
            columns = "Competency_Level",
            backgroundColor = styleEqual(
              c("Beginner", "Advanced Beginner", "Competent", "Proficient", "Expert"),
              c("#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26")
            )
          )
      }
    })
  }
  
  shinyApp(ui, server, ...)
}

# Run the app
milestones_app()
