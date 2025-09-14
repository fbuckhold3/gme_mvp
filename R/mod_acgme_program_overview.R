# ============================================================================
# R/mod_acgme_program_overview.R
# ACGME Program Overview Module for gmed-mvp
# ============================================================================

#' ACGME Program Overview Module UI
#'
#' Program-level analysis that adapts to any specialty's milestone structure
#'
#' @param id Module namespace ID
#' @export
mod_acgme_program_overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Controls row
    fluidRow(
      column(3,
             selectInput(ns("period_select"), "Period:",
                         choices = c("Total"), selected = "Total")
      ),
      column(3,
             selectInput(ns("year_filter"), "Resident Year:",
                         choices = list("All Years" = "all"),
                         selected = "all")
      ),
      column(3,
             radioButtons(ns("view_type"), "View:",
                          choices = list("Summary" = "summary", "Spider Plot" = "spider", 
                                         "Heat Map" = "heatmap"),
                          selected = "summary", inline = TRUE)
      ),
      column(3,
             uiOutput(ns("specialty_info"))
      )
    ),
    
    # Dynamic content based on view type
    uiOutput(ns("main_content"))
  )
}

#' ACGME Program Overview Module Server
#'
#' Server logic that adapts to discovered milestone structure
#'
#' @param id Module namespace ID  
#' @param processed_data Reactive containing output from transform_acgme_data_dynamic()
#' @export
mod_acgme_program_overview_server <- function(id, processed_data) {
  moduleServer(id, function(input, output, session) {
    
    # Update choices when data changes
    observe({
      req(processed_data())
      
      # Update period choices
      periods <- c("Total", processed_data()$periods)
      updateSelectInput(session, "period_select", choices = periods)
      
      # Update year choices based on discovered resident years
      years <- processed_data()$resident_years
      year_choices <- c("All Years" = "all")
      for (year in years) {
        year_choices[[paste("PGY", year)]] <- as.character(year)
      }
      updateSelectInput(session, "year_filter", choices = year_choices)
    })
    
    # Display specialty information
    output$specialty_info <- renderUI({
      req(processed_data())
      
      structure_info <- processed_data()$structure_info
      
      div(style = "margin-top: 25px; font-size: 0.9em;",
          tags$strong("Specialty Structure:"), tags$br(),
          paste(nrow(structure_info$competency_counts), "competencies"), tags$br(),
          paste(structure_info$total_milestones, "total milestones"), tags$br(),
          paste(length(processed_data()$resident_years), "training years")
      )
    })
    
    # Filter data based on selections
    filtered_data <- reactive({
      req(processed_data(), input$period_select, input$year_filter)
      
      milestone_data <- processed_data()$milestone_data_long
      
      if (input$period_select != "Total") {
        milestone_data <- milestone_data %>% 
          filter(prog_mile_period == input$period_select)
      }
      
      if (input$year_filter != "all") {
        milestone_data <- milestone_data %>% 
          filter(Level == input$year_filter)
      }
      
      # Return updated processed_data structure
      updated_data <- processed_data()
      updated_data$milestone_data_long <- milestone_data
      return(updated_data)
    })
    
    # Dynamic UI content
    output$main_content <- renderUI({
      req(input$view_type)
      
      if (input$view_type == "summary") {
        acgme_summary_content_ui(session$ns)
      } else if (input$view_type == "spider") {
        acgme_spider_content_ui(session$ns)
      } else if (input$view_type == "heatmap") {
        acgme_heatmap_content_ui(session$ns)
      }
    })
    
    # Summary statistics
    summary_stats <- reactive({
      req(filtered_data())
      create_dynamic_summary_stats(filtered_data(), input$period_select)
    })
    
    # Performance tables
    performance_tables <- reactive({
      req(filtered_data())
      create_dynamic_performance_tables(filtered_data(), input$period_select)
    })
    
    # Summary view outputs
    output$total_residents <- renderText({
      req(summary_stats())
      as.character(summary_stats()$total_residents)
    })
    
    output$avg_performance <- renderText({
      req(summary_stats())
      as.character(summary_stats()$overall_median)
    })
    
    output$below_benchmark <- renderText({
      req(summary_stats())
      paste0(summary_stats()$below_benchmark_pct, "%")
    })
    
    output$above_target <- renderText({
      req(summary_stats())
      paste0(summary_stats()$above_target_pct, "%")
    })
    
    output$total_competencies <- renderText({
      req(summary_stats())
      as.character(summary_stats()$total_competencies)
    })
    
    output$total_milestones <- renderText({
      req(summary_stats())
      as.character(summary_stats()$total_milestones)
    })
    
    # Performance tables
    output$strengths_table <- DT::renderDataTable({
      req(performance_tables())
      performance_tables()$strengths
    }, options = list(dom = 't', pageLength = 10, scrollX = TRUE))
    
    output$improvements_table <- DT::renderDataTable({
      req(performance_tables())
      performance_tables()$improvements
    }, options = list(dom = 't', pageLength = 10, scrollX = TRUE))
    
    output$subcompetency_table <- DT::renderDataTable({
      req(performance_tables())
      performance_tables()$subcompetency_details
    }, options = list(pageLength = 15, scrollX = TRUE))
    
    # Competency breakdown table
    output$competency_breakdown <- DT::renderDataTable({
      req(summary_stats())
      summary_stats()$competency_breakdown %>%
        rename(
          `Competency Code` = competency_code,
          `Competency Name` = competency_name,
          `Sub-competencies` = n_subcompetencies,
          `Range` = subcompetency_range
        )
    }, options = list(dom = 't', pageLength = 10))
    
    # Spider plot
    output$program_spider <- renderPlotly({
      req(processed_data())
      create_dynamic_spider_plot(
        processed_data(), 
        period_filter = input$period_select,
        selected_years = if(input$year_filter == "all") {
          as.character(processed_data()$resident_years)
        } else {
          input$year_filter
        }
      )
    })
    
    # Heat map with metric selector
    output$competency_heatmap <- renderPlotly({
      req(processed_data())
      metric <- if(exists("input$heatmap_metric") && !is.null(input$heatmap_metric)) {
        input$heatmap_metric
      } else {
        "median"
      }
      create_dynamic_heatmap(processed_data(), input$period_select, metric)
    })
  })
}

# ============================================================================
# HELPER UI FUNCTIONS
# ============================================================================

#' Summary Content UI Helper
acgme_summary_content_ui <- function(ns) {
  fluidRow(
    # Key metrics cards
    column(2, div(class = "gmed-stat-card",
                  h3(textOutput(ns("total_residents")), class = "text-primary"),
                  p("Total Residents"))),
    column(2, div(class = "gmed-stat-card", 
                  h3(textOutput(ns("avg_performance")), class = "text-success"),
                  p("Median Score"))),
    column(2, div(class = "gmed-stat-card",
                  h3(textOutput(ns("below_benchmark")), class = "text-warning"), 
                  p("Below Benchmark"))),
    column(2, div(class = "gmed-stat-card",
                  h3(textOutput(ns("above_target")), class = "text-info"),
                  p("Above Target"))),
    column(2, div(class = "gmed-stat-card",
                  h3(textOutput(ns("total_competencies")), class = "text-secondary"),
                  p("Competencies"))),
    column(2, div(class = "gmed-stat-card",
                  h3(textOutput(ns("total_milestones")), class = "text-secondary"),
                  p("Milestones"))),
    
    # Competency structure breakdown
    column(6, div(class = "gmed-card mt-3",
                  div(class = "gmed-card-header", h5("📊 Competency Structure")),
                  DT::dataTableOutput(ns("competency_breakdown")))),
    
    # Performance tables
    column(6, div(class = "gmed-card mt-3",
                  div(class = "gmed-card-header", h5("🎯 Strengths & Improvements")),
                  tags$h6("Strengths", class = "text-success"),
                  DT::dataTableOutput(ns("strengths_table")),
                  tags$h6("Improvement Areas", class = "text-warning mt-3"),
                  DT::dataTableOutput(ns("improvements_table")))),
    
    # Sub-competency details
    column(12, div(class = "gmed-card mt-3",
                   div(class = "gmed-card-header", h5("📋 Sub-competency Details")),
                   DT::dataTableOutput(ns("subcompetency_table"))))
  )
}

#' Spider Plot Content UI Helper
acgme_spider_content_ui <- function(ns) {
  fluidRow(
    column(9, 
           div(class = "gmed-card",
               div(class = "gmed-card-header", h5("Program Performance Spider Plot")),
               plotlyOutput(ns("program_spider"), height = "500px"))),
    column(3,
           div(class = "gmed-card",
               div(class = "gmed-card-header", h5("Legend & Info")),
               tags$div(
                 tags$p("Year-based comparison showing median scores by competency"),
                 tags$hr(),
                 tags$h6("Scale:"),
                 tags$p("1-3: Developing", style = "margin: 2px 0;"),
                 tags$p("4-5: Competent", style = "margin: 2px 0;"), 
                 tags$p("6-7: Proficient", style = "margin: 2px 0;"),
                 tags$p("8-9: Expert", style = "margin: 2px 0;")
               )))
  )
}

#' Heat Map Content UI Helper
acgme_heatmap_content_ui <- function(ns) {
  fluidRow(
    column(9,
           div(class = "gmed-card",
               div(class = "gmed-card-header", h5("Milestone Performance Heat Map")),
               plotlyOutput(ns("competency_heatmap"), height = "600px"))),
    column(3,
           div(class = "gmed-card",
               div(class = "gmed-card-header", h5("Heat Map Options")),
               radioButtons(ns("heatmap_metric"), "Display Metric:",
                            choices = list(
                              "Median Score" = "median",
                              "Mean Score" = "mean",
                              "Below Benchmark %" = "below_benchmark",
                              "Above Target %" = "above_target"
                            ),
                            selected = "median"),
               tags$hr(),
               tags$h6("Color Legend:"),
               tags$div(
                 style = "background: linear-gradient(to right, #d32f2f, #fff, #2e7d32); height: 20px; border-radius: 4px;"
               ),
               div(class = "d-flex justify-content-between mt-1",
                   tags$small("Low"),
                   tags$small("Average"), 
                   tags$small("High")
               )))
  )
}