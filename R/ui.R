# =============================================================================
# GME MILESTONE VISUALIZATION PROJECT - UI
# R/ui.R
# =============================================================================

# Define custom theme
my_theme <- bs_theme(
  version = 5,
  primary = "#2C3E50",
  "bg-body" = "#F8F9FA",
  "card-bg" = "#FFFFFF",
  "font-size-base" = "0.95rem"
)

ui <- page_fluid(
  theme = my_theme,
  title = "GME Milestone Visualization Project",
  style = "min-height: 100vh;",
  
  # Application Header
  card(
    height = "80px",
    class = "mb-3",
    style = "background: linear-gradient(90deg, #2C3E50 0%, #3498DB 100%);",
    div(
      style = "color: white; padding: 15px;",
      h2("GME Milestone Visualization Project",
         style = "margin: 0; font-weight: 300;")
    )
  ),
  
  # Main Layout
  layout_columns(
    col_widths = c(3, 9),
    
    # Sidebar Controls
    card(
      full_screen = TRUE,
      height = "calc(100vh - 120px)",
      class = "shadow-sm",
      card_header(
        class = "bg-light",
        "Analysis Controls"
      ),
      
      # File Upload
      fileInput("files", "Upload CSV Files", 
                multiple = TRUE,
                accept = c(".csv")),
      
      # Dynamic period selector
      uiOutput("period_selector"),
      
      br(),
      
      # View Level Selection
      radioButtons("view_level", "Analysis Level:",
                   choices = c("Category Overview" = "category",
                               "Individual Detail" = "individual"),
                   selected = "category"),
      
      # Category selection for individual view
      conditionalPanel(
        condition = "input.view_level == 'individual'",
        selectInput("selected_category", "Select Category:",
                    choices = c("PC", "MK", "SBP", "PBL", "PROF", "ICS"),
                    selected = "PC")
      ),
      
      br(),
      
      # Visualization Type
      radioButtons("visualization_type", "Primary Visualization:",
                   choices = c("Heatmap" = "heatmap",
                               "Progression Lines" = "progression"),
                   selected = "heatmap"),
      
      br(),
      
      # Additional Options
      checkboxInput("show_summary", "Show Summary Table", value = TRUE),
      
      br(),
      
      # Question Index (conditionally shown)
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
    ),
    
    # Main Content Area
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
      
      # Secondary Visualizations and Summary
      layout_columns(
        col_widths = c(6, 6),
        
        # Secondary Plot
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
              "Summary Statistics"
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