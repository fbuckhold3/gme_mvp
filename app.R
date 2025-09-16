# =============================================================================
# app.R - Fixed version with proper function sourcing
# =============================================================================

# Load all required libraries FIRST
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(stringr)

# Source the helper functions - use tryCatch to debug any issues
tryCatch({
  source("R/helpers.R")
  cat("✓ Loaded helpers.R\n")
}, error = function(e) {
  cat("✗ Error loading helpers.R:", e$message, "\n")
})

tryCatch({
  source("R/program_visualization_enhanced.R")
  cat("✓ Loaded program_visualization_enhanced.R\n")
}, error = function(e) {
  cat("✗ Error loading program_visualization_enhanced.R:", e$message, "\n")
})

tryCatch({
  source("R/individual_visualization_enhanced.R") 
  cat("✓ Loaded individual_visualization_enhanced.R\n")
}, error = function(e) {
  cat("✗ Error loading individual_visualization_enhanced.R:", e$message, "\n")
})

# Test if key functions are available
if (exists("get_performance_category")) {
  cat("✓ get_performance_category function found\n")
} else {
  cat("✗ get_performance_category function NOT found\n")
}

if (exists("create_enhanced_spider_plot")) {
  cat("✓ create_enhanced_spider_plot function found\n")
} else {
  cat("✗ create_enhanced_spider_plot function NOT found\n")
}

# Source UI and Server AFTER everything is loaded
source("R/ui.R")
source("R/server.R")

# Run the app
shinyApp(ui = ui, server = server)