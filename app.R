# =============================================================================
# GME MILESTONE VISUALIZATION PROJECT - MAIN APP (CORRECTED)
# app.R
# =============================================================================

# Load required libraries
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(DT)
library(purrr)
library(ggplot2)
library(fmsb)
library(viridis)

# Source existing gmed components
source("R/helpers.R")

# Try to source existing modules (with error handling for missing files)
tryCatch(source("R/milestone_functions.R"), error = function(e) message("milestone_functions.R not found"))
tryCatch(source("R/ui_components.R"), error = function(e) message("ui_components.R not found"))
tryCatch(source("R/load_rdm_simple.R"), error = function(e) message("load_rdm_simple.R not found"))
tryCatch(source("R/assessment_viz_module.R"), error = function(e) message("assessment_viz_module.R not found"))
tryCatch(source("R/mod_plus_delta_table.R"), error = function(e) message("mod_plus_delta_table.R not found"))
tryCatch(source("R/milestone_module.R"), error = function(e) message("milestone_module.R not found"))

# Source new ACGME modules
source("R/acgme_dynamic_functions.R")
source("R/acgme_visualization_functions.R")  # This needs to be fixed
source("R/mod_acgme_program_overview.R")

# Source UI and Server
source("R/ui.R")
source("R/server.R")

# Launch the application
shinyApp(ui = ui, server = server)