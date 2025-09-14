# =============================================================================
# GME MILESTONE VISUALIZATION PROJECT - MAIN APP
# app.R
# =============================================================================

# Load required libraries
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(fmsb)
library(viridis)

# Source app components
source("R/ui.R")
source("R/server.R")
source("R/helpers.R")

# Launch the application
shinyApp(ui = ui, server = server)