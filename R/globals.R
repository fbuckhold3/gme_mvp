# Load all required libraries FIRST
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(data.table)

# Source the helper functions
source("R/helpers.R")  # Your existing data loader
source("R/program_visualization_enhanced.R")  # New program visualizations
source("R/individual_visualization_enhanced.R")  # New individual visualizations
source("R/milestone_analysis.R")

# Source UI and Server AFTER libraries are loaded
source("R/ui.R")
source("R/server.R")

