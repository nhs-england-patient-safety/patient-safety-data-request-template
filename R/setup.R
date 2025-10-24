# R/setup.R
# Load all dependencies and source code

if (!requireNamespace("renv", quietly = TRUE)) {
  stop("renv not installed. Install with: install.packages('renv')")
}

if (!dir.exists("renv/library") || length(list.files("renv/library")) == 0){
  stop("renv library empty. Run renv::init() first.")
}

# message("Loading packages...")

# library(tidyverse)
# library(dbplyr)
# library(janitor)
# library(here)
# library(openxlsx)
# library(glue)
# library(Microsoft365R)
# library(zoo)

# message("Packages loaded successfully.")

message("Loading source code...")

source("R/config/connections.R")
source("R/config/column_selection_lookups.R")
source("R/utils/functions.R") 
source("R/config/styles.R")
source("R/config/neopaeds.R")

message("Source code loaded successfully.")

message("Loading orchestrator...")

source("R/orchestrator.R")

message("\n=== All modules loaded successfully ===\n")



