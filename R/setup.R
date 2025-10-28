# R/setup.R
# Load all dependencies and source code

if (!requireNamespace("renv", quietly = TRUE)) {
  stop("renv not installed. Install with: install.packages('renv')")
}

if (!dir.exists("renv/library") || length(list.files("renv/library")) == 0){
  stop("renv library empty. Run renv::init() first.")
}

# load packages
load_packages <- function(packages) {
  message("Loading packages...")

  for (pkg in packages) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE, warn.conflicts = FALSE)
    )
  }

  message(sprintf("Loaded %d packages", length(packages)))
}

# define required packages
required_packages <- c(
  "dplyr", "stringr", "dbplyr", "tidyr",
  "lubridate", "here", "openxlsx"
)

# load packages quietly
load_packages(required_packages)

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



