# R/setup.R
# Project setup: check environment, load dependencies, source project files

# ==============================================================================
# CHECK RENV ENVIRONMENT ----
# ==============================================================================

#' Check that renv is properly initialised 
#' 
#' Ensures that all team members use the same package versions

check_renv <- function() {
  
  # check if renv is installed
  if (!requireNamespace("renv", quietly = TRUE)) {
    stop(
      "\n\n renv is not installed.\n",
      " Install with: install.packages('renv')\n",
      " Then run: renv::init()\n"
    )
  }
  
  # check if renv library exists and is populated
  if (!dir.exists("renv/library")) {
    stop(
      "\n\n renv library directory not found. \n",
      " Initialise renv with: renv::init()\n"
    )
  }
  
  if (length(list.files("renv/library")) == 0) {
    stop(
      "\n\n renv library is empty. \n",
      " Restore packages with renv::init()\n"
    )
  }
  
  invisible(TRUE)
}

# run renv check first (before loading any packages)
message("Checking renv environment...", appendLF = FALSE)
check_renv()
message("✓")

# ==============================================================================
# LOAD PACKAGES ----
# ==============================================================================


#' Load packages silently with error checking
#' 
#' @param packages Character vector of package names
load_packages <- function(packages) {
  message("Loading packages...", appendLF = FALSE)

  for (pkg in packages) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE, warn.conflicts = FALSE)
    )
  }
  message("✓")
  message(sprintf("Loaded %d packages", length(packages)))
}

# define required packages
required_packages <- c(
  # data wrangling
  "dplyr", "stringr", "tidyr",
  # database
  "dbplyr", 
  # dates
  "lubridate", 
  # file paths
  "here", 
  # excel output
  "openxlsx"
)

# load packages quietly
load_packages(required_packages)

# ==============================================================================
# SOURCE PROJECT FILES ----
# ==============================================================================

#' Source R files 
#' @param files Character vector of file paths
#' @param label Character. Label for group of files
source_files <- function(files, label) {
  message("Loading ", label, "...", appendLF = FALSE)
  
  for (file in files) {
    if (!file.exists(file)) {
      stop(sprintf("\n\n Required file not found: %s\n", file))
    }
    source(file, local = FALSE)
  }
  message("✓")
}

# configuration files
source_files(
  files = c(
    "R/config/connections.R",
    "R/config/column_selection_lookups.R",
    "R/config/neopaeds.R",
    "R/config/styles.R"
  ),
  label = "configuration"
)

# utility functions
source_files(
  files = c(
    "R/utils/functions.R",
    "R/utils/text_filtering.R",
    "R/utils/date_utils.R",
    "R/utils/neopaed_utils.R",
    "R/utils/sampling_utils.R",
    "R/utils/output_utils.R"
  ),
  label = "utilities"
)

# orchestrator
source_files(
  files = c(
    "R/orchestrator.R"
  ),
  label = "orchestrator"
)


message("\n=== Setup complete ===\n")
