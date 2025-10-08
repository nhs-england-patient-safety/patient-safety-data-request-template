# R/setup.R
# Load all dependencies and source code

message("Loading packages...")

library(tidyverse)
library(dbplyr)
library(janitor)
library(here)
library(openxlsx)
library(glue)
library(Microsoft365R)
library(zoo)

message("Packages loaded successfully.")

message("Loading source code...")

source("R/config/connections.R")
source("R/config/column_selection_lookups.R")
source("R/utils/functions.R") 
source("R/config/styles.R")
source("R/config/neopaeds.R")

message("Source code loaded successfully.")
