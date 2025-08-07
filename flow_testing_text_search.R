library(tidyverse)
library(dbplyr)
library(janitor)
library(here)
library(openxlsx)
library(glue)
library(Microsoft365R)
library(zoo)

gc()
source("setup_params_1.R")
text_search <- "all"
source("flow.R")
rm(list=ls())


source("setup_params_1.R")
text_search <- "device"
source("flow.R")
rm(list=ls())
gc()

source("setup_params_1.R")
text_search <- "medication"
source("flow.R")
rm(list=ls())
gc()

source("setup_params_1.R")
text_search <- "base"
source("flow.R")
rm(list=ls())
gc()

