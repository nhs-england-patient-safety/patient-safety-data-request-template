library(tidyverse)
library(dbplyr)
library(janitor)
library(here)
library(openxlsx)
library(glue)
library(Microsoft365R)

# datasets to be searched (T/F)
search_nrls <- T
search_lfpse <- T
search_steis <- T

# connect to (relevant) data bases and bring corresponding look ups 
source("connections.R")

# date filter (type is occurring/reported)
start_date <- "2024-01-01"
end_date <- "2024-01-31"
date_type <- "occurring"

# TODO: cols to extract (all/default)
cols_to_extract <- "default"

# nrls categorical filters (wrap in expr() or set to 0)
nrls_categorical <- expr(IN05_lvl1 == 10)
# lfpse categorical filters (wrap in expr() or set to 0)
lfpse_categorical <- expr(A001 == '4')

# steis categorical filters (wrap in expr() or set to 0)
steis_categorical <- expr(type_of_incident == 'Medication incident meeting SI criteria')
steis_filename <- 'SUI_2_1258.csv'

# text terms
#example below- not real example
text_terms <- list(
  group_A = c("(?i)\\bparacetamol", "(?i)\\bco(-)?codamol"),
  group_B = c("(i)\\bibuprofen"),
  group_C = c("(?i)\\bunwell")
)
 
text_filter <- expr((group_A | group_B) & group_C)
# text_terms<- list()
# text_filter<- expr(0)

# sampling strategy (default/FOI/none)
# TODO: custom
sampling_strategy <- "default"

# neopaed logic (neonate/paed/none)
is_neopaed <- "neonate"


source("flow.R")
