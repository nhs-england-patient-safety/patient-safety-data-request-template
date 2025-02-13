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
source("functions.R")

# date filter (type is occurring/reported)
start_date <- "2024-01-01"
end_date <- "2024-12-31"
date_type <- "occurring"

# TODO: cols to extract (all/default)
cols_to_extract <- "default"

# nrls categorical filters (wrap in expr() or set to 0)
nrls_categorical <- expr(IN05_LVL1 == 10)
# lfpse categorical filters (wrap in expr() or set to 0)
lfpse_categorical <- expr(A001 == '4')
# steis categorical filters (wrap in expr() or set to 0)
steis_categorical <- expr(type_of_incident == 'Medication incident meeting SI criteria')
steis_filename <- 'SUI_2_87360.csv'

# text terms
#example below- not real example
text_terms <- list(
  group_A = c("(?i)\\bparacetamol", "(?i)\\bco(-)?codamol"),
  group_B = c("(i)\\bibuprofen"),
  group_C = c("(?i)\\bunwell")
)
 
text_filter <- expr((group_A | group_B) & group_C)

#text_terms<- list()
#text_filter<- expr(0)

# is incident level data required? "yes" or "no"
incident_level_required<- "yes"

# create a list with an element containing for each table you would like 
#first element is what you want as rows, second is what you want as columns
# or you can just have one element
# summary_categories_nrls <- list(c(expr(PD09)),
#                                 c(expr(year_of_incident),expr(month_of_incident)))
# summary_categories_lfpse <- list(c(expr(`Largest physical harm (across all patients in incident)`)),
#                                  c(expr(`Year of Incident`),expr(`Month of Incident`)))
# summary_categories_steis <- list(c(expr(`Type of Incident`)),
#                                  c(expr(`Year of Incident`),expr(`Month of Incident`)))
summary_categories_lfpse <- list(c(expr(`Largest physical harm (across all patients in incident)`)))
summary_categories_steis <- list(c(expr(`Year of Incident`)))

#summary_categories_nrls <- list(c(expr(PD09)))

# sampling strategy (default/FOI/none)
# TODO: custom
sampling_strategy <- "default"

source("flow.R")
