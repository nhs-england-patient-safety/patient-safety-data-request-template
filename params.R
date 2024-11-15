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
search_steis <- F

# connect to (relevant) data bases and bring corresponding look ups 
source("connections.R")

# date filter (type is occurring/reported)
start_date <- "2024-01-01"
end_date <- "2024-03-01"
date_type <- "occurring"

#type of data extract -"summary" for summary, "summary_plus_incident_level" for summary and incident level
type_of_output<- "summary_plus_incident_level"

# create a list with an element containing for each table you would like 
#first element is what you want as rows, second is what you want as columns
# or you can just have one element
#Year and Month variables can be used
#see lfpse_for_summary_table, nrls_for_summary_table and steis_for_summary_table
 # summary_categories_nrls <- list(c(expr(`MD02 Med Error Category`)),
 #                                  c(expr(`PD09 Degree of harm (severity)`), expr(Year)))
 # summary_categories_lfpse <- list(c(expr(`CL001 - Event Type`)),
 #                                  c(expr(`Largest harm`), expr(Year)),
 #                                  c(expr(`OT001 - Largest Physical harm`), expr(Year)),
 #                                   c(expr(`OT002 - Largest Psychological harm`), expr(Year)),
 #                                   c(expr(`A001 - Involved Agents`)))
 # summary_categories_steis <- list(c(expr(`Type of Incident`)))
summary_categories_nrls <- list(c(expr(0)))
summary_categories_lfpse <- list(c(expr(0)))
summary_categories_steis <- list(c(expr(0)))


# TODO: cols to extract (all/default)
cols_to_extract <- "default"

# nrls categorical filters (wrap in expr() or set to 0)
nrls_categorical <- expr(0)
# lfpse categorical filters (wrap in expr() or set to 0)
lfpse_categorical <- expr(0)
# steis categorical filters (wrap in expr() or set to 0)
steis_categorical <- 0
steis_filename <- ''

# text terms
text_terms <- NA

# sampling strategy (default/FOI/none)
# TODO: custom
sampling_strategy <- "default"

source("functions.R")
source("flow.R")
