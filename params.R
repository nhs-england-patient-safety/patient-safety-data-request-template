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
start_date <- "2022-01-01"
end_date <- "2024-01-01"
date_type <- "occurring"

#type of data extract -"summary" for summary, "summary_plus_incident_level" for summary and incident level
type_of_output<- "summary_plus_incident_level"

# create a list with an element containing for each table you would like 
#first element is what you want as rows, second is what you want as columns
# or you can just have one element
# summary_categories_nrls <- list(c(expr(`PD09 Degree of harm (severity)`)),
#                                 c(expr(`Year of Incident`),expr(`Month of Incident`)))
# summary_categories_lfpse <- list(c(expr(`Largest physical or psychological harm (across all patients in incident)`)),
#                                  c(expr(`Year of Incident`),expr(`Month of Incident`)))
# summary_categories_steis <- list(c(expr(`Type of Incident`)),
#                                  c(expr(`Year of Incident`),expr(`Month of Incident`)))
summary_categories_nrls <- list(c(expr(`PD09 Degree of harm (severity)`)))
summary_categories_lfpse <- list(c(expr(`Largest physical or psychological harm (across all patients in incident)`)))
summary_categories_steis <- list()


# TODO: cols to extract (all/default)
cols_to_extract <- "default"

# nrls categorical filters (wrap in expr() or set to 0)
nrls_categorical <- 0
# lfpse categorical filters (wrap in expr() or set to 0)
lfpse_categorical <- 0
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
