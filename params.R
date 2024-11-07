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

#type of data extract - "data" for full tables, "summary" for summary, "both" for both
#Note: if "both" is selected, any sampling will apply to both the data and summary tables 
type_of_output<- "data"

# create a list with an element containing for each table you would like 
#first element is what you want as rows, second is what you want as columns
# or you can just have one element
#Year and Month variables can be used
# summary_categories_nrls <- list(c(expr(`MD02 Med Error Category`)),
#                                 c(expr(`PD09 Degree of harm (severity)`), expr(Year)))
# summary_categories_lfpse <- list(c(expr(`CL001 - Event Type`)),
#                                  c(expr(`OT001 - Physical harm`), expr(Year)))
# summary_categories_steis <- list(c(expr(`Type of Incident`)))

summary_categories_nrls <- list(c(expr(0)))
summary_categories_lfpse <- list(c(expr(0)))
summary_categories_steis <- list(c(expr(0)))


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
text_terms <- "(?i)\\boxbr(y|i)ta|\\bvoxelotor"

# sampling strategy (default/FOI/none)
# TODO: custom
sampling_strategy <- "FOI"

source("flow.R")
