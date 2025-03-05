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
end_date <- "2024-01-31"
date_type <- "occurring"

# TODO: cols to extract (all/default)
cols_to_extract <- "default"

# Note- to allow the translate_categorical_string function to work properly, filters should be written in a specific way:
# - the code uses |, & and brackets to split the long filter into individual filters
# - filters can be combined using & or | - however | and & may not be used within a filter (i.e. A001== 3|4 would cause an error)
# - brackets can be used to create more complex logic

# nrls categorical filters (wrap in expr() or set to 0)
nrls_categorical <- expr( (DE01 == 23) & (ST04==1))
# lfpse categorical filters (wrap in expr() or set to 0)
lfpse_categorical <- expr((A001 == "3") & (A008 %in% c("1","2") & is.na(A001) & !is.na(A001) & ' ' + A001 +  ' ' %LIKE% '% 3 %') |  (!is.na(A001)))
# steis categorical filters (wrap in expr() or set to 0)
steis_categorical <- expr(type_of_incident == 'Medication incident meeting SI criteria')
steis_filename <- 'SUI_2_87360.csv'

expanded_categorical_filter_lfpse<-translate_categorical_string(lfpse_categorical, "lfpse")
expanded_categorical_filter_nrls<-translate_categorical_string(nrls_categorical, "nrls")
expanded_categorical_filter_steis<-translate_categorical_string(steis_categorical, "steis")
message("LFPSE filter is:")
message(expanded_categorical_filter_lfpse)
message("NRLS filter is:")
message(expanded_categorical_filter_nrls)
message("StEIS filter is:")
message(expanded_categorical_filter_steis)

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

source("flow.R")
