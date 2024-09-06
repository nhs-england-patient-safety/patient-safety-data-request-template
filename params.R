library(tidyverse)
library(dbplyr)
library(janitor)
library(here)
library(openxlsx)
library(glue)
library(Microsoft365R)

# datasets to be searched (T/F)
search_nrls <- T
search_lfpse <- F
search_steis <- F

# date filter (type is occurring/reported)
start_date <- "2024-01-01"
end_date <- "2024-04-01"
date_type <- "occurring"

# TODO: cols to extract (all/default)
cols_to_extract <- "default"

# nrls categorical filters (wrap in quote())

# set up filters in the form col_name == value or col_name %in% c(value1, value2, value3)
# e.g. nrls_filter_1 <- quote( IN05_LVL1 == 10)
# e.g.2 nrls_filter_2 <-quote(IN03_LVL1 %in% c(1,2,3))

vector_of_nrls_filters <- apropos("nrls_filter_")
list_of_nrls_filters <- vector_of_nrls_filters %>%
  set_names() %>%
  map(~get(.))

# combine filters using | or & 
#example: nrls_categorical = substitute( nrls_filter_1 & nrls_filter_2 & (nrls_filter_3 | nrls_filter_4 | nrls_filter_5), list_of_nrls_filters)
#example: nrls_categorical = substitute( nrls_filter_1 | nrls_filter_2 , list_of_nrls_filters)
nrls_categorical <- substitute(0 , list_of_nrls_filters)
nrls_full_string<-NA

# lfpse categorical filters (wrap in quote())
# set up filters in the form col_name == value or col_name %in% c(value1, value2, value3)
#  e.g. lfpse_filter_2 =  quote(A008 ==  "19")
#  e.g2 lfpse_filter_3 = quote( A001 %in% c("3","4","5"))
# For multi-response questions, you need to add additional filters to include values which occur in a string with other values
# str_detect does not work for SQL so you need to use LIKE (or filter after collect() step)
# lfpse_filter_2 <- quote(A001 == "4") #picks up cases where only one value selected
# lfpse_filter_3 <- quote(A001 %like% " 4 {~@~}%") #picks up cases where 4 is the first value selected
# lfpse_filter_4 <- quote(A001 %like% "%{~@~} 4 {~@~}%") # picks up cases where 4 is in between two other numbers 
# lfpse_filter_5 <- quote(A001 %like% "% {~@~} 4") # picks up case where 4 is the last value selected


vector_of_lfpse_filters <- apropos("lfpse_filter_")
list_of_lfpse_filters<-vector_of_lfpse_filters %>%
  set_names() %>%
  map(~get(.))

# combine filters using | or & - only use filters that you have populated
#example : lfpse_categorical = substitute(( lfpse_filter_1|lfpse_filter_2) & lfpse_filter_3 & lfpse_filter_4, list_of_lfpse_filters)
lfpse_categorical <- substitute(0 , list_of_lfpse_filters)
lfpse_full_string<- NA

# steis categorical filters (wrap in expr())
#steis_filter_1 <- quote(type_of_incident =="Treatment delay meeting SI criteria")
vector_of_steis_filters <- apropos("steis_filter_")
list_of_steis_filters<-vector_of_steis_filters %>%
  set_names() %>%
  map(~get(.))

#example : steis_categorical = substitute(steis_filter_1, list_of_steis_filters)
steis_categorical <- substitute(0, list_of_steis_filters)
steis_full_string<- NA
steis_filename <- ''

# text terms
text_terms <- NA

# sampling strategy (default/none)
# TODO: custom
sampling_strategy <- "default"

source("functions.R")
source("connections.R")
source("flow.R")
