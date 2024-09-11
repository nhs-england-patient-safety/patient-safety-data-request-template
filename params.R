library(tidyverse)
library(dbplyr)
library(janitor)
library(here)
library(openxlsx)
library(glue)
library(Microsoft365R)
library(readxl)

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


# text terms
text_terms <- NA

# sampling strategy (default/none)
# TODO: custom
sampling_strategy <- "default"

# NRLS filters -------------------------------------------------------------
# Add nrls categorical filters in the form col_name == value or col_name %in% c(value1, value2, value3), wrapped in quote()
# e.g. nrls_filter_1 <- quote( IN05_LVL1 == 10)



# combine filters using | and @ - you must use !! before each filter- example: nrls_categorical <- expr(!!nrls_filter_1 & !!nrls_filter_2)
nrls_categorical <- 0



# LFPSE filters---------------------------------------------------------------
# lfpse categorical filters in the form col_name == value or col_name %in% c(value1, value2, value3), wrapped in quote()
# e.g lfpse_filter_3 = quote( A001 %in% c("3","4","5"))


# For multi-response questions, you need to add additional filters to include values which occur in a string with other values
# str_detect does not work for SQL so you need to use LIKE (or filter after collect() step)
# lfpse_filter_2 <- quote(A001 == "4") #picks up cases where only one value selected
# lfpse_filter_3 <- quote(A001 %like% " 4 {~@~}%") #picks up cases where 4 is the first value selected
# lfpse_filter_4 <- quote(A001 %like% "%{~@~} 4 {~@~}%") # picks up cases where 4 is in between two other numbers 
# lfpse_filter_5 <- quote(A001 %like% "% {~@~} 4") # picks up case where 4 is the last value selected




# combine filters using | or & - you must use !! before each filter- example: lfpse_categorical <- expr(!!lfpse_filter_1 | !!lfpse_filter_3)
lfpse_categorical <- 0



# STEIS filters and filename-----------------------------------------------------

# Add steis categorical filters in the form col_name == value or col_name %in% c(value1, value2, value3), wrapped in quote()
# e.g. steis_filter_1 <- quote(type_of_incident =="Treatment delay meeting SI criteria")



# combine filters using | and @ - you must use !! before each filter- example: steis_categorical <- expr(!!steis_filter_1 | !!steis_filter_2)
steis_categorical <- 0



# steis # steis # steis categorical filters (wrap in expr())
#steis_filter_1 <- quote(type_of_incident =="Treatment delay meeting SI criteria")


# combine filters using | or & - you must use !! before each filter
#example: 
steis_filename <- ''





# Flow  -------------------------------------------------------------------
source("functions.R")
source("connections.R")
source("flow.R")
