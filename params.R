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

# date filter (type is occurring/reported)
start_date <- "2024-01-01"
end_date <- "2024-01-01"
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
# variable names must begin with nrls_filter_
# e.g. nrls_filter_1 <- quote( IN05_LVL1 == 10)


# combine filters using | and @ - you must use !! before each filter- example: nrls_categorical <- expr(!!nrls_filter_1 & !!nrls_filter_2)
nrls_categorical <- 0


# LFPSE filters---------------------------------------------------------------
# lfpse categorical filters in the form col_name == value or col_name %in% c(value1, value2, value3), wrapped in quote()
# variable names must begin with lfpse_filter_
# e.g lfpse_filter_3 = quote( A001 %in% c("3","4","5")
# For multi-response questions, word boundaries are not possible but this works-  quote( (' ' + A001 + ' ') %LIKE% '% 4 %'). for multiple values, you have to create multiple filters.


# combine filters using | or & - you must use !! before each filter- example: lfpse_categorical <- expr(!!lfpse_filter_1 | !!lfpse_filter_3)
lfpse_categorical <- 0


# STEIS filters and filename-----------------------------------------------------

# Add steis categorical filters in the form col_name == value or col_name %in% c(value1, value2, value3), wrapped in quote()
# variable names must begin with steis_filter_
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
