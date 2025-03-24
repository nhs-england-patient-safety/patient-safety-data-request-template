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


# date filter (type is occurring/reported)
start_date <- "2023-01-01"
end_date <- "2024-12-31"

date_type <- "occurring"


# connect to (relevant) data bases and bring corresponding look ups 
source("connections.R")

#source helper files
source("functions.R") 
source("column_selection_lookups.R") # contains column names to be extracted
source("styles.R") #contains styles used by openxlsx
source("neopaeds.R") #contains search terms used to categorise incidents as neonate/paediatric


# TODO: cols to extract (all/default)
cols_to_extract <- "default"

# translate_categorical_string() can translate filters from the codes used to access the database, to more readable code.
# it splits up filters by & or | and translates each filter one by one. some examples of filters it can translate are:
#nrls_categorical <- expr((IN05_LVL1 == 10 | IN05_LVL2 == 3) & (IN04 == 1 | IN04 %in% c(97, 99) & !is.na(RP07) | is.na(IN04) & !is.na(RP07)))
#lfpse_categorical <- expr((' ' + A001 + ' ') %LIKE% '% 4 %'| ((' ' + A001 + ' ') %LIKE% '% 3 %' & (' ' + A001 + ' ') %LIKE% '% 10 %'))

# nrls categorical filters (wrap in expr() or set to 0)
nrls_categorical <- expr(IN05_LVL1 == 10 )
# lfpse categorical filters (wrap in expr() or set to 0)
lfpse_categorical <- expr((' ' + A001 + ' ') %LIKE% '% 4 %')
# steis categorical filters (wrap in expr() or set to 0)
steis_categorical <- expr(type_of_incident == 'Medication incident meeting SI criteria')
steis_filename <- 'SUI_2_87360.csv'

expanded_categorical_filter_lfpse<-translate_categorical_string(lfpse_categorical, "lfpse")
expanded_categorical_filter_nrls<-translate_categorical_string(nrls_categorical, "nrls")
expanded_categorical_filter_steis<-translate_categorical_string(steis_categorical, "steis")
message(str_glue("LFPSE filter is: \n{expanded_categorical_filter_lfpse}"))
message(str_glue("NRLS filter is: \n{expanded_categorical_filter_nrls}"))
message(str_glue("StEIS filter is: \n{expanded_categorical_filter_steis}"))

# text terms
#example below- not real example
text_terms <- list(
  group_A = c("(?i)\\bparacetamol", "(?i)\\bco(-)?codamol"),
  group_B = c("(?i)\\bibuprofen"),
  group_C = c("(?i)\\bunwell")
)

text_filter <- expr((group_A | group_B) & group_C)

#text_terms<- list()
#text_filter<- expr(1 == 1)


# neopaed logic (neonate/paed/either/none)
is_neopaed <- "neonate"

# is incident level data required? "yes" or "no"
incident_level_required<- "yes"

# create a list with an element containing for each table you would like 
# first element is what you want as rows, second is what you want as columns
# or you can just have one element
# the month, year and month-year columns will be the date type specified in date_type 
# examples:
#  list_of_tables_to_create_lfpse <- list(
#                                   c(expr(max_physical_harm_level)),
#                                   c(expr(year_reported_or_occurred),expr(month_reported_or_occurred)),
#                                   c(expr(month_year_reported_or_occurred), expr(max_physical_harm_level))
#                                   )
#  list_of_tables_to_create_steis <- list(
#                                       c(expr(type_of_incident)),
#                                       c(expr(year_reported_or_occurred),expr(month_reported_or_occurred)),
#                                       c(expr(month_year_reported_or_occurred),expr(type_of_incident))
#                                       )
# list_of_tables_to_create_nrls <- list(
#                                     c(expr(PD09)),
#                                     c(expr(year_reported_or_occurred),expr(month_reported_or_occurred)),
#                                     c(expr(month_year_reported_or_occurred), expr(PD09))
#                                     )
list_of_tables_to_create_lfpse <- list(c(expr(max_physical_harm_level)))
list_of_tables_to_create_steis <- list(c(expr(year_reported_or_occurred)))
list_of_tables_to_create_nrls <- list(c(expr(PD09)))



# sampling strategy (default/FOI/none)
# TODO: custom
sampling_strategy <- "default"


#start flow
source("flow.R")

