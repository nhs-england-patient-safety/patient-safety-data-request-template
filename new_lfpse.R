library(tidyverse)
library(dbplyr)
library(janitor)
library(here)
library(openxlsx)
library(glue)
library(Microsoft365R)
library(zoo)
library(diffdf)

# This is required to find the minimum physical or psychological harm level
min_safe <- function(vec) {
  ifelse(length(vec[!is.na(vec)]) == 0, NA_real_, min(vec, na.rm = TRUE))
}


# datasets to be searched (T/F)
search_nrls <- F
search_lfpse <- T
search_steis <- F


# date filter (type is occurring/reported)

start_date <- "2024-01-01"
end_date <- "2024-12-31"

date_type <- "occurring"


# connect to (relevant) data bases and bring corresponding look ups 
source("connections.R")
source("column_selection_lookups.R")


#not a function- but this creates the object "date_filter" from date_type
date_filter <- if (date_type == 'occurring') {
  expr(occurred_date)
} else if (date_type == 'reported') {
  expr(reported_date)
}



# NEW ---------------------------------------------------------------------




lfpse_categorical <- expr((' ' + A001_InvolvedAgents + ' ') %LIKE% '% 4 %')



lfpse_analysis_table_new <- tbl(con_lfpse, in_schema("analysis", "vwEventsTransposedAll")) 

lfpse_filtered_categorical_new <- lfpse_analysis_table_new|>
  rename(occurred_date = OccurredDate) |>
  #mutate(reported_date = sql('CAST("reported_date" AS DATE)')) |>
  #reported date in the old method is min(submission_date) - but we have the most recent revision here, this won't work)
  #mutate(P004_days = as.numeric(P004)) 
  # column is called P004_AgeAtTimeOfIncidentDays - as numeric might not be required anymore?
  ### Apply categorical and date filters
  filter(
    between(date_filter, start_date, end_date),
    # apply categorical filters here
    lfpse_categorical
  ) |>
  
  ### all of these names will be wrong
  #select(any_of(unname(rename_lookup[["LFPSE"]])), P004_days)|> #P004_days needs to be included but is not a named column due to DQ issues
  
  ### Generate additional columns (grouping by Reference)
  
  group_by(Reference)  |>
  mutate(OT001_min= min(as.numeric(OT001_PhysicalHarm)), #calculate the worst physical harm per incident
         OT002_min= min(as.numeric(OT002_PsychologicalHarm)), # calculate the worst psychological harm per incident
         npatient = max(EntityId)) |># calculate the number of incidents
  ungroup() |>
  
  ### Collecting here so that we can apply text filters later
  collect() |>
  
  ### Generate additional columns (without grouping)
  mutate(year_reported_or_occurred = as.numeric(substr(as.character(!!date_filter), 1, 4)),
         month_reported_or_occurred = as.numeric(substr(as.character(!!date_filter), 6, 7)),
         #zoo package is used to create a year-month object because this will sort in the correct order when tabulated
         month_year_reported_or_occurred = zoo::as.yearmon(str_glue("{year_reported_or_occurred}-{month_reported_or_occurred}")),
         # create financial year while month_reported_or_occurred is still a number
         financial_year_reported_or_occurred = ifelse(month_reported_or_occurred>3,
                                                      (paste0(year_reported_or_occurred, '/', year_reported_or_occurred+1)),
                                                      paste0(year_reported_or_occurred-1,  '/', year_reported_or_occurred)
         ),
         month_reported_or_occurred= month.abb[month_reported_or_occurred],
        # reported_date = as.character(reported_date),
        #we don't have reported date
         occurred_date = as.character(occurred_date),
         OT002_min_plus_one = OT002_min + 1 #to make psychological and physical harm comparable, add 1 to psychological (as there is no fatal psychological harm)
  )|>
  
  ### Combine physical harm and psychological harm to find maximum harm (of any type)- rowwise calculation
  rowwise() |>
  mutate(max_harm= min_safe(c(OT001_min, OT002_min_plus_one))) |>
  ungroup() |>
  
  ## Label the different harm levels 
  mutate(max_harm_level= case_when(max_harm==1 ~ "Fatal",
                                   max_harm==2 ~ "Severe harm",
                                   max_harm==3 ~ "Moderate harm",
                                   max_harm==4 ~ "Low harm",
                                   max_harm==5 ~ "No harm"),
         max_physical_harm_level= case_when(OT001_min==1 ~ "Fatal",
                                            OT001_min==2 ~ "Severe physical harm",
                                            OT001_min==3 ~ "Moderate physical harm",
                                            OT001_min==4 ~ "Low physical harm",
                                            OT001_min==5 ~ "No physical harm",
                                            is.na(npatient) ~ "Not applicable",
                                            .default = "Harm level missing"),
         max_psychological_harm_level= case_when(OT002_min==1 ~ "Severe psychological harm",
                                                 OT002_min==2 ~ "Moderate psychological harm",
                                                 OT002_min==3 ~ "Low psychological harm",
                                                 OT002_min==4 ~ "No psychological harm",  
                                                 is.na(npatient) ~ "Not applicable",
                                                 .default = "Harm level missing")
  ) |>
  ### Remove columns that are not required
  select(-OT001_min,- OT002_min, -OT002_min_plus_one,#remove helper columns
         -max_harm) #remove max_harm as we do not use currently
  
  #DMD exists but not sure how - one row per item???
  #group_by(across(-starts_with("DMD"))) |>
  #summarise(across(starts_with("DMD"), ~ str_flatten(., collapse = ", "), .names = "{.col}"), .groups="drop")



# OLD ---------------------------------------------------------------------

lfpse_categorical <- expr((' ' + A001 + ' ') %LIKE% '% 4 %')


# set latest revision table from events
latest_revision_table <- tbl(con_lfpse, in_schema("analysis", "Events")) |>
  group_by(Reference) |>
  summarise(
    Revision = max(Revision), # i.e., the most up to date version of a record
    reported_date = min(SubmissionDate)
  ) |>
  ungroup()

# gather analysis tables
analysis_table_names <- c(
  "Metadata_Responses",
  "Incident_Responses",
  "Risk_Responses",
  "Outcome_Responses",
  "GoodCare_Responses",
  "EventDetails_Responses",
  "EventTime_Responses",
  "Location_Responses",
  "Events",
  "Patient_Responses",
  "Medication_Responses",
  "Devices_Responses",
  "Reporter_Responses",
  "Governance_Responses",
  # "Findings_Responses"#,
  "DmdMedication_Responses"
)

# bring all tables together
analysis_tables <- lapply(analysis_table_names, function(x) {
  table <- tbl(con_lfpse, in_schema("analysis", x))
  #Rename EntityId in DmdMedication_Responses so it has a different name to the patient EntityId column
  if(x == "DmdMedication_Responses") {table <- table |> rename(DmdEntityId = EntityId)}
  return(table)
})

lfpse_analysis_tables <- c(list(latest_revision_table), analysis_tables)

# duplicates will be present due to inclusion of Patient_Responses which is one row per patient (EntityId)
lfpse_parsed <- reduce(lfpse_analysis_tables,
                       left_join,
                       by = c("Reference", "Revision")
) |>
  rename(occurred_date = T005) |>
  mutate(reported_date = sql('CAST("reported_date" AS DATE)')) |>
  # a conversion factor from days will be needed here, but appears to be DQ issues
  # suggest we wait for resolution before converting from days to years
  mutate(P004_days = as.numeric(P004))

# sql_render(lfpse_parsed) this is a useful step to check the SQL has rendered sensibly

# record time to keep track of query speeds
tic_lfpse <- Sys.time()

lfpse_filtered_categorical <- lfpse_parsed |>
  
  ### Apply categorical and date filters
  filter(
    between(date_filter, start_date, end_date),
    # apply categorical filters here
    lfpse_categorical
  ) |>
  
  ### Select only relevant columns- use the lookup but do not rename at this step
  #to use additional columns, add them to column_selection_lookups.R
  select(any_of(unname(rename_lookup[["LFPSE"]])), P004_days)|> #P004_days needs to be included but is not a named column due to DQ issues
  
  ### Generate additional columns (grouping by Reference)
  
  group_by(Reference)  |>
  mutate(OT001_min= min(as.numeric(OT001)), #calculate the worst physical harm per incident
         OT002_min= min(as.numeric(OT002)), # calculate the worst psychological harm per incident
         npatient = max(EntityId)) |># calculate the number of incidents
  ungroup() |>
  
  ### Collecting here so that we can apply text filters later
  collect() |>
  
  ### Generate additional columns (without grouping)
  mutate(year_reported_or_occurred = as.numeric(substr(as.character(!!date_filter), 1, 4)),
         month_reported_or_occurred = as.numeric(substr(as.character(!!date_filter), 6, 7)),
         #zoo package is used to create a year-month object because this will sort in the correct order when tabulated
         month_year_reported_or_occurred = zoo::as.yearmon(str_glue("{year_reported_or_occurred}-{month_reported_or_occurred}")),
         # create financial year while month_reported_or_occurred is still a number
         financial_year_reported_or_occurred = ifelse(month_reported_or_occurred>3,
                                                      (paste0(year_reported_or_occurred, '/', year_reported_or_occurred+1)),
                                                      paste0(year_reported_or_occurred-1,  '/', year_reported_or_occurred)
         ),
         month_reported_or_occurred= month.abb[month_reported_or_occurred],
         reported_date = as.character(reported_date),
         occurred_date = as.character(occurred_date),
         OT002_min_plus_one = OT002_min + 1 #to make psychological and physical harm comparable, add 1 to psychological (as there is no fatal psychological harm)
  )|>
  
  ### Combine physical harm and psychological harm to find maximum harm (of any type)- rowwise calculation
  rowwise() |>
  mutate(max_harm= min_safe(c(OT001_min, OT002_min_plus_one))) |>
  ungroup() |>
  
  ## Label the different harm levels 
  mutate(max_harm_level= case_when(max_harm==1 ~ "Fatal",
                                   max_harm==2 ~ "Severe harm",
                                   max_harm==3 ~ "Moderate harm",
                                   max_harm==4 ~ "Low harm",
                                   max_harm==5 ~ "No harm"),
         max_physical_harm_level= case_when(OT001_min==1 ~ "Fatal",
                                            OT001_min==2 ~ "Severe physical harm",
                                            OT001_min==3 ~ "Moderate physical harm",
                                            OT001_min==4 ~ "Low physical harm",
                                            OT001_min==5 ~ "No physical harm",
                                            is.na(npatient) ~ "Not applicable",
                                            .default = "Harm level missing"),
         max_psychological_harm_level= case_when(OT002_min==1 ~ "Severe psychological harm",
                                                 OT002_min==2 ~ "Moderate psychological harm",
                                                 OT002_min==3 ~ "Low psychological harm",
                                                 OT002_min==4 ~ "No psychological harm",  
                                                 is.na(npatient) ~ "Not applicable",
                                                 .default = "Harm level missing")
  ) |>
  ### Remove columns that are not required
  select(-OT001_min,- OT002_min, -OT002_min_plus_one,#remove helper columns
         -max_harm) |> #remove max_harm as we do not use currently
  
  ### Handle row duplication brought in by the DMD table
  # this step is done after collecting because putting it before slowed down collection process substantially
  # summarise and str_flatten to combine DMD rows into comma seperated string
  group_by(across(-starts_with("DMD"))) |>
  summarise(across(starts_with("DMD"), ~ str_flatten(., collapse = ", "), .names = "{.col}"), .groups="drop")


# COMPARE -----------------------------------------------------------------

#different data for same search
nrow(lfpse_filtered_categorical_new)
nrow(lfpse_filtered_categorical)  #more in the old method

nrow(distinct(lfpse_filtered_categorical, Reference))
nrow(distinct(lfpse_filtered_categorical_new, Reference))
#looks to be because there are multiple rows per incident

#one reason is no entity id > 1 in the new method
nrow(distinct(lfpse_filtered_categorical_new, EntityId))
nrow(distinct(lfpse_filtered_categorical, EntityId))


#lets filter those out in extra patients in the old method
lfpse_filtered_categorical_entity_id_1<-lfpse_filtered_categorical %>% filter(EntityId==1) 


#but that isn't the only thing

nrow(lfpse_filtered_categorical_entity_id_1)
nrow(lfpse_filtered_categorical_new) # now there's more in the new method!

# check are there multiple references? no- same length with and without distinct() call
nrow(distinct(lfpse_filtered_categorical_new, Reference))
nrow(distinct(lfpse_filtered_categorical_entity_id_1, Reference))

#not sure what these are! 
extra_in_new <- lfpse_filtered_categorical_new %>% filter(!Reference %in% lfpse_filtered_categorical_entity_id_1$Reference)



# DMD
old_dmd <-lfpse_filtered_categorical_entity_id_1 %>%
  select(Reference, EntityId, Revision, DMD002, DMD004) %>% 
  filter(!is.na(DMD002)|!is.na(DMD004))

new_dmd <- lfpse_filtered_categorical_new %>%
  select(Reference, EntityId, Revision,DMD002 = DMD002_VTMString, DMD004 = DMD004_VMPString) %>%
  filter(!is.na(DMD002)|!is.na(DMD004))


diffdf(old_dmd, new_dmd) # it looks like just the first dmd med is present in this dataset

new_dmd %>% filter(! Reference %in% old_dmd$Reference) # some rows are present in the new database but not the old one
