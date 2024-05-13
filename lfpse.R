# lfpse

library(DBI)
library(tidyverse)
library(dbplyr)
library(here)
library(janitor)

# connection ####

con_lfpse <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("psims_server"),
                       database = Sys.getenv("lfpse_database"),
                       uid = Sys.getenv("lfpse_uid"),
                       pwd = Sys.getenv("lfpse_pwd")
)

# reading reference tables
QuestionReference <- tbl(con_lfpse, "QuestionReference") |> collect()
ResponseReference <- tbl(con_lfpse, "ResponseReference") |> collect()

analysis_table_names <- c(
  "Events",
  "Metadata_Responses",
  "Incident_Responses",
  "Risk_Responses",
  "Outcome_Responses",
  "GoodCare_Responses",
  "EventDetails_Responses ",
  "EventTime_Responses",
  "Location_Responses",
  "Patient_Responses",
  "Medication_Responses",
  "Devices_Responses",
  "Reporter_Responses",
  "Governance_Responses" # ,
  # "Findings_Responses"#, 
  # "DmdMedication_Responses"
)

analysis_tables <- lapply(analysis_table_names, function(x){
  tbl(con_lfpse, in_schema("analysis", x))
})

patient_reponses <- tbl(con_lfpse, in_schema("analysis", "patient_responses"))
data <- reduce(analysis_tables, left_join, by = c("Reference", "Revision")) |>
  group_by(Reference) |>
  mutate(ReportedDate = min(SubmissionDate)) |>
  rename(RevisionDate = SubmissionDate) |>
  filter(Revision == max(Revision)) |>
  ungroup()

# duplicates will be present due to inclusion of Patient_Responses which is one row per patient (EntityId)
data_parsed <- data |>
  # a conversion factor from days will be needed here, but appears to be DQ issues
  # suggest we wait for resolution before converting from days to years
  mutate(P004_years = as.numeric(P004) 
  #outstanding: number of person involved in incidents 
  )

# categorical filters ####

data_filtered_categorical <- data_parsed |>
  # apply categorical filters here
  filter(
    T005 == "2022-01-01"
  ) |>
  # collecting here so that we can apply text filters later
  collect()

# text filters ####

data_filtered_text <- data_filtered_categorical |>
  # apply text filters here
  filter(
    grepl("ambulance", F001, ignore.case = T)
  )

# sampling ####
# Default (if > 300: all death/severe, 100 moderate, 100 low/no harm)

if (nrow(data_filtered_text) > 300) {
  data_death_severe <- data_filtered_text |>
    # deaths or severe physical / psychological harm
    filter(OT001 %in% c("1", "2") | 
             OT002 == "1")
  
  set.seed(123)
  data_moderate <- data_filtered_text |>
    # moderate physical / psychological harm
    filter(OT001 == "3" | OT002 == "2") |>
    collect() |>
    sample_n(min(n(),100))
  
  set.seed(123)
  data_low_no_other <- data_filtered_text |>
    filter(!OT001 %in% c("1", "2", "3"),
           !OT002 %in% c("1", "2")) |>
    collect() |>
    sample_n(min(n(),100))
  
  data_sampled <- bind_rows(
    data_death_severe,
    data_moderate,
    data_low_no_other
  )
} else {
  data_sampled <- data_filtered_text
}

# columns for release ####

data_for_release <- data_sampled |>
   # pivot the coded columns
  pivot_longer(cols = any_of(ResponseReference$QuestionId)) |>
  # separate the multi-responses into single row per selection
  separate_rows(value, sep = " {~@~} ") |>
  # arrange so that multi-responses appear alphabetised later
  arrange(value) |>
  # bring through the value labels 
  left_join(ResponseReference, by = c(
    "name" = "QuestionId",
    "value" = "ResponseCode",
    "TaxonomyVersion" = "TaxonomyVersion"
  )) |>
  # remove the unnecessary columns
  select(!c(value, Property, LastUpdated, IsActive)) |>
  # pivot back into columns
  pivot_wider(
    id_cols = !any_of(ResponseReference$QuestionId),
    names_from = name,
    values_from = ResponseText,
    # collapse multi-responses into single row per entity
    values_fn = list(ResponseText = ~paste(., collapse = "; "))) |> 
  group_by(Reference) |>  
  mutate(npatient = max(EntityId)) |> 
  # select the columns for release
  select(c(
    Reference, 
    TaxonomyVersion, 
    Revision, 
    OccurredOrganisationCode, 
    ReporterOrganisationCode, 
    ReportedDate, 
    "Number of patients" = npatient, 
    "Patient no." = EntityId, 
    "T005 - Event date" = T005, 
    # TODO: check whether these are needed
    #"T005 - Event year" = year(T005), 
    #"T005 - Event moth" = month(T005),
    "F001 - Describe what happened" = F001, 
    "AC001 - What was done immediately to reduce harm caused by the event?" = AC001, 
    "OT003 - What was the clinical outcome for the patient?" = OT003, 
    "A008 - Device Type" = A008, 
    "A008 - Device Type (Other)" = A008_Other, 
    "A001 - Involved Agents" = A001, 
    "AC001 - Immediate Actions" = AC001, 
    "CL001 - Event Type" = CL001,
    "CL021 - Reference Number (Optional)" = CL021, 
    "CL022 - From Online Forms" = CL022, 
    "L001 - Organisation Known" = L001, 
    "L002 - Organisation" = L002, 
    "L003 - Service Area" = L003, 
    "L004 - Location Within Service" = L004, 
    "L006 - Specialty" = L006, 
    "L006_Other - Specialty (Other)" = L006_Other, 
    "R006 - Reporter Organisation" = R006, 
    "R006_Other - Reporter Organisation (Other)" = R006_Other, 
    "RI003 - Is there imminent risk of severe harm or death?" = RI003, 
    "OT001 - Physical harm" = OT001, 
    "OT002 - Psychological harm " = OT002, 
    "OT008 - Outcome Type" = OT008, 
    "A002 - Medicine types involved" = A002, 
    "A016 - BuildingsInfrastructure" = A016, 
    "A016_Other - BuildingsInfrastructure (other)" = A016_Other
    # TODO: add age columns once DQ issues resolved
  )) |>
  remove_empty("cols") 

write_csv(data_for_release, here("csv", "LFPSE.csv"))

