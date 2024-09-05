# lfpse
dataset <- "LFPSE"
print(glue("Running {dataset} search..."))

if(lfpse_categorical==0){
  lfpse_categorical <- expr(1==1)
}

# reading reference tables
QuestionReference <- tbl(con_lfpse, "QuestionReference") |> collect()
ResponseReference <- tbl(con_lfpse, "ResponseReference") |> collect()


analysis_table_names <- c(
  "Metadata_Responses",
  "Incident_Responses",
  "Risk_Responses",
  "Outcome_Responses",
  "GoodCare_Responses",
  "EventDetails_Responses",
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

#create table for all tables except events
analysis_tables_non_event <- lapply(analysis_table_names, function(x){
  tbl(con_lfpse, in_schema("analysis", x)) 
})

#create table for events, filter for most recent revision
event_table<- tbl(con_lfpse, in_schema("analysis","Events")) %>%
  group_by(Reference) |>
  mutate(reported_date = min(SubmissionDate),
         max_revision = max(Revision)) %>%
  ungroup() %>%
  filter(Revision == max_revision)

#combine event table and non event tables into one list
analysis_tables<- c(list(event_table), analysis_tables_non_event)

# duplicates will be present due to inclusion of Patient_Responses which is one row per patient (EntityId)
lfpse_parsed <- reduce(analysis_tables, left_join, by = c("Reference", "Revision")) |>
  rename(occurred_date = T005,
         revision_date = SubmissionDate) |>
  # a conversion factor from days will be needed here, but appears to be DQ issues
  # suggest we wait for resolution before converting from days to years
  mutate(P004_years = as.numeric(P004))|>
  filter(between(date_filter, start_date, end_date),
         #apply categorical filters here
         lfpse_categorical) 

#sql_render(lfpse_parsed) this is a useful step to check the SQL has rendered sensibly

lfpse_filtered_categorical <- lfpse_parsed |>
# collecting here so that we can apply text filters later
  collect()

print(glue("- {dataset} categorical filters retrieved {nrow(lfpse_filtered_categorical)} incidents."))

# text filters ####
if (!is.na(text_terms)) {
  print(glue("Running {dataset} text search..."))
  lfpse_text_filter_refs <- lfpse_filtered_categorical |>
    pivot_longer(cols = where(is.character)) |>
    filter(str_detect(value, text_terms)) |>
    pivot_wider(
      id_cols = !where(is.character),
      names_from = name,
      values_from = value
    ) |>
    distinct(Reference)
  
  lfpse_filtered_text <- lfpse_filtered_categorical |>
    filter(Reference %in% lfpse_text_filter_refs$Reference)
  
  print(glue("{dataset} text search retrieved {nrow(lfpse_filtered_text)} incidents."))
} else {
  print("- No text terms supplied. Skipping text search...")
  lfpse_filtered_text <- lfpse_filtered_categorical
}

# sampling ####
# Default (if > 300: all death/severe, 100 moderate, 100 low/no harm)
if(sampling_strategy == 'default'){
if (nrow(lfpse_filtered_text) > 300) {
  print("- Sampling according to default strategy...")
  lfpse_death_severe <- lfpse_filtered_text |>
    # deaths or severe physical / psychological harm
    filter(OT001 %in% c("1", "2") | 
             OT002 == "1")
  
  set.seed(123)
  lfpse_moderate <- lfpse_filtered_text |>
    # moderate physical / psychological harm
    filter(OT001 == "3" | OT002 == "2") |>
    collect() |>
    sample_n(min(n(),100))
  
  set.seed(123)
  lfpse_low_no_other <- lfpse_filtered_text |>
    filter(!OT001 %in% c("1", "2", "3"),
           !OT002 %in% c("1", "2")) |>
    collect() |>
    sample_n(min(n(),100))
  
  lfpse_sampled <- bind_rows(
    lfpse_death_severe,
    lfpse_moderate,
    lfpse_low_no_other
  )
} else {
  print("- Sampling not required, default threshold not met.")
  lfpse_sampled <- lfpse_filtered_text
}
} else if(sampling_strategy == 'none'){
  print("- Skipping sampling...")
  lfpse_sampled <- lfpse_filtered_text
}

# columns for release ####

lfpse_for_release <- lfpse_sampled |>
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
    reported_date, 
    "Number of patients" = npatient, 
    "Patient no." = EntityId, 
    "T005 - Event date" = occurred_date, 
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

print(glue("- Final {dataset} dataset contains {nrow(lfpse_for_release)} incidents."))

dbDisconnect(con_lfpse)

if (search_steis) {
  source("steis.R")
} else {
  source("formatter.R")
}
