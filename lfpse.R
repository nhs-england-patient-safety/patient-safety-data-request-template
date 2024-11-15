# lfpse
dataset <- "LFPSE"
print(glue("Running {dataset} search..."))

if (lfpse_categorical == 0) {
  lfpse_categorical <- expr(1 == 1)
}

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
  "Governance_Responses" # ,
  # "Findings_Responses"#,
  # "DmdMedication_Responses"
)

# bring all tables together
analysis_tables <- lapply(analysis_table_names, function(x) {
  tbl(con_lfpse, in_schema("analysis", x))
})

lfpse_analysis_tables <- c(list(latest_revision_table), analysis_tables)

# duplicates will be present due to inclusion of Patient_Responses which is one row per patient (EntityId)
lfpse_parsed <- reduce(lfpse_analysis_tables,
  left_join,
  by = c("Reference", "Revision")
) |>
  rename(occurred_date = T005) |>
  # a conversion factor from days will be needed here, but appears to be DQ issues
  # suggest we wait for resolution before converting from days to years
  mutate(P004_days = as.numeric(P004)) 

# sql_render(lfpse_parsed) this is a useful step to check the SQL has rendered sensibly

# record time to keep track of query speeds
tic_lfpse <- Sys.time()

lfpse_filtered_categorical <- lfpse_parsed |>
  filter(
    between(date_filter, start_date, end_date),
    # apply categorical filters here
    lfpse_categorical
  ) |>
  mutate(year_of_incident = as.numeric(substr(as.character(occurred_date), 1, 4)),
         month_of_incident = as.numeric(substr(as.character(occurred_date), 6, 7)))|>
  group_by(Reference)  |>
   mutate(OT001_min= min(as.numeric(OT001)), #calculate the worst physical harm per incident
          OT002_min= min(as.numeric(OT002)), # calculate the worst psychological harm per incident
          npatient = max(EntityId)) |>
   ungroup() |>
  # collecting here so that we can apply text filters later
  collect() |>
  mutate(month_of_incident= month.abb[month_of_incident],
         OT002_min_plus_one = OT002_min + 1 #to make psychological and physical harm comparable, add 1 to psychological (as there is no fatal psychological harm)
         )|>
  rowwise() |>
  mutate(max_harm= min_safe(c(OT001_min, OT002_min_plus_one))) |>
  ungroup() |>
  mutate(max_harm_level= case_when(max_harm==1 ~ "Fatal",
                                   max_harm==2 ~ "Severe harm",
                                   max_harm==3 ~ "Moderate harm",
                                   max_harm==4 ~ "Low harm",
                                   max_harm==5 ~ "No harm"),
         max_physical_harm_level= case_when(OT001_min==1 ~ "Fatal",
                                            OT001_min==2 ~ "Severe physical harm",
                                            OT001_min==3 ~ "Moderate physical harm",
                                            OT001_min==4 ~ "Low physical harm",
                                            OT001_min==5 ~ "No physical harm"),
         max_psychological_harm_level= case_when(OT002_min==1 ~ "Severe psychological harm",
                                                 OT002_min==2 ~ "Moderate psychological harm",
                                                 OT002_min==3 ~ "Low psychological harm",
                                                 OT002_min==4 ~ "No psychological harm")
  ) |>
  select(-OT001_min,- OT002_min, -OT002_min_plus_one, -max_harm)
  
toc_lfpse <- Sys.time()

time_diff_lfpse <- toc_lfpse - tic_lfpse

print(glue("Extraction from {dataset} server: {round(time_diff_lfpse[[1]], 2)} {attr(time_diff_lfpse, 'units')}"))

print(glue("- {dataset} categorical filters retrieved {format(nrow(lfpse_filtered_categorical), big.mark = ',')} incidents."))

# text filters ####
if (!is.na(text_terms)) {
  print(glue("Running {dataset} text search..."))

  lfpse_filtered_text <- lfpse_filtered_categorical |>
    filter(if_any(c(F001, AC001, OT003, A008_Other, A008), ~ str_detect(., text_terms)))
  # A002 may need to be added for a medication incident
  print(glue("{dataset} text search retrieved {format(nrow(lfpse_filtered_text), big.mark = ',')} incidents."))
} else {
  print("- No text terms supplied. Skipping text search...")
  lfpse_filtered_text <- lfpse_filtered_categorical
}


convert_to_for_release <- function(df){
  a=Sys.time()
  df<-df |>
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
      values_fn = list(ResponseText = ~ str_c(., collapse = "; "))
    ) |>
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
      "Month of Incident" = month_of_incident,
      "Year of Incident" = year_of_incident,
      # TODO: check whether these are needed
      # "T005 - Event year" = year(T005),
      # "T005 - Event moth" = month(T005),
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
      "OT002 - Psychological harm" = OT002,
      "OT008 - Outcome Type" = OT008,
      "A002 - Medicine types involved" = A002,
      "A016 - BuildingsInfrastructure" = A016,
      "A016_Other - BuildingsInfrastructure (other)" = A016_Other,
      "Largest physical or psychological harm (across all patients in incident)" =  max_harm_level,
      "Largest psychological harm (across all patients in incident)" =  max_psychological_harm_level,
      "Largest physical harm (across all patients in incident)" =  max_physical_harm_level,
      #keep matching information if it is present
      starts_with("match_")
      # TODO: add age columns once DQ issues resolved
    )) |>
    ungroup() |> # Added the ungroup() here, I was running into an error where I couldn't sample because the data was still grouped
    remove_empty("cols")   |>
    mutate(
      `Largest physical harm (across all patients in incident)` = fct_relevel(
        `Largest physical harm (across all patients in incident)`,
        "No physical harm",
        "Low physical harm",
        "Moderate physical harm",
        "Severe physical harm",
        "Fatal"
      ),
      `Largest psychological harm (across all patients in incident)` = fct_relevel(
        `Largest psychological harm (across all patients in incident)`,
        "No psychological harm",
        "Low psychological harm",
        "Moderate psychological harm",
        "Severe psychological harm"
      ),
      `Largest physical or psychological harm (across all patients in incident)` = fct_relevel(
        `Largest physical or psychological harm (across all patients in incident)`,
        "No harm",
        "Low harm",
        "Moderate harm",
        "Severe harm",
        "Fatal"
      ))
  print(a-Sys.time())
  return(df)
  
} 
 




# check whether the text search generated results
if (nrow(lfpse_filtered_text) != 0) {
  # sampling ####
  # Default (if > 300: all death/severe, 100 moderate, 100 low/no harm)
  if (sampling_strategy == "default") {
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
        sample_n(min(n(), 100))

      set.seed(123)
      lfpse_low_no_other <- lfpse_filtered_text |>
        filter(
          !OT001 %in% c("1", "2", "3"),
          !OT002 %in% c("1", "2")
        ) |>
        collect() |>
        sample_n(min(n(), 100))

      lfpse_sampled <- bind_rows(
        lfpse_death_severe,
        lfpse_moderate,
        lfpse_low_no_other
      )
    } else {
      print("- Sampling not required, default threshold not met.")
      lfpse_sampled <- lfpse_filtered_text
    }
  } else if (sampling_strategy == "FOI") {
    print("- Extracting a sample of 30 incidents for redaction...")
    set.seed(123)
    lfpse_sampled <- lfpse_filtered_text |>
      distinct(Reference, .keep_all = T) |>
      sample_n(min(n(), 30))
  } else if (sampling_strategy == "none") {
    print("- Skipping sampling...")
    lfpse_sampled <- lfpse_filtered_text
  }

lfpse_for_release_all <-convert_to_for_release(lfpse_filtered_text) 

if(nrow(lfpse_sampled)==nrow(lfpse_filtered_text)){
  lfpse_for_release_incident <- lfpse_for_release_all
}else{
  lfpse_for_release_incident <- convert_to_for_release(lfpse_sampled)
}

lfpse_for_release_summary <- lfpse_for_release_all %>% 
  distinct(Reference, .keep_all = TRUE) %>% 
  select(-`Patient no.`,`OT001 - Physical harm`,`OT002 - Psychological harm`)


  # columns for release ####

  print(glue("- Final summary {dataset} dataset contains {nrow(lfpse_for_release_summary)} incidents."))
  print(glue("- Final incident level {dataset} dataset contains {nrow(lfpse_for_release_incident)} incidents."))

} else {
  print(glue("**The search criteria has produced no results in {dataset}**"))
  print(glue("Moving on..."))
}

dbDisconnect(con_lfpse)

if (search_steis) {
  source("steis.R")
} else {
  source("formatter.R")
}
