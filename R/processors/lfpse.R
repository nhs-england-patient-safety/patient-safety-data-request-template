# R/processors/lfpse.R

dataset <- "LFPSE"
log_dataset_start(dataset)

if (lfpse_categorical == 0) {
  lfpse_categorical <- expr(1 == 1)
}

# set latest revision table from events
latest_revision_table <- tbl(con_lfpse, in_schema("analysis", "Events")) |>
  group_by(Reference) |>
  summarise(
    Revision = max(Revision),
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
  "DmdMedication_Responses"
)

# bring all tables together
analysis_tables <- lapply(analysis_table_names, function(x) {
  table <- tbl(con_lfpse, in_schema("analysis", x))
  # Rename EntityId in DmdMedication_Responses
  if(x == "DmdMedication_Responses") {
    table <- table |> rename(DmdEntityId = EntityId)
  }
  return(table)
})

lfpse_analysis_tables <- c(list(latest_revision_table), analysis_tables)

# join all tables
lfpse_parsed <- purrr::reduce(lfpse_analysis_tables,
                              left_join,
                              by = c("Reference", "Revision")
) |>
  rename(occurred_date = T005) |>
  mutate(reported_date = sql('CAST("reported_date" AS DATE)')) |>
  mutate(P004_days = as.numeric(P004))

# record time
tic_lfpse <- Sys.time()

lfpse_filtered_categorical <- lfpse_parsed |>
  ### Apply categorical and date filters
  filter(
    between(date_filter, start_date, end_date),
    lfpse_categorical
  ) |>
  # select relevant columns - use the lookup but do not rename at this step to use
  # additional columns, add them to R/config/column_selection_lookups.R
  select(any_of(unname(rename_lookup[["LFPSE"]])), P004_days) |>
  ### Generate additional columns (grouping by Reference)
  group_by(Reference)  |>
  mutate(
    OT001_min = min(as.numeric(OT001)), # calculate the worst physical harm per incident
    OT002_min = min(as.numeric(OT002)), # calculate the worst psychological harm per incident
    npatient = max(EntityId) # calculate the number of incidents
  ) |>
  ungroup() |>
  ### Collect
  collect() |>
  ### Generate date columns
  add_date_columns_lfpse(date_filter) |>
  mutate(
    reported_date = as.character(reported_date),
    occurred_date = as.character(occurred_date),
    # to make psychological and physical harm comparable, add 1 to psychological 
    # (as there is no fatal psychological harm)
    OT002_min_plus_one = OT002_min + 1 
  ) |>
  ### Combine physical and psychological harm
  rowwise() |>
  mutate(max_harm = min_safe(c(OT001_min, OT002_min_plus_one))) |>
  ungroup() |>
  ### Label harm levels
  mutate(
    max_harm_level = case_when(
      max_harm == 1 ~ "Fatal",
      max_harm == 2 ~ "Severe harm",
      max_harm == 3 ~ "Moderate harm",
      max_harm == 4 ~ "Low harm",
      max_harm == 5 ~ "No harm"
    ),
    max_physical_harm_level = case_when(
      OT001_min == 1 ~ "Fatal",
      OT001_min == 2 ~ "Severe physical harm",
      OT001_min == 3 ~ "Moderate physical harm",
      OT001_min == 4 ~ "Low physical harm",
      OT001_min == 5 ~ "No physical harm",
      is.na(npatient) ~ "Not applicable",
      .default = "Harm level missing"
    ),
    max_psychological_harm_level = case_when(
      OT002_min == 1 ~ "Severe psychological harm",
      OT002_min == 2 ~ "Moderate psychological harm",
      OT002_min == 3 ~ "Low psychological harm",
      OT002_min == 4 ~ "No psychological harm",  
      is.na(npatient) ~ "Not applicable",
      .default = "Harm level missing"
    )
  ) |>
  ### Remove helper columns
  select(-OT001_min, -OT002_min, -OT002_min_plus_one, -max_harm) |>
  ### Handle DMD table duplication
  group_by(across(-starts_with("DMD"))) |>
  summarise(across(starts_with("DMD"), ~ str_flatten(., collapse = ", "), .names = "{.col}"), .groups="drop")

toc_lfpse <- Sys.time()
time_diff_lfpse <- toc_lfpse - tic_lfpse
log_extraction_time(dataset, time_diff_lfpse)
log_categorical_filter_count(dataset, nrow(lfpse_filtered_categorical))

# text filters
lfpse_text_columns <- c("F001", "AC001", "OT003", "A008_Other", "A008", "A002", "DMD002", "DMD004")
lfpse_filtered_text <- apply_text_search(
  lfpse_filtered_categorical,
  text_terms,
  text_filter,
  lfpse_text_columns,
  dataset
)

# Check for empty results
if (check_and_log_empty_result(lfpse_filtered_text, dataset, "text")) {
  dbDisconnect(con_lfpse)
  if (search_steis) {
    source("R/processors/steis.R")
  } else {
    source("R/output/formatter.R")
  }
} else {
  
  lfpse_labelled<- lfpse_filtered_text |>
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
    ) 
  
  # create a new column for age following validation 
  lfpse_age_validated<- lfpse_labelled |>
    mutate(age_unit = case_when(
      is.na(P004_days) ~ 'age missing',
      between(P004_days, 1, 30) ~ 'days',
      between(P004_days, 31, 371) ~ 'months',
      between(P004_days, 372, 74028) ~ 'years',
      .default = 'age outside bounds')) |>
    mutate(age_compliance = case_when(
      age_unit == 'age outside bounds' ~ 'age outside bounds',
      age_unit == 'age missing' ~ 'age missing',
      age_unit == 'days' & between(P004_days, 1, 30) ~ 'yes',
      age_unit == 'months' & P004_days %% 31 == 0 ~ 'yes',
      age_unit == 'years' & P004_days %% 372 == 0 ~ 'yes',
      .default = 'no')) |>
    mutate(P004_days_validated = if_else(
      age_compliance == "yes", P004_days, NA
    ))
  
  # age classification for neopaeds
  lfpse_age_classified <- lfpse_age_validated |>
    mutate(
      concat_col = paste(F001, AC001, OT003, A008_Other, L006, L006_Other, sep = "_"),
      age_category = case_when(
        (P004_days_validated > 0 & P004_days_validated <= 28) | 
          (P007 %in% c("0-14 days", "15-28 days")) ~ "neonate",
        (P004_days_validated > 28 & P004_days_validated < 6696) | 
          (P007 %in% c("1-11 months", "1-4 years", "5-9 years", "10-15 years", "16 and 17 years")) ~ "paediatric",
        (!is.na(P007) | !is.na(P004_days_validated)) ~ 'adult estimated',
        is.na(P004_days_validated) ~ 'unknown',# includes those where age is below zero / above believable threshold
        .default = 'other' 
      ),
      
      neonate_specialty_flag = str_detect(L006, neonatal_specialty_terms),
      neonate_terms_flag = str_detect(concat_col, neonatal_terms),
      missing_specialty = is.na(L006),
      no_adult_specialty_flag = str_detect(L006, adult_specialty_terms, negate = TRUE), 
      paediatric_specialty_flag = str_detect(L006, paediatric_specialty_terms),
      paediatric_term_flag = str_detect(concat_col, paediatric_terms),
      
      neonate_category = case_when(
        age_category == 'neonate' ~ "neonate_by_age",
        neonate_specialty_flag ~ "neonate_by_specialty",
        (neonate_terms_flag & (no_adult_specialty_flag | missing_specialty)) ~ "neonate_by_text",
        .default = "not neonate related"
      ),
      
      paediatric_category = case_when(
        age_category == 'paediatric' ~ "paediatric_by_age",
        paediatric_specialty_flag ~ "paediatric_by_specialty",
        (paediatric_term_flag & (no_adult_specialty_flag | missing_specialty)) ~ "paediatric_by_text",
        .default = "not paediatric related"
      )
    )
  
  # Apply neopaed filter
  lfpse_neopaed <- filter_by_neopaed_strategy(lfpse_age_classified, is_neopaed)
  
  # Check for empty results
  if (check_and_log_empty_result(lfpse_neopaed, dataset, "neopaed")) {
    dbDisconnect(con_lfpse)
    if (search_steis) {
      source("R/processors/steis.R")
    } else {
      source("R/output/formatter.R")
    }
  } else {
    
    # Sampling
    lfpse_sampled <- apply_sampling_strategy(
      lfpse_neopaed,
      sampling_strategy,
      harm_column = "OT001",
      death_severe_values = c("Fatal", "Severe physical harm"),
      moderate_values = c("Moderate physical harm"),
      reference_column = "Reference"
    )
    
    # Rename columns
    lfpse_neopaed <- lfpse_neopaed |>
      select(any_of(rename_lookup[["LFPSE"]]), starts_with("group_")) 
    
    lfpse_sampled <- lfpse_sampled |>
      select(any_of(rename_lookup[["LFPSE"]]), starts_with("group_")) 
    
    # Create patient level tables
    lfpse_for_release_sampled_pt_level <- lfpse_sampled |> 
      select(!c(contains("_term_"), `Month`, `Year`, `Month - Year`)) 
    
    lfpse_for_release_unsampled_pt_level <- lfpse_neopaed |> 
      select(!c(contains("_term_"), `Month`, `Year`, `Month - Year`))
    
    # Get data for summary tables
    lfpse_for_summary_table_unsampled <- lfpse_neopaed  
    lfpse_for_summary_table_sampled <- lfpse_sampled  
    
    # Handle incident vs patient level for summary tables
    if (summary_tables_incident_or_patient_level == "incident") {
      lfpse_for_summary_table_unsampled <- lfpse_for_summary_table_unsampled |>
        # remove columns that contain patient specific info (for summary tables)
        select(-any_of(c("Patient no.",
                         "OT001 - Physical harm",
                         "OT002 - Psychological harm",
                         "P004 - Age in days", 
                         "P007 - Age Range",
                         "OT003 - What was the clinical outcome for the patient?"
        ))) |> 
        distinct(Reference, .keep_all = TRUE)
      
      lfpse_for_summary_table_sampled <- lfpse_for_summary_table_sampled |>
        select(-any_of(c("Patient no.",
                         "OT001 - Physical harm",
                         "OT002 - Psychological harm",
                         "P004 - Age in days", 
                         "P007 - Age Range",
                         "OT003 - What was the clinical outcome for the patient?"
        ))) |> 
        distinct(Reference, .keep_all = TRUE)
    }
    
    # Log final counts
    log_final_counts(
      dataset,
      lfpse_for_summary_table_unsampled,
      lfpse_for_summary_table_sampled,
      lfpse_for_release_unsampled_pt_level,
      lfpse_for_release_sampled_pt_level
    )
  }
}

dbDisconnect(con_lfpse)

if (search_steis) {
  source("R/processors/steis.R")
} else {
  source("R/output/formatter.R")
}