# R/processors/lfpse.R

dataset <- "LFPSE"
log_dataset_start(dataset)

if (lfpse_categorical == 0) {
  lfpse_categorical <- expr(1 == 1)
}

# load the all patients view
lfpse_analysis_table <-
  tbl(con_lfpse, in_schema("analysis", "vwEventsTransposedAll"))

# identify the set of references that meet the date range and category criteria
valid_refs <-
  lfpse_analysis_table |>
  mutate(occurred_date = OccurredDate) |>
  filter(
    EntityId == 1 | is.na(EntityId),
    between(date_filter, start_date, end_date),
    lfpse_categorical
  ) |>
  select(Reference) |>
  distinct()

# record time
tic_lfpse <- Sys.time()

lfpse_filtered_categorical <-
  lfpse_analysis_table |>
  # restrict to only the valid references identified above
  inner_join(valid_refs, by = "Reference") |>
  mutate(
    occurred_date = OccurredDate,
    reported_date = OriginalSubmissionDate,
    P004_AgeAtTimeOfIncidentDays = as.numeric(P004_AgeAtTimeOfIncidentDays)
  ) |>
  # select relevant columns - use the lookup but do not rename at this step to use
  # additional columns, add them to R/config/column_selection_lookups.R
  select(any_of(unname(rename_lookup[["LFPSE"]])), P004_AgeAtTimeOfIncidentDays) |>
  # compute the most severe harm scores and patient count per incident
  mutate(
    OT001_min = min(as.numeric(OT001_PhysicalHarm), na.rm = FALSE),
    OT002_min = min(as.numeric(OT002_PsychologicalHarm), na.rm = FALSE),
    npatient = max(EntityId, na.rm = FALSE),
    .by = Reference
  ) |>
  # pull results from the database into local memory
  collect() |>
  mutate(
    # extract year and month
    year_reported_or_occurred =
      as.numeric(substr(as.character(!!date_filter), 1, 4)),
    month_reported_or_occurred =
      as.numeric(substr(as.character(!!date_filter), 6, 7)),
    # combine into year-month
    month_year_reported_or_occurred =
      zoo::as.yearmon(
        str_glue("{year_reported_or_occurred}-{month_reported_or_occurred}")
      ),
    # assign financial year
    financial_year_reported_or_occurred = ifelse(
      month_reported_or_occurred > 3,
      paste0(year_reported_or_occurred, '/', year_reported_or_occurred + 1),
      paste0(year_reported_or_occurred - 1, '/', year_reported_or_occurred)
    ),
    month_reported_or_occurred = month.abb[month_reported_or_occurred],
    reported_date = as.character(reported_date),
    occurred_date = as.character(occurred_date),
    # adjust psychological harm score to match the physical harm scale
    OT002_min_plus_one = OT002_min + 1,
    # take the element-wise minimum across both adjusted scores
    # pmin is used because it is quicker (and results are the same)
    max_harm = pmin(OT001_min, OT002_min_plus_one, na.rm = FALSE),
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
  # drop intermediate columns
  select(-OT001_min, -OT002_min, -OT002_min_plus_one, -max_harm) |>
  # replace database separator token with a readable comma separator
  mutate(across(starts_with("DMD"), ~ str_replace_all(.x, fixed(" {~@~} "), ", ")))

toc_lfpse <- Sys.time()
time_diff_lfpse <- toc_lfpse - tic_lfpse
log_extraction_time(dataset, time_diff_lfpse)
log_categorical_filter_count(dataset, nrow(lfpse_filtered_categorical))

# text filters
lfpse_text_columns <- c(
  "F001_Description", 
  "AC001_ImmediateActions", 
  "OT003_ClinicalOutcome", 
  "A008_DeviceType",
  "A008_Other_DeviceTypeOther", 
  "A002_DrugsInvolved", 
  "DMD002_VTMString", 
  "DMD004_VMPString"
  )

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
  
  question_cols_pattern <- 
    paste0("^(", paste(ResponseReference$QuestionId, collapse = "|"), ")(_|$)")
  
  lfpse_labelled <- lfpse_filtered_text |>
    mutate(across(matches(question_cols_pattern), as.character)) |>
    # match columns based on code (before underscore)
    pivot_longer(cols = matches(question_cols_pattern)) |>
    separate_rows(value, sep = " {~@~} ") |>
    arrange(value) |>
    # extract code
    mutate(QuestionId = str_extract(name, "^[^_]+")) |>
    left_join(ResponseReference, by = c(
      "QuestionId" = "QuestionId",
      "value" = "ResponseCode",
      "TaxonomyVersion" = "TaxonomyVersion"
    )) |>
    select(!c(value, QuestionId, Property, LastUpdated, IsActive)) |>
    pivot_wider(
      id_cols    = !any_of(ResponseReference$QuestionId),
      names_from = name,
      values_from = ResponseText,
      values_fn  = list(ResponseText = ~ str_c(., collapse = "; "))
    )
  
  # create a new column for age following validation 
  lfpse_age_validated<- lfpse_labelled |>
    mutate(age_unit = case_when(
      is.na(P004_AgeAtTimeOfIncidentDays) ~ 'age missing',
      between(P004_AgeAtTimeOfIncidentDays, 1, 30) ~ 'days',
      between(P004_AgeAtTimeOfIncidentDays, 31, 371) ~ 'months',
      between(P004_AgeAtTimeOfIncidentDays, 372, 74028) ~ 'years',
      .default = 'age outside bounds')) |>
    mutate(age_compliance = case_when(
      age_unit == 'age outside bounds' ~ 'age outside bounds',
      age_unit == 'age missing' ~ 'age missing',
      age_unit == 'days' & between(P004_AgeAtTimeOfIncidentDays, 1, 30) ~ 'yes',
      age_unit == 'months' & P004_AgeAtTimeOfIncidentDays %% 31 == 0 ~ 'yes',
      age_unit == 'years' & P004_AgeAtTimeOfIncidentDays %% 372 == 0 ~ 'yes',
      .default = 'no')) |>
    mutate(P004_days_validated = if_else(
      age_compliance == "yes", P004_AgeAtTimeOfIncidentDays, NA
    ))
  
  # age classification for neopaeds
  lfpse_age_classified <- lfpse_age_validated |>
    mutate(
      concat_col = paste(
        F001_Description, 
        AC001_ImmediateActions, 
        OT003_ClinicalOutcome, 
        A008_DeviceType,
        A008_Other_DeviceTypeOther, 
        L006_ResponsibleSpecialty, 
        L006_Other_ResponsibleSpecialtyOther, 
        sep = "_"
        ),
      age_category = case_when(
        (P004_days_validated > 0 & P004_days_validated <= 28) | 
          (P007_AgeBracket %in% c("0-14 days", "15-28 days")) ~ "neonate",
        (P004_days_validated > 28 & P004_days_validated < 6696) | 
          (P007_AgeBracket %in% c("1-11 months", "1-4 years", "5-9 years", "10-15 years", "16 and 17 years")) ~ "paediatric",
        (!is.na(P007_AgeBracket) | !is.na(P004_days_validated)) ~ 'adult estimated',
        is.na(P004_days_validated) ~ 'unknown',# includes those where age is below zero / above believable threshold
        .default = 'other' 
      ),
      
      neonate_specialty_flag = str_detect(L006_ResponsibleSpecialty, neonatal_specialty_terms),
      neonate_terms_flag = str_detect(concat_col, neonatal_terms),
      missing_specialty = is.na(L006_ResponsibleSpecialty),
      no_adult_specialty_flag = str_detect(L006_ResponsibleSpecialty, adult_specialty_terms, negate = TRUE), 
      paediatric_specialty_flag = str_detect(L006_ResponsibleSpecialty, paediatric_specialty_terms),
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
      harm_column = "OT001_PhysicalHarm",
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