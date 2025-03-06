# lfpse
dataset <- "LFPSE"
message(glue("Running {dataset} search..."))

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
  #select only relevant columns- use the lookup but do not rename at this step
  #to use additional columns, add them to column_selection_lookups.R
  select(any_of(unname(rename_lookup[["LFPSE"]])))|> 
  group_by(Reference)  |>
  mutate(OT001_min= min(as.numeric(OT001)), #calculate the worst physical harm per incident
         OT002_min= min(as.numeric(OT002)), # calculate the worst psychological harm per incident
         npatient = max(EntityId)) |># calculate the number of incidents
  ungroup() |>
  # collecting here so that we can apply text filters later
  collect() |>
  mutate(year_reported_or_occurred = as.numeric(substr(as.character(!!date_filter), 1, 4)),
         month_reported_or_occurred = as.numeric(substr(as.character(!!date_filter), 6, 7)),
         month_year_reported_or_occurred = zoo::as.yearmon(str_glue("{year_reported_or_occurred}-{month_reported_or_occurred}")),
         month_reported_or_occurred= month.abb[month_reported_or_occurred],
         reported_date = as.character(reported_date),
         occurred_date = as.character(occurred_date),
         OT002_min_plus_one = OT002_min + 1 #to make psychological and physical harm comparable, add 1 to psychological (as there is no fatal psychological harm)
  )|>
  rowwise() |>
  #combine physical harm and psychological harm to find maximum harm (of any type)
  mutate(max_harm= min_safe(c(OT001_min, OT002_min_plus_one))) |>
  ungroup() |>
  #label the different harm levels 
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
  select(-OT001_min,- OT002_min, -OT002_min_plus_one,#remove helper columns
         -max_harm) #remove max_harm as we do not use currently

toc_lfpse <- Sys.time()

time_diff_lfpse <- toc_lfpse - tic_lfpse

message(glue("Extraction from {dataset} server: {round(time_diff_lfpse[[1]], 2)} {attr(time_diff_lfpse, 'units')}"))

message(glue("- {dataset} categorical filters retrieved {format(nrow(lfpse_filtered_categorical), big.mark = ',')} incidents."))

# text filters 
if (sum(!is.na(text_terms))>0) {
  message(glue("Running {dataset} text search..."))

  #A002 may need to be added for a medication incident
  lfpse_filtered_text_precursor<- lfpse_filtered_categorical |>
    mutate(concat_col=paste(F001, AC001, OT003, A008_Other, A008, sep=" "))
  
  groups <- names(text_terms)
  for (group in groups) {
    terms <- text_terms[[group]]
    for (term in terms) {
      lfpse_filtered_text_precursor <- lfpse_filtered_text_precursor |>
        mutate("{group}_term_{term}" := str_detect(concat_col, term))
    }
    
    lfpse_filtered_text_precursor <- lfpse_filtered_text_precursor |>
      mutate("{group}" := rowSums(across(starts_with(group))) > 0)
  }
  
  lfpse_filtered_text <- lfpse_filtered_text_precursor %>%
    filter(!!text_filter) %>%
    select(!c(contains("_term_"), concat_col))
  
  message(glue("{dataset} text search retrieved {format(nrow(lfpse_filtered_text), big.mark = ',')} incidents."))
} else {
  message("- No text terms supplied. Skipping text search...")
  lfpse_filtered_text <- lfpse_filtered_categorical
}

# check whether the text search generated results
if (nrow(lfpse_filtered_text) != 0) {
  
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
  
    # sampling ####
    # Default (if > 300: all death/severe, 100 moderate, 100 low/no harm)
    if (sampling_strategy == "default") {
      if (nrow(lfpse_labelled) > 300) {
        message("- Sampling according to default strategy...")
        lfpse_death_severe <- lfpse_labelled |>
          # deaths or severe physical harm
          filter(OT001 %in% c("Fatal", "Severe physical harm"))
        
        set.seed(123)
        lfpse_moderate <- lfpse_labelled |>
          # moderate physical  harm
          filter(OT001 == "Moderate physical harm") |>
          collect() |>
          sample_n(min(n(), 100))
        
        set.seed(123)
        lfpse_low_no_other <- lfpse_labelled |>
          filter(
            !OT001 %in% c("Fatal", "Severe physical harm", "Moderate physical harm")
          ) |>
          collect() |>
          sample_n(min(n(), 100))
        
        lfpse_sampled <- bind_rows(
          lfpse_death_severe,
          lfpse_moderate,
          lfpse_low_no_other
        )
      } else {
        message("- Sampling not required, default threshold not met.")
        lfpse_sampled <- lfpse_labelled
      }
    } else if (sampling_strategy == "FOI") {
      message("- Extracting a sample of 30 incidents for redaction...")
      set.seed(123)
      lfpse_sampled <- lfpse_labelled |>
        distinct(Reference, .keep_all = T) |>
        sample_n(min(n(), 30))
    } else if (sampling_strategy == "none") {
      message("- Skipping sampling...")
      lfpse_sampled <- lfpse_labelled
    }
   
  #create incident level table from unsampled dataframe and rename columns - this is for summary tab
  lfpse_for_release_unsampled_incident_level <-  lfpse_labelled  |>
    # rename columns
    select(any_of(rename_lookup[["LFPSE"]]), starts_with("group_")) |>
    # remove columns that contain patient specific info (for summary tables)
    select(-any_of(c("Patient no.",
                     "OT001 - Physical harm",
                     "OT002 - Psychological harm",
                     "P004 - Age in days", 
                     "P007 - Age Range",
                     "OT003 - What was the clinical outcome for the patient?"
                     ))) |> 
    # get distinct References, so only one row per incident
    distinct(Reference, .keep_all = TRUE)
    
  #create incident level table from sampled dataframe and rename columns - this is for summary tab
  lfpse_for_release_sampled_incident_level <-  lfpse_sampled |>
    # rename columns
    select(any_of(rename_lookup[["LFPSE"]]), starts_with("group_")) |>
    # remove columns that contain patient specific info (for summary tables)
    select(-any_of(c("Patient no.",
                     "OT001 - Physical harm",
                     "OT002 - Psychological harm",
                     "P004 - Age in days", 
                     "P007 - Age Range",
                     "OT003 - What was the clinical outcome for the patient?"
    ))) |> 
    # get distinct References, so only one row per incident
    distinct(Reference, .keep_all = TRUE)
  
  #create patient level table from sampled dataframe and rename columns - this is for data tab
    lfpse_for_release_sampled_pt_level <-  lfpse_sampled  |> 
      #rename columns using lookup
      select(any_of(rename_lookup[["LFPSE"]]), starts_with("group_")) |>
      select(-`Month`, -`Year`, -`Month - Year`)
    
    #create patient level table from sampled dataframe and rename columns - this is for data tab
    lfpse_for_release_unsampled_pt_level <-  lfpse_labelled  |> 
      #rename columns using lookup
      select(any_of(rename_lookup[["LFPSE"]]), starts_with("group_"))|>
      select(-`Month`, -`Year`, -`Month - Year`)
    
    message(glue("- Final {dataset} dataset contains {nrow(lfpse_for_release_unsampled_incident_level)} unsampled incidents"))
    message(glue("- Final {dataset} dataset contains {nrow(lfpse_for_release_sampled_incident_level)} sampled incidents."))
    message(glue("- Final {dataset} dataset contains {nrow(lfpse_for_release_sampled_pt_level)} sampled incidents (pt level)"))
    message(glue("- Final {dataset} dataset contains {nrow(lfpse_for_release_unsampled_pt_level)} unsampled incidents (pt level)"))
    
    }else{
    message(glue("**The search criteria has produced no results in {dataset}**"))
    message(glue("Moving on..."))
}

dbDisconnect(con_lfpse)

if (search_steis) {
  source("steis.R")
} else {
  source("formatter.R")
}
