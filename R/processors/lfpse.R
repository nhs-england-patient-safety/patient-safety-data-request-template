# lfpse
dataset <- "LFPSE"
message(glue::glue("Running {dataset} search..."))

if (lfpse_categorical == 0) {
  lfpse_categorical <- dplyr::expr(1 == 1)
}

# set latest revision table from events
latest_revision_table <- dplyr::tbl(con_lfpse, dbplyr::in_schema("analysis", "Events")) |>
  dplyr::group_by(Reference) |>
  dplyr::summarise(
    Revision = max(Revision), # i.e., the most up to date version of a record
    reported_date = min(SubmissionDate)
  ) |>
  dplyr::ungroup()

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
  table <- dplyr::tbl(con_lfpse, dbplyr::in_schema("analysis", x))
    #Rename EntityId in DmdMedication_Responses so it has a different name to the patient EntityId column
    if(x == "DmdMedication_Responses") {table <- table |> dplyr::rename(DmdEntityId = EntityId)}
  return(table)
})

lfpse_analysis_tables <- c(list(latest_revision_table), analysis_tables)

# duplicates will be present due to inclusion of Patient_Responses which is one row per patient (EntityId)
lfpse_parsed <- purrr::reduce(lfpse_analysis_tables,
                       dplyr::left_join,
                       by = c("Reference", "Revision")
) |>
  dplyr::rename(occurred_date = T005) |>
  dplyr::mutate(reported_date = dbplyr::sql('CAST("reported_date" AS DATE)')) |>
  # a conversion factor from days will be needed here, but appears to be DQ issues
  # suggest we wait for resolution before converting from days to years
  dplyr::mutate(P004_days = as.numeric(P004))

# dbplyr::sql_render(lfpse_parsed) this is a useful step to check the SQL has rendered sensibly

# record time to keep track of query speeds
tic_lfpse <- Sys.time()

lfpse_filtered_categorical <- lfpse_parsed |>
  
  ### Apply categorical and date filters
  dplyr::filter(
    dplyr::between(date_filter, start_date, end_date),
    # apply categorical filters here
    lfpse_categorical
  ) |>
  
  ### Select only relevant columns- use the lookup but do not rename at this step
  #to use additional columns, add them to column_selection_lookups.R
  dplyr::select(dplyr::any_of(unname(rename_lookup[["LFPSE"]])), P004_days)|> #P004_days needs to be included but is not a named column due to DQ issues

  ### Generate additional columns (grouping by Reference)

  dplyr::group_by(Reference)  |>
  dplyr::mutate(OT001_min= min(as.numeric(OT001)), #calculate the worst physical harm per incident
         OT002_min= min(as.numeric(OT002)), # calculate the worst psychological harm per incident
         npatient = max(EntityId)) |># calculate the number of incidents
  dplyr::ungroup() |>
  
  ### Collecting here so that we can apply text filters later
  dplyr::collect() |>
  
  ### Generate additional columns (without grouping)
  dplyr::mutate(year_reported_or_occurred = as.numeric(substr(as.character(!!date_filter), 1, 4)),
         month_reported_or_occurred = as.numeric(substr(as.character(!!date_filter), 6, 7)),
         #zoo package is used to create a year-month object because this will sort in the correct order when tabulated
         month_year_reported_or_occurred = zoo::as.yearmon(stringr::str_glue("{year_reported_or_occurred}-{month_reported_or_occurred}")),
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
  dplyr::rowwise() |>
  dplyr::mutate(max_harm= min_safe(c(OT001_min, OT002_min_plus_one))) |>
  dplyr::ungroup() |>
  
  ## Label the different harm levels 
  dplyr::mutate(max_harm_level= dplyr::case_when(max_harm==1 ~ "Fatal",
                                   max_harm==2 ~ "Severe harm",
                                   max_harm==3 ~ "Moderate harm",
                                   max_harm==4 ~ "Low harm",
                                   max_harm==5 ~ "No harm"),
         max_physical_harm_level= dplyr::case_when(OT001_min==1 ~ "Fatal",
                                            OT001_min==2 ~ "Severe physical harm",
                                            OT001_min==3 ~ "Moderate physical harm",
                                            OT001_min==4 ~ "Low physical harm",
                                            OT001_min==5 ~ "No physical harm",
                                            is.na(npatient) ~ "Not applicable",
                                            .default = "Harm level missing"),
         max_psychological_harm_level= dplyr::case_when(OT002_min==1 ~ "Severe psychological harm",
                                                 OT002_min==2 ~ "Moderate psychological harm",
                                                 OT002_min==3 ~ "Low psychological harm",
                                                 OT002_min==4 ~ "No psychological harm",  
                                                 is.na(npatient) ~ "Not applicable",
                                                 .default = "Harm level missing")
  ) |>
  ### Remove columns that are not required
  dplyr::select(-OT001_min,- OT002_min, -OT002_min_plus_one,#remove helper columns
         -max_harm) |> #remove max_harm as we do not use currently
  
  ### Handle row duplication brought in by the DMD table
  # this step is done after collecting because putting it before slowed down collection process substantially
  # summarise and str_flatten to combine DMD rows into comma seperated string
  dplyr::group_by(dplyr::across(-dplyr::starts_with("DMD"))) |>
  dplyr::summarise(dplyr::across(dplyr::starts_with("DMD"), ~ stringr::str_flatten(., collapse = ", "), .names = "{.col}"), .groups="drop")


toc_lfpse <- Sys.time()

time_diff_lfpse <- toc_lfpse - tic_lfpse

message(glue::glue("Extraction from {dataset} server: {round(time_diff_lfpse[[1]], 2)} {attr(time_diff_lfpse, 'units')}"))

message(glue::glue("- {dataset} categorical filters retrieved {format(nrow(lfpse_filtered_categorical), big.mark = ',')} incidents."))

# text filters 
if (sum(!is.na(text_terms))>0) {
  message(glue::glue("Running {dataset} text search..."))

  #A002, DMD002, DMD004 may need to be removed if adding noise to a non medication-related request
  lfpse_filtered_text_precursor<- lfpse_filtered_categorical |>
    dplyr::mutate(concat_col=paste(F001, AC001, OT003, A008_Other, A008, A002, DMD002, DMD004,
                            sep=" "))
  
  groups <- names(text_terms)
  for (group in groups) {
    terms <- text_terms[[group]]
    for (term in terms) {
      lfpse_filtered_text_precursor <- lfpse_filtered_text_precursor |>
        dplyr::mutate("{group}_term_{term}" := stringr::str_detect(concat_col, term))
    }
    
    lfpse_filtered_text_precursor <- lfpse_filtered_text_precursor |>
      dplyr::mutate("{group}" := rowSums(dplyr::across(dplyr::starts_with(group))) > 0)
  }
  
  lfpse_filtered_text <- lfpse_filtered_text_precursor |>
    dplyr::filter(!!text_filter) |>
    dplyr::select(-concat_col)
  
  message(glue::glue("{dataset} text search retrieved {format(nrow(lfpse_filtered_text), big.mark = ',')} incidents."))
} else {
  message("- No text terms supplied. Skipping text search...")
  lfpse_filtered_text <- lfpse_filtered_categorical
}

# check whether the text search generated results
if (nrow(lfpse_filtered_text) != 0) {
  
  lfpse_labelled<- lfpse_filtered_text |>
  # pivot the coded columns
  tidyr::pivot_longer(cols = dplyr::any_of(ResponseReference$QuestionId)) |>
    # separate the multi-responses into single row per selection
    tidyr::separate_rows(value, sep = " {~@~} ") |>
    # arrange so that multi-responses appear alphabetised later
    dplyr::arrange(value) |>
    # bring through the value labels
    dplyr::left_join(ResponseReference, by = c(
      "name" = "QuestionId",
      "value" = "ResponseCode",
      "TaxonomyVersion" = "TaxonomyVersion"
    )) |>
    # remove the unnecessary columns
    dplyr::select(!c(value, Property, LastUpdated, IsActive)) |>
    # pivot back into columns
    tidyr::pivot_wider(
      id_cols = !dplyr::any_of(ResponseReference$QuestionId),
      names_from = name,
      values_from = ResponseText,
      # collapse multi-responses into single row per entity
      values_fn = list(ResponseText = ~ stringr::str_c(., collapse = "; "))
    ) 
  
  #create a new column for age following validation 
  lfpse_age_validated<- lfpse_labelled |>
    dplyr::mutate(age_unit = dplyr::case_when(
      is.na(P004_days) ~ 'age missing',
      dplyr::between(P004_days, 1, 30) ~ 'days',
      dplyr::between(P004_days, 31, 371) ~ 'months',
      dplyr::between(P004_days, 372, 74028) ~ 'years',
      .default = 'age outside bounds')) |>
    dplyr::mutate(age_compliance = dplyr::case_when(
      age_unit == 'age outside bounds' ~ 'age outside bounds',
      age_unit == 'age missing' ~ 'age missing',
      age_unit == 'days' & dplyr::between(P004_days, 1, 30) ~ 'yes',
      age_unit == 'months' & P004_days %% 31 == 0 ~ 'yes',
      age_unit == 'years' & P004_days %% 372 == 0 ~ 'yes',
      .default = 'no')) |>
    dplyr::mutate(P004_days_validated = dplyr::if_else(
      age_compliance == "yes", P004_days, NA
    ))
  
  lfpse_age_classified <- lfpse_age_validated |>
    dplyr::mutate(
      concat_col = paste(F001, AC001, OT003, A008_Other, L006, L006_Other, sep = "_"),
      age_category = dplyr::case_when(
        (P004_days_validated > 0 & P004_days_validated <= 28) | (P007 %in% c("0-14 days", "15-28 days")) ~ "neonate",
        (P004_days_validated > 28 & P004_days_validated < 6696) | (P007 %in% c("1-11 months", "1-4 years", "5-9 years", "10-15 years", "16 and 17 years")) ~ "paediatric",
        (!is.na(P007)|!is.na(P004_days_validated)) ~ 'adult estimated',
        is.na(P004_days_validated) ~ 'unknown',# includes those where age is below zero / above believable threshold
        .default = 'other' 
      ),
      #these flags are to create the categorisations- they may be useful to keep like this, as they help with QA
      neonate_specialty_flag = stringr::str_detect(L006, neonatal_specialty_terms),
      neonate_terms_flag = stringr::str_detect(concat_col, neonatal_terms),
      missing_specialty = is.na(L006),
      no_adult_specialty_flag = stringr::str_detect(L006, adult_specialty_terms, negate=T), 
      paediatric_specialty_flag = stringr::str_detect(L006, paediatric_specialty_terms),
      paediatric_term_flag = stringr::str_detect(concat_col, paediatric_terms),
      neonate_category = dplyr::case_when(
        # Neonate by age: age is between 0 and 28 days
        age_category == 'neonate' ~ "neonate_by_age",
        # Neonate by specialty: age is 0 or NA and specialty indicates neonate
        neonate_specialty_flag ~ "neonate_by_specialty",
        # Neonate by text: age is 0 or NA and text indicates neonate and specialty is not adult
        (neonate_terms_flag & (no_adult_specialty_flag | missing_specialty)) ~ "neonate_by_text",
        # Default: not neonate-related
        .default = "not neonate related"
      ),
      paediatric_category = dplyr::case_when(
        # Paediatrics by age: age is older than 1 month and younger than 18 years
        age_category == 'paediatric' ~ "paediatric_by_age",
        # Paediatrics by specialty: age is 0 or NA and specialty indicates paediatrics
        paediatric_specialty_flag ~ "paediatric_by_specialty",
        # Paediatrics by text: age is 0 or NA and text indicates paediatrics
        (paediatric_term_flag & (no_adult_specialty_flag | missing_specialty)) ~ "paediatric_by_text",
        # Default: not paediatrics-related
        .default = "not paediatric related"
      )
    )
  
  # Now filter based on `is_neopaed` parameter
  if (is_neopaed == "neonate") {
    print("- Running neonate strategy...")
    
    lfpse_neopaed <- lfpse_age_classified |>
      dplyr::filter(neonate_category %in% c("neonate_by_age", "neonate_by_specialty", "neonate_by_text"))

  } else if (is_neopaed == "paed") {
    print("- Running paediatric strategy...")
    
    lfpse_neopaed <- lfpse_age_classified |>
      dplyr::filter(paediatric_category %in% c("paediatric_by_age", "paediatric_by_specialty", "paediatric_by_text"))

  } else if (is_neopaed == "either") {
    print("- Running either strategy...")
    
    lfpse_neopaed <- lfpse_age_classified |>
      dplyr::filter(paediatric_category %in% c("paediatric_by_age", "paediatric_by_specialty", "paediatric_by_text")|
               neonate_category %in% c("neonate_by_age", "neonate_by_specialty", "neonate_by_text") )
    
  } else if (is_neopaed == "none") {
    print("- Skipping neopaeds strategy...")
    
    lfpse_neopaed <- lfpse_age_classified
  }
  
  # check whether the text search generated results
  if (nrow(lfpse_neopaed) != 0) {
  
    # sampling ####
    # Default (if > 300: all death/severe, 100 moderate, 100 low/no harm)
    if (sampling_strategy == "default") {
      if (nrow(lfpse_neopaed) > 300) {
        message("- Sampling according to default strategy...")
        lfpse_death_severe <- lfpse_neopaed |>
          # deaths or severe physical harm
          dplyr::filter(OT001 %in% c("Fatal", "Severe physical harm"))
        
        set.seed(123)
        lfpse_moderate <- lfpse_neopaed |>
          # moderate physical  harm
          dplyr::filter(OT001 == "Moderate physical harm") |>
          dplyr::collect() |>
          dplyr::sample_n(min(dplyr::n(), 100))
        
        set.seed(123)
        lfpse_low_no_other <- lfpse_neopaed |>
          dplyr::filter(
            !OT001 %in% c("Fatal", "Severe physical harm", "Moderate physical harm")
          ) |>
          dplyr::collect() |>
          dplyr::sample_n(min(dplyr::n(), 100))
        
        lfpse_sampled <- dplyr::bind_rows(
          lfpse_death_severe,
          lfpse_moderate,
          lfpse_low_no_other
        )
      } else {
        message("- Sampling not required, default threshold not met.")
        lfpse_sampled <- lfpse_neopaed
      }
    } else if (sampling_strategy == "FOI") {
      message("- Extracting a sample of 30 incidents for redaction...")
      set.seed(123)
      lfpse_sampled <- lfpse_neopaed |>
        dplyr::distinct(Reference, .keep_all = T) |>
        dplyr::sample_n(min(dplyr::n(), 30))
    } else if (sampling_strategy == "none") {
      message("- Skipping sampling...")
      lfpse_sampled <- lfpse_neopaed
    }
   
  lfpse_neopaed <-  lfpse_neopaed |>
    # rename columns
    dplyr::select(dplyr::any_of(rename_lookup[["LFPSE"]]), dplyr::starts_with("group_")) 
    
  lfpse_sampled <-  lfpse_sampled |>
    # rename columns
    dplyr::select(dplyr::any_of(rename_lookup[["LFPSE"]]), dplyr::starts_with("group_")) 
    
  #create patient level table from sampled dataframe and remove unnecessary columns - this is for data tab
    lfpse_for_release_sampled_pt_level <-  lfpse_sampled  |> 
      dplyr::select(!c(dplyr::contains("_term_"), `Month`, `Year`, `Month - Year`)) 
    
    #create patient level table from sampled dataframe and remove unnecessary columns - this is for data tab
    lfpse_for_release_unsampled_pt_level <-  lfpse_neopaed  |> 
      dplyr::select(!c(dplyr::contains("_term_"), `Month`, `Year`, `Month - Year`))
    
    
    lfpse_for_summary_table_unsampled<- lfpse_neopaed  

    lfpse_for_summary_table_sampled<- lfpse_sampled  
    
    if (summary_tables_incident_or_patient_level=="incident"){

      lfpse_for_summary_table_unsampled<- lfpse_for_summary_table_unsampled |>
      # remove columns that contain patient specific info (for summary tables)
      dplyr::select(-dplyr::any_of(c("Patient no.",
                       "OT001 - Physical harm",
                       "OT002 - Psychological harm",
                       "P004 - Age in days", 
                       "P007 - Age Range",
                       "OT003 - What was the clinical outcome for the patient?"
      ))) |> 
        # get distinct References, so only one row per incident
        dplyr::distinct(Reference, .keep_all = TRUE)
      
      
      lfpse_for_summary_table_sampled<- lfpse_for_summary_table_sampled |>
        # remove columns that contain patient specific info (for summary tables)
        dplyr::select(-dplyr::any_of(c("Patient no.",
                         "OT001 - Physical harm",
                         "OT002 - Psychological harm",
                         "P004 - Age in days", 
                         "P007 - Age Range",
                         "OT003 - What was the clinical outcome for the patient?"
        ))) |> 
        # get distinct References, so only one row per incident
        dplyr::distinct(Reference, .keep_all = TRUE)
      
      
    }
    
    message(glue::glue("- Final {dataset} dataset for creating summary table contains {nrow(lfpse_for_summary_table_unsampled)} unsampled incidents ({summary_tables_incident_or_patient_level} level)"))
    message(glue::glue("- Final {dataset} dataset for creating summary table contains {nrow(lfpse_for_summary_table_sampled)} sampled incidents  ({summary_tables_incident_or_patient_level} level)"))
    message(glue::glue("- Final {dataset} dataset contains {nrow(lfpse_for_release_sampled_pt_level)} sampled incidents (pt level)"))
    message(glue::glue("- Final {dataset} dataset contains {nrow(lfpse_for_release_unsampled_pt_level)} unsampled incidents (pt level)"))
 
    } else {
    message(glue::glue("**The neopaed search has produced no results in {dataset}**"))
    message(glue::glue("Moving on..."))
    }
  }else{
    message(glue::glue("**The search criteria has produced no results in {dataset}**"))
    message(glue::glue("Moving on..."))
}

dbDisconnect(con_lfpse)

if (search_steis) {
  source("R/processors/steis.R")
} else {
  source("R/output/formatter.R")
}
