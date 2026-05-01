# R/processors/nrls.R

dataset <- "NRLS"
log_dataset_start(dataset)

if (nrls_categorical == 0) {
  nrls_categorical <- expr(1 == 1)
}

# read tables
nrls <- tbl(con_nrls, in_schema("art", "vw_clean"))

organisations <- tbl(con_nrls, in_schema("art", "trust_details")) |>
  collect()

# parse columns
nrls_parsed <- nrls |>
  rename(
    IN05_LVL2 = IN05_lvl2,
    occurred_date = IN01,
    reported_date = CREATEDDT
  ) |>
  mutate(reported_date = sql('CAST("reported_date" AS DATE)')) 

# categorical filters
tic_nrls <- Sys.time()

nrls_filtered_categorical <- nrls_parsed |>
  # apply categorical filters
  filter(between(date_filter, start_date, end_date)) |>
  filter(!!nrls_categorical) |>
  # select relevant columns - use the lookup but do not rename at this step to use
  # additional columns, add them to R/config/column_selection_lookups.R
  select(any_of(unname(rename_lookup[["NRLS"]]))) |>
  # collect here to apply text filters later
  collect() |>
  # generate date columns
  add_date_columns(date_filter) |>
  mutate(
    reported_date = as.character(reported_date),
    occurred_date = as.character(occurred_date)
  )

toc_nrls <- Sys.time()
time_diff_nrls <- toc_nrls - tic_nrls
log_extraction_time(dataset, time_diff_nrls)
log_categorical_filter_count(dataset, nrow(nrls_filtered_categorical))

# text filters
nrls_text_columns <- c("IN07", "IN03_TEXT", "IN05_TEXT", "IN11", "IN10", 
                       "MD05", "MD06", "MD30", "MD31", "DE01_TEXT", "DE03")
nrls_filtered_text <- apply_text_search(
  nrls_filtered_categorical,
  text_terms,
  text_filter,
  nrls_text_columns,
  dataset
)

# check for empty results
if (check_and_log_empty_result(nrls_filtered_text, dataset, "text")) {
  dbDisconnect(con_nrls)
  if (search_lfpse) {
    source("R/processors/lfpse.R")
  } else if (search_steis) {
    source("R/processors/steis.R")
  } else {
    source("R/output/formatter.R")
  }
} else {
  
  # label codes and join organisations
  nrls_labelled <- nrls_filtered_text |>
    pivot_longer(cols = any_of(codes$col_name)) |>
    left_join(codes, by = c(
      "name" = "col_name",
      "value" = "SASCODE"
    )) |>
    select(!value) |>
    pivot_wider(
      names_from = name,
      values_from = OPTIONTEXT
    ) |>
    left_join(organisations, by = c("RP07" = "ORGANISATIONCODE"))
  
  # age categorisation for neopaeds
  nrls_age_categorised <- nrls_labelled |>
    mutate(
      concat_col = paste(IN07, IN10, IN11, IN05_TEXT, PD05_LVL1, PD05_LVL2, PD05_TEXT, sep = " "),
      age_less_28_days = between(AGE_AT_INCIDENT, 0, 28/365),
      specialty_neonatology = PD05_LVL2 == 'Neonatology',
      poss_neonate_specialty = (PD05_LVL1 == 'Obstetrics and gynaecology' | 
                                  PD04 == 'A paediatrics specialty' | 
                                  PD20 == 'Yes'),
      neonate_terms_text = str_detect(concat_col, neonatal_terms),
      age_over_28_days_under_18_years = AGE_AT_INCIDENT >= (28/365) & AGE_AT_INCIDENT < 18,
      camhs_and_age_na = PD05_LVL2 == 'Child and adolescent mental health' & is.na(AGE_AT_INCIDENT),
      other_paed_specialty = PD05_LVL2 %in% c('Community paediatrics', 'Paedodontics'),
      paed_flags = PD04 == 'A paediatrics specialty' | PD20 == 'Yes',
      paed_terms_text = str_detect(concat_col, paediatric_terms)
    ) |>
    mutate(
      neonate_category = case_when(
        age_less_28_days ~ 'neonate_by_age',
        specialty_neonatology ~ 'neonate_by_specialty',
        poss_neonate_specialty & neonate_terms_text ~ 'neonate_by_text',
        .default = "not neonate related"
      ),
      paediatric_category = case_when(
        age_over_28_days_under_18_years ~ 'paediatric_by_age',
        camhs_and_age_na | other_paed_specialty ~ 'paediatric_by_specialty',
        paed_flags & paed_terms_text ~ 'paediatric_by_text',
        .default = "not paediatric related"
      )
    )
  
  # apply neopaed filter
  nrls_neopaed <- filter_by_neopaed_strategy(nrls_age_categorised, is_neopaed)
  
  # print(str_glue("nrls_neopaed has {nrow(nrls_neopaed)} incidents"))
  
  # check for empty results
  if (check_and_log_empty_result(nrls_neopaed, dataset, "neopaed")) {
    dbDisconnect(con_nrls)
    if (search_lfpse) {
      source("R/processors/lfpse.R")
    } else if (search_steis) {
      source("R/processors/steis.R")
    } else {
      source("R/output/formatter.R")
    }
  } else {
    
    # get ods organisation data
    nrls_ods_joined <- fetch_and_join_ods(nrls_neopaed, english_only = TRUE)
    
    # sampling
    nrls_sampled <- apply_sampling_strategy(
      nrls_ods_joined,
      sampling_strategy,
      harm_column = "PD09",
      death_severe_values = c("Death", "Severe"),
      moderate_values = c("Moderate"),
      reference_column = "INCIDENTID"
    )
    
    # get data for summary tables (with renamed columns)
    nrls_for_summary_table_unsampled <- nrls_ods_joined |>
      select(any_of(rename_lookup[["NRLS"]]), starts_with("group_"))
    
    nrls_for_summary_table_sampled <- nrls_sampled |>
      select(any_of(rename_lookup[["NRLS"]]), starts_with("group_"))
    
    # create patient level tables
    nrls_for_release_sampled_pt_level <- nrls_sampled |>
      select(any_of(rename_lookup[["NRLS"]]), starts_with("group_")) |>
      select(!c(contains("_term_"), `Month`, `Year`, `Month - Year`)) 
    
    nrls_for_release_unsampled_pt_level <- nrls_ods_joined |>
      select(any_of(rename_lookup[["NRLS"]]), starts_with("group_")) |>
      select(!c(contains("_term_"), `Month`, `Year`, `Month - Year`)) 
    
    # log final counts
    log_final_counts(
      dataset,
      nrls_for_summary_table_unsampled,
      nrls_for_summary_table_sampled,
      nrls_for_release_unsampled_pt_level,
      nrls_for_release_sampled_pt_level
    )
  }
}

dbDisconnect(con_nrls)

if (search_lfpse) {
  source("R/processors/lfpse.R")
} else if (search_steis) {
  source("R/processors/steis.R")
} else {
  source("R/output/formatter.R")
}