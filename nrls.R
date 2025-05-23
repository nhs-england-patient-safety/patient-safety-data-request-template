# nrls

dataset <- "NRLS"
message(glue("Running {dataset} search..."))

if (nrls_categorical == 0) {
  nrls_categorical <- expr(1 == 1)
}

# read tables ####

nrls <- tbl(con_nrls, in_schema("art", "vw_clean"))

organisations <- tbl(con_nrls, in_schema("art", "trust_details")) |>
  collect()

# parse columns ####

nrls_parsed <- nrls |>
  # IN05_LVL2 is capitalised for consistency to enable join later
  rename(
    IN05_LVL2 = IN05_lvl2,
    occurred_date = IN01,
    reported_date = CREATEDDT
  ) |>
  mutate(reported_date = sql('CAST("reported_date" AS DATE)')) 

# categorical filters ####
tic_nrls <- Sys.time()

nrls_filtered_categorical <- nrls_parsed |>
  # apply categorical filters here
  filter(between(date_filter, start_date, end_date)) |>
  filter(!!nrls_categorical) |>
  #select only relevant columns- use the lookup but do not rename at this step
  #to use additional columns, add them to column_selection_lookups.R
  select(any_of(unname(rename_lookup[["NRLS"]])))|>
  # collecting here so that we can apply text filters later
  collect() |>
  mutate(year_reported_or_occurred = year(!!date_filter),
         month_reported_or_occurred = as.character(month(!!date_filter, label = TRUE, abbr = TRUE)),
         #zoo package is used to create a year-month object because this will sort in the correct order when tabulated
         month_year_reported_or_occurred = zoo::as.yearmon(!!date_filter),
         financial_year_reported_or_occurred = ifelse(month(!!date_filter)>3, 
                                                      (paste0(year(!!date_filter), '/', year(!!date_filter)+1)),
                                                      paste0(year(!!date_filter)-1,  '/', year(!!date_filter))
         ),
         reported_date = as.character(reported_date),
         occurred_date = as.character(occurred_date))

toc_nrls <- Sys.time()

time_diff_nrls <- toc_nrls - tic_nrls

message(glue("Extraction from {dataset} server: {round(time_diff_nrls[[1]], 2)} {attr(time_diff_nrls, 'units')}"))

message(glue("- {dataset} categorical filters retrieved {format(nrow(nrls_filtered_categorical), big.mark = ',')} incidents."))

# text filters ####
if (sum(!is.na(text_terms)) > 0) {
  message(glue("Running {dataset} text search..."))
  
  nrls_filtered_text_precursor <- nrls_filtered_categorical |>
    mutate(concat_col = paste(IN07, IN03_TEXT, IN05_TEXT, IN11, IN10, MD05, MD06, MD30, MD31, DE01_TEXT, DE03, sep = " "))
  
  # iterate through each group
  groups <- names(text_terms)
  for (group in groups) {
    # iterate through each term
    terms <- text_terms[[group]]
    for (term in terms) {
      nrls_filtered_text_precursor <- nrls_filtered_text_precursor |>
        # create column for term match
        mutate("{group}_term_{term}" := str_detect(concat_col, term))
    }
    
    nrls_filtered_text_precursor <- nrls_filtered_text_precursor |>
      # create column for group match
      mutate("{group}" := rowSums(across(starts_with(group))) > 0)
  }
  
  nrls_filtered_text <- nrls_filtered_text_precursor |>
    # apply text filter logic
    filter(!!text_filter) |>
    select(-concat_col)
  
  message(glue("{dataset} text search retrieved {format(nrow(nrls_filtered_text), big.mark = ',')} incidents."))
} else {
  message("- No text terms supplied. Skipping text search...")
  nrls_filtered_text <- nrls_filtered_categorical
  
}


# check whether the text search generated results
if (nrow(nrls_filtered_text) != 0) {

  #label; rename columns
  nrls_labelled<- nrls_filtered_text |>
  pivot_longer(cols = any_of(codes$col_name)) |>
    left_join(codes, by = c(
      "name" = "col_name",
      "value" = "SASCODE"
    )) |>
    select(!value) |>
    pivot_wider(
      names_from = name,
      values_from = OPTIONTEXT
    )|>
    left_join(organisations, by = c("RP07" = "ORGANISATIONCODE"))

  # Neonatal logic
  # AGE_AT_INCIDENT appears to be derived from DV01 so DV01 shouldn't need checking separately
  # Searching PD01_B doesn't contain any more information than  AGE_AT_INCIDENTS (from SQL searches)- presume derived field, and removed it
  nrls_age_categorised <- nrls_labelled %>%
    mutate(
      #note that PD05_LVL1, PD05_LVL2, PD04 and PD20 contain NAs- this currently doesn't impact logic so these have been left in
      concat_col = paste(IN07, IN10, IN11, IN05_TEXT, PD05_LVL1, PD05_LVL2, PD05_TEXT, sep = " "),
      #these flags are to create the categorisations- they may be useful to keep like this, as they help with QA
      age_less_28_days= between(AGE_AT_INCIDENT, 0, 28/365),
      specialty_neonatology= PD05_LVL2 == 'Neonatology',
      poss_neonate_specialty = (PD05_LVL1 == 'Obstetrics and gynaecology' | PD04 == 'A paediatrics specialty' | PD20 == 'Yes'),
      neonate_terms_text = str_detect(concat_col, neonatal_terms),
      age_over_28_days_under_18_years = AGE_AT_INCIDENT >= (28/365) & AGE_AT_INCIDENT < 18,
      camhs_and_age_na = PD05_LVL2 == 'Child and adolescent mental health' & is.na(AGE_AT_INCIDENT),
      other_paed_specialty = PD05_LVL2 %in% c('Community paediatrics', 'Paedodontics'),
      paed_flags = PD04 == 'A paediatrics specialty' | PD20 == 'Yes',
      paed_terms_text = str_detect(concat_col, paediatric_terms))|>
    #build up neonate and paediatric categories from these flags
    mutate(
      neonate_category = case_when(
        # Neonate by age: age is between 0 and 28 days
        age_less_28_days ~ 'neonate_by_age',
        # Neonate by specialty: neonatology
        specialty_neonatology ~ 'neonate_by_specialty',
        # Neonate by text: obs and gynae or paeds specialty and neo text terms found
        poss_neonate_specialty & neonate_terms_text  ~ 'neonate_by_text',
        # otherwise not neonate related
        .default = "not neonate related"
      ),
      paediatric_category = case_when(
        # paediatric by age: between 28 days and 18 
        age_over_28_days_under_18_years ~ 'paediatric_by_age',
        # paediatric by specialty: camhs where age missing, or community paaeds / paedodontics
        camhs_and_age_na | other_paed_specialty ~ 'paediatric_by_specialty',
        # paediatric by text: paediatric specialty and paediatric terms found
        paed_flags & paed_terms_text ~ 'paediatric_by_text',
        # otherwise not paeds related
        .default = "not paediatric related"
      )
    )
  
  
  # Now filter based on `is_neopaed` parameter
  if (is_neopaed == "neonate") {
    print("- Running neonate strategy...")
    
    nrls_neopaed <- nrls_age_categorised %>%
      filter(neonate_category %in% c("neonate_by_age", "neonate_by_specialty", "neonate_by_text"))
    
  } else if (is_neopaed == "paed") {
    print("- Running paediatric strategy...")
    
    nrls_neopaed <- nrls_age_categorised %>%
      filter(paediatric_category %in% c("paediatric_by_age", "paediatric_by_specialty", "paediatric_by_text"))
    
  } else if (is_neopaed == "either") {
    print("- Running either strategy...")
    
    nrls_neopaed <- nrls_age_categorised %>%
      filter(paediatric_category %in% c("paediatric_by_age", "paediatric_by_specialty", "paediatric_by_text")|
               neonate_category %in% c("neonate_by_age", "neonate_by_specialty", "neonate_by_text") )
    
  } else if (is_neopaed == "none") {
    print("- Skipping neopaeds strategy...")
    
    nrls_neopaed <- nrls_age_categorised
  }
  
  print(str_glue("nrls_neopaed has {nrow(nrls_neopaed)} incidents"))
  
  # check whether the neopaed search generated results
  if (nrow(nrls_neopaed) != 0) {
  
    # sampling ####
    # Default (if > 300: all death/severe, 100 moderate, 100 low/no harm)
    if (sampling_strategy == "default") {
      if (nrow(nrls_neopaed) > 300) {
        message("- Sampling according to default strategy...")
        
        nrls_death_severe <- nrls_neopaed |>
          filter(PD09  %in% c("Death", "Severe"))
        
        set.seed(123)
        
        nrls_moderate <- nrls_neopaed |>
          filter(PD09 == "Moderate") |>
          # sample 100, or if fewer than 100, bring all
          sample_n(min(n(), 100))
        
        set.seed(123)
        
        nrls_low_no_other <- nrls_neopaed |>
          filter(!PD09 %in% c("Moderate", "Severe", "Death")) |>
          sample_n(min(n(), 100))
        
        nrls_sampled <- bind_rows(
          nrls_death_severe,
          nrls_moderate,
          nrls_low_no_other
        )
      } else {
        message("- Sampling not required, default threshold not met.")
        nrls_sampled <- nrls_neopaed
      }
    } else if (sampling_strategy == "FOI") {
      message("- Extracting a sample of 30 incidents for redaction...")
      
      set.seed(123)
      nrls_sampled <- nrls_neopaed |>
        sample_n(min(n(), 30))
      
    } else if (sampling_strategy == "none") {
      
      message("- Skipping sampling...")
      nrls_sampled <- nrls_neopaed
    }
    
    #create incident level table from unsampled dataframe and rename columns - this is for summary tab
    nrls_for_summary_table_unsampled <- nrls_neopaed  |>
      select(any_of(rename_lookup[["NRLS"]]), starts_with("group_"))

    #create incident level table from sampled dataframe and rename columns - this is for summary tab
    nrls_for_summary_table_sampled <- nrls_sampled  |>
      select(any_of(rename_lookup[["NRLS"]]), starts_with("group_"))
      
    #note: below is very similar to incident level dataframe as nrls is already one row per incident
    #create patient level table from sampled dataframe and rename columns - this is for data tab
    nrls_for_release_sampled_pt_level<- nrls_sampled |>
      select(any_of(rename_lookup[["NRLS"]]), starts_with("group_")) |>
      select(-`Month`, -`Year`, -`Month - Year`)
    
    #note: below is very similar to incident level dataframe as nrls is already one row per incident
    #create patient level table from unsampled dataframe and rename columns - this is for data tab
    nrls_for_release_unsampled_pt_level<- nrls_neopaed |>
      select(any_of(rename_lookup[["NRLS"]]), starts_with("group_"))|>
      select(-`Month`, -`Year`, -`Month - Year`)
  
    message(glue("- Final {dataset} dataset contains {nrow(nrls_for_summary_table_unsampled)} unsampled incidents"))
    message(glue("- Final {dataset} dataset contains {nrow(nrls_for_summary_table_sampled)} sampled incidents."))
    message(glue("- Final {dataset} dataset contains {nrow(nrls_for_release_sampled_pt_level)} sampled incidents (pt level)"))
    message(glue("- Final {dataset} dataset contains {nrow(nrls_for_release_unsampled_pt_level)} unsampled incidents (pt level)"))
    
    } else {
    message(glue("**The neopaed search has produced no results in {dataset}**"))
    message(glue("Moving on..."))
    }
  } else {
    message(glue("**The search strategy has produced no results in {dataset}**"))
    message(glue("Moving on..."))
  }

dbDisconnect(con_nrls)

if (search_lfpse) {
  source("lfpse.R")
} else if (search_steis) {
  source("steis.R")
} else {
  source("formatter.R")
}
