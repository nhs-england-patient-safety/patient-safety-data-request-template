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
  )

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
         month_year_reported_or_occurred = zoo::as.yearmon(!!date_filter),
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
    # drop individual term columns
    #select(!c(contains("_term_"), concat_col))
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

  #neopaeds logic will go here

  
 
  
  # sampling ####
  # Default (if > 300: all death/severe, 100 moderate, 100 low/no harm)
  if (sampling_strategy == "default") {
    if (nrow(nrls_labelled) > 300) {
      message("- Sampling according to default strategy...")
      
      nrls_death_severe <- nrls_labelled |>
        filter(PD09  %in% c("Death", "Severe"))
      
      set.seed(123)
      
      nrls_moderate <- nrls_labelled |>
        filter(PD09 == "Moderate") |>
        # sample 100, or if fewer than 100, bring all
        sample_n(min(n(), 100))
      
      set.seed(123)
      
      nrls_low_no_other <- nrls_labelled |>
        filter(!PD09 %in% c("Moderate", "Severe", "Death")) |>
        sample_n(min(n(), 100))
      
      nrls_sampled <- bind_rows(
        nrls_death_severe,
        nrls_moderate,
        nrls_low_no_other
      )
    } else {
      message("- Sampling not required, default threshold not met.")
      nrls_sampled <- nrls_labelled
    }
  } else if (sampling_strategy == "FOI") {
    message("- Extracting a sample of 30 incidents for redaction...")
    
    set.seed(123)
    nrls_sampled <- nrls_labelled |>
      sample_n(min(n(), 30))
    
  } else if (sampling_strategy == "none") {
    
    message("- Skipping sampling...")
    nrls_sampled <- nrls_labelled
  }

  #create incident level table from unsampled dataframe and rename columns - this is for summary tab
  nrls_for_release_unsampled_incident_level <- nrls_labelled  |>
    select(any_of(rename_lookup[["NRLS"]]), starts_with("group_"))

  #create incident level table from sampled dataframe and rename columns - this is for summary tab
  nrls_for_release_sampled_incident_level <- nrls_sampled  |>
    select(any_of(rename_lookup[["NRLS"]]), starts_with("group_"))
    
  #note: below is very similar to incident level dataframe as nrls is already one row per incident
  #create patient level table from sampled dataframe and rename columns - this is for data tab
  nrls_for_release_sampled_pt_level<- nrls_sampled |>
    select(any_of(rename_lookup[["NRLS"]]), starts_with("group_")) |>
    select(-`Month`, -`Year`, -`Month - Year`)
  
  #note: below is very similar to incident level dataframe as nrls is already one row per incident
  #create patient level table from unsampled dataframe and rename columns - this is for data tab
  nrls_for_release_unsampled_pt_level<- nrls_labelled |>
    select(any_of(rename_lookup[["NRLS"]]), starts_with("group_"))|>
    select(-`Month`, -`Year`, -`Month - Year`)

  message(glue("- Final {dataset} dataset contains {nrow(nrls_for_release_unsampled_incident_level)} unsampled incidents"))
  message(glue("- Final {dataset} dataset contains {nrow(nrls_for_release_sampled_incident_level)} sampled incidents."))
  message(glue("- Final {dataset} dataset contains {nrow(nrls_for_release_sampled_pt_level)} sampled incidents (pt level)"))
  message(glue("- Final {dataset} dataset contains {nrow(nrls_for_release_unsampled_pt_level)} unsampled incidents (pt level)"))
  
  } else {
  message(glue("**The search criteria has produced no results in {dataset}**"))
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
