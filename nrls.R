# nrls

dataset <- "NRLS"
print(glue("Running {dataset} search..."))

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
  # collecting here so that we can apply text filters later
  collect() |>
  mutate(year_of_incident = year(occurred_date),
         month_of_incident = as.character(month(occurred_date, label = TRUE, abbr = TRUE)))

toc_nrls <- Sys.time()

time_diff_nrls <- toc_nrls - tic_nrls

print(glue("Extraction from {dataset} server: {round(time_diff_nrls[[1]], 2)} {attr(time_diff_nrls, 'units')}"))

print(glue("- {dataset} categorical filters retrieved {format(nrow(nrls_filtered_categorical), big.mark = ',')} incidents."))

# text filters ####
if (sum(!is.na(text_terms)) > 0) {
  print(glue("Running {dataset} text search..."))
  
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
  
  nrls_filtered_text <- nrls_filtered_text_precursor %>%
    # apply text filter logic
    filter(!!text_filter) %>%
    # drop individual term columns
    select(!c(contains("_term_"), concat_col))
  
  print(glue("{dataset} text search retrieved {format(nrow(nrls_filtered_text), big.mark = ',')} incidents."))
} else {
  print("- No text terms supplied. Skipping text search...")
  nrls_filtered_text <- nrls_filtered_categorical
  
}



# check whether the text search generated results
if (nrow(nrls_filtered_text) != 0) {

  #nrls logic will go here
  
  # sampling ####
  # Default (if > 300: all death/severe, 100 moderate, 100 low/no harm)
  if (sampling_strategy == "default") {
    if (nrow(nrls_filtered_text) > 300) {
      print("- Sampling according to default strategy...")
      
      nrls_death_severe <- nrls_filtered_text |>
        filter(PD09  %in% c(4, 5))
      
      set.seed(123)
      
      nrls_moderate <- nrls_filtered_text |>
        filter(PD09  == 3) |>
        # sample 100, or if fewer than 100, bring all
        sample_n(min(n(), 100))
      
      set.seed(123)
      
      nrls_low_no_other <- nrls_filtered_text |>
        filter(PD09  %in% c(3, 4, 5)) |>
        sample_n(min(n(), 100))
      
      nrls_sampled <- bind_rows(
        nrls_death_severe,
        nrls_moderate,
        nrls_low_no_other
      )
    } else {
      print("- Sampling not required, default threshold not met.")
      nrls_sampled <- nrls_filtered_text
    }
  } else if (sampling_strategy == "FOI") {
    print("- Extracting a sample of 30 incidents for redaction...")
    
    set.seed(123)
    nrls_sampled <- nrls_filtered_text |>
      sample_n(min(n(), 30))
    
  } else if (sampling_strategy == "none") {
    
    print("- Skipping sampling...")
    nrls_sampled <- nrls_selected_columns
  }

  #note the pivot_longer step below is not feasible for all columns in the dataset if there are more than 100,000 rows.
  #It has been split into:
  #-creating the data for the summary tables (using just required columns) 
  #-creating the data for the incident level data (usually post sampling, or small number of rows)
  
  
  #select the columns required for the summary tables
  categories_for_summary_tables_nrls<-unique(as.character(unlist(summary_categories_nrls)))

  #create for release for sampling table
  nrls_for_release_full_for_summary <- nrls_filtered_text  |>
    #select columns required for summary tables-
    #add incident id column
    #add PD09 because the pivot longer will only work if at least one of the column names is in codes
    #add PD09 and month of incident because they are mutated later, and there will be a error if they don't exist.
    select(INCIDENTID, !!categories_for_summary_tables_nrls, PD09, month_of_incident)|>
    #get the column values from the codes table
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
    #rename columns using lookup 
    select(any_of(rename_lookup[["NRLS"]]), starts_with("group_"))|>
    #convert columns to factors, and set order
    mutate(`Month of Incident`= fct_relevel(`Month of Incident`, month.abb),
           `PD09 Degree of harm (severity)`= factor(`PD09 Degree of harm (severity)`,
                                                    levels= c("No Harm", "Low",
                                                              "Moderate", "Severe",
                                                              "Death"))
           )

  #create incident level table from sampled dataframe
  nrls_for_release_incident_level<- nrls_sampled |>
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
    left_join(organisations, by = c("RP07" = "ORGANISATIONCODE")) |>
    select(any_of(rename_lookup[["NRLS"]]), starts_with("group_"))
  
  print(glue("- Final sampled {dataset} dataset contains {nrow(nrls_for_release_incident_level)} incidents."))
  print(glue("- Final {dataset} dataset contains {nrow(nrls_for_release_full_for_summary)} incidents."))
} else {
  print(glue("**The search criteria has produced no results in {dataset}**"))
  print(glue("Moving on..."))
}

dbDisconnect(con_nrls)

if (search_lfpse) {
  source("lfpse.R")
} else if (search_steis) {
  source("steis.R")
} else {
  source("formatter.R")
}
