# steis
dataset <- "StEIS"
message(glue("Running {dataset} search..."))

if(steis_categorical == 0){
  steis_categorical <- expr(1==1)
}

# read data ####

steis <- read_csv(here("data", steis_filename), show_col_types = F) |>
  clean_names() |>
  mutate_if(
    is.character,
    function(row) iconv(row, to = "UTF-8", sub = "")
  ) |>
  mutate_if(is.character, ~ gsub("[^ -~]", "", .))

# remove duplicates ####
steis_deduped <- steis |>
  separate_rows(modified_date, sep = ";") |>
  arrange(log_no, desc(modified_date)) |>
  distinct(log_no, .keep_all = T)

# parse columns ####
steis_parsed <- steis_deduped |>
  rename(occurred_date = date_of_incident,
         reported_date = created_on) |> 
  mutate(
    occurred_date = as.character(dmy(occurred_date)),
    reported_date = as.character(dmy_hms(reported_date)),
    year_reported_or_occurred = year(!!date_filter),
    month_reported_or_occurred = as.character(month(!!date_filter, label = TRUE, abbr = TRUE)),
    month_year_reported_or_occurred = zoo::as.yearmon(!!date_filter),
    patient_date_of_birth = dmy(patient_date_of_birth),
    patient_age_years = floor((patient_date_of_birth %--% occurred_date) / years(1)),
    patient_age_months = ifelse(patient_age_years < 2,
                                floor((patient_date_of_birth %--% occurred_date) / months(1)),
                                NA
    )
  )

# categorical filters ####

steis_filtered_categorical <- steis_parsed |>
  filter(between(!! date_filter, start_date, end_date)) |>
  filter(!!steis_categorical)

message(glue("- {dataset} categorical filters retrieved {format(nrow(steis_filtered_categorical), big.mark = ',')} incidents."))

message(glue("- No sampling for StEIS since no harm grading."))

# text filters ####
if (sum(!is.na(text_terms))>0) {
  message(glue("Running {dataset} text search..."))
  
  steis_filtered_text_precursor<- steis_filtered_categorical |>
    mutate(concat_col=paste(description_of_what_happened,
                            immediate_action_taken,
                            key_findings,
                            how_will_lessons_be_disseminated_to_interested_parties,
                            type_of_incident_other, sep=" "))
  
  # iterate through each group
  groups <- names(text_terms)
  for (group in groups) {
    # iterate through each term
    terms <- text_terms[[group]]
    for (term in terms) {
      steis_filtered_text_precursor <- steis_filtered_text_precursor |>
        # create column for term match
        mutate("{group}_term_{term}" := str_detect(concat_col, term))
    }
    
    steis_filtered_text_precursor <- steis_filtered_text_precursor |>
      # create column for group match
      mutate("{group}" := rowSums(across(starts_with(group))) > 0)
  }
  
  steis_filtered_text <- steis_filtered_text_precursor %>%
    # apply text filter logic
    filter(!!text_filter) %>%
    # drop individual term columns
    select(!c(contains("_term_"), concat_col))
  
  message(glue("{dataset} text search retrieved {format(nrow(steis_filtered_text), big.mark = ',')} incidents."))
} else {
  message("- No text terms supplied. Skipping text search...")
  steis_filtered_text <- steis_filtered_categorical
}

# check whether the text search generated results 
if(nrow(steis_filtered_text) != 0){
  
  # columns for release ####
  if(cols_to_extract == 'all'){
    steis_for_release <- steis_filtered_text
  } else if (cols_to_extract == 'default'){
    steis_for_release <- steis_filtered_text |>
      # select columns to be released and rename using lookup
      select(any_of(rename_lookup[["STEIS"]]), starts_with("group_"))
 
    steis_for_release_unsampled_pt_level<- steis_for_release
    steis_for_release_sampled_pt_level <- steis_for_release
    steis_for_release_sampled_incident_level <- steis_for_release
    steis_for_release_unsampled_incident_level <- steis_for_release
    
  }
  
  message(glue("- Final {dataset} dataset contains {nrow(steis_for_release_unsampled_incident_level)} unsampled incidents"))
  message(glue("- Final {dataset} dataset contains {nrow(steis_for_release_sampled_incident_level)} sampled incidents."))
  message(glue("- Final {dataset} dataset contains {nrow(steis_for_release_sampled_pt_level)} sampled incidents (pt level)"))
  message(glue("- Final {dataset} dataset contains {nrow(steis_for_release_unsampled_pt_level)} unsampled incidents (pt level)"))
  
} else {
  message(glue('**The search criteria has produced no results in {dataset}**'))
  message(glue('Moving on...'))
}

source('formatter.R')
