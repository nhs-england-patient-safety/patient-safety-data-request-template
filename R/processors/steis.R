# R/processors/steis.R

dataset <- "StEIS"
message(glue::glue("Running {dataset} search..."))

if(steis_categorical == 0){
  steis_categorical <- expr(1==1)
}

# read data
steis <- readr::read_csv(here::here("data", steis_filename), show_col_types = F) |>
  janitor::clean_names() |>
  mutate_if(
    is.character,
    function(row) iconv(row, to = "UTF-8", sub = "")
  ) |>
  mutate_if(is.character, ~ gsub("[^ -~]", "", .))

# remove duplicates
steis_deduped <- steis |>
  separate_rows(modified_date, sep = ";") |>
  arrange(log_no, desc(modified_date)) |>
  distinct(log_no, .keep_all = T)

# parse columns
steis_parsed <- steis_deduped |>
  rename(
    occurred_date = date_of_incident,
    reported_date = created_on
  ) |> 
  mutate(
    occurred_date = as.character(dmy(occurred_date)),
    reported_date = dmy_hms(reported_date),
    reported_date = as.character(floor_date(reported_date, "days")),
    patient_date_of_birth = dmy(patient_date_of_birth),
    patient_age_years = floor((patient_date_of_birth %--% occurred_date) / years(1)),
    patient_age_months = ifelse(
      patient_age_years < 2,
      floor((patient_date_of_birth %--% occurred_date) / months(1)),
      NA
    )
  ) |>
  # Generate date columns
  add_date_columns(date_filter)

# categorical filters
steis_filtered_categorical <- steis_parsed |>
  filter(between(!! date_filter, start_date, end_date)) |>
  filter(!!steis_categorical)

message(glue::glue("- {dataset} categorical filters retrieved {format(nrow(steis_filtered_categorical), big.mark = ',')} incidents."))
message(glue::glue("- No sampling for StEIS since no harm grading."))

# Text filters
steis_text_columns <- c(
  "description_of_what_happened",
  "immediate_action_taken",
  "key_findings",
  "how_will_lessons_be_disseminated_to_interested_parties",
  "type_of_incident_other"
)
steis_filtered_text <- apply_text_search(
  steis_filtered_categorical,
  text_terms,
  text_filter,
  steis_text_columns,
  dataset
)

# Check for empty results
if (check_and_log_empty_result(steis_filtered_text, dataset, "text")) {
  source('R/output/formatter.R')
} else {
  
  # columns for release
  if(cols_to_extract == 'all'){
    steis_for_release <- steis_filtered_text
  } else if (cols_to_extract == 'default'){
    steis_for_release <- steis_filtered_text |>
      select(any_of(rename_lookup[["STEIS"]]), starts_with("group_"))
    
    # Create output tables (StEIS doesn't have sampling or patient level distinction)
    steis_for_summary_table_unsampled <- steis_for_release 
    steis_for_summary_table_sampled <- steis_for_release 
    
    steis_for_release_unsampled_pt_level <- steis_for_release |>
      select(!c(contains("_term_"), `Month`, `Year`, `Month - Year`))
    
    steis_for_release_sampled_pt_level <- steis_for_release |>
      select(!c(contains("_term_"), `Month`, `Year`, `Month - Year`))
  }
  
  # Log final counts
  message(glue::glue("- Final {dataset} dataset contains {nrow(steis_for_summary_table_unsampled)} unsampled incidents"))
  message(glue::glue("- Final {dataset} dataset contains {nrow(steis_for_summary_table_sampled)} sampled incidents."))
  message(glue::glue("- Final {dataset} dataset contains {nrow(steis_for_release_sampled_pt_level)} sampled incidents (pt level)"))
  message(glue::glue("- Final {dataset} dataset contains {nrow(steis_for_release_unsampled_pt_level)} unsampled incidents (pt level)"))
}

source('R/output/formatter.R')