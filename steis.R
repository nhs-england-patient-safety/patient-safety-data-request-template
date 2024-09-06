# steis
dataset <- "StEIS"
print(glue("Running {dataset} search..."))

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
    year_of_incident = year(occurred_date),
    month_of_incident = month(occurred_date),
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
  filter(!! steis_categorical)

print(glue("- {dataset} categorical filters retrieved {nrow(steis_filtered_categorical)} incidents."))

print(glue("- No sampling for StEIS since no harm grading."))

# text filters ####
if (!is.na(text_terms)) {
  print(glue("Running {dataset} text search..."))
  
  steis_filtered_text <- steis_filtered_categorical |>
    filter(if_any(c(description_of_what_happened,
                    immediate_action_taken,
                    key_findings,
                    how_will_lessons_be_disseminated_to_interested_parties,
                    type_of_incident_other),
                  ~str_detect(.,text_terms)))
  
  print(glue("{dataset} text search retrieved {nrow(steis_filtered_text)} incidents."))
  
} else {
  print('- No text terms supplied. Skipping text search...')
  steis_filtered_text <- steis_filtered_categorical
}

# columns for release ####
if(cols_to_extract == 'all'){
  steis_for_release <- steis_filtered_text
} else if (cols_to_extract == 'default'){
  steis_for_release <- steis_filtered_text |>
    # select columns to be released
    select(
      `Log No` = log_no,
      `Created on` = reported_date,
      `Organisation reporting SI on STEIS` = organisation_reporting_si_on_steis,
      `Organisation leading investigation` = organisation_leading_investigation,
      `CCG/CSU Name` = ccg_csu_name,
      `Region - Geography` = region_geography,
      `Status` = status,
      `Date of Incident:` = occurred_date,
      `Year of Incident` = year_of_incident,
      `Month of Incident` = month_of_incident,
      `Time of Incident:` = time_of_incident,
      `Site of Incident:` = site_of_incident,
      `Location of Incident:` = location_of_incident,
      `Location of Incident (Other):` = location_of_incident_other,
      `Care Sector` = care_sector,
      `Care Sector (Other)` = care_sector_other,
      `Clinical Area:` = clinical_area,
      `Clinical Area (Other)` = clinical_area_other,
      `Patient Age (years)` = patient_age_years,
      `Patient Age (months)` = patient_age_months,
      `Patient Type` = patient_type,
      `Legal Status of Patient` = legal_status_of_patient,
      `Type of Incident` = type_of_incident,
      `Type of Incident (Other)` = type_of_incident_other,
      `Where is patient at time of reporting:` = where_is_patient_at_time_of_reporting,
      `Internal Inverstigation Required` = internal_investigation_required,
      `Non Health led Investigation Required` = non_health_led_investigation_required,
      `Description of what happened:` = description_of_what_happened,
      `Reason for Reporting` = reason_for_reporting,
      `Immediate action taken` = immediate_action_taken,
      `Case Summary` = case_summary,
      `Key Findings` = key_findings,
      `How will lessons be disseminated to interested parties` = how_will_lessons_be_disseminated_to_interested_parties,
      starts_with("group")
    )
}

print(glue("- Final {dataset} dataset contains {nrow(steis_for_release)} incidents."))
steis_full_string<-expand_categorical_filters(deparse(steis_categorical), list_of_steis_filters, "STEIS" )

source('formatter.R')