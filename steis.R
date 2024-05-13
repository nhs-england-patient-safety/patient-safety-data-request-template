# steis

library(tidyverse)
library(here)
library(janitor)

# link to steis search for qa purposes ####

steis_url <- "https://steis.improvement.nhs.uk/ExampleStEISLink"

# read data ####

data <- read_csv(here("data", "SUI_2_13560.csv")) |>
  clean_names() |>
  mutate_if(
    is.character,
    function(row) iconv(row, to = "UTF-8", sub = "")
  ) |>
  mutate_if(is.character, ~ gsub("[^ -~]", "", .))

# remove duplicates ####
data_deduped <- data |>
  separate_rows(modified_date, sep = ";") |>
  arrange(log_no, desc(modified_date)) |>
  distinct(log_no, .keep_all = T)

# parse columns ####
data_parsed <- data_deduped |>
  mutate(
    date_of_incident = dmy(date_of_incident),
    year_of_incident = year(date_of_incident),
    month_of_incident = month(date_of_incident),
    created_on = dmy_hms(created_on),
    patient_date_of_birth = dmy(patient_date_of_birth),
    patient_age_years = floor((patient_date_of_birth %--% date_of_incident) / years(1)),
    patient_age_months = ifelse(patient_age_years < 2,
      floor((patient_date_of_birth %--% date_of_incident) / months(1)),
      NA
    )
  )

# categorical filters ####

data_filtered_categorical <- data_parsed |>
  # apply categorical filters here
  filter(
    date_of_incident >= "2022-01-01" & date_of_incident <= "2022-01-31"
  )

# text filters ####

data_filtered_text <- data_filtered_categorical |>
  # apply text filters here
  mutate(
    group1_term1 = ifelse(str_detect(description_of_what_happened, "(?i)\\bpatient"), 1, 0)
  ) |>
  filter(group1_term1 > 0)

# no sampling for StEIS since no harm grading

# columns for release ####

data_for_release <- data_filtered_text |>
  # select columns to be released
  select(
    `Log No` = log_no,
    `Created on` = created_on,
    `Organisation reporting SI on STEIS` = organisation_reporting_si_on_steis,
    `Organisation leading investigation` = organisation_leading_investigation,
    `CCG/CSU Name` = ccg_csu_name,
    `Region - Geography` = region_geography,
    `Status` = status,
    `Date of Incident:` = date_of_incident,
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

write_csv(data_for_release, here("csv", "StEIS.csv"))
