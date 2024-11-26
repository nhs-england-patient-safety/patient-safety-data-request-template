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
  filter(!!steis_categorical)

print(glue("- {dataset} categorical filters retrieved {format(nrow(steis_filtered_categorical), big.mark = ',')} incidents."))

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
  
  print(glue("{dataset} text search retrieved {format(nrow(steis_filtered_text), big.mark = ',')} incidents."))
  
} else {
  print('- No text terms supplied. Skipping text search...')
  steis_filtered_text <- steis_filtered_categorical
}

# Neonate Search
steis_with_category <- steis_filtered_text %>%
  mutate(
    neonate_category = case_when(
      # Neonate by age: age is less than 1 year
      (patient_age_years < 1 | patient_age_months == 1) ~ "neonates_by_age",
      
      # Neonate by specialty: age is NA and specialty indicates neonate
      (is.na(patient_age_years)) &
        grepl("(?i)\\bneonat|\\bbaby", paste(clinical_area_other, care_sector_other, sep = "")) ~ "neonates_by_specialty",
      
      # Neonate by text: age is 0 or NA and text indicates neonate
      (is.na(patient_age_years)) &
        str_detect(paste(care_sector_other, type_of_incident_other, clinical_area_other, description_of_what_happened, immediate_action_taken, key_findings, case_summary, how_will_lessons_be_disseminated_to_interested_parties, sep = ""), 
                   "(?i)\\bn(?:|\\W)i(?:|\\W)c(?:|\\W)u\\b|\\bn(?:|\\W)n(?:|\\W)u\\b|\\bs(?:|\\W)c(?:|\\W)b(?:|\\W)u\\b|\\bneonat|\\bbaby") ~ "neonates_by_text",
      
      (str_detect(paste(care_sector_other, type_of_incident_other, clinical_area_other, description_of_what_happened, immediate_action_taken, key_findings, case_summary, how_will_lessons_be_disseminated_to_interested_parties, sep = ""),
                  "(?i)\\badult|\\bold|\\belderly|\\bgeriat")) ~ "adult_specialty",
      
      # Default: not neonate-related
      TRUE ~ "other"
    ),
    paediatric_category = case_when(
      # Paediatrics by age: age is between 0 and 17 years
      ((patient_age_years > 0 & patient_age_years <= 17) | patient_age_months %in% c(1:12)) ~ "paediatrics_by_age",
      
      # Paediatrics by specialty: age is NA and specialty indicates paediatrics
      (is.na(patient_age_years) &
         grepl("(?i)\\bpaed|\\bchild", paste(clinical_area_other, care_sector_other, sep = ""))) ~ "paediatrics_by_specialty",
      
      # Paediatrics by text: age is NA and text indicates paediatrics
      (is.na(patient_age_years) &
         str_detect(paste(care_sector_other, type_of_incident_other, clinical_area_other, description_of_what_happened, immediate_action_taken, key_findings, case_summary, how_will_lessons_be_disseminated_to_interested_parties, sep = ""), 
                    "(?i)\\bp(?:|\\W)i(?:|\\W)c(?:|\\W)u\\b|\\bc(?:|\\W)a(?:|\\W)m(?:|\\W)h(?:|\\W)s\\b|\\bpaed|\\binfant|\\bschool\\bchild")) ~ "paediatrics_by_text",
      
      (str_detect(paste(care_sector_other, type_of_incident_other, clinical_area_other, description_of_what_happened, immediate_action_taken, key_findings, case_summary, how_will_lessons_be_disseminated_to_interested_parties, sep = ""),
                  "(?i)\\badult|\\bold|\\belderly|\\bgeriat")) ~ "adult_specialty",
      
      # Default: not neonate/paediatrics-related
      TRUE ~ "other"
    )
  )

# Now filter based on `is_neopaed` parameter
if (is_neopaed == "neonate") {
  print("- Running neonate strategy...")
  
  steis_neopaed <- steis_with_category %>%
    filter(neonate_category %in% c("neonates_by_age", "neonates_by_specialty", "neonates_by_text") &
             neonate_category != "adult_specialty")
  
} else if (is_neopaed == "paed") {
  print("- Running paediatric strategy...")
  
  steis_neopaed <- steis_with_category %>%
    filter(paediatric_category %in% c("paediatrics_by_age", "paediatrics_by_specialty", "paediatrics_by_text") &
             paediatric_category != "adult_specialty")
  
} else if (is_neopaed == "none") {
  print("- Skipping neopaeds strategy...")
  
  steis_neopaed <- steis_with_category
}

# check whether the text search generated results 
if(nrow(steis_neopaed) != 0){

# columns for release ####
if(cols_to_extract == 'all'){
  steis_for_release <- steis_neopaed
} else if (cols_to_extract == 'default'){
  steis_for_release <- steis_neopaed |>
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

} else {
print(glue('**The search criteria has produced no results in {dataset}**'))
print(glue('Moving on...'))
}

source('formatter.R')
