library(tidyverse)
library(dbplyr)
library(janitor)
library(here)
library(openxlsx)
library(glue)
library(Microsoft365R)

# datasets to be searched (T/F)
search_nrls <- T
search_lfpse <- T
search_steis <- T

# connect to (relevant) data bases and bring corresponding look ups 
source("connections.R")
source("neopaeds.R")

# date filter (type is occurring/reported)

start_date <- "2023-01-01"
end_date <- "2024-12-31"
date_type <- "occurring"

source("functions.R")

# lfpse
dataset <- "LFPSE"
print(glue("Running {dataset} search..."))


# set latest revision table from events
latest_revision_table <- tbl(con_lfpse, in_schema("analysis", "Events")) |>
  group_by(Reference) |>
  summarise(
    Revision = max(Revision), # i.e., the most up to date version of a record
    reported_date = min(SubmissionDate)
  ) |>
  ungroup()

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
  "Governance_Responses" # ,
  # "Findings_Responses"#,
  # "DmdMedication_Responses"
)

# bring all tables together
analysis_tables <- lapply(analysis_table_names, function(x) {
  tbl(con_lfpse, in_schema("analysis", x))
})

lfpse_analysis_tables <- c(list(latest_revision_table), analysis_tables)

# duplicates will be present due to inclusion of Patient_Responses which is one row per patient (EntityId)
lfpse_parsed <- reduce(lfpse_analysis_tables,
                       left_join,
                       by = c("Reference", "Revision")
) |>
  rename(occurred_date = T005) |>
  # a conversion factor from days will be needed here, but appears to be DQ issues
  # suggest we wait for resolution before converting from days to years
  mutate(P004_days = as.numeric(P004))

# sql_render(lfpse_parsed) this is a useful step to check the SQL has rendered sensibly

# record time to keep track of query speeds
tic_lfpse <- Sys.time()

lfpse_filtered_categorical <- lfpse_parsed |>
  filter(
    between(date_filter, start_date, end_date),
    # apply categorical filters here
  ) |>
  # collecting here so that we can apply text filters later
  collect()

toc_lfpse <- Sys.time()

time_diff_lfpse <- toc_lfpse - tic_lfpse

print(glue("Extraction from {dataset} server: {round(time_diff_lfpse[[1]], 2)} {attr(time_diff_lfpse, 'units')}"))

print(glue("- {dataset} categorical filters retrieved {format(nrow(lfpse_filtered_categorical), big.mark = ',')} incidents."))

# UP TO HERE

  # labelling ####
  # Adding in field names
  lfpse_labelled <- lfpse_filtered_categorical |>
    select(Reference, TaxonomyVersion, EntityId, F001, AC001, OT003, A008_Other, L006, L006_Other, P004_days, P007)|>
    # pivot the coded columns
    pivot_longer(cols = any_of(ResponseReference$QuestionId)) |>
    # separate the multi-responses into single row per selection
    separate_rows(value, sep = " {~@~} ") |>
    # arrange so that multi-responses appear alphabetised later
    arrange(value) |>
    # bring through the value labels
    left_join(ResponseReference, by = c(
      "name" = "QuestionId",
      "value" = "ResponseCode",
      "TaxonomyVersion" = "TaxonomyVersion"
    )) |>
    # remove the unnecessary columns
    select(!c(value, Property)) |>
    # pivot back into columns
    pivot_wider(
      id_cols = !any_of(ResponseReference$QuestionId),
      names_from = name,
      values_from = ResponseText,
      # collapse multi-responses into single row per entity
      values_fn = list(ResponseText = ~ str_c(., collapse = "; "))
    ) |>
    group_by(Reference) |>
    mutate(npatient = max(EntityId)) |>
    ungroup()
  
  
  lfpse_age_validated<- lfpse_labelled |>
    mutate(age_unit = case_when(
      is.na(P004_days) ~ 'age missing',
      between(P004_days, 1, 30) ~ 'days',
      between(P004_days, 31, 341) ~ 'months',
      between(P004_days, 372, 74028) ~ 'years',
      .default = 'age outside bounds')) |>
    mutate(age_compliance = case_when(
      age_unit == 'age outside bounds' ~ 'age outside bounds',
      age_unit == 'age missing' ~ 'age missing',
      age_unit == 'days' & between(P004_days, 1, 30) ~ 'yes',
      age_unit == 'months' & P004_days %% 31 == 0 ~ 'yes',
      age_unit == 'years' & P004_days %% 372 == 0 ~ 'yes',
      .default = 'no')) |>
    mutate(P004_days_validated = if_else(
      age_compliance == "yes", P004_days, NA
    ))
  
  
  lfpse_age_classified <- lfpse_age_validated |>
    mutate(
      concat_col = paste(F001, AC001, OT003, A008_Other, L006, L006_Other, sep = "_"),
      age_category = case_when(
        (P004_days_validated > 0 & P004_days_validated <= 28) | (P007 %in% c("0-14 days", "15-28 days")) ~ "neonate",
        (P004_days_validated > 28 & P004_days_validated < 6696) | (P007 %in% c("1-11 months", "1-4 years", "5-9 years", "10-15 years", "16 and 17 years")) ~ "paediatric",
        (!is.na(P007)|!is.na(P004_days_validated)) ~ 'adult estimated',
       is.na(P004_days_validated) ~ 'unknown',# includes those where age is below zero / above believable threshold
       .default = 'other' 
      ),
      L006 = if_else(is.na(L006),"", L006), #required for text search to work as expected
      neonate_specialty_flag = str_detect(L006, neonatal_specialty_terms),
      neonate_terms_flag = str_detect(concat_col, neonatal_terms),
      neonate_terms_flag_no_baby = str_detect(concat_col, neonatal_terms_no_baby),
      adult_specialty_flag = str_detect(L006, adult_specialty_terms),
      paediatric_specialty_flag = str_detect(L006, paediatric_specialty_terms),
      paediatric_term_flag = str_detect(concat_col, paediatric_terms))
  
  lfpse_age_classified_with_categorisation <- 
    lfpse_age_classified |>
    mutate(
      neopaeds_category = case_when(
        # Neonate by age: age is between 0 and 28 days
        age_category == 'neonate' ~ "neonate_by_age",
        # Neonate by specialty: age is 0 or NA and specialty indicates neonate
        age_category == 'unknown' &  neonate_specialty_flag ~ "neonate_by_specialty",
        # Neonate by text: age is 0 or NA and text indicates neonate and specialty is not adult
        (age_category == 'unknown' & neonate_terms_flag & ! adult_specialty_flag) ~ "neonate_by_text",
        # Paediatrics by age: age is older than 1 month and younger than 18 years
        age_category == 'paediatric' ~ "paediatric_by_age",
        # Paediatrics by specialty: age is 0 or NA and specialty indicates paediatrics
        (age_category == 'unknown' & paediatric_specialty_flag) ~ "paediatric_by_specialty",
        # Paediatrics by text: age is 0 or NA and text indicates paediatrics
        (age_category == 'unknown' & paediatric_term_flag & ! adult_specialty_flag) ~ "paediatric_by_text",
        # Default: not neonate/paediatrics-related
        .default = "adult"
      ),
      neopaeds_category_without_baby = case_when(
        # Neonate by age: age is between 0 and 28 days
        age_category == 'neonate' ~ "neonate_by_age",
        # Neonate by specialty: age is 0 or NA and specialty indicates neonate
        age_category == 'unknown' &  neonate_specialty_flag ~ "neonate_by_specialty",
        # Neonate by text: age is 0 or NA and text indicates neonate and specialty is not adult
        (age_category == 'unknown' & neonate_terms_flag_no_baby & ! adult_specialty_flag) ~ "neonate_by_text",
        # Paediatrics by age: age is older than 1 month and younger than 18 years
        age_category == 'paediatric' ~ "paediatric_by_age",
        # Paediatrics by specialty: age is 0 or NA and specialty indicates paediatrics
        (age_category == 'unknown' & paediatric_specialty_flag) ~ "paediatric_by_specialty",
        # Paediatrics by text: age is 0 or NA and text indicates paediatrics
        (age_category == 'unknown' & paediatric_term_flag & ! adult_specialty_flag) ~ "paediatric_by_text",
        # Default: not neonate/paediatrics-related
        .default = "adult"
      ),
      neopaeds_category_reorder = case_when(
        # Neonate by age: age is between 0 and 28 days
        age_category == 'neonate' ~ "neonate_by_age",
        # Paediatrics by age: age is older than 1 month and younger than 18 years
        age_category == 'paediatric' ~ "paediatric_by_age",
        # Neonate by specialty: age is 0 or NA and specialty indicates neonate
        age_category == 'unknown' &  neonate_specialty_flag ~ "neonate_by_specialty",
        # Paediatrics by specialty: age is 0 or NA and specialty indicates paediatrics
        (age_category == 'unknown' & paediatric_specialty_flag) ~ "paediatric_by_specialty",
        # Neonate by text: age is 0 or NA and text indicates neonate and specialty is not adult
        (age_category == 'unknown' & neonate_terms_flag & ! adult_specialty_flag) ~ "neonate_by_text",
        # Paediatrics by text: age is 0 or NA and text indicates paediatrics
        (age_category == 'unknown' & paediatric_term_flag & ! adult_specialty_flag) ~ "paediatric_by_text",
        # Default: not neonate/paediatrics-related
        .default = "adult"
      ),
      neopaeds_category_age_not_unknown = case_when(
        # Neonate by age: age is between 0 and 28 days
        age_category == 'neonate' ~ "neonate_by_age",
        # Neonate by specialty: age is 0 or NA and specialty indicates neonate
        neonate_specialty_flag ~ "neonate_by_specialty",
        # Neonate by text: age is 0 or NA and text indicates neonate and specialty is not adult
        (neonate_terms_flag & ! adult_specialty_flag) ~ "neonate_by_text",
        # Paediatrics by age: age is older than 1 month and younger than 18 years
        age_category == 'paediatric' ~ "paediatric_by_age",
        # Paediatrics by specialty: age is 0 or NA and specialty indicates paediatrics
        ( paediatric_specialty_flag) ~ "paediatric_by_specialty",
        # Paediatrics by text: age is 0 or NA and text indicates paediatrics
        (paediatric_term_flag & ! adult_specialty_flag) ~ "paediatric_by_text",
        # Default: not neonate/paediatrics-related
        .default = "adult"
      ),
      neopaeds_category_not_well_baby= case_when(
        # Neonate by age: age is between 0 and 28 days
        age_category == 'neonate' ~ "neonate_by_age",
        # Neonate by specialty: age is 0 or NA and specialty indicates neonate
        age_category == 'unknown' &  str_detect(L006, "(?i)\\bneonat") ~ "neonate_by_specialty",
        # Neonate by text: age is 0 or NA and text indicates neonate and specialty is not adult
        (age_category == 'unknown' & neonate_terms_flag_no_baby & ! adult_specialty_flag) ~ "neonate_by_text",
        # Paediatrics by age: age is older than 1 month and younger than 18 years
        age_category == 'paediatric' ~ "paediatric_by_age",
        # Paediatrics by specialty: age is 0 or NA and specialty indicates paediatrics
        (age_category == 'unknown' & paediatric_specialty_flag) ~ "paediatric_by_specialty",
        # Paediatrics by text: age is 0 or NA and text indicates paediatrics
        (age_category == 'unknown' & paediatric_term_flag & ! adult_specialty_flag) ~ "paediatric_by_text",
        # Default: not neonate/paediatrics-related
        .default = "adult"
      )
      )
  
  
  #get counts 
  
  lfpse_age_classified_with_categorisation %>% 
    mutate(neopaeds_shorter= str_sub(neopaeds_category,1,6)) %>%
    count(neopaeds_shorter)
  lfpse_age_classified_with_categorisation %>% count(neopaeds_category)
  
  
  # get counts with "baby"
 neonate_by_text_baby<- lfpse_age_classified_with_categorisation %>%
    filter(neopaeds_category=="neonate_by_text") %>%
    mutate(nicu=str_detect(concat_col,"(?i)\\bn(?:|\\W)i(?:|\\W)c(?:|\\W)u\\b"),
           nnu=str_detect(concat_col,"(?i)\\bn(?:|\\W)n(?:|\\W)u\\b"),
           scbu= str_detect(concat_col,"(?i)\\bs(?:|\\W)c(?:|\\W)b(?:|\\W)u\\b"),
           neonat=str_detect(concat_col,"(?i)\\bneonat"),
           baby = str_detect(concat_col,"(?i)\\bbaby")) %>%
    mutate(non_baby_flag=nicu + nnu + scbu + neonat)
  
  
  neonate_by_text_baby %>% 
    count(non_baby_flag>0, baby)
  neonate_by_text_filter_baby_only<- neonate_by_text_baby %>% filter(non_baby_flag==0, baby)
  
  #what does removing "baby" do
  neonate_by_text_baby %>% 
    filter(neopaeds_category!=neopaeds_category_without_baby) %>%
    count(neopaeds_category, neopaeds_category_without_baby)
  
  lfpse_age_classified_with_categorisation %>% 
    filter(neopaeds_category!=neopaeds_category_without_baby) %>%
    count(neopaeds_category, neopaeds_category_without_baby)
  #what does reordering do FOR THESE SPECIFIC INCIDENTS
  neonate_by_text_baby %>% 
    filter(neopaeds_category!=neopaeds_category_reorder) %>%
    count(neopaeds_category, neopaeds_category_reorder)
  #what would reordering do for all incidents
  lfpse_age_classified_with_categorisation %>% 
    filter(neopaeds_category!=neopaeds_category_reorder) %>%
    count(neopaeds_category, neopaeds_category_reorder)
  
  #neonate specialties
  lfpse_age_classified_with_categorisation %>% 
    filter(neopaeds_category=="neonate_by_specialty") %>%
    count(L006)
  
  #well baby filter
  well_baby_neonate_specialty<-lfpse_age_classified_with_categorisation %>%
    filter(neopaeds_category=="neonate_by_specialty") %>%
    filter(str_detect(L006, "Well"))
  
  #effect of removing well baby clinic 
  lfpse_age_classified_with_categorisation %>% 
    filter(neopaeds_category!=neopaeds_category_not_well_baby)%>%
    count(neopaeds_category, neopaeds_category_not_well_baby)
 
   well_baby_neonate_specialty %>% 
    filter(neopaeds_category!=neopaeds_category_not_well_baby)%>%
    count(neopaeds_category, neopaeds_category_not_well_baby)
  
  #effecting of reordering for well baby specialty
  well_baby_neonate_specialty %>% 
    filter(neopaeds_category!=neopaeds_category_reorder)%>%
    count(neopaeds_category, neopaeds_category_reorder)
  
  
  #effect of unknown
  
  lfpse_age_classified_with_categorisation %>% 
    filter(neopaeds_category!=neopaeds_category_age_not_unknown)%>%
    count(neopaeds_category,neopaeds_category_age_not_unknown)
  
  
  lfpse_age_classified_with_categorisation <- 
    lfpse_age_classified_with_categorisation |>
    mutate(
      neonate_by_age = 
        # Neonate by age: age is between 0 and 28 days
        age_category == 'neonate' ,
      neonate_by_specialty=
        # Neonate by specialty: age is 0 or NA and specialty indicates neonate
        age_category == 'unknown' &  neonate_specialty_flag ,
      neonate_by_text = 
        # Neonate by text: age is 0 or NA and text indicates neonate and specialty is not adult
        (age_category == 'unknown' & neonate_terms_flag & ! adult_specialty_flag),
      paediatric_by_age= 
        # Paediatrics by age: age is older than 1 month and younger than 18 years
        age_category == 'paediatric' ,
      paediatric_by_specialty=
        # Paediatrics by specialty: age is 0 or NA and specialty indicates paediatrics
        (age_category == 'unknown' & paediatric_specialty_flag),
      paediatrics_by_text = 
        # Paediatrics by text: age is 0 or NA and text indicates paediatrics
        (age_category == 'unknown' & paediatric_term_flag & ! adult_specialty_flag)
    )
  

  #how often are things categorised as neonate and paediatric
  lfpse_age_classified_with_categorisation<-lfpse_age_classified_with_categorisation %>% 
    mutate(neonate=neonate_by_age+ neonate_by_specialty+ neonate_by_text,
           paediatric= paediatric_by_age + paediatric_by_specialty+ paediatrics_by_text) 
  lfpse_age_classified_with_categorisation%>%
    count(neonate>0 & paediatric>0) 
  

  #what are the categories when both neonate and paeditric
  categories_neonate_and_paed<-lfpse_age_classified_with_categorisation %>%
    filter(neonate>0 & paediatric>0) %>%
    count(neonate_by_age, neonate_by_specialty, neonate_by_text,
          paediatric_by_age, paediatric_by_specialty, paediatrics_by_text, sort=T)
  
  
  
  lfpse_age_classified_with_categorisation %>%
     filter(neopaeds_category!=neopaeds_category_reorder) %>%
     count(neopaeds_category, neopaeds_category_reorder)
  
  
  #age validation
  lfpse_age_classified_with_categorisation %>% filter(is.na(P004_days_validated)) %>%nrow()/  nrow(lfpse_age_classified_with_categorisation)
  # % of incidents in lfpse with no valid age
  lfpse_age_classified_with_categorisation %>% filter((!is.na(P004_days) & P004_days!=0) & is.na(P004_days_validated)) %>%nrow()/  nrow(lfpse_age_classified_with_categorisation)
  # % of incidents with a value in P004 with an invalid age
  lfpse_age_classified_with_categorisation %>% filter(!is.na(P004_days) & is.na(P004_days_validated)) %>%nrow()/  nrow(lfpse_age_classified_with_categorisation)
  #% of incidents with a non 0 value in P004 with an invalid age
  lfpse_age_classified_with_categorisation %>% filter(age_category=="unknown") %>%nrow() / nrow(lfpse_age_classified_with_categorisation)
  #% of incidents with an unknown age category
  

  