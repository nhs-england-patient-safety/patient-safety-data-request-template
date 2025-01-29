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
      neonate_specialty_flag = str_detect(L006, neonatal_specialty_terms),
      neonate_terms_flag = str_detect(concat_col, neonatal_terms),
      neonate_terms_flag_no_baby = str_detect(concat_col, neonatal_terms_no_baby),
      adult_specialty_flag = str_detect(L006, adult_specialty_terms),
      paediatric_specialty_flag = str_detect(L006, paediatric_specialty_terms),
      paediatric_term_flag = str_detect(concat_col, paediatric_terms)) #|>
    # #for speed reasons, filter for relevant flags
    # filter(age_category %in% c("neonate","paediatric") | 
    #          neonate_specialty_flag | neonate_terms_flag| neonate_terms_flag_no_baby| 
    #          paediatric_specialty_flag| paediatric_term_flag)
  
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
        .default = NA
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
        .default = NA
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
        .default = NA
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
        .default = NA
      )
    )
  
  lfpse_without_baby<- lfpse_age_classified_with_categorisation %>%
    filter(neopaeds_category!=neopaeds_category_without_baby)
  write.csv(lfpse_without_baby, "data/lfpse_without_baby.csv")
  
  lfpse_without_baby %>%
    count(neopaeds_category, neopaeds_category_without_baby)
   #neonates by text categorised as paediatric by specialty or text
  lfpse_without_baby %>%
    count(neopaeds_category, neopaeds_category_without_baby, L006)
  # these appear to be a mix of older babies and neonates (although most appear to be neonates)- is there a different set of terms to use instead of baby?
  
  
  
  set.seed(123)
  sample_without_baby_ps <-lfpse_without_baby %>% 
    select(L006, concat_col:neopaeds_category_age_not_unknown) %>% 
    filter(neopaeds_category_without_baby == "paediatric_by_specialty") %>%
    sample_n(50) 
  sample_without_baby_pt <-lfpse_without_baby %>% 
    select(L006, concat_col:neopaeds_category_age_not_unknown) %>% 
    filter(neopaeds_category_without_baby == "paediatric_by_text") %>%
    sample_n(50) 
  
  lfpse_age_not_unknown <-lfpse_age_classified_with_categorisation %>%
    filter(neopaeds_category!=neopaeds_category_age_not_unknown) 
  write.csv(lfpse_age_not_unknown, "data/lfpse_age_not_unknown.csv")
  lfpse_age_not_unknown%>%
    count(neopaeds_category, neopaeds_category_age_not_unknown, neopaeds_category_reorder)
  # setting age to be unknown means more incidents are categorised as paediatric (would have been categorised as neonate otherwise)
  
  lfpse_age_classified_with_categorisation %>% 
    count(neopaeds_category, neopaeds_category_without_baby, neopaeds_category_reorder, neopaeds_category_age_not_unknown, sort=T)
  
  
  lfpse_reorder <-lfpse_age_classified_with_categorisation %>%
    filter(neopaeds_category != neopaeds_category_reorder)
  
  write.csv(lfpse_reorder,"data/lfpse_reorder")
  lfpse_reorder %>% count(neopaeds_category, neopaeds_category_reorder)
 # reordering means neonate by text are paediatric by specialty
  
  lfpse_reorder %>% count(L006, neopaeds_category, neopaeds_category_reorder, sort=T)
  # reordering means neonate by text are paediatric by specialty
  
  sample_reorder <-lfpse_reorder    %>%
    select(L006, concat_col:neopaeds_category_age_not_unknown) %>% 
    sample_n(50) 
  # mix of paeds and 
  
  lfpse_reorder %>% count(L006, sort=T)
  
  # would child and adolescent psychiatry ever be a neonate?
  
  well_baby_service<-lfpse_age_classified_with_categorisation %>% filter(str_detect(L006, "Well"))
 write.csv(well_baby_service, "data/well_baby_service.csv")
  well_baby_service %>% count(neopaeds_category, neopaeds_category_reorder, neopaeds_category_without_baby, neopaeds_category_age_not_unknown, sort=T)  
  # reordering doesn't effect - looks like filtering for age unknown is helping                          