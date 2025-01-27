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

start_date <- "2022-01-01"
end_date <- "2023-12-31"
date_type <- "occurring"

source("functions.R")


# nrls

dataset <- "NRLS"
print(glue("Running {dataset} search..."))


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


nrls_filtered_categorical <- nrls_parsed |>
  # apply categorical filters here
  filter(between(date_filter, start_date, end_date)) |>
  # collecting here so that we can apply text filters later
  collect()


# label 
nrls_labelled <- nrls_filtered_categorical |>
  select(INCIDENTID, AGE_AT_INCIDENT, PD04, PD20,IN07, IN10, IN11, IN05_TEXT, PD05_LVL1, PD05_LVL2, PD05_TEXT) |>
  pivot_longer(cols = any_of(codes$col_name)) |>
  left_join(codes, by = c(
    "name" = "col_name",
    "value" = "SASCODE"
  )) |>
  select(!value) |>
  pivot_wider(
    names_from = name,
    values_from = OPTIONTEXT
  )

# Neonatal logic
# AGE_AT_INCIDENT appears to be derived from DV01 so DV01 shouldn't need checking separately
# Searching PD01_B doesn't contain any more information than  AGE_AT_INCIDENTS (from SQL searches)- presume derived field, and removed it
nrls_age_categorised <- nrls_labelled %>%
  mutate(concat_col = paste(IN07, IN10, IN11, IN05_TEXT, PD05_LVL1, PD05_LVL2, PD05_TEXT, sep = "_"),
         age_less_28_days= between(AGE_AT_INCIDENT, 0, 28/365),
         specialty_neonatology= PD05_LVL2 == 'Neonatology',
         poss_neonate_specialty = (PD05_LVL1 == 'Obstetrics and gynaecology' | PD04 == 'A paediatrics specialty' | PD20 == 'Yes'),
         neonate_terms_text = str_detect(concat_col, neonatal_terms),
         neonate_terms_text_no_baby = str_detect(concat_col, neonatal_terms_no_baby),
         age_less_18 = (AGE_AT_INCIDENT >= (28/365) & AGE_AT_INCIDENT < 18),
         camhs_and_age_na = PD05_LVL2 == 'Child and adolescent mental health' & is.na(AGE_AT_INCIDENT),
         other_paed_specialty = PD05_LVL2 %in% c('Community paediatrics', 'Paedodontics'),
         paed_flags = PD04 == 'A paediatrics specialty' | PD20 == 'Yes',
         paed_terms_text = str_detect(concat_col, paediatric_terms))|>
  mutate(
    neopaeds_category = case_when(
      # Neonate by age: age is between 0 and 28 days
      age_less_28_days ~ 'neonates_by_age',
      # Neonate by specialty: neonatology
      specialty_neonatology ~ 'neonates_by_specialty',
      # Neonate by text: obs and gynae or paeds specialty and neo text terms found
      poss_neonate_specialty & neonate_terms_text  ~ 'neonates_by_text',
      
      # paeds by age: between 28 days and 18 
      age_less_18 ~ 'paeds_by_age',
      # paeds by specialty: camhs where age missing, or community paaeds / paedodontics
      camhs_and_age_na | other_paed_specialty ~ 'paeds_by_specialty',
      # paeds by text: paeds specialty and paeds terms found
      paed_flags & paed_terms_text ~ 'paeds_by_text',
      
      # otherwise other
      .default = "other"
    ),
    neopaeds_category_no_baby = case_when(
      # Neonate by age: age is between 0 and 28 days
      age_less_28_days ~ 'neonates_by_age',
      # Neonate by specialty: neonatology
      specialty_neonatology ~ 'neonates_by_specialty',
      # Neonate by text: obs and gynae or paeds specialty and neo text terms found
      poss_neonate_specialty & neonate_terms_text_no_baby  ~ 'neonates_by_text',
      
      # paeds by age: between 28 days and 18 
      age_less_18 ~ 'paeds_by_age',
      # paeds by specialty: camhs where age missing, or community paaeds / paedodontics
      camhs_and_age_na | other_paed_specialty ~ 'paeds_by_specialty',
      # paeds by text: paeds specialty and paeds terms found
      paed_flags & paed_terms_text ~ 'paeds_by_text',
      
      # otherwise other
      .default = "other"
    ),
    neopaeds_category_filter_age = case_when(
      # Neonate by age: age is between 0 and 28 days
      age_less_28_days ~ 'neonates_by_age',
      # Neonate by specialty: neonatology
      specialty_neonatology & is.na(AGE_AT_INCIDENT) ~ 'neonates_by_specialty',
      # Neonate by text: obs and gynae or paeds specialty and neo text terms found
      poss_neonate_specialty & neonate_terms_text  & is.na(AGE_AT_INCIDENT) ~ 'neonates_by_text',
      
      # paeds by age: between 28 days and 18 
      age_less_18 ~ 'paeds_by_age',
      # paeds by specialty: camhs where age missing, or community paaeds / paedodontics
      camhs_and_age_na | (other_paed_specialty & is.na(AGE_AT_INCIDENT)) ~ 'paeds_by_specialty',
      # paeds by text: paeds specialty and paeds terms found
      paed_flags & paed_terms_text & is.na(AGE_AT_INCIDENT) ~ 'paeds_by_text',
      
      # otherwise other
      .default = "other"
    ),
    neopaeds_category_reorder = case_when(
      # Neonate by age: age is between 0 and 28 days
      age_less_28_days ~ 'neonates_by_age',
      # paeds by age: between 28 days and 18 
      age_less_18 ~ 'paeds_by_age',
      # Neonate by specialty: neonatology
      specialty_neonatology ~ 'neonates_by_specialty',
      # paeds by specialty: camhs where age missing, or community paaeds / paedodontics
      camhs_and_age_na | other_paed_specialty ~ 'paeds_by_specialty',
      # Neonate by text: obs and gynae or paeds specialty and neo text terms found
      poss_neonate_specialty & neonate_terms_text  ~ 'neonates_by_text',
      # paeds by text: paeds specialty and paeds terms found
      paed_flags & paed_terms_text ~ 'paeds_by_text',
      
      # otherwise other
      .default = "other"
    ),
  )
rm(nrls_filtered_categorical)

nrls_age_categorised %>% count(neonate_terms_text, neonate_terms_text_no_baby)


incidents_changed_by_removing_baby <- nrls_age_categorised %>%
  filter(neopaeds_category != neopaeds_category_no_baby)

incidents_changed_by_removing_baby %>% count(neopaeds_category, neopaeds_category_no_baby)
#less categorised as neonates when we remove baby from logic, these are mix of other and paeds
#mix of adults and babies>1 month
#use of age column or reordering logic could help with this
write.csv(incidents_changed_by_removing_baby, "data/incidents_changed_by_removing_baby_nrls.csv")

incidents_changed_by_filter_age_na <- nrls_age_categorised %>%
  filter(neopaeds_category != neopaeds_category_filter_age)
#less categorised as neonates when we only look for specialty and by text when age is NA
# some are instead categorised as paeds, some other
#note - reordering logic could help with this
incidents_changed_by_filter_age_na %>% count(neopaeds_category, neopaeds_category_filter_age)
write.csv(incidents_changed_by_filter_age_na, "data/incidents_changed_by_filter_age_NA_nrls.csv")


incidents_changed_by_reorder <-nrls_age_categorised %>% 
  filter(neopaeds_category!=neopaeds_category_reorder) 
incidents_changed_by_reorder %>% count(neopaeds_category, neopaeds_category_filter_age)

# many of the incidents changed by reordering were also changed by filtering where age is NA
incidents_changed_by_reorder %>% filter(INCIDENTID %in% incidents_changed_by_filter_age_na$INCIDENTID) %>% nrow()
# some of the incidents changed by filtering where age is na were also changed by reordering
incidents_changed_by_filter_age_na %>% filter(INCIDENTID %in% incidents_changed_by_reorder$INCIDENTID) %>% nrow()

#these changes look to be the same
incidents_changed_by_reorder %>% filter(INCIDENTID %in% incidents_changed_by_filter_age_na$INCIDENTID) %>% count(neopaeds_category, neopaeds_category_filter_age, neopaeds_category_reorder)


#incidents where filter age and category reorder are not the same 
incidents_different_between_reorder_and_filter_na<-nrls_age_categorised %>% filter(neopaeds_category_filter_age!=neopaeds_category_reorder)
incidents_different_between_reorder_and_filter_na %>% 
  count(neopaeds_category, neopaeds_category_filter_age, neopaeds_category_reorder) 
#reordering captures more incidents-  still retreives neonate incidents with a non NA innapropriate age, but will prioritise "better" logic first 

write.csv(incidents_different_between_reorder_and_filter_na, "data/incidents_different_between_reorder_and_filter_na_nrls.csv")


#get percent for neonate/paed/other when using different logic

counts_initial <- nrls_age_categorised %>% 
  mutate(neopaeds_categorical_shorter_initial = case_when(str_detect(neopaeds_category, "neo")~ "neonate",
                                                  str_detect(neopaeds_category, "paed")~ "paediatric",
                                                  neopaeds_category=="other" ~ "other")) %>%
  count(neopaeds_categorical_shorter_initial, name = "n_initial") %>%
  mutate(proportion_initial= n_initial/sum(n_initial)) %>%
  arrange(neopaeds_categorical_shorter_initial)


counts_no_baby <- nrls_age_categorised %>% 
  mutate(neopaeds_categorical_shorter_no_baby = case_when(str_detect(neopaeds_category_no_baby, "neo")~ "neonate",
                                                          str_detect(neopaeds_category_no_baby, "paed")~ "paediatric",
                                                          neopaeds_category_no_baby=="other" ~ "other")) %>%
  count(neopaeds_categorical_shorter_no_baby, name = "n_no_baby") %>%
  mutate(proportion_no_baby= n_no_baby/sum(n_no_baby)) %>%
  arrange(neopaeds_categorical_shorter_no_baby)

counts_filter_age <- nrls_age_categorised %>% 
  mutate(neopaeds_categorical_shorter_filter_age = case_when(str_detect(neopaeds_category_filter_age, "neo")~ "neonate",
                                                          str_detect(neopaeds_category_filter_age, "paed")~ "paediatric",
                                                          neopaeds_category_filter_age=="other" ~ "other")) %>%
  count(neopaeds_categorical_shorter_filter_age, name = "n_filter_age") %>%
  mutate(proportion_filter_age= n_filter_age/sum(n_filter_age)) %>% 
  arrange(neopaeds_categorical_shorter_filter_age)


counts_reorder <- nrls_age_categorised %>% 
  mutate(neopaeds_categorical_shorter_reorder = case_when(str_detect(neopaeds_category_reorder, "neo")~ "neonate",
                                                             str_detect(neopaeds_category_reorder, "paed")~ "paediatric",
                                                             neopaeds_category_reorder=="other" ~ "other")) %>%
  count(neopaeds_categorical_shorter_reorder, name = "n_reorder") %>%
  mutate(proportion_reorder= n_reorder/sum(n_reorder)) %>%
  arrange(neopaeds_categorical_shorter_reorder)



count_df<-bind_cols(counts_initial, counts_no_baby, counts_filter_age, counts_reorder)
write.csv(count_df, "data/nrls_counts_category.csv")






counts_subcategory_initial <- nrls_age_categorised %>% 
  count(neopaeds_category, name = "n_initial") %>%
  mutate(proportion_initial= n_initial/sum(n_initial)) %>%
  arrange(neopaeds_category)

counts_subcategory_no_baby <- nrls_age_categorised %>% 
  count(neopaeds_category_no_baby, name = "n_no_baby") %>%
  mutate(proportion_no_baby= n_no_baby/sum(n_no_baby)) %>%
  arrange(neopaeds_category_no_baby)

counts_subcategory_filter_age <- nrls_age_categorised %>% 
  count(neopaeds_category_filter_age, name = "n_filter_age") %>%
  mutate(proportion_filter_age= n_filter_age/sum(n_filter_age)) %>%
  arrange(neopaeds_category_filter_age)

counts_subcategory_reorder <- nrls_age_categorised %>% 
  count(neopaeds_category_reorder, name = "n_reorder") %>%
  mutate(proportion_reorder= n_reorder/sum(n_reorder)) %>%
  arrange(neopaeds_category_reorder)

count_df_subcategory<-bind_cols(counts_subcategory_initial, counts_subcategory_no_baby, counts_subcategory_filter_age, counts_subcategory_reorder)
write.csv(count_df_subcategory, "data/nrls_counts_subcategory.csv")










