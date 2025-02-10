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

count(nrls_labelled,PD05_LVL2, sort=T)
count(nrls_labelled, PD05_LVL1, sort=T)

# Neonatal logic
# AGE_AT_INCIDENT appears to be derived from DV01 so DV01 shouldn't need checking separately
# Searching PD01_B doesn't contain any more information than  AGE_AT_INCIDENTS (from SQL searches)- presume derived field, and removed it
nrls_age_categorised <- nrls_labelled %>%
  mutate(PD05_LVL1= if_else(is.na(PD05_LVL1), "", PD05_LVL1),
         PD05_LVL2= if_else(is.na(PD05_LVL2), "", PD05_LVL2),
         PD04 = if_else(is.na(PD04), "", PD04),
         PD20 = if_else(is.na(PD20), "", PD20),
         )|>
  mutate(concat_col = paste(IN07, IN10, IN11, IN05_TEXT, PD05_LVL1, PD05_LVL2, PD05_TEXT, sep = "_"),
         age_less_28_days= !is.na(AGE_AT_INCIDENT) & between(AGE_AT_INCIDENT, 0, 28/365),
         specialty_neonatology= PD05_LVL2 == 'Neonatology',
         poss_neonate_specialty = (PD05_LVL1 == 'Obstetrics and gynaecology' | PD04 == 'A paediatrics specialty' | PD20 == 'Yes'),
         neonate_terms_text = str_detect(concat_col, neonatal_terms),
         neonate_terms_text_no_baby = str_detect(concat_col, neonatal_terms_no_baby),
         age_less_18 =!is.na(AGE_AT_INCIDENT) &  AGE_AT_INCIDENT >= (28/365) & AGE_AT_INCIDENT < 18,
         camhs_and_age_na = PD05_LVL2 == 'Child and adolescent mental health' & is.na(AGE_AT_INCIDENT),
         other_paed_specialty = PD05_LVL2 %in% c('Community paediatrics', 'Paedodontics'),
         paed_flags = PD04 == 'A paediatrics specialty' | PD20 == 'Yes',
         paed_terms_text = str_detect(concat_col, paediatric_terms))|>
  mutate(
    neopaeds_category = case_when(
      # Neonate by age: age is between 0 and 28 days
      age_less_28_days ~ 'neonate_by_age',
      # Neonate by specialty: neonatology
      specialty_neonatology ~ 'neonate_by_specialty',
      # Neonate by text: obs and gynae or paeds specialty and neo text terms found
      poss_neonate_specialty & neonate_terms_text  ~ 'neonate_by_text',
      
      # paediatric by age: between 28 days and 18 
      age_less_18 ~ 'paediatric_by_age',
      # paediatric by specialty: camhs where age missing, or community paaeds / paedodontics
      camhs_and_age_na | other_paed_specialty ~ 'paediatric_by_specialty',
      # paediatric by text: paediatric specialty and paediatric terms found
      paed_flags & paed_terms_text ~ 'paediatric_by_text',
      
      # otherwise other
      .default = "other"
    ),
    neopaeds_category_no_baby = case_when(
      # Neonate by age: age is between 0 and 28 days
      age_less_28_days ~ 'neonate_by_age',
      # Neonate by specialty: neonatology
      specialty_neonatology ~ 'neonate_by_specialty',
      # Neonate by text: obs and gynae or paediatric specialty and neo text terms found
      poss_neonate_specialty & neonate_terms_text_no_baby  ~ 'neonate_by_text',
      
      # paediatric by age: between 28 days and 18 
      age_less_18 ~ 'paediatric_by_age',
      # paediatric by specialty: camhs where age missing, or community paaeds / paedodontics
      camhs_and_age_na | other_paed_specialty ~ 'paediatric_by_specialty',
      # paediatric by text: paediatric specialty and paediatric terms found
      paed_flags & paed_terms_text ~ 'paediatric_by_text',
      
      # otherwise other
      .default = "other"
    ),
    neopaeds_category_filter_age = case_when(
      # Neonate by age: age is between 0 and 28 days
      age_less_28_days ~ 'neonate_by_age',
      # Neonate by specialty: neonatology
      specialty_neonatology & is.na(AGE_AT_INCIDENT) ~ 'neonate_by_specialty',
      # Neonate by text: obs and gynae or paediatric specialty and neo text terms found
      poss_neonate_specialty & neonate_terms_text  & is.na(AGE_AT_INCIDENT) ~ 'neonate_by_text',
      
      # paediatric by age: between 28 days and 18 
      age_less_18 ~ 'paediatric_by_age',
      # paediatric by specialty: camhs where age missing, or community paaeds / paedodontics
      camhs_and_age_na | (other_paed_specialty & is.na(AGE_AT_INCIDENT)) ~ 'paediatric_by_specialty',
      # paediatric by text: paediatric specialty and paediatric terms found
      paed_flags & paed_terms_text & is.na(AGE_AT_INCIDENT) ~ 'paediatric_by_text',
      
      # otherwise other
      .default = "other"
    ),
    neopaeds_category_reorder = case_when(
      # Neonate by age: age is between 0 and 28 days
      age_less_28_days ~ 'neonate_by_age',
      # paediatric by age: between 28 days and 18 
      age_less_18 ~ 'paediatric_by_age',
      # Neonate by specialty: neonatology
      specialty_neonatology ~ 'neonate_by_specialty',
      # paediatric by specialty: camhs where age missing, or community paaeds / paedodontics
      camhs_and_age_na | other_paed_specialty ~ 'paediatric_by_specialty',
      # Neonate by text: obs and gynae or paediatric specialty and neo text terms found
      poss_neonate_specialty & neonate_terms_text  ~ 'neonate_by_text',
      # paediatric by text: paediatric specialty and paediatric terms found
      paed_flags & paed_terms_text ~ 'paediatric_by_text',
      
      # otherwise other
      .default = "other"
    ),
    # Neonate by age: age is between 0 and 28 days
    neonate_by_age = age_less_28_days,
    # Neonate by specialty: neonatology
    neonate_by_specialty = specialty_neonatology,
    # Neonate by text: obs and gynae or paediatric specialty and neo text terms found
    neonate_by_text = poss_neonate_specialty & neonate_terms_text ,
    # paediatric by age: between 28 days and 18 
    paediatric_by_age = age_less_18,
    # paediatric by specialty: camhs where age missing, or community paaeds / paedodontics
    paediatric_by_specialty = camhs_and_age_na | other_paed_specialty,
    # paediatric by text: paediatric specialty and paediatric terms found
    paediatric_by_text = paed_flags & paed_terms_text,
    neonate=neonate_by_age+ neonate_by_specialty+ neonate_by_text,
    paediatric= paediatric_by_age + paediatric_by_specialty+ paediatric_by_text
  )

gc()

nrls_age_categorised <- nrls_age_categorised %>% 
  mutate()



#counts of all flags- check for NAs

nrls_age_categorised %>% count(age_less_28_days, sort=T)
nrls_age_categorised %>% count(specialty_neonatology, sort=T)
nrls_age_categorised %>% count(poss_neonate_specialty, sort=T)
nrls_age_categorised %>% count(neonate_terms_text, sort=T)
nrls_age_categorised %>% count(neonate_terms_text_no_baby, sort=T)
nrls_age_categorised %>% count(age_less_18, sort=T)
nrls_age_categorised %>% count(camhs_and_age_na, sort=T)
nrls_age_categorised %>% count(other_paed_specialty, sort=T)
nrls_age_categorised %>% count(paed_flags, sort=T)
nrls_age_categorised %>% count(paed_terms_text, sort=T)
nrls_age_categorised %>%  count(neonate_by_specialty)
nrls_age_categorised %>% count(neopaeds_category)
rm(nrls_filtered_categorical)



#get counts 

nrls_age_categorised %>%
     mutate(neopaeds_shorter= str_sub(neopaeds_category,1,6)) %>%
     count(neopaeds_shorter)

nrls_age_categorised %>% count(neopaeds_category)

 
# get counts with "baby"
 neonate_by_text_baby<- nrls_age_categorised %>%
   filter(neopaeds_category=="neonate_by_text") %>%
   mutate(nicu=str_detect(concat_col,"(?i)\\bn(?:|\\W)i(?:|\\W)c(?:|\\W)u\\b"),
          nnu=str_detect(concat_col,"(?i)\\bn(?:|\\W)n(?:|\\W)u\\b"),
          scbu= str_detect(concat_col,"(?i)\\bs(?:|\\W)c(?:|\\W)b(?:|\\W)u\\b"),
          neonat=str_detect(concat_col,"(?i)\\bneonat"),
          baby = str_detect(concat_col,"(?i)\\bbaby")) %>%
   mutate(non_baby_flag=nicu + nnu + scbu + neonat)

# 
 
 neonate_by_text_baby %>% 
   count(non_baby_flag>0, baby)

neonate_by_text_filter_baby_only<- neonate_by_text_baby %>% filter(non_baby_flag==0, baby)
# 
# #what does removing "baby" do
 neonate_by_text_baby %>% 
   filter(neopaeds_category!=neopaeds_category_no_baby) %>%
   count(neopaeds_category, neopaeds_category_no_baby)
# 
 nrls_age_categorised   %>% 
   filter(neopaeds_category!=neopaeds_category_no_baby) %>%
   count(neopaeds_category, neopaeds_category_no_baby)

 # #what does reordering do FOR THESE SPECIFIC INCIDENTS
 neonate_by_text_baby %>% 
   filter(neopaeds_category!=neopaeds_category_reorder) %>%
   count(neopaeds_category, neopaeds_category_reorder)
# #what would reordering do for all incidents
 nrls_age_categorised %>% 
   filter(neopaeds_category!=neopaeds_category_reorder) %>%
   count(neopaeds_category, neopaeds_category_reorder)

# 
 #effect of unknown
# 
 nrls_age_categorised %>% 
   filter(neopaeds_category!=neopaeds_category_filter_age)%>%
   count(neopaeds_category,neopaeds_category_filter_age, sort=T)

 
# 
# 
# #how often are things categorised as neonate and paediatric

nrls_age_categorised%>%
   count(neonate>0 & paediatric>0) 



#what are the categories when both neonate and paeditric
 categories_neonate_and_paed<-nrls_age_categorised %>%
   filter(neonate>0 & paediatric>0) %>%
   count(neonate_by_age, neonate_by_specialty, neonate_by_text,
         paediatric_by_age, paediatric_by_specialty, paediatric_by_text, sort=T)
# 
# 
# 
nrls_age_categorised %>%
   filter(neopaeds_category!=neopaeds_category_reorder) %>%
   count(neopaeds_category, neopaeds_category_reorder)
# 
# 
# #age validation
nrls_age_categorised %>% filter(is.na(AGE_AT_INCIDENT)) %>%nrow()/  nrow(nrls_age_categorised)
# # % of incidents with NA age
















































nrls_age_categorised %>% count(neonate_terms_text, neonate_terms_text_no_baby)


incidents_changed_by_removing_baby <- nrls_age_categorised %>%
  filter(neopaeds_category != neopaeds_category_no_baby)

incidents_changed_by_removing_baby %>% count(neopaeds_category, neopaeds_category_no_baby)
#less categorised as neonate when we remove baby from logic, these are mix of other and paeds
#mix of adults and babies>1 month
#use of age column or reordering logic could help with this
write.csv(incidents_changed_by_removing_baby, "data/incidents_changed_by_removing_baby_nrls.csv")

incidents_changed_by_filter_age_na <- nrls_age_categorised %>%
  filter(neopaeds_category != neopaeds_category_filter_age)
#less categorised as neonate when we only look for specialty and by text when age is NA
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










