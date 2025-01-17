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
  collect()

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

if(nrow(nrls_filtered_text) != 0){

# label #### 
nrls_labelled <- nrls_filtered_text |>
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
# it would be good to confirm this for PD01_b - if so, PD01_b references can be removed
nrls_age_categorised <- nrls_labelled %>%
  mutate(
    concat_col = paste(IN07, IN10, IN11, IN05_TEXT, PD05_LVL1, PD05_LVL2, PD05_TEXT, sep = "_"),
    neopaeds_category = case_when(
      # Neonate by age: age is between 0 and 28 days
      PD01_b == 1 | between(AGE_AT_INCIDENT, 0, 28/365) ~ 'neonates_by_age',
      # Neonate by specialty: neonatology
      PD05_lvl2 == 'Neonatology' ~ 'neonates_by_specialty',
      # Neonate by text: obs and gynae or paeds specialty and neo text terms found
      (PD05_lvl1 == 'Obstetrics and gynaecology' | PD04 == 'A paediatrics specialty' | PD20 == 'Yes') & str_detect(concat_col, neo_terms) ~ 'neonates_by_text',
      # paeds by age: between 28 days and 18 
      (PD01_b > 1 & PD01_b <= 5) | (AGE_AT_INCIDENT >= (28/365) & AGE_AT_INCIDENT < 18) ~ 'paeds_by_age',
      # paeds by specialty: camhs where age missing, or community paaeds / paedodontics
      (PD05_lvl2 == 'Child and adolescent mental health' & (is.na(AGE_AT_INCIDENT) | is.na(PD01_b))) | PD05_lvl2 %in% c('Community paediatrics', 'Paedodontics') ~ 'paeds_by_specialty',
      # paeds by text: paeds specialty and paeds terms found
      (PD04 == 'A paediatrics specialty' | PD20 == 'Yes') & str_detect(concat_col, paed_terms) ~ 'paeds_by_text'
      # otherwise other
      .default = "other"
    )
  )

# Now filter based on `is_neopaed` parameter
if (is_neopaed == "neonate") {
  print("- Running neonate strategy...")
  
  nrls_neopaed <- nrls_age_categorised %>%
    filter(neopaeds_category %in% c("neonates_by_age", "neonates_by_specialty", "neonates_by_text"))
  
} else if (is_neopaed == "paed") {
  print("- Running paediatric strategy...")
  
  nrls_neopaed <- nrls_age_categorised %>%
    filter(neopaeds_category %in% c("paeds_by_age", "paeds_by_specialty", "paeds_by_text"))
  
} else if (is_neopaed == "none") {
  print("- Skipping neopaeds strategy...")
  
  nrls_neopaed <- nrls_age_categorised
}

# check whether the text search generated results
if (nrow(nrls_neopaed) != 0) {
  # sampling ####
  # Default (if > 300: all death/severe, 100 moderate, 100 low/no harm)
  if (sampling_strategy == "default") {
    if (nrow(nrls_neopaed) > 300) {
      print("- Sampling according to default strategy...")
      nrls_death_severe <- nrls_neopaed |>
        filter(PD09 %in% c("Death", "Severe"))

      set.seed(123)
      nrls_moderate <- nrls_neopaed |>
        filter(PD09 == "Moderate") |>
        # sample 100, or if fewer than 100, bring all
        sample_n(min(n(), 100))

      set.seed(123)
      nrls_low_no_other <- nrls_neopaed |>
        filter(!PD09 %in% c("Moderate", "Severe", "Death")) |>
        sample_n(min(n(), 100))

      nrls_sampled <- bind_rows(
        nrls_death_severe,
        nrls_moderate,
        nrls_low_no_other
      )
    } else {
      print("- Sampling not required, default threshold not met.")
      nrls_sampled <- nrls_neopaed
    }
  } else if (sampling_strategy == "FOI") {
    print("- Extracting a sample of 30 incidents for redaction...")
    set.seed(123)
    nrls_sampled <- nrls_neopaed |>
      sample_n(min(n(), 30))
  } else if (sampling_strategy == "none") {
    print("- Skipping sampling...")
    nrls_sampled <- nrls_neopaed
  }

  # columns for release ####
  nrls_for_release <- nrls_sampled |>
    left_join(organisations, by = c("RP07" = "ORGANISATIONCODE")) |>
    select(
      `RP01 Unique Incident ID` = INCIDENTID,
      `Local Trust incident ID` = TRUSTINCIDENTID,
      `RP02 Care Setting of Occurrence` = RP02,
      `RP07 NHS Organisation Code` = RP07,
      `Organisation Name` = ORGANISATIONNAME,
      `Date of Incident` = occurred_date,
      `IN03 Location (lvl1)` = IN03_LVL1,
      `IN03 Location (lvl2)` = IN03_LVL2,
      `IN03 Location (lvl3)` = IN03_LVL3,
      `IN03 Location - Free Text` = IN03_TEXT,
      `IN04 Country` = IN04,
      `IN05 Incident Category - Lvl1` = IN05_LVL1,
      `IN05 Incident Category - Lvl2` = IN05_LVL2,
      `IN05 Incident Category - Free Text` = IN05_TEXT,
      `Age at time of Incident (years)` = AGE_AT_INCIDENT,
      `DV01 Age Range` = DV01,
      `PD05 Specialty - Lvl 1` = PD05_LVL1,
      `PD05 Specialty - Lvl 2` = PD05_LVL2,
      `PD05 Speciality - Free Text` = PD05_TEXT,
      `IN07 Description of what happened` = IN07,
      `IN10 Actions Preventing Reoccurrence` = IN10,
      `IN11 Apparent Causes` = IN11,
      `PD09 Degree of harm (severity)` = PD09,
      `RM04 Source of Notification` = RM04,
      `MD01 Med Process` = MD01,
      `MD01 Med Process Free Text` = MD01_TEXT,
      `MD02 Med Error Category` = MD02,
      `MD02 Med Error Category Free Text` = MD02_TEXT,
      `MD05 Approved Name (Drug 1)` = MD05,
      `MD06 Proprietary Name (Drug 1)` = MD06,
      `PD02 Patient Sex` = PD02,
      `PD04 Adult/Paediatrics Specialty` = PD04,
      `PD20 Paediatric ward/department/unit` = PD20, # Changed this from PD04 to PD20
      `MD30 Approved Name (Drug 2)` = MD30,
      `MD31 Proprietary Name (Drug 2)` = MD31,
      `DE01 Type of Device` = DE01,
      `DE01 Type of device - free text` = DE01_TEXT,
      `Date incident received by NRLS` = reported_date
    ) |>
    remove_empty("cols")

  print(glue("- Final {dataset} dataset contains {nrow(nrls_for_release)} incidents."))
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
