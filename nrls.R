# nrls

library(tidyverse)
library(DBI)
library(dbplyr)
library(janitor)
library(here)

# connection ####

con_nrls <- dbConnect(odbc::odbc(),
  Driver = "ODBC Driver 17 for SQL Server",
  Server = Sys.getenv("psims_server"),
  database = Sys.getenv("nrls_database"),
  uid = Sys.getenv("nrls_uid"),
  pwd = Sys.getenv("nrls_pwd")
)

# read tables ####

data <- tbl(con_nrls, in_schema("art", "vw_clean"))

sascodes <- tbl(con_nrls, in_schema("art", "sascodes")) |>
  collect()

codes_ex_rm04 <- sascodes |>
  # fixing in03 lvl1 which is incorrect in the table
  mutate(OPTIONTEXT = str_replace_all(OPTIONTEXT, "Treatment  ", "Treatment ")) |>
  group_by(REFERENCECODE) |>
  mutate(
    n_levels = max(DEPTH) + 1,
    col_name = ifelse(n_levels > 1,
                      paste0(REFERENCECODE, "_LVL", DEPTH + 1),
                      REFERENCECODE
    )
  ) |>
  ungroup() |>
  distinct(col_name, SASCODE, OPTIONTEXT) |>
  # removing these vals which aren't required
  filter(!col_name %in% c('PD01-B',
                          'DV08',
                          'IN01-A-0',
                          'IN02-A-01',
                          'IN02-A-02',
                          'DE10', 
                          'DE11',
                          # these cols have duplicates but don't appear in the data
                          'IN06',
                          'MD03',
                          'PD10_LVL1',
                          'PD10_LVL2'))

codes_rm04 <- data.frame(
  col_name = rep("RM04", times = 4),
  SASCODE = c(3, 4, 8, 9, 10, 50, 51, 60),
  OPTIONTEXT = c("LRMS", "Comm - Pharmacy", "nww - eForm", "www - eForm", "OA - eForm", "GPOOH", "GP - eForm", "ASB")
)

codes <- bind_rows(codes_ex_rm04, codes_rm04)

# parse columns ####

data_parsed <- data |>
  # INCIDENTID is made non-numeric so that it is not included in the codes joining later
  mutate(INCIDENTID = as.character(INCIDENTID)) |>
  # IN05_LVL2 is capitalised for consistency to enable join later
  rename(IN05_LVL2 = IN05_lvl2)

# categorical filters ####

data_filtered_categorical <- data_parsed |>
  # apply categorical filters here
  filter(
   IN01 == "2023-01-01"
  ) |>
  # collecting here so that we can apply text filters later
  collect()

# text filters ####

data_filtered_text <- data_filtered_categorical |>
  #mutate_if(is.character, utf8::utf8_encode) |>
  # apply text filters here
  filter(
    grepl("\\bemergency", IN07, ignore.case = T)
  )

# sampling ####
# Default (if > 300: all death/severe, 100 moderate, 100 low/no harm)

if (nrow(data_filtered_text) > 300) {
  data_death_severe <- data_filtered_text |>
    filter(PD09 %in% c("5", "4"))

  set.seed(123)
  data_moderate <- data_filtered_text |>
    filter(PD09 == "3") |>
    # sample 100, or if fewer than 100, bring all
    sample_n(min(n(),100))

  set.seed(123)
  data_low_no_other <- data_filtered_text |>
    filter(!PD09 %in% c("3", "4", "5")) |>
    sample_n(min(n(),100))

  data_sampled <- bind_rows(
    data_death_severe,
    data_moderate,
    data_low_no_other
  )
} else {
  data_sampled <- data_filtered_text
}

# columns for release ####

data_for_release <- data_sampled |>
  pivot_longer(cols = any_of(codes$col_name)) |>
  left_join(codes, by = c(
    "name" = "col_name",
    "value" = "SASCODE"
  )) |>
  select(!value) |> 
  pivot_wider(
    names_from = name,
    values_from = OPTIONTEXT
  ) |>
  select(
    # the commented columns below need addressing at a later date
    `RP01 Unique Incident ID` = INCIDENTID,
    `Local Trust incident ID` = TRUSTINCIDENTID,
    `RP02 Care Setting of Occurrence` = RP02,
    `RP07 NHS Organisation Code` = RP07,
    `Date of Incident` = IN01,
    `IN03 Location (lvl1)` = IN03_LVL1,
    `IN03 Location (lvl2)` = IN03_LVL2,
    `IN03 Location - Free Text` = IN03_TEXT,
    `IN05 Incident Category - Lvl1` = IN05_LVL1,
    `IN05 Incident Category - Lvl2` = IN05_LVL2,
    `IN05 Incident Category - Free Text` = IN05_TEXT,
    `IN07 Description of what happened` = IN07,
    `IN10 Actions Preventing Reoccurrence` = IN10,
    `IN11 Apparent Causes` = IN11,
    `Age at time of Incident (years)` = AGE_AT_INCIDENT,
    `PD05 Specialty - Lvl 1` = PD05_LVL1,
    `PD05 Specialty - Lvl 2` = PD05_LVL2,
    `PD05 Speciality - Free Text` = PD05_TEXT,
    `PD09 Degree of harm (severity) - display` = PD09,
    `RM04 Source of Notification` = RM04,
    `MD01 Med Process` = MD01,
    `MD01 Med Process Free Text` = MD01_TEXT,
    `MD02 Med Error Category` = MD02,
    `MD02 Med Error Category Free Text` = MD02_TEXT,
    `MD05 Approved Name (Drug 1)` = MD05,
    `MD06 Proprietary Name (Drug 1)` = MD06,
    `MD30 Approved Name (Drug 2)` = MD30,
    `MD31 Proprietary Name (Drug 2)` = MD31 ,
    `DE01 Type of Device` = DE01,
    `DE01 Type of device - free text` = DE01_TEXT,
    `Date incident received by NRLS` = CREATEDDT
    # organisationname
  ) |>
  remove_empty("cols") 

write_csv(data_for_release, here("csv", "NRLS.csv"))
