# connections

library(DBI)

# nrls ####
if (search_nrls) {
  con_nrls <- dbConnect(odbc::odbc(),
                        Driver = "ODBC Driver 17 for SQL Server",
                        Server = Sys.getenv("psims_server"),
                        database = Sys.getenv("nrls_database"),
                        uid = Sys.getenv("nrls_uid"),
                        pwd = Sys.getenv("nrls_pwd")
                        )
  
  # reading categorical reference tables
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
    filter(!col_name %in% c(
      "PD01-B",
      "DV08",
      "IN01-A-0",
      "IN02-A-01",
      "IN02-A-02",
      "DE10",
      "DE11",
      # these cols have duplicates but don't appear in the data
      "IN06",
      "MD03",
      "PD10_LVL1",
      "PD10_LVL2"
    ))
  
  codes_rm04 <- data.frame(
    col_name = rep("RM04", times = 4),
    SASCODE = c(3, 4, 8, 9, 10, 50, 51, 60),
    OPTIONTEXT = c("LRMS", "Comm - Pharmacy", "nww - eForm", "www - eForm", "OA - eForm", "GPOOH", "GP - eForm", "ASB")
  )
  
  codes <- bind_rows(codes_ex_rm04, codes_rm04)
  
  } 

# lfpse ####
if (search_lfpse) {
    
    con_lfpse <- dbConnect(odbc::odbc(),
                           Driver = "ODBC Driver 17 for SQL Server",
                           Server = Sys.getenv("psims_server"),
                           database = Sys.getenv("lfpse_database"),
                           uid = Sys.getenv("lfpse_uid"),
                           pwd = Sys.getenv("lfpse_pwd")
    )
    
    # reading categorical reference tables
    QuestionReference <- tbl(con_lfpse, "QuestionReference") |> collect()
    ResponseReference <- tbl(con_lfpse, "ResponseReference") |> collect()
    }




rename_lookup_nrls<- c(
  `RP01 Unique Incident ID` = "INCIDENTID",
  `Local Trust incident ID` = "TRUSTINCIDENTID",
  `RP02 Care Setting of Occurrence` = "RP02",
  `RP07 NHS Organisation Code` = "RP07",
  `Organisation Name` = "ORGANISATIONNAME",
  `Date of Incident` = "occurred_date",
  `Month of Incident` = "month_of_incident",
  `Year of Incident` = "year_of_incident",
  `IN03 Location (lvl1)` = "IN03_LVL1",
  `IN03 Location (lvl2)` = "IN03_LVL2",
  `IN03 Location (lvl3)` = "IN03_LVL3",
  `IN03 Location - Free Text` = "IN03_TEXT",
  `IN04 Country` = "IN04",
  `IN05 Incident Category - Lvl1` = "IN05_LVL1",
  `IN05 Incident Category - Lvl2` = "IN05_LVL2",
  `IN05 Incident Category - Free Text` = "IN05_TEXT",
  `IN07 Description of what happened` = "IN07",
  `IN10 Actions Preventing Reoccurrence` = "IN10",
  `IN11 Apparent Causes` = "IN11",
  `Age at time of Incident (years)` = "AGE_AT_INCIDENT",
  `PD05 Specialty - Lvl 1` = "PD05_LVL1",
  `PD05 Specialty - Lvl 2` = "PD05_LVL2",
  `PD05 Speciality - Free Text` = "PD05_TEXT",
  `PD09 Degree of harm (severity)` ="PD09",
  `RM04 Source of Notification` = "RM04",
  `MD01 Med Process` = "MD01",
  `MD01 Med Process Free Text` = "MD01_TEXT",
  `MD02 Med Error Category` = "MD02",
  `MD02 Med Error Category Free Text` = "MD02_TEXT",
  `MD05 Approved Name (Drug 1)` = "MD05",
  `MD06 Proprietary Name (Drug 1)` = "MD06",
  `PD02 Patient Sex` = "PD02",
  `PD04 Adult/Paediatrics Specialty` = "PD04",
  `PD20 Paediatric ward/department/unit` = "PD20", # Changed this from PD04 to PD20
  `MD30 Approved Name (Drug 2)` = "MD30",
  `MD31 Proprietary Name (Drug 2)` = "MD31",
  `DE01 Type of Device` = "DE01",
  `DE01 Type of device - free text` = "DE01_TEXT",
  `DE03`="DE03",
  `Date incident received by NRLS` = "reported_date")

rename_lookup_lfpse<-c(
  Reference = "Reference",
  "Taxonomy Version"= "TaxonomyVersion",
  "Revision" = "Revision",
  "Occurred Organisation Code" = "OccurredOrganisationCode",
  "Reporter Organisation Code" = "ReporterOrganisationCode",
  "Reported Date" = "reported_date",
  "Month of Incident" = "month_of_incident",
  "Year of Incident" = "year_of_incident",
  "Number of patients" = "npatient",
  "Patient no." = "EntityId",
  "T005 - Event date" = "occurred_date",
  # TODO: check whether these are needed
  # "T005 - Event year" = "year(T005)",
  # "T005 - Event moth" = "month(T005)",
  "P004 - Age in days" = "P004_days", 
  "P007 - Age Range" = "P007",
  "L003 - Service Area" = "L003",
  "L004 - Location Within Service" = "L004",
  "L006 - Specialty" = "L006",
  "L006_Other - Specialty (Other)" = "L006_Other",
  "F001 - Describe what happened" = "F001",
  "AC001 - What was done immediately to reduce harm caused by the event?" = "AC001",
  "OT003 - What was the clinical outcome for the patient?" = "OT003",
  "A008 - Device Type" = "A008",
  "A008 - Device Type (Other)" = "A008_Other",
  "A001 - Involved Agents" = "A001",
  "AC001 - Immediate Actions" = "AC001",
  "CL001 - Event Type" = "CL001",
  "CL021 - Reference Number (Optional)" = "CL021",
  "CL022 - From Online Forms" = "CL022",
  "L001 - Organisation Known" = "L001",
  "L002 - Organisation" = "L002",
  "R006 - Reporter Organisation" = "R006",
  "R006_Other - Reporter Organisation (Other)" = "R006_Other",
  "RI003 - Is there imminent risk of severe harm or death?" = "RI003",
  "OT001 - Physical harm" = "OT001",
  "OT002 - Psychological harm" = "OT002",
  "OT008 - Outcome Type" = "OT008",
  "A002 - Medicine types involved" = "A002",
  "A016 - BuildingsInfrastructure" = "A016",
  "A016_Other - BuildingsInfrastructure (other)" = "A016_Other",
  #"Largest physical or psychological harm (across all patients in incident)" = "max_harm_level",
  "Largest psychological harm (across all patients in incident)" = "max_psychological_harm_level",
  "Largest physical harm (across all patients in incident)" =  "max_physical_harm_level"
)

rename_lookup_steis<-c(
    `Log No` = "log_no",
    `Created on` = "reported_date",
    `Organisation reporting SI on STEIS` = "organisation_reporting_si_on_steis",
    `Organisation leading investigation` = "organisation_leading_investigation",
    `CCG/CSU Name` = "ccg_csu_name",
    `Region - Geography` = "region_geography",
    `Status` = "status",
    `Date of Incident:` = "occurred_date",
    `Year of Incident` = "year_of_incident",
    `Month of Incident` = "month_of_incident",
    `Time of Incident:` = "time_of_incident",
    `Site of Incident:` = "site_of_incident",
    `Location of Incident:` = "location_of_incident",
    `Location of Incident (Other):` = "location_of_incident_other",
    `Care Sector` = "care_sector",
    `Care Sector (Other)` = "care_sector_other",
    `Clinical Area:` = "clinical_area",
    `Clinical Area (Other)` = "clinical_area_other",
    `Patient Age (years)` = "patient_age_years",
    `Patient Age (months)` = "patient_age_months",
    `Patient Type` = "patient_type",
    `Legal Status of Patient` = "legal_status_of_patient",
    `Type of Incident` = "type_of_incident",
    `Type of Incident (Other)` = "type_of_incident_other",
    `Where is patient at time of reporting:` = "where_is_patient_at_time_of_reporting",
    `Internal Inverstigation Required` = "internal_investigation_required",
    `Non Health led Investigation Required` = "non_health_led_investigation_required",
    `Description of what happened:` = "description_of_what_happened",
    `Reason for Reporting` = "reason_for_reporting",
    `Immediate action taken` = "immediate_action_taken",
    `Case Summary` = "case_summary",
    `Key Findings` = "key_findings",
    `How will lessons be disseminated to interested parties` = "how_will_lessons_be_disseminated_to_interested_parties"
  )



rename_lookup <- list("LFPSE"= rename_lookup_lfpse,
                      "NRLS" =rename_lookup_nrls,
                      "STEIS"= rename_lookup_steis)
