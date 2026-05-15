#This script contains the list of columns that will be extracted by each of the scripts
# Note, if some of these columns are removed, the code will not work as intended.


rename_lookup_nrls<- c(
  `RP01 Unique Incident ID` = "INCIDENTID", # required
  `Local Trust incident ID` = "TRUSTINCIDENTID",
  `RP02 Care Setting of Occurrence` = "RP02",
  `RP07 NHS Organisation Code` = "RP07", # required for join
  `Organisation Name` = "ORGANISATIONNAME",
  `Date of Incident` = "occurred_date", # required 
  `Date incident received by NRLS` = "reported_date", # required
  `Month` = "month_reported_or_occurred", # required
  `Year` = "year_reported_or_occurred", # required
  `Month - Year` = "month_year_reported_or_occurred", # required
  `Financial Year` = "financial_year_reported_or_occurred", # required
  `IN03 Location (lvl1)` = "IN03_LVL1",
  `IN03 Location (lvl2)` = "IN03_LVL2",
  `IN03 Location (lvl3)` = "IN03_LVL3",
  `IN03 Location - Free Text` = "IN03_TEXT", # required for text search
  `IN04 Country` = "IN04",
  `IN05 Incident Category - Lvl1` = "IN05_LVL1",
  `IN05 Incident Category - Lvl2` = "IN05_LVL2",
  `IN05 Incident Category - Free Text` = "IN05_TEXT", # required for text search
  `IN07 Description of what happened` = "IN07", # required for text search
  `IN10 Actions Preventing Reoccurrence` = "IN10", # required for text search
  `IN11 Apparent Causes` = "IN11",  # required for text search
  `Age at time of Incident (years)` = "AGE_AT_INCIDENT", # required for neopaed search
  `PD05 Specialty - Lvl 1` = "PD05_LVL1", # required for neopaed search
  `PD05 Specialty - Lvl 2` = "PD05_LVL2", # required for neopaed search
  `PD05 Speciality - Free Text` = "PD05_TEXT", # required for neopaed search
  `PD09 Degree of harm (severity)` ="PD09", # required
  `RM04 Source of Notification` = "RM04",
  `MD01 Med Process` = "MD01",
  `MD01 Med Process Free Text` = "MD01_TEXT",
  `MD02 Med Error Category` = "MD02",
  `MD02 Med Error Category Free Text` = "MD02_TEXT",
  `MD05 Approved Name (Drug 1)` = "MD05", # required for text search
  `MD06 Proprietary Name (Drug 1)` = "MD06", # required for text search
  `PD02 Patient Sex` = "PD02", 
  `PD04 Adult/Paediatrics Specialty` = "PD04", # required for neopaed search
  `PD20 Paediatric ward/department/unit` = "PD20", # required for neopaed search
  `MD30 Approved Name (Drug 2)` = "MD30", # required for text search
  `MD31 Proprietary Name (Drug 2)` = "MD31", # required for text search
  `DE01 Type of Device` = "DE01",
  `DE01 Type of device - free text` = "DE01_TEXT", # required for text search
  `DE03 Device name`="DE03", # required for text search
  `Neonate Categorisation` = "neonate_category",
  `Paediatric Categorisation` = "paediatric_category"
  )

rename_lookup_lfpse<-c(
  Reference = "Reference", # required
  "Taxonomy Version"= "TaxonomyVersion", # required
  "Revision" = "Revision",
  "Occurred Organisation Code" = "OccurredOrganisationCode",
  "Reporter Organisation Code" = "ReporterOrganisationCode",
  # Using old column name for occurred_date - should it use "Occurred Date" instead?
  "T005 - Event Date" = "occurred_date", # required
  "Reported Date" = "reported_date", # required
  `Month` = "month_reported_or_occurred", # required
  `Year` = "year_reported_or_occurred", # required
  `Month - Year` = "month_year_reported_or_occurred", # required
  `Financial Year` = "financial_year_reported_or_occurred", # required
  "Number of patients" = "npatient", # required
  "Patient no." = "EntityId", # required
  # TODO: check whether these are needed
  # "T005 - Event Year" = "year(OccurredDate)",
  # "T005 - Event Month" = "month(OccurredDate)",
  "P004 - Age in days" = "P004_days_validated", # required for neopaed search 
  "P007 - Age Range" = "P007_AgeBracket", # required for neopaed search
  "L003 - Service Area" = "L003_ServiceArea",
  "L004 - Location Within Service" = "L004_LocationWithinService",
  "L006 - Specialty" = "L006_ResponsibleSpecialty", # required for neopaed search
  "L006_Other - Specialty (Other)" = "L006_Other_ResponsibleSpecialtyOther", # required for neopaed search
  "F001 - Describe what happened" = "F001_Description", # required for text search
  "AC001 - What was done immediately to reduce harm caused by the event?" = "AC001_ImmediateActions", # required for text search
  "OT003 - What was the clinical outcome for the patient?" = "OT003_ClinicalOutcome", # required for text search
  "A008 - Device Type" = "A008_DeviceType", # required for text search
  "A008 - Device Type (Other)" = "A008_Other_DeviceTypeOther", # required for text search
  "A001 - Involved Agents" = "A001_InvolvedAgents",
  "AC001 - Immediate Actions" = "AC001_ImmediateActions", # required for text search
  # Used CL001_Type, but could also be CL001_Event. Which is correct?
  "CL001 - Event Type" = "CL001_Type",
  "CL021 - Reference Number (Optional)" = "CL021_ReferenceNumber",
  "CL022 - From Online Forms" = "CL022_FromOnlineForms",
  "L001 - Organisation Known" = "L001_LocationKnown",
  "L002 - Organisation" = "L002_Organisation",
  "R006 - Reporter Organisation" = "R006_ReporterOrganisation",
  "R006_Other - Reporter Organisation (Other)" = "R006_Other_ReporterOrganisationOther",
  "RI003 - Is there imminent risk of severe harm or death?" = "RI003_RiskImminent",
  "OT001 - Physical harm" = "OT001_PhysicalHarm", # required
  "OT002 - Psychological harm" = "OT002_PsychologicalHarm", # required
  # OT008 is missing in the new LFPSE tables. Is there a reason for this?
  # "OT008 - Outcome Type" = "OT008", 
  "A002 - Medicine types involved" = "A002_DrugsInvolved", # required for text search
  "DMD002 - Medicine types (VTM)" = "DMD002_VTMString", # required for text search
  "DMD004 - Medicine types (VMP)" = "DMD004_VMPString", # required for text search
  "A016 - BuildingsInfrastructure" = "A016_BuildingsInfrastructure",
  "A016_Other - BuildingsInfrastructure (other)" = "A016_Other_BuildingsInfrastructureOther",
  #"Largest physical or psychological harm (across all patients in incident)" = "max_harm_level",
  "Largest psychological harm (across all patients in incident)" = "max_psychological_harm_level", # required
  "Largest physical harm (across all patients in incident)" =  "max_physical_harm_level", # required
  `Neonate Categorisation` = "neonate_category",
  `Paediatric Categorisation` = "paediatric_category"
)

rename_lookup_steis<-c(
  `Log No` = "log_no",
  `Organisation reporting SI on STEIS` = "organisation_reporting_si_on_steis",
  `Organisation leading investigation` = "organisation_leading_investigation",
  `CCG/CSU Name` = "ccg_csu_name",
  `Region - Geography` = "region_geography",
  `Status` = "status",
  `Date Incident Reported` = "reported_date", # required
  `Date of Incident`= "occurred_date", # required
  `Month` = "month_reported_or_occurred", # required
  `Year` = "year_reported_or_occurred", # required
  `Month - Year` = "month_year_reported_or_occurred", # required
  `Financial Year` = "financial_year_reported_or_occurred",
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