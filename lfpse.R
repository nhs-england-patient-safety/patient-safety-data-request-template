# lfpse
dataset <- "LFPSE"
print(glue("Running {dataset} search..."))

if (lfpse_categorical == 0) {
  lfpse_categorical <- expr(1 == 1)
}

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
    lfpse_categorical
  ) |>
  # collecting here so that we can apply text filters later
  collect()

toc_lfpse <- Sys.time()

time_diff_lfpse <- toc_lfpse - tic_lfpse

print(glue("Extraction from {dataset} server: {round(time_diff_lfpse[[1]], 2)} {attr(time_diff_lfpse, 'units')}"))

print(glue("- {dataset} categorical filters retrieved {format(nrow(lfpse_filtered_categorical), big.mark = ',')} incidents."))

# text filters ####
if (sum(!is.na(text_terms))>0) {
  print(glue("Running {dataset} text search..."))
  
  lfpse_filtered_text_precursor<- lfpse_filtered_categorical |>
    mutate(concat_col=paste(F001, AC001, OT003, A008_Other, A008, sep=" "))
  
  groups <- names(text_terms)
  for (group in groups) {
    terms <- text_terms[[group]]
    for (term in terms) {
      lfpse_filtered_text_precursor <- lfpse_filtered_text_precursor |>
        mutate("{group}_term_{term}" := str_detect(concat_col, term))
    }
    
    lfpse_filtered_text_precursor <- lfpse_filtered_text_precursor |>
      mutate("{group}" := rowSums(across(starts_with(group))) > 0)
  }
  
  lfpse_filtered_text <- lfpse_filtered_text_precursor %>%
    filter(!!text_filter) %>%
    select(!c(contains("_term_"), concat_col))
  
  #A002 may need to be added for a medication incident
  print(glue("{dataset} text search retrieved {format(nrow(lfpse_filtered_text), big.mark = ',')} incidents."))
} else {
  print("- No text terms supplied. Skipping text search...")
  lfpse_filtered_text <- lfpse_filtered_categorical
}

# check whether the text search generated results
if (nrow(lfpse_filtered_text) != 0) {
  # sampling ####
  # Default (if > 300: all death/severe, 100 moderate, 100 low/no harm)
  if (sampling_strategy == "default") {
    if (nrow(lfpse_filtered_text) > 300) {
      print("- Sampling according to default strategy...")
      lfpse_death_severe <- lfpse_filtered_text |>
        # deaths or severe physical / psychological harm
        filter(OT001 %in% c("1", "2") |
                 OT002 == "1")
      
      set.seed(123)
      lfpse_moderate <- lfpse_filtered_text |>
        # moderate physical / psychological harm
        filter(OT001 == "3" | OT002 == "2") |>
        collect() |>
        sample_n(min(n(), 100))
      
      set.seed(123)
      lfpse_low_no_other <- lfpse_filtered_text |>
        filter(
          !OT001 %in% c("1", "2", "3"),
          !OT002 %in% c("1", "2")
        ) |>
        collect() |>
        sample_n(min(n(), 100))
      
      lfpse_sampled <- bind_rows(
        lfpse_death_severe,
        lfpse_moderate,
        lfpse_low_no_other
      )
    } else {
      print("- Sampling not required, default threshold not met.")
      lfpse_sampled <- lfpse_filtered_text
    }
  } else if (sampling_strategy == "FOI") {
    print("- Extracting a sample of 30 incidents for redaction...")
    set.seed(123)
    lfpse_sampled <- lfpse_filtered_text |>
      distinct(Reference, .keep_all = T) |>
      sample_n(min(n(), 30))
  } else if (sampling_strategy == "none") {
    print("- Skipping sampling...")
    lfpse_sampled <- lfpse_filtered_text
  }
  
  
  # columns for release ####
  
  lfpse_for_release <- lfpse_sampled |>
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
    select(!c(value, Property, LastUpdated, IsActive)) |>
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
    # select the columns for release
    select(c(
      Reference,
      TaxonomyVersion,
      Revision,
      OccurredOrganisationCode,
      ReporterOrganisationCode,
      reported_date,
      "Number of patients" = npatient,
      "Patient no." = EntityId,
      "T005 - Event date" = occurred_date,
      # TODO: check whether these are needed
      # "T005 - Event year" = year(T005),
      # "T005 - Event moth" = month(T005),
      "F001 - Describe what happened" = F001,
      "AC001 - What was done immediately to reduce harm caused by the event?" = AC001,
      "OT003 - What was the clinical outcome for the patient?" = OT003,
      "A008 - Device Type" = A008,
      "A008 - Device Type (Other)" = A008_Other,
      "A001 - Involved Agents" = A001,
      "AC001 - Immediate Actions" = AC001,
      "CL001 - Event Type" = CL001,
      "CL021 - Reference Number (Optional)" = CL021,
      "CL022 - From Online Forms" = CL022,
      "L001 - Organisation Known" = L001,
      "L002 - Organisation" = L002,
      "L003 - Service Area" = L003,
      "L004 - Location Within Service" = L004,
      "L006 - Specialty" = L006,
      "L006_Other - Specialty (Other)" = L006_Other,
      "R006 - Reporter Organisation" = R006,
      "R006_Other - Reporter Organisation (Other)" = R006_Other,
      "RI003 - Is there imminent risk of severe harm or death?" = RI003,
      "OT001 - Physical harm" = OT001,
      "OT002 - Psychological harm " = OT002,
      "OT008 - Outcome Type" = OT008,
      "A002 - Medicine types involved" = A002,
      "A016 - BuildingsInfrastructure" = A016,
      "A016_Other - BuildingsInfrastructure (other)" = A016_Other,
      starts_with("group")
      # TODO: add age columns once DQ issues resolved
    )) |>
    ungroup() |> # Added the ungroup() here, I was running into an error where I couldn't sample because the data was still grouped
    remove_empty("cols")
  
  print(glue("- Final {dataset} dataset contains {nrow(lfpse_for_release)} incidents."))
} else {
  print(glue("**The search criteria has produced no results in {dataset}**"))
  print(glue("Moving on..."))
}

dbDisconnect(con_lfpse)

if (search_steis) {
  source("steis.R")
} else {
  source("formatter.R")
}