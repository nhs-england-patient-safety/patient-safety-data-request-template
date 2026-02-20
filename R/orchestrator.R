# R/orchestrator.R
# Main execution function that orchestrates the data request process


# ==============================================================================
# MAIN ORCHESTRATOR FUNCTION ----
# ==============================================================================

#' Run a patient safety data request
#' 
#' This function coordinates the entire data extraction and reporting pipeline.
#' It processes selected databases, applies filters, and generates Excel output.
#' 
#' @param search_nrls Logical. Search NRLS database?
#' @param search_lfpse Logical. Search LFPSE database?
#' @param search_steis Logical. Search StEIS database?
#' @param start_date Character. Start date in "YYYY-MM-DD" format
#' @param end_date Character. End date in "YYYY-MM-DD" format
#' @param date_type Character. Either "occurring" or "reported"
#' @param nrls_categorical Expression. NRLS categorical filter (use expr() or 0)
#' @param lfpse_categorical Expression. LFPSE categorical filter (use expr() or 0)
#' @param steis_categorical Expression. StEIS categorical filter (use expr() or 0)
#' @param steis_filename Character. Name of StEIS CSV file in data/ folder
#' @param text_terms Named list. Text search terms grouped by category
#' @param text_filter Expression. Logic combining text term groups
#' @param is_neopaed Character. One of "neonate", "paed", "either", or "none"
#' @param sampling_strategy Character. One of "default", "FOI", or "none"
#' @param include_term_tally_table Character. "yes" or "no"
#' @param incident_level_required Character. "yes" or "no"
#' @param write_to_sp Logical. Write to SharePoint?
#' @param list_of_tables_to_create_nrls List. Table specifications for NRLS
#' @param list_of_tables_to_create_lfpse List. Table specifications for LFPSE
#' @param list_of_tables_to_create_steis List. Table specifications for StEIS
#' @param summary_tables_incident_or_patient_level Character. One of "incident" or "patient"
#' 
#' @return Invisible list containing summary and results

run_data_request <- function(
  # database selection
  search_nrls,
  search_lfpse,
  search_steis,
  
  # data parameters
  start_date,
  end_date,
  date_type,
  
  # categorical filters
  nrls_categorical,
  lfpse_categorical,
  steis_categorical,
  steis_filename = NULL,
  
  # text filters
  text_terms,
  text_filter,
  
  # classification
  is_neopaed,
  
  # sampling
  sampling_strategy,
  
  # output configuration
  include_term_tally_table,
  incident_level_required,
  write_to_sp,
  
  # table specifications
  list_of_tables_to_create_nrls,
  list_of_tables_to_create_lfpse,
  list_of_tables_to_create_steis,
  summary_tables_incident_or_patient_level
) {
  
  log_section_header("PATIENT SAFETY DATA REQUEST")
  
  # global environment variables for backward compatability (to be removed after refactoring)
  assign("start_date", start_date, envir = .GlobalEnv)
  assign("end_date", end_date, envir = .GlobalEnv)
  assign("date_type", date_type, envir = .GlobalEnv)
  assign("search_nrls", search_nrls, envir = .GlobalEnv)
  assign("search_lfpse", search_lfpse, envir = .GlobalEnv)
  assign("search_steis", search_steis, envir = .GlobalEnv)
  assign("nrls_categorical", nrls_categorical, envir = .GlobalEnv)
  assign("lfpse_categorical", lfpse_categorical, envir = .GlobalEnv)
  assign("steis_categorical", steis_categorical, envir = .GlobalEnv)
  assign("steis_filename", steis_filename, envir = .GlobalEnv)
  assign("text_terms", text_terms, envir = .GlobalEnv)
  assign("text_filter", text_filter, envir = .GlobalEnv)
  assign("is_neopaed", is_neopaed, envir = .GlobalEnv)
  assign("sampling_strategy", sampling_strategy, envir = .GlobalEnv)
  assign("include_term_tally_table", include_term_tally_table, envir = .GlobalEnv)
  assign("incident_level_required", incident_level_required, envir = .GlobalEnv)
  assign("write_to_sp", write_to_sp, envir = .GlobalEnv)
  assign("list_of_tables_to_create_nrls", list_of_tables_to_create_nrls, envir = .GlobalEnv)
  assign("list_of_tables_to_create_lfpse", list_of_tables_to_create_lfpse, envir = .GlobalEnv)
  assign("list_of_tables_to_create_steis", list_of_tables_to_create_steis, envir = .GlobalEnv)
  assign("summary_tables_incident_or_patient_level", summary_tables_incident_or_patient_level, envir = .GlobalEnv)
  
  # create date filter
  date_filter <- if (date_type == 'occurring') {
    expr(occurred_date)
  } else if (date_type == 'reported') {
    expr(reported_date)
  }
  assign("date_filter", date_filter, envir = .GlobalEnv)
  
  # expand categorical filters for documentation
  message("Translating categorical filters...", appendLF = FALSE)
  source("R/utils/expand_categorical_filters.R")
  message("✓")
  
  # identify database search order
  if (search_nrls) {
    log_processor_start("NRLS")
    source("R/processors/nrls.R")
  } else if (search_lfpse) {
    log_processor_start("LFPSE")
    source("R/processors/lfpse.R")
  } else if (search_steis) {
    log_processor_start("StEIS")
    source("R/processors/steis.R")
  } else {
    stop(
      "No databases selected for searching. Set at least one of search_nrls, ",
      "search_lfpse, or search_steis to TRUE."
      )
  }
  
  log_section_header("PROCESS COMPLETE")
  
}
