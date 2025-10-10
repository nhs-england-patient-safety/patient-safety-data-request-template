# R/orchestrator.R
# Main execution function that orchestrates the data request process

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
  list_of_tables_to_create_steis
) {
  
  message("========================================")
  message("PATIENT SAFETY DATA REQUEST")
  message("========================================\n")
  
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
  assign("incide_level_required", incident_level_required, envir = .GlobalEnv)
  assign("write_to_sp", write_to_sp, envir = .GlobalEnv)
  assign("list_of_tables_to_create_nrls", list_of_tables_to_create_nrls, envir = .GlobalEnv)
  assign("list_of_tables_to_create_lfpse", list_of_tables_to_create_lfpse, envir = .GlobalEnv)
  assign("list_of_tables_to_create_steis", list_of_tables_to_create_steis, envir = .GlobalEnv)
  
  # create date filter
  date_filter <- if (date_type == 'occurring') {
    expr(occurred_date)
  } else if (date_type == 'reported') {
    expr(reported_date)
  }
  assign("date_filter", date_filter, envir = .GlobalEnv)
  
  # expand categorical filters for documentation
  message("Translating categorical filters...")
  source("R/utils/expand_categorical_filters.R")
  message("Filters translated\n")
  
  # identify database search order
  if (search_nrls) {
    message("Starting with NRLS...\n")
    source("R/processors/nrls.R")
  } else if (search_lfpse) {
    message("Starting with LFPSE...\n")
    source("R/processors/lfpse.R")
  } else if (search_steis) {
    message("Starting with StEIS...\n")
    source("R/processors/steis.R")
  } else {
    stop("No databases selected for searching")
  }
  
  message("\n========================================")
  message("PROCESS COMPLETE")
  message("========================================\n")
  
  # return summary of what was created
  file_list <- apropos('for_release_unsampled_incident_level')
  
  if (length(file_list) == 0) {
    message("Warning: No output datasets created")
    return(invisible(NULL))
  }
  
  summary <- tibble(
    database = str_to_upper(str_extract(file_list, "^[^_]+")),
    n_incidents = sapply(file_list, function(x) nrow(get(x)))
  )
  
  print(summary)
  
  return(invisible(summary))
}
