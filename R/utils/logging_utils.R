# Logging Utilities
# Centralised logging functions for consistent messaging across processors


#' Check if result is empty and log appropriate message
#' 
#' @param data Dataframe to check
#' @param dataset_name Name of dataset for logging
#' @param search_type Type of search ('text', 'neopaed', 'general')
#' 
#' @return TRUE if empty, FALSE otherwise

check_and_log_empty_result <- function(data, dataset_name, search_type="general") {
  
  if (nrow(data) == 0) {
    if (search_type == "text") {
      message(str_glue("**The text search has produced no results in {dataset_name}**"))
    } else if (search_type == "neopaed") {
      message(str_glue("**The neopaed search has produced no results in {dataset_name}**"))
    } else {
      message(str_glue("**The search criteria has produced no results in {dataset_name}**"))
    }
    message("Moving on...")
    return(TRUE)
  }
  
  return(FALSE)
}


#' Log dataset initialisation
#' 
#' @param dataset_name Name of the dataset (e.g., 'NRLS', 'LFPSE', 'StEIS)

log_dataset_start <- function(dataset_name) {
  message(str_glue("Running {dataset_name} search..."))
}


#' Log database extraction timing
#' 
#' @param dataset_name Name of the dataset (e.g., 'NRLS', 'LFPSE', 'StEIS)
#' @param time_diff Time difference object from difftime

log_extraction_time <- function(dataset_name, time_diff) {
  message(str_glue(
    "Extraction from {dataset_name} server:",
    "{round(time_diff[[1]], 2)} {attr(time_diff, 'units')}"
  ))
}


#' Log categorical filter results
#' 
#' @param dataset_name Name of the dataset (e.g., 'NRLS', 'LFPSE', 'StEIS)
#' @param n_incidents Number of incidents retrieved

log_categorical_filter_count <- function(dataset_name, n_incidents) {
  formatted_count <- format(n_incidents, big.mark = ',')
  message(str_glue(
    "{dataset_name} categorical filters retrieved {formatted_count} incidents."
  ))
}


#' Log sampling information for StEIS
#' 
#' @param dataset_name Name of the dataset (should be 'StEIS)

log_no_sampling <- function(dataset_name = "StEIS") {
  message(str_glue("No sampling for {dataset_name} since no harm grading."))
}


#' Log final dataset counts
#' 
#' @param dataset_name Name of dataset
#' @param unsampled_summary Unsampled summary table
#' @param sampled summary Sampled summary table
#' @param unsampled_pt Unsampled patient-level table
#' @param sampled_pt Sampled patient-level table

log_final_counts <- function(dataset_name, unsampled_summary, sampled_summary,
                             unsampled_pt, sampled_pt) {
  
  message(str_glue(
    "Final {dataset_name} dataset contains ",
    "{nrow(unsampled_summary)} unsampled incidents."
    ))
  
  message(str_glue(
    "Final {dataset_name} dataset contains ",
    "{nrow(sampled_summary)} sampled incidents."
    ))
  
  message(str_glue(
    "Final {dataset_name} dataset contains ",
    "{nrow(unsampled_pt)} unsampled incidents (pt level)"
    ))
  
  message(str_glue("Final {dataset_name} dataset contains ",
  "{nrow(sampled_pt)} sampled incidents (pt level)"
  ))
}


#' Log orchestrator section headers
#' 
#' @param title Title for the section

log_section_header <- function(title) {
  line <- paste(rep("=", 40), collapse = "")
  message(line)
  message(title)
  message(line)
  if (title != "") message("") #add blank line unless just a separator
}


#' Log processor start message
#' 
#' @param dataset_name Name of the dataset (e.g., 'NRLS', 'LFPSE', 'StEIS)

log_processor_start <- function(dataset_name) {
  message(str_glue("Starting with {dataset_name}... \n"))
}
