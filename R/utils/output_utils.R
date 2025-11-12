# Output Utilities
# Shared logic for preparing output datasets

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
      message(glue::glue("**The text search has produced no results in {dataset_name}**"))
    } else if (search_type == "neopaed") {
      message(glue::glue("**The neopaed search has produced no results in {dataset_name}**"))
    } else {
      message(glue::glue("**The search criteria has produced no results in {dataset_name}**"))
    }
    message("Moving on...")
    return(TRUE)
  }
  
  return(FALSE)
}

#' Log final dataset sizes
#' 
#' @param dataset_name Name of dataset
#' @param unsampled_summary Unsampled summary table
#' @param sampled summary Sampled summary table
#' @param unsampled_pt Unsampled patient-level table
#' @param sampled_pt Sampled patient-level table
#' @param level_description Description of level (e.g., "patient level", "incident level")

log_final_counts <- function(dataset_name, unsampled_summary, sampled_summary,
                             unsampled_pt, sampled_pt, level_description = "") {
  
  message(glue::glue("Final {dataset_name} dataset for creating summary table contains {nrow(sampled_summary)} sampled incidents ({level_description})"))
  message(glue::glue("Final {dataset_name} dataset for creating summary table contains {nrow(unsampled_summary)} unsampled incidents ({level_description})"))
  message(glue::glue("Final {dataset_name} dataset contains {nrow(sampled_pt)} sampled incidents (pt level)"))
  message(glue::glue("Final {dataset_name} dataset contains {nrow(unsampled_pt)} unsampled incidents (pt level)"))
}