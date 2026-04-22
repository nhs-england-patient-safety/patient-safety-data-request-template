# ODS Organisation Utilities
# Shared logic for fetching, wrangling, and joining organisation data from ODS API


#' Get ODS organisation data
#' 
#' @param org_code A list of organisation codes
#'
#' @return Dataframe with organisation data

get_ods_orgs <- function(org_code) {
  
  api_key <- Sys.getenv("ods_key")
  
  tryCatch({
    response <- 
      httr2::request("https://api.service.nhs.uk/organisation-data-terminology-api/fhir") |>
      httr2::req_url_path_append("Organization", org_code) |>
      httr2::req_headers(apikey = api_key) |>
      httr2::req_throttle(rate = 5) |>
      httr2::req_retry(max_tries = 3, backoff = \(x) 2) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    
    tibble(
      org_code = purrr::pluck(response, "id",      .default = NA),
      org_name = purrr::pluck(response, "name",    .default = NA),
      org_type = purrr::pluck(response, "extension", 1, "extension", 2,
                              "valueCodeableConcept", "coding", 1, "display",
                              .default = NA),
      org_country  = purrr::pluck(response, "address", 1, "country", .default = NA)
    )
    
  }, error = function(e) {
    message(glue::glue("No info available for {org_code}: {e$message}"))
    tibble(org_code = org_code, org_name = NA_character_,
           org_type = NA_character_, org_country = NA_character_)
  })
}


#' Memoised version of `get_ods_orgs`
#'
#' Wraps `get_ods_orgs` with a disk cache that persists across sessions (set to 
#' expire after 30 days). Use `get_ods_orgs` directly to force a fresh API call.
#'
#' @inheritParams get_ods_orgs
#'
#' @return Dataframe with organisation data, retrieved from cache where available

get_ods_orgs_cached <- memoise::memoise(
  get_ods_orgs,
  cache = cachem::cache_disk(
    dir = here::here(".ods_cache"),
    max_age = 60 * 60 * 24 * 30  # 30 days
  )
)

#' Fetch and join ODS organisation data to a dataset
#'
#' @param data A dataframe containing organisation codes
#' @param dataset_name Name of dataset (e.g., 'NRLS', 'LFPSE', 'StEIS')
#'
#' @return Original dataframe with organisation data joined on

fetch_and_join_ods <- function(data, dataset_name = dataset, 
                               english_only = TRUE) {
  
  # map database name to the relevant org code column
  org_code_col <- 
    switch(
      dataset_name,
      "LFPSE"  = "ReporterOrganisationCode",
      "NRLS"   = "RP07",
      "StEIS"  = "reporting_organisation_code",
      stop(
        glue::glue(
          "Unknown dataset_name: '{dataset_name}'. ",
          "Expected 'LFPSE', 'NRLS', or 'StEIS'."
          )
        )
  )
  
  # check the column exists in the dataframe
  if (!org_code_col %in% names(data)) {
    stop(glue::glue("Column '{org_code_col}' not found in data."))
  }
  
  # extract unique, non-NA org codes
  org_codes <- data |>
    dplyr::pull(all_of(org_code_col)) |>
    na.omit() |>
    unique()
  
  n_codes <- length(org_codes)
  message(glue::glue("{n_codes} unique org codes to process."))

  # use furrr if over threshold, otherwise purrr
  if (n_codes >= 250) {
    workers <- max(1, parallel::detectCores() - 1)
    future::plan(future::multisession, workers = workers)
    
    orgs <- furrr::future_map(org_codes, get_ods_orgs_cached, .progress = TRUE) |>
      purrr::list_rbind()
    
    future::plan(future::sequential)
    
  } else {
    orgs <- purrr::map(org_codes, get_ods_orgs_cached, .progress = TRUE) |>
      purrr::list_rbind()
  }
  
  # join org data back to original dataframe
  result <- data |>
    dplyr::left_join(
      orgs, by = dplyr::join_by(!!rlang::sym(org_code_col) == org_code)
      )
  
  # optionally filter to English and unknown organisations
  if (english_only) {
    
    rows_before <- nrow(result)
    orgs_before <- dplyr::n_distinct(result[[org_code_col]], na.rm = TRUE)
    
    result <- result |>
      dplyr::filter(org_country == "ENGLAND" | is.na(org_country))
    
    rows_removed <- rows_before - nrow(result)
    orgs_removed <- orgs_before - dplyr::n_distinct(result[[org_code_col]], na.rm = TRUE)
    
    if (rows_removed > 0 | orgs_removed > 0) {
      message(glue::glue(
        "{rows_removed} rows and {orgs_removed} non-English organisations removed."
      ))
    }
  }
  
  result
}
