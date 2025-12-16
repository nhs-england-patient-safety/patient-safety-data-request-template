# Date Utilities
# Shared logic for generating date-related columns

#' Generate standard date columns
#' 
#' @param data Dataframe
#' @param date_col Symbol or expression for date column to use
#'
#' @return Dataframe with additional date columns

add_date_columns <- function(data, date_col) {
  data |> 
    mutate(
      year_reported_or_occurred = year(!!date_col),
      month_reported_or_occurred = as.character(month(!!date_col, label = TRUE, abbr = TRUE)),
      # zoo package is used to create a year-month object because this will sort in the 
      # correct order when tabulated
      month_year_reported_or_occurred = zoo::as.yearmon(!!date_col),
      financial_year_reported_or_occurred = ifelse(
        month(!!date_col) > 3,
        paste0(year(!!date_col), '/', year(!!date_col) + 1),
        paste0(year(!!date_col) -1, '/', year(!!date_col))
      )
    )
}

#' Alternative version for LFPSE (using string parsing)
#' 
#' NOTE: Not sure if this can be wrapped up in the first function but wanted to 
#' stick with the current logic and get this first phase of refactoring done 
#' before making any other changes.
#' 
#' @param data Dataframe
#' @param date_col Symbol or expression for date column to use
#'
#' @return Dataframe with additional date columns

add_date_columns_lfpse <- function(data, date_col) {
  data |> 
    mutate(
      year_reported_or_occurred = as.numeric(substr(as.character(!!date_col), 1, 4)),
      month_reported_or_occurred = as.numeric(substr(as.character(!!date_col), 6, 7)),
      # zoo package is used to create a year-month object because this will sort in the 
      # correct order when tabulated
      month_year_reported_or_occurred = zoo::as.yearmon(
        str_glue("{year_reported_or_occurred}-{month_reported_or_occurred}")
      ),
      # create financial year while month_reported_or_occurred is still a number
      financial_year_reported_or_occurred = ifelse(
        month_reported_or_occurred > 3,
        paste0(year_reported_or_occurred, '/', year_reported_or_occurred + 1),
        paste0(year_reported_or_occurred -1, '/', year_reported_or_occurred)
      ),
      month_reported_or_occurred = month.abb[month_reported_or_occurred]
    )
}
