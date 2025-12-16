#function to find the minimum number in a vector, and return NA if all values are NA

# This is required to find the minimum physical or psychological harm level
min_safe <- function(vec) {
  ifelse(length(vec[!is.na(vec)]) == 0, NA_real_, min(vec, na.rm = TRUE))
}




# function to convert month and level of harm columns to factors (depending on database)
convert_columns_to_factors <- function(df_without_factors, database_name) {
  # convert month and harm level to ordered factors
  if (database_name == "LFPSE") {
    # relevel factor of columns
    df_with_factors <- df_without_factors |>
      mutate(
        `Largest physical harm (across all patients in incident)` = factor(
          `Largest physical harm (across all patients in incident)`,
          levels = c(
            "No physical harm", "Low physical harm",
            "Moderate physical harm", "Severe physical harm", "Fatal",
            "Harm level missing", "Not applicable"
          )
        ),
        `Largest psychological harm (across all patients in incident)` = factor(
          `Largest psychological harm (across all patients in incident)`,
          levels = c(
            "No psychological harm",
            "Low psychological harm",
            "Moderate psychological harm",
            "Severe psychological harm",
            "Harm level missing", 
            "Not applicable"
          )
        ),
        `Month` = factor(`Month`, levels = month.abb),
        `Year` = factor(`Year`,
                        levels = sort(unique(`Year`))
        ),
        `Month - Year` = factor(zoo::as.yearmon(`Month - Year`),
                                levels = sort(unique(`Month - Year`))
        )
      )
  } else if (database_name == "NRLS") {
    df_with_factors <- df_without_factors |>
      mutate(
        `Month` = factor(`Month`, levels = month.abb),
        `Year` = factor(`Year`,
                        levels = sort(unique(`Year`))
        ),
        `PD09 Degree of harm (severity)` =
          factor(`PD09 Degree of harm (severity)`,
                 levels = c("No Harm", "Low", "Moderate", "Severe", "Death")
          ),
        `Month - Year` = factor(zoo::as.yearmon(`Month - Year`),
                                levels = sort(unique(`Month - Year`))
        )
      )
  } else if (database_name == "STEIS") {
    df_with_factors <- df_without_factors |>
      mutate(
        `Month` = factor(`Month`, levels = month.abb),
        `Year` = factor(`Year`,
                        levels = sort(unique(`Year`))
        ),
        `Month - Year` = factor(zoo::as.yearmon(`Month - Year`),
                                levels = sort(unique(`Month - Year`))
        )
      )
  } else {
    print("database name not found")
  }
  
  return(df_with_factors)
}