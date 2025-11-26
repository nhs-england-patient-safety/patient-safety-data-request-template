# Sampling Utilites
# Shared logic for sampling strategies

#' Apply default sampling strategy
#' 
#' @param data Dataframe to sample
#' @param harm_column Name of the harm column
#' @param death_severe_values Vector of values indicating death/severe harm
#' @param moderate_values Vector of values indicating moderate harm
#' @param threshold Only sample if rows exceed this number (default 300)
#' 
#' @return Sampled dataframe

apply_default_sampling <- function(data, harm_column, death_severe_values,
                                   moderate_values, threshold = 300) {
  if (nrow(data) > threshold) {
    message("Sampling according to default strategy...")
    
    # get all death/severe harm
    death_severe <- data |> 
      filter(.data[[harm_column]] %in% death_severe_values)
    
    # sample 100 moderate harm
    set.seed(123)
    moderate <- data |> 
      filter(.data[[harm_column]] %in% moderate_values) |> 
      sample_n(min(n(), 100))
    
    # sample 100 low/no/other harm
    set.seed(123)
    low_no_other <- data |> 
      filter(!.data[[harm_column]] %in% c(death_severe_values, moderate_values)) |> 
      sample_n(min(n(), 100))
    
    return(bind_rows(death_severe, moderate, low_no_other))
  
  } else {
    
    message("Sampling not required, default threshold not met.")
    return(data)
  } 
}

#' Apply FOI sampling strategy
#' 
#' @param data Dataframe to sample
#' @param reference_column Name of the reference/ID column (default "Reference")
#' 
#' @return Sampled dataframe

apply_foi_sampling <- function(data, reference_column = "Reference") {
  message("Extracting a sample of 30 incidents for redaction...")
  
  set.seed(123)
  data |> 
    distinct(!!sym(reference_column), .keep_all=TRUE) |> 
    sample_n(min(n(), 30))
}

#' Apply sampling strategy based on strategy name
#' 
#' @param data Dataframe to sample
#' @param strategy One of: "default", "FOI", or "none"
#' @param harm_column Name of harm column (for default strategy)
#' @param death_severe_values Values indicating death/severe (for default strategy)
#' @param moderate_values Values indicating moderate (for default strategy)
#' @param reference_column Name of reference_column (for FOI strategy)
#' 
#' @return Sampled dataframe

apply_sampling_strategy <- function(data, strategy,
                                    harm_column = NULL,
                                    death_severe_values = NULL,
                                    moderate_values = NULL,
                                    reference_column = "Reference") {
  
  if (strategy == "default") {
    
    return(apply_default_sampling(data, harm_column, death_severe_values, moderate_values))
  
  } else if (strategy == "FOI") {
    
    return(apply_foi_sampling(data, reference_column))
    
  } else if (strategy == "none") {
    
    message("Skipping sampling...")
    return(data)
  
  }
  
  stop(str_glue("Unknown sampling strategy: {strategy}"))
}
