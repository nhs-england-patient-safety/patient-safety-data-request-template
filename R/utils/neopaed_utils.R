# Neopaed Utilities
# Shared logic for neopaed (neonate/paediatric) filtering

#' Apply neopaed filtering strategy
#' 
#' @param data Dataframe with neonate_category and paediatric_category columns
#' @param strategy One of: "neonate", "paed", "either", or "none"
#' 
#' @return Filtered dataframe

filter_by_neopaed_strategy <- function(data, strategy) {
  
  if (strategy == "neonate") {
    
    print("Running neonate strategy...")
    
    return(
      data |> 
        filter(
          neonate_category %in% c("neonate_by_age", "neonate_by_specialty", "neonate_by_text")
          )
      )
    
  } else if (strategy == "paed") {
    
    print(" Running paediatric strategy...")
    
    return(
      data |> 
        filter(
          paediatric_category %in% c("paediatric_by_age", "paediatric_by_specialty", "paediatric_by_text")
          )
      )
  
  } else if (strategy == "either") {
    
    print("Running either strategy...")
    
    return(
      data |> 
        filter(
          neonate_category %in% c("neonate_by_age", "neonate_by_specialty", "neonate_by_text") |
            paediatric_category %in% c("paediatric_by_age", "paediatric_by_specialty", "paediatric_by_text")
        )
    )
    
  } else if (strategy == "none") {
    
    print("Skipping neopaeds strategy...")
    
    return(data)
    
  }
  
  stop(glue::glue("Unknown neopaed strategy: {strategy}"))
}