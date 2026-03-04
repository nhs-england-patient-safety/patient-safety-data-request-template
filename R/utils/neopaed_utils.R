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
    
    message("Running neonate strategy...")
    
    return(
      data |> 
        filter(
          neonate_category %in% c("neonate_by_age", "neonate_by_specialty", "neonate_by_text")
          )
      )
    
  } else if (strategy == "paed") {
    
    message(" Running paediatric strategy...")
    
    return(
      data |> 
        filter(
          paediatric_category %in% c("paediatric_by_age", "paediatric_by_specialty", "paediatric_by_text")
          )
      )
  
  } else if (strategy == "either") {
    
    message("Running either strategy...")
    
    return(
      data |> 
        filter(
          neonate_category %in% c("neonate_by_age", "neonate_by_specialty", "neonate_by_text") |
            paediatric_category %in% c("paediatric_by_age", "paediatric_by_specialty", "paediatric_by_text")
        )
    )
    
  } else if (strategy == "none") {
    
    message("Skipping neopaeds strategy...")
    
    return(data)
    
  }
  
  stop(str_glue("Unknown neopaed strategy: {strategy}"))
}
