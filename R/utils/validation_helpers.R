# Validation Helper Utilities


#' Check if column contains multi-select values
#' 
#' @param df Dataframe to check
#' @param variable_name Name of column to check
#' 
#' @return Logical. TRUE if column has multi-select values ("; " present), 
#' FALSE if no multi-select values, NA if column doesn't exist

is_multi_select<- function(df, variable_name){
  
  if(variable_name %in% colnames(df)){
    
    n_multi<-df |> 
      select(all_of(c("col" = variable_name))) |>
      mutate(multi=str_detect(col,"; ")) |>
      filter(multi) |>
      nrow()
    
    return(n_multi > 0)
    
  } else {
    message("The column does not exist")
    return(NA)
  }
  
}
