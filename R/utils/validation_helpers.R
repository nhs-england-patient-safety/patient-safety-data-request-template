is_multi_select<- function(df, variable_name){
  
  if(variable_name %in% colnames(df)){
    
    n_multi<-df |> 
      select(all_of(c("col" = variable_name))) |>
      mutate(multi=str_detect(col,"; ")) |>
      filter(multi) |>
      nrow()
    
    return(n_multi > 0)
    
  } else {
    print("The column does not exist")
    return(NA)
  }
  
}