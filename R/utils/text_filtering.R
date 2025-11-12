# Text Filtering Utilities
# Shared logic for text-based filtering across all datasets

#' Apply text filters to a dataset
#' 
#' @param data Dataframe to filter
#' @param text_terms Named list of terms by group, e.g. list(group_A = c("term1", "term2"))
#' @param text_filter Expression defining filter logic, e.g. expr(group_A | group_B)
#' @param columns_to_search Character vector of column names to concatenate and search
#' @param dataset_name Name of dataset (for logging)
#' 
#' @return Filtered dataframe with group columns added

apply_text_search <- function(data, text_terms, text_filter, columns_to_search, dataset_name) {
  
  # check if text search is needed
  if (sum(!is.na(text_terms)) == 0) {
    message("No text terms supplied. Skipping text search...")
    return(data)
  }
  
  message(glue::glue("Running {dataset_name} text search..."))
  
  # create concatenated column from specified columns
  data_with_concat <- data |> 
    mutate(concat_col = paste(!!!syms(columns_to_search), sep = " "))
  
  # iterate through each group
  groups <- names(text_terms)
  for (group in groups) {
    terms <- text_terms[[group]]
    
    # iterate through each term in the group
    for (term in terms) {
      data_with_concat <- data_with_concat |> 
        mutate("{group}_term_{term}" := str_detect(concat_col, term))
    }
    
    # create group-level flag (TRUE if any term in group matches)
    data_with_concat <- data_with_concat |> 
      mutate("{group}" := rowSums(across(starts_with(group))) > 0)
  }
  
  # apply the text filter logic
  filtered_data <- data_with_concat |> 
    filter(!!text_filter) |> 
    select(-concat_col)
  
  message(glue::glue("{dataset_name} text search retrieved {format(nrow(filtered_data), big.mark = ',')} indicents."))
  
  return(filtered_data)
}