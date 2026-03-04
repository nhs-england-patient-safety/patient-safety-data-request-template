# Lookup & Translation Utilities


#' Get label for a code value
#' 
#' @param column Column name where code appears
#' @param code Code value to look up
#' @param database_name Name of database (e.g., 'NRLS', 'LFPSE', 'StEIS')
#' 
#' @return Text label for the code, or the original code if not found

get_code_text <- function(column, code, database_name) {
  if (database_name == "steis") {
    code <- str_replace_all(code, "#", "")
    return(code)
  } else if (database_name == "nrls") {
    code_text_df <- codes |>
      filter(col_name == column, SASCODE == code) |>
      select(OPTIONTEXT)
  } else if (database_name == "lfpse") {
    code <- str_replace_all(code, "#", "")
    code_text_df <- ResponseReference |>
      filter(QuestionId == column, ResponseCode == code) |>
      filter(TaxonomyVersion == max(TaxonomyVersion)) |>
      select(ResponseText) |>
      distinct(ResponseText)
  }
  if (nrow(code_text_df) == 1) {
    code_text <- pull(code_text_df)
  } else {
    code_text <- code
    print(str_glue("{code} was not found in {column} column in lookup table for {database_name}. (or it was found with duplicates)"))
  }
  return(code_text)
}


#' Get label for a column name
#' 
#' @param column Column name to look up
#' @param database_name Name of database (e.g., 'NRLS', 'LFPSE', 'StEIS')
#' 
#' @return Column label, or original column name if not found

get_column_text <- function(column, database_name) {
  if (database_name == "steis") {
    return(column)
  } else if (database_name == "lfpse") {
    column_text_df <- QuestionReference |>
      filter(QuestionId == column) |>
      filter(TaxonomyVersion == max(TaxonomyVersion)) |>
      distinct(QuestionId, Property) |>
      select(Property)
  } else if (database_name == "nrls") {
    column_text_df <- nrls_lookup |>
      filter(Code == column) |>
      select(Label)
  }
  if (nrow(column_text_df) == 1) {
    column_new <- pull(column_text_df)
  } else {
    column_new <- column
    print(str_glue("{column} column was not found in lookup table for {database_name}"))
  }
  return(column_new)
}


#' Translate individual filter expression
#' 
#' @param individual_filter Single filter expression string
#' @param database_name Name of database (e.g., 'NRLS', 'LFPSE', 'StEIS')
#' 
#' @return Translated filter expression

translate_individual_filter <- function(individual_filter, database_name) {
  # different logic depending on what the filter is
  if (str_detect(individual_filter, "(IS NOT NA)|(IS NA)")) {
    split_string <- str_replace(individual_filter, "(IS NOT NA)|(IS NA)", "")
    column <- split_string |>
      str_replace_all(fixed("("), "") |>
      str_replace_all(fixed(")"), "") |>
      str_trim()
    column_new <- get_column_text(column, database_name)
    
    translated_filter <- individual_filter |>
      # replace IS NOT NA
      str_replace(str_glue("IS NOT NA\\({column}\\)"), str_glue("{column_new} IS NOT NA")) |>
      # replace IS NA
      str_replace(str_glue("IS NA\\({column}\\)"), str_glue("{column_new} IS NA"))
  } else if (str_detect(individual_filter, "(CONTAINS)|(IS IN)|(IS)")) {
    split_string <- str_split(individual_filter, "(CONTAINS)|(IS IN)|(IS)") |> unlist()
    column <- split_string[1] |>
      str_replace_all(fixed("("), "") |>
      str_replace_all(fixed(")"), "") |>
      str_trim()
    value <- split_string[2] |>
      str_replace_all(fixed("("), "") |>
      str_replace_all(fixed(")"), "") |>
      str_trim()
    column_new <- get_column_text(column, database_name)
    
    
    if (str_detect(individual_filter, "(IS IN)")) {
      value_new <- c()
      value_split <- str_split(value, ",") |> unlist()
      for (each_value in value_split) {
        # find the text corresponding to the code
        code_text <- get_code_text(column, str_replace(each_value, " ", ""), database_name)
        # append this value to a vector of values
        value_new <- append(value_new, code_text)
      }
      
      value_new <- str_c(value_new, collapse = ", ")
    } else {
      value_new <- get_code_text(column, value, database_name)
    }
    
    
    translated_filter <- individual_filter |>
      # if brackets around both sides of column name- replace brackets (this is relevant for CONTAINS)
      # (this is a separate step because we only want to remove brackets where they occur on both sides)
      str_replace(str_glue("\\( *{column} *\\)"), column_new) |>
      # if brackets not around column name- replace column name
      str_replace(str_glue("{column}"), column_new) |>
      str_replace(value, value_new)
  }else{
    message(str_glue("{individual_filter} could not be translated"))
    translated_filter<- individual_filter
  }
  
  return(translated_filter)
}


#' Translate categorical filter expression
#' 
#' @param categorical_filter Expression object or 0. Filter to translate.
#' @param database_name Name of database (e.g., 'NRLS', 'LFPSE', 'StEIS')
#' 
#' @return Translated filter string (or status message)

translate_categorical_string <- function(categorical_filter, database_name) {
  if (!get(str_glue("search_{database_name}"))){
    message(str_glue("{database_name} is not being searched for this query."))
    if (categorical_filter!= 0){
      warning(str_glue("{database_name} is not being searched for this query but a filter has been provided. This will not be used."))
    }
    return("Database not searched")
  }
  
  if (categorical_filter == 0) {
    message(str_glue("No {database_name} filter was provided."))
    return("No categorical filter")
  }
  
  # turn the categorical filter into a string
  categorical_filter_string <- deparse(categorical_filter, width.cutoff = 500)
  
  # the maximum width cutoff for deparse is 500-
  # for strings above this length, it will automatically split up the string into a vector.
  # if this happens, it may break up filters, so we need to combine the vectors back into one string.
  if (length(categorical_filter_string) > 1) {
    categorical_filter_string <- str_c(categorical_filter_string, collapse = "")
  }
  
  # translate filter into more human readable format, including removing speech marks
  categorical_filter_copy <- categorical_filter_string |>
    str_replace_all(fixed('"'), "") |>
    str_replace_all("==", "IS") |>
    str_replace_all(fixed("+"), "") |>
    str_replace_all("!is.na", "IS NOT NA") |>
    str_replace_all("is.na", "IS NA") |>
    str_replace_all("(?i)%LIKE%", "CONTAINS") |>
    str_replace_all("(?i)%IN%", "IS IN") |>
    str_replace_all("%", "") |>
    str_replace_all("&", "AND") |>
    str_replace_all("c\\(", "(") |>
    str_replace_all(fixed("|"), "OR") |>
    str_replace_all(" +", " ") # this replaces multiple spaces with a single space
  
  string_to_split_by <- "(OR|AND)"
  # if the string contains AND  or OR, we'll need to separate and loop through
  categorical_filter_copy_split <- str_split(categorical_filter_copy, string_to_split_by) |> unlist()
  # pull out whether split by and or or
  bracket_breaks <- str_extract_all(categorical_filter_copy, string_to_split_by) |> unlist()
  
  translated_filters <- ""
  for (filter_number in 1:length(categorical_filter_copy_split)) {
    # translate the individual filter
    individual_filter_translated <- translate_individual_filter(categorical_filter_copy_split[filter_number], database_name)
    # extract the AND or OR between filters
    break_between_filters <- if_else(is.na(bracket_breaks[filter_number]), "", bracket_breaks[filter_number])
    # add the translated filter and AND or OR to the translated_filters string
    translated_filters <- str_c(translated_filters, individual_filter_translated, break_between_filters)
  }
  message(str_glue("{database_name} filter is: \n {translated_filters}"))
  return(translated_filters)
}
