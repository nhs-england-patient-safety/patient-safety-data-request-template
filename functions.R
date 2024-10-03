



#' get code text
#'
#' this function takes a column code and a value code
#' and uses the appropriate lookup table to turn the value into a more informative value
#'
#' @param column The column code (string)
#' @param code The value code to be converted to actual value (string)
#' @param dataset either steis, lfpse or nrls (string)
#'
#' @return value as text
get_code_text <- function(column, code, dataset) {
  #no steis lookup table, so this returns the code value input
  if (dataset == "steis") {
    return(code)
    
  } else if (dataset == "nrls") {
    code_text_df <- codes |>
      filter(col_name == column, SASCODE == code) |>
      select(OPTIONTEXT)
    
  } else if (dataset == "lfpse") {
    code_text_df <- ResponseReference |>
      filter(QuestionId == column, ResponseCode == code) |>
      filter(TaxonomyVersion == max(TaxonomyVersion)) |>
      select(ResponseText) %>%
      distinct(ResponseText)
  }
  # only returns value if exactly one possible code_text is found
  if (nrow(code_text_df) == 1) {
    code_text <- pull(code_text_df)
    return(code_text)
  } else {
    print(
      str_glue(
        "{code} was not found in {column} column in lookup table for {dataset}. (or it was found with duplicates)"
      )
    )
    return(code)
  }
}

#' get column text
#'
#' this function takes a column code
#' and uses the appropriate lookup table to turn the value into the more informative column name
#'
#' @param column The column code (string)
#' @param dataset either steis, lfpse or nrls (string)
#'
#' @return value as text
get_column_text <- function(column, dataset) {
  #no steis lookup table, so this returns the code value input
  if (dataset == "steis") {
    return(column)
    
  } else if (dataset == "lfpse") {
    column_text_df <- QuestionReference |>
      filter(QuestionId == column) |>
      filter(TaxonomyVersion == max(TaxonomyVersion)) |>
      distinct(QuestionId, Property) |>
      select(Property)
  } else if (dataset == "nrls") {
    column_text_df <- nrls_colname_lookup %>%
      filter(NAME == column) %>%
      select(LABEL)
  }
  # only returns value if exactly one possible column_new is found
  if (nrow(column_text_df) == 1) {
    column_new <- pull(column_text_df)
    return(column_new)
  } else{
    print(str_glue("{column} column was not found in lookup table for {dataset}"))
    return(column)
  }
  
  return(column_new)
}





#' expand categorical filters
#' This function takes a string representing the categorical filters,
#' and converts it into a more informative and readable string
#'
#' @param string categorical filters (string)
#' @param dataset lfpse, nrls or steis (string)
#'
#' @return new string (string)
expand_categorical_filters <- function(string, dataset) {
  #look in environment for filters starting with dataset name _filter, and convert to string
  vector_of_filters <-   apropos(str_glue("{dataset}_filter_"))
  list_of_filters <- vector_of_filters %>%
    set_names() %>%
    map( ~ get(.))
  
  #manipulate full string - to make later processing simpler
  string_formatted <- string %>%
    str_replace_all('\"', '') %>% #get rid of speech marks
    str_replace_all(" +", " ") %>% #get rid of excess spaces - most important for (" " + col_name + " ") pattern
    str_replace_all("\\( \\+ ", "") %>% #replace ( + pattern - important for (" " + col_name + " ") pattern
    str_replace_all(" \\+ \\)", "") %>% # replace + ) pattern - important for (" " + col_name + " ") pattern
    str_replace_all("c\\(", "\\(") #remove c from the start of vectors
  
  #loop through all filters, replacing  filter with full text version of filter
  for (i in list_of_filters) {
    string_formatted <- replace_filter(string_formatted, i, dataset)
  }
  
  #replace |, & , ==, like and %in% with more understandable phrases
  string_formatted <- string_formatted %>%
    str_replace_all("\\|", "OR") %>%
    str_replace_all("&", "AND") %>%
    str_replace_all("==", "EQUALS") %>%
    str_replace_all("!=", "IS NOT ") %>%
    str_replace_all("!%in%", "IS NOT IN") %>%
    str_replace_all("%in%", "IS IN") %>%
    str_replace_all("%LIKE%", "CONTAINS")
  
  return(string_formatted)
}



#' replace filter
#' This function takes the a string of categorical filters and a filter from the environment
#' It converts the string to a more informative string
# and returns the modified string of categorical filters
#'
#' @param string_formatted string of categorical filters (string)
#' @param i  filter from the environment (call)
#' @param dataset dataset that the categorical filters refer to - lfpse, steis or nrls (string)
#'
#' @return string formatted with "i" replaced with a more informative string
replace_filter <- function(string_formatted, i, dataset) {
  #Logic is different for the different filter categories - e.g. A001 %in% c(1,2,3) will be converted differently to !is.na(A001)
  # so this step categorises the filter. Categories are in , not_in, equals, multi and filter_na
  filter_category <- find_filter_category(i)
  # form A001 %in% c(1,2,3) or !A001 %in% c(1,2,3)
  if (filter_category == "in" | filter_category == "not_in") {
    if (filter_category == "in") {
      #as the filters are "calls" , they need to be converted to a character vector
      # the second element is the column name
      column <- as.character(i[2])    
      # the third element is the c(1,2,3)- remove speechmarks
      value_string <- str_replace_all(as.character(i[[3]]), '\"', "")
      #converting c(1,2,3) into a character includes the letter "c" - this must be removed
      value_string <- value_string[value_string != "c"]
      # convert to a string (to make consistent with next section)
      value_string <- str_c(value_string, collapse = ",")
      not_string <- ""
    } else{
      #the first element is the ! so the second element has the "in"
      i_second <- as.character(i)[2]      
      i_second <- str_replace_all(i_second, 'c\\(|\\)|\"', "")
      #pull out the column name (lhs of %in%)
      column <- str_split_i(i_second, " %in% ", 1)
      #pull out the c(1,2,3) (rhs of %in%)
      value_string <- str_split_i(i_second, " %in% ", 2)
      value_string <- str_replace_all(value_string, ", ", ",") # remove spaces after commas to make consistent with above
      not_string <- "!"
    }
    # split into vector
    value_vec_old <- str_split(value_string, ",")[[1]]
    value_vec_old <- as.character(value_vec_old)
    #find column name
    column_new <- get_column_text(column, dataset)
    #find the new values
    value_vec_new <- map_chr(value_vec_old, ~ get_code_text(column, .x, dataset))
    
    #convert from vector to string
    value_str_new <- str_c(value_vec_new, collapse = ", ")
    value_str_old <- str_c(value_vec_old, collapse = ", ")
    # add brackets to either side of value_old - to allow matching to occur
    value_old <- str_c("\\(", value_str_old, "\\)")
    # add brackets to either side of value_new_string- helpful for clarity
    value_new <- str_c("(", value_str_new, ")")
    #recreate the filter by combining column, operator and value old
    filter_initial <- str_c(not_string, column, " ", "%in% ", value_old)
    filter_nice <- str_c(column_new, " ", not_string, "%in% ", value_new)
    
  } else if (filter_category == "equals") {
    column <- as.character(i[2])
    column_new <- get_column_text(column, dataset)
    value_old <- i[[3]]
    operator <- as.character(i[1])
    value_new_string <- get_code_text(column, value_old, dataset)
    #recreate the filter by combining column, operator and value old
    filter_initial <- str_c(column, operator, value_old , sep = " ")
    filter_nice <- str_c(column_new, operator, value_new_string, sep = " ")
    
  } else if (filter_category == "multi") {
    column <- as.character(i[2]) %>%
      str_extract("[a-zA-Z0-9_.-]+")
    column_new <- get_column_text(column, dataset)
    
    value_old <- i[[3]]
    operator <- as.character(i[1])
    
    value_pretty <- value_old %>%
      str_replace_all("% ", "") %>% #get rid of "% "- which is present with a multi-select column
      str_replace_all(" %", "") #get rid of " %"- which is present with a multi-select column
    value_new_string <- get_code_text(column, value_pretty, dataset)
    #recreate the filter by combining column, operator and value old
    filter_initial <- str_c(column, operator, value_old , sep = " ")
    filter_nice <- str_c(column_new, operator, value_new_string, sep = " ")
    
  } else if (filter_category == "filter_na") {
    not_na <- as.character(i)[1] == "!"
    column <- as.character(i)[2] %>%
      str_extract_all("[a-zA-Z0-9_-]{3,}")
    column_new <- get_column_text(column , dataset)
    if (not_na) {
      filter_initial <- str_glue("!is.na\\({column}\\)")
      filter_nice <- str_glue("{column_new} IS NOT NA")
    } else {
      filter_initial <- str_glue("([^!]|^)is.na\\({column}\\)")
      filter_nice <- str_glue("{column_new} IS NA")
    }
    
  }
  
  if (!is.na(filter_category)) {
    string_formatted <-  string_formatted %>%
      #replace initial filter with the formatted filter
      str_replace_all(filter_initial, filter_nice)
    
  }
  return(string_formatted)
}


find_filter_category <- function(i) {
  like_present <- sum(str_detect(as.character(i), "%LIKE%")) == 1
  space_number_space_present <- sum(str_detect(as.character(i), "% \\d+ %")) == 1
  space_colname_space_present <- sum(str_detect(as.character(i), '" " *\\+ *\\w+ *\\+ *" "')) == 1
  space_colname_space_present <- sum(str_detect(as.character(i), '" " *\\+ *\\w+ *\\+ *" "')) == 1
  
  vector_present <- sum(str_detect(as.character(i), "c\\(")) == 1
  in_present <- sum(str_detect(as.character(i), "%in%")) == 1
  not_at_start <- sum(as.character(i) == "!") == 1
  equals_present <- sum(str_detect(as.character(i), "==|!=")) == 1
  
  
  type_na <- sum(str_detect(as.character(i), "na"))
  
  type_equals <- if_else(equals_present & !vector_present, 1, 0)
  
  type_in <- if_else(vector_present &
                       in_present & !not_at_start, 1, 0)
  
  type_not_in <- if_else(vector_present &
                           in_present & not_at_start, 1, 0)
  
  type_multi <- if_else(like_present &
                          space_number_space_present &
                          space_colname_space_present,
                        1,
                        0)
  
  filter_category <- NA
  
  if (type_equals == 1  &
      sum(type_in, type_not_in, type_na, type_multi) == 0) {
    filter_category = "equals"
  }
  else if (type_in == 1  &
           sum(type_not_in, type_equals, type_na, type_multi) == 0) {
    filter_category = "in"
  }
  else if (type_not_in == 1  &
           sum(type_in, type_equals, type_na, type_multi) == 0) {
    filter_category = "not_in"
  }
  else if (type_na == 1  &
           sum(type_in, type_not_in, type_equals, type_multi) == 0) {
    filter_category = "filter_na"
  }
  else if (type_multi == 1  &
           sum(type_in, type_not_in, type_na, type_equals) == 0) {
    filter_category = "multi"
  } else{
    print("Filter category not found")
  }
  return(filter_category)
}

add_summary_sheet <- function(wb, i, title, database_name, sheet) {
  summary_categories_list <- get(str_glue("summary_categories_{database_name}"))
  
  df <- get(i)
  
  addWorksheet(wb, sheet, gridLines = FALSE)
  
  # set column widths
  setColWidths(wb,
               sheet = sheet,
               cols = 1,
               widths = 50)
  #
  
  #Add text style
  addStyle(wb,
           sheet = sheet,
           textStyle,
           rows = 1:6,
           cols = 1:1)
  
  
  # Write text
  writeData(
    wb,
    sheet,
    paste(sheet, "Confidential", sep = " - "),
    startCol = 1,
    startRow = 1
  )
  writeData(wb, sheet, title, startCol = 1, startRow = 3)
  writeData(
    wb,
    sheet,
    paste("Number of Incidents", nrow(df), sep = ": "),
    startCol = 1,
    startRow = 5
  )
  
  
  
  start_row = 7
  
  for (category in summary_categories_list) {
    if (length(category) == 1) {
      one_variable = TRUE
      category[[2]] = category[[1]]
    } else{
      one_variable = FALSE
    }
    
    if (database_name == "lfpse") {
      df <- df %>%
        mutate(
          Year = year(`T005 - Event date`),
          Month = month(`T005 - Event date`, label = TRUE, abbr = TRUE)
        ) %>%
        mutate(
          `OT001 - Physical harm` = fct_relevel(
            `OT001 - Physical harm`,
            "No physical harm",
            "Low physical harm",
            "Moderate physical harm",
            "Severe physical harm",
            "Fatal"
          )
        )
      
    } else if (database_name == "nrls") {
      df <- df %>%
        mutate(
          Year = year(`Date of Incident`),
          Month = month(`Date of Incident`, label = TRUE, abbr = TRUE)
        ) %>%
        mutate(
          `PD09 Degree of harm (severity)` = fct_relevel(
            `PD09 Degree of harm (severity)`,
            "No Harm",
            "Low",
            "Moderate",
            "Severe",
            "Death"
          )
        )
      
    } else if (database_name == "steis") {
      print("steis")
      df <- df %>%
        mutate(
          Year = year(`Created on`),
          Month = month(`Created on`, label = TRUE, abbr = TRUE)
        )
      
    }
    
    summary_table <- df %>%
      count(!!category[[1]], !!category[[2]])
    print(summary_table)
    
    if (!one_variable) {
      summary_table <- summary_table %>%
        pivot_wider(names_from = !!category[[2]],
                    values_from = n) %>%
        adorn_totals('both')
      
    } else{
      summary_table <- summary_table %>%
        mutate(percent = scales::percent(n / sum(n))) %>%
        adorn_totals('row')
    }
    
    print(summary_table)
    writeData(wb, sheet, summary_table, startRow = start_row)
    
    #
    # # Add header style
    addStyle(
      wb,
      sheet = sheet,
      headerStyle,
      rows = start_row,
      cols = 1:(ncol(summary_table))
    )
    
    addStyle(
      wb,
      sheet = sheet,
      rowTitleStyle,
      rows = (start_row + 1):(nrow(summary_table) + start_row),
      cols = 1
    )
    #
    # # Add body style
    addStyle(
      wb,
      sheet = sheet,
      bodyStyle,
      rows = (start_row + 1):(nrow(summary_table) + start_row),
      cols = 2:(ncol(summary_table)),
      gridExpand = T
    )
    
    # set row heights
    
    setRowHeights(
      wb,
      sheet = sheet,
      rows = start_row:(start_row + nrow(summary_table)),
      heights = 34
    )
    
    start_row <- start_row + nrow(summary_table) + 3
    
  }
  return(wb)
}

add_data_sheet <- function(wb, i, title, sheet) {
  df <- get(i)
  
  addWorksheet(wb, sheet, gridLines = FALSE)
  
  # set column widths
  setColWidths(wb,
               sheet = sheet,
               cols = 1:ncol(df),
               widths = 35)
  
  # set row heights - header row
  
  setRowHeights(wb,
                sheet = sheet,
                rows = 7:7,
                heights = 34)
  
  # set row heights - body
  setRowHeights(wb,
                sheet = sheet,
                rows = 8:(nrow(df) + 7),
                heights = 150)
  
  
  # Add text style
  addStyle(wb,
           sheet = sheet,
           textStyle,
           rows = 1:6,
           cols = 1:1)
  
  # Add header style
  addStyle(
    wb,
    sheet = sheet,
    headerStyle,
    rows = 7,
    cols = 1:ncol(df)
  )
  
  # Add body style
  addStyle(
    wb,
    sheet = sheet,
    bodyStyle,
    rows = 8:(nrow(df) + 7),
    cols = 1:ncol(df),
    gridExpand = T
  )
  
  # Write text
  writeData(
    wb,
    sheet,
    paste(sheet, "Confidential", sep = " - "),
    startCol = 1,
    startRow = 1
  )
  writeData(wb, sheet, title, startCol = 1, startRow = 3)
  writeData(
    wb,
    sheet,
    paste("Number of Incidents", nrow(df), sep = ": "),
    startCol = 1,
    startRow = 5
  )
  
  # Write data
  writeData(wb, sheet, df, startRow = 7)
  return(wb)
}