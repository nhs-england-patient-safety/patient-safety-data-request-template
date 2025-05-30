
#not a function- but this creates the object "date_filter" from date_type
date_filter <- if (date_type == 'occurring') {
  expr(occurred_date)
} else if (date_type == 'reported') {
  expr(reported_date)
}


#function to find the minimum number in a vector, and return NA if all values are NA

# This is required to find the minimum physical or psychological harm level
min_safe <- function(vec) {
  ifelse(length(vec[!is.na(vec)]) == 0, NA_real_, min(vec, na.rm = TRUE))
}


# this function adds the header section to the sheet- the contents depend on the database and what sheet is being made
add_header_to_sheet <- function(wb, title,
                                database_name,
                                sheet,
                                summary_sheet,
                                number_of_rows_sampled,
                                number_of_rows_unsampled,
                                summary_tables_incident_or_patient_level) {
  # Write title
  writeData(wb,
    sheet,
    paste(sheet, "Confidential", sep = " - "),
    startCol = 1,
    startRow = 1
  )

  # write subtitle
  writeData(wb, sheet, title, startCol = 1, startRow = 3)

  content_start_row <- 5

  # add caveats to lfpse summary tab
  
  if (database_name == "LFPSE" & summary_sheet) {
  #multi-select caveat
      note <- c(
      "Note: Where a question can have multiple answers, these have been separated out so will sum to a larger number than the number of incidents."
    )
  #caveat for incident level summary tables
    if (summary_tables_incident_or_patient_level=="incident"){
      note<- c( note, "Note: The data here has been aggregated for the patients within an incident, selecting the largest physical harm level accross patients")
    }
    
    # write note
    writeData(
      wb,
      sheet,
      note,
      startCol = 1,
      startRow = content_start_row,
    )

    content_start_row <- content_start_row + 3
  }

  
  incident_or_pt_level <- case_when(summary_sheet & database_name == "LFPSE"  ~ str_glue("({summary_tables_incident_or_patient_level} level)"),
    !summary_sheet & database_name == "LFPSE" ~ "(patient level)",
    .default = ""
  )
  
  # format number of sampled/unsampled incidents to have comma separation
  number_of_rows_unsampled_formatted <- format(number_of_rows_unsampled, big.mark = ",", scientific=F)
  number_of_rows_sampled_formatted <- format(number_of_rows_sampled, big.mark = ",", scientific=F)
  
  # write number of incidents
  writeData(
    wb,
    sheet,
    paste(str_glue("Number of Incidents retrieved by search strategy{incident_or_pt_level}: {number_of_rows_unsampled_formatted}")),
    startCol = 1,
    startRow = content_start_row
  )

  writeData(
    wb,
    sheet,
    paste(str_glue("Number of Incidents in Sample{incident_or_pt_level}: {number_of_rows_sampled_formatted}")),
    startCol = 1,
    startRow = content_start_row + 1
  )

  # set start row for summary tables
  table_start_row <- content_start_row + 3

  # Add text style
  addStyle(wb, sheet = sheet, textStyle, rows = 1:(table_start_row - 1), cols = 1)

  return(table_start_row)
}


# this function adds a note prior to the term tally table in the summary sheets
add_text_to_summary_sheets <- function(wb, sheet,
                                       content_start_row,
                                       text_to_add) {
  
  if(text_to_add=="term_tally_table_heading"){
    # Explain what the group/term tally tables show
    writeData(
      wb, sheet,
      paste("The tables below present the number of incidents identified by each group and text term in the search strategy."),
      startCol = 1,
      startRow = content_start_row
    )
    
    # Add in caveat about not being able to sum the numbers of terms/groups
    writeData(
      wb, sheet,
      paste("Note: A single incident can be identified by multiple groups and text terms, so the number of incidents identified by different terms/groups are not summable."),
      startCol = 1,
      startRow = content_start_row + 1
    )
    
    # set start row for next content
    table_start_row <- content_start_row + 3
    
    # Add text style
    addStyle(wb, sheet = sheet, textStyle, rows = content_start_row:(content_start_row + 1), cols = 1)
  }
  
  if(text_to_add=="sampled_table_headers"){
    # Add header for unsampled tables
    writeData(
      wb, sheet,
      paste("Search strategy:"),
      startCol = 1,
      startRow = content_start_row
    )
    
    # Add header for sampled tables
    writeData(
      wb, sheet,
      paste("Sample:"),
      startCol = ncol(summary_table_unsampled) + 2,
      startRow = content_start_row
    )
    
    # set the start row for tables
    table_start_row <- content_start_row + 2
    
    # Add text style
    addStyle(wb, sheet = sheet, textStyle, rows = content_start_row, cols = 1:(ncol(summary_table_unsampled) + 2))
  }
  
  return(table_start_row)
}


# this function creates a summary table from a dataframe and a list containing the variables to tabulate by
create_summary_table <- function(df_to_create_summary_table,
                                 variables_to_tabulate_by_list,
                                 database_name) {
  # work out if table needs to have one or 2 variables.
  if (length(variables_to_tabulate_by_list) == 1) {
    # extract the variable from list of variables
    variable_to_tabulate_by_one <- unlist(variables_to_tabulate_by_list)[[1]]

    # convert the variable which data will be tabulated to a more human readable name, using lookup
    renamed_variable_to_tabulate_by_one <- names(which(rename_lookup[[database_name]] == variable_to_tabulate_by_one))

    if (length(renamed_variable_to_tabulate_by_one) == 0) {
      message(str_glue("{variable_to_tabulate_by_one} does not exist. Table cannot be created. "))
      return(tibble(`Table could not be made` = str_glue("{variable_to_tabulate_by_one} doesn't exist.")))
    }
    
    # allow the variable to be used as a column name
    renamed_variable_to_tabulate_by_one_col_name <- sym(renamed_variable_to_tabulate_by_one)
    
    summary_table <- df_to_create_summary_table |>
      # separate multi select values
      separate_rows(!!renamed_variable_to_tabulate_by_one_col_name, sep = "; ") |>
      convert_columns_to_factors(database_name) |>
      # use count to tabulate
      tabyl(!!renamed_variable_to_tabulate_by_one_col_name,
        show_missing_levels = TRUE,
        show_na = TRUE
      ) |>
      adorn_pct_formatting() |>
      select(-any_of("valid_percent")) |># remove additional percent column 
      untabyl()
    
    if(!is_multi_select(df_to_create_summary_table,renamed_variable_to_tabulate_by_one)){
      summary_table <- summary_table |>
        adorn_totals("row")
    }
    

    } else if (length(variables_to_tabulate_by_list) == 2) {
    # extract the variables from list of variables
    variable_to_tabulate_by_one <- unlist(variables_to_tabulate_by_list)[[1]]
    variable_to_tabulate_by_two <- unlist(variables_to_tabulate_by_list)[[2]]

    # convert the variables which data will be tabulated to a more human readable name, using lookup
    renamed_variable_to_tabulate_by_one <- names(which(rename_lookup[[database_name]] == variable_to_tabulate_by_one))
    renamed_variable_to_tabulate_by_two <- names(which(rename_lookup[[database_name]] == variable_to_tabulate_by_two))
    if (length(renamed_variable_to_tabulate_by_one) == 0) {
      message(str_glue("{variable_to_tabulate_by_one} does not exist. Table cannot be created. "))
      return(tibble(`Table could not be made` = str_glue("{variable_to_tabulate_by_one} doesn't exist.")))
    }
    if (length(renamed_variable_to_tabulate_by_two) == 0) {
      message(str_glue("{renamed_variable_to_tabulate_by_two} does not exist. Table cannot be created. "))
      return(tibble(`Table could not be made` = str_glue("{variable_to_tabulate_by_two} doesn't exist.")))
    }

    # allow the variable to be used as a column name
    renamed_variable_to_tabulate_by_one_col_name <- sym(renamed_variable_to_tabulate_by_one)
    renamed_variable_to_tabulate_by_two_col_name <- sym(renamed_variable_to_tabulate_by_two)

    summary_table <- df_to_create_summary_table |>
      # separate multi select values
      separate_rows(!!renamed_variable_to_tabulate_by_one_col_name, sep = "; ") |>
      separate_rows(!!renamed_variable_to_tabulate_by_two_col_name, sep = "; ") |>
      convert_columns_to_factors(database_name) |>
      # use count to get a table
      tabyl(!!renamed_variable_to_tabulate_by_one_col_name,
        !!renamed_variable_to_tabulate_by_two_col_name,
        show_missing_levels = TRUE,
        show_na = TRUE
      ) |>
      rename(any_of(c(`Not available` = "NA_"))) |>
      untabyl()
    
    
    if(!is_multi_select(df_to_create_summary_table,renamed_variable_to_tabulate_by_one)){
      summary_table <- summary_table |>
        adorn_totals("row")
    }
    if(!is_multi_select(df_to_create_summary_table,renamed_variable_to_tabulate_by_two)){
      summary_table <- summary_table |>
        adorn_totals("col")
    }
    
  } else {
    message(str_glue("TOO MANY VARIABLES INCLUDED FOR {database_name}"))
    message(paste(variables_to_tabulate_by_list, collapse = ", "))
    return(tibble(`Table could not be made` = "Too many variables"))
  }

  return(summary_table)
}

# function to create term tally table
create_term_tally_table <- function(df_to_create_term_tally,
                                    cols_to_use = c("term_columns")) {
  
  # calculate the number of incidences identified by each search term
  if(cols_to_use=="term_columns") {
    # sum the number of True values in each term column
    summary_table <- df_to_create_term_tally |>
      select(matches("term")) |>
      summarise(across(everything(), ~sum(. == TRUE, na.rm = TRUE))) |>
      pivot_longer(cols = everything(), names_to = "Search term", values_to = "n")
    # style the format of the search term column in the table
    summary_table$`Search term` <- sapply(summary_table$`Search term`, make_text_terms_pretty)
  }

  # calculate the number of incidences identified by each search group
  if(cols_to_use=="group_columns") {
    # sum the number of True values in each group column
    summary_table <- df_to_create_term_tally |>
      select(matches("group_\\D{1}\\b")) |>
      summarise(across(everything(), ~sum(. == TRUE, na.rm = TRUE))) |>
      pivot_longer(cols = everything(), names_to = "Group", values_to = "n")
    # style the format of the group column in the table
    summary_table$Group <- sapply(summary_table$Group, make_text_terms_pretty)
  }

  return(summary_table)
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
            "Moderate physical harm", "Severe physical harm", "Fatal"
          )
        ),
        `Largest psychological harm (across all patients in incident)` = factor(
          `Largest psychological harm (across all patients in incident)`,
          levels = c(
            "No psychological harm",
            "Low psychological harm",
            "Moderate psychological harm",
            "Severe psychological harm"
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





# this function adds a summary table to a sheet
add_summary_table_to_sheet <- function(wb,
                                       sheet,
                                       summary_table,
                                       table_start_row,
                                       table_start_col) {
  #assess if a total row is present in summary table
  total_row = summary_table[nrow(summary_table),1]=="Total"
  # add summary table to sheet
  writeData(wb, sheet, summary_table, startRow = table_start_row, startCol = table_start_col, keepNA = TRUE, na.string = "Not available")

  setColWidths(wb,
    sheet = sheet,
    cols = table_start_col,
    widths = 30
  )
  
  setColWidths(wb,
    sheet = sheet,
    cols = (table_start_col + 1):(table_start_col + ncol(summary_table) - 1),
    widths = 15
  )

  # style table - header
  addStyle(
    wb,
    sheet = sheet,
    summaryTableTopBottomStyle,
    rows = table_start_row,
    cols = table_start_col:(table_start_col + ncol(summary_table) - 1)
  )

  # style table- row titles
  addStyle(
    wb,
    sheet = sheet,
    rowTitleStyle,
    rows = (table_start_row + 1):(nrow(summary_table) + table_start_row),
    cols = table_start_col
  )

  # style table - main body
  addStyle(
    wb,
    sheet = sheet,
    bodyStyleNoBorder,
    rows = (table_start_row + 1):(nrow(summary_table) + table_start_row),
    cols = (table_start_col + 1):(table_start_col + ncol(summary_table) - 1),
    gridExpand = T
  )

  # style table- footer with border when there is a total row
  if(total_row){
    addStyle(
        wb,
        sheet = sheet,
        summaryTableTopBottomStyle,
        rows = nrow(summary_table) + table_start_row,
        cols = table_start_col:(table_start_col + ncol(summary_table) - 1),
        gridExpand = T
      ) 
  }
}


# this function adds a data table to a sheet and styles it
add_data_table_to_sheet <- function(wb,
                                    sheet,
                                    data_table,
                                    table_start_row) {
  # Write data
  writeData(wb, sheet, data_table, startRow = table_start_row)

  # set column widths
  setColWidths(wb,
    sheet = sheet,
    cols = 1:ncol(data_table),
    widths = 35
  )

  # set row heights - header row

  setRowHeights(wb,
    sheet = sheet,
    rows = table_start_row:table_start_row,
    heights = 34
  )

  # set row heights - body
  setRowHeights(wb,
    sheet = sheet,
    rows = (table_start_row + 1):(nrow(data_table) + table_start_row),
    heights = 150
  )

  # Add header style
  addStyle(
    wb,
    sheet = sheet,
    headerStyle,
    rows = table_start_row,
    cols = 1:ncol(data_table)
  )

  # Add body style
  addStyle(
    wb,
    sheet = sheet,
    bodyStyle,
    rows = (table_start_row + 1):(nrow(data_table) + table_start_row),
    cols = 1:ncol(data_table),
    gridExpand = T
  )
}


# function to get find the label for a column value from the column name, code and database name
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

# function to get find the column label for a column from the column name and database name
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
# function to translate an individual filter into a more human readable value, given the filter string and database name
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


# function to translate a categorical filter (as an expression object) into a more human readable string given a database name
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



make_text_terms_pretty <- function(term){
  term |>
    str_replace_all(pattern = fixed("(?:\\W|)"), "~") |>
    str_replace_all(pattern = "\\|", " OR ") |>
    str_replace_all(pattern = fixed('\\b'), "%" ) |>
    str_replace_all(pattern = fixed('(?i)'), "" ) |>
    str_replace_all("term_", "term: ") |>
    str_replace_all("group_", "Group ") |>
    str_replace_all("_", " ")
}




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
