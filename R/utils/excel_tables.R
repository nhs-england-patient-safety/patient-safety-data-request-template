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
      janitor::tabyl(!!renamed_variable_to_tabulate_by_one_col_name,
                     show_missing_levels = TRUE,
                     show_na = TRUE
      ) |>
      janitor::adorn_pct_formatting() |>
      select(-any_of("valid_percent")) |># remove additional percent column 
      janitor::untabyl()
    
    if(!is_multi_select(df_to_create_summary_table,renamed_variable_to_tabulate_by_one)){
      summary_table <- summary_table |>
        janitor::adorn_totals("row")
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
      janitor::tabyl(!!renamed_variable_to_tabulate_by_one_col_name,
                     !!renamed_variable_to_tabulate_by_two_col_name,
                     show_missing_levels = TRUE,
                     show_na = TRUE
      ) |>
      rename(any_of(c(`Not available` = "NA_"))) |>
      janitor::untabyl()
    
    
    if(!is_multi_select(df_to_create_summary_table,renamed_variable_to_tabulate_by_one)){
      summary_table <- summary_table |>
        janitor::adorn_totals("row")
    }
    if(!is_multi_select(df_to_create_summary_table,renamed_variable_to_tabulate_by_two)){
      summary_table <- summary_table |>
        janitor::adorn_totals("col")
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
