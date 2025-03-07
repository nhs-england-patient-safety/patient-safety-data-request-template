#function to find the minimum number in a vector, and return NA if all values are NA
# This is required to find the minimum physical or psychological harm level
min_safe<- function(vec){
   ifelse(length(vec[!is.na(vec)]) == 0, NA_real_, min(vec,na.rm=TRUE))
}


#this function adds the header section to the sheet- the contents depend on the database and what sheet is being made
add_header_to_sheet<-function(wb, title, 
                              database_name, 
                              sheet, 
                              summary_sheet, 
                              number_of_rows_sampled,
                              number_of_rows_unsampled){
  
  # Write title
  writeData( wb, 
             sheet, 
             paste(sheet, "Confidential", sep = " - "),
             startCol = 1,
             startRow = 1
  )
  
  #write subtitle
  writeData(wb, sheet, title, startCol = 1, startRow = 3)
  
  content_start_row = 5
  
  #add caveats to lfpse summary tab
  if (database_name == "LFPSE" & summary_sheet){
    
    note<- c("Note: The data here has been aggregated for the patients within an incident, selecting the largest physical harm level accross patients",
             "Note: Where a question can have multiple answers, these have been separated out so will sum to a larger number than the number of incidents.")
    
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
  
  incident_or_pt_level<- case_when(summary_sheet & database_name=="LFPSE" ~ " (incident level)",
                                   !summary_sheet & database_name=="LFPSE" ~ " (patient level)",
                                   .default = "")
  # write number of incidents
  writeData(
    wb,
    sheet,
    paste(str_glue("Number of Incidents retrieved by search strategy{incident_or_pt_level}: {number_of_rows_unsampled}")),
    startCol = 1,
    startRow = content_start_row
  )
  
  writeData(
    wb,
    sheet,
    paste(str_glue("Number of Incidents in Sample{incident_or_pt_level}: {number_of_rows_sampled}")),
    startCol = 1,
    startRow = content_start_row + 1
  )
  
  #set start row for summary tables
  table_start_row = content_start_row + 3
  
  #Add text style
  addStyle(wb, sheet = sheet,  textStyle, rows = 1:(table_start_row - 1), cols = 1)
  
  return(table_start_row)
  
}


#this function creates a summary table from a dataframe and a list containing the variables to tabulate by
create_summary_table<-function(df_to_create_summary_table,
                               variables_to_tabulate_by_list, 
                               database_name){
    #work out if table needs to have one or 2 variables. 
  if (length(variables_to_tabulate_by_list) == 1) {
    
    # extract the variable from list of variables
    variable_to_tabulate_by_one<- unlist(variables_to_tabulate_by_list)[[1]]
    
    #convert the variable which data will be tabulated to a more human readable name, using lookup
    renamed_variable_to_tabulate_by_one<-names(which(rename_lookup[[database_name]]==variable_to_tabulate_by_one))
    
    if (length(renamed_variable_to_tabulate_by_one)==0){
      message(str_glue("{variable_to_tabulate_by_one} does not exist. Table cannot be created. "))
      return(tibble(`Table could not be made`= str_glue("{variable_to_tabulate_by_one} doesn't exist.")))
    }
    
    #allow the variable to be used as a column name
    renamed_variable_to_tabulate_by_one_col_name<- sym(renamed_variable_to_tabulate_by_one) 

    summary_table <- df_to_create_summary_table |>
      #seperate multi select values
      separate_rows(!!renamed_variable_to_tabulate_by_one_col_name,sep = " {~@~} ")|>
      convert_columns_to_factors(database_name) |>
      #use count to tabulate
      tabyl(!!renamed_variable_to_tabulate_by_one_col_name, 
            show_missing_levels =TRUE,
            show_na=TRUE)|>
      adorn_totals('row') |>
      adorn_pct_formatting()|>
      select(-any_of("valid_percent")) # remove additional percent column
    
  } else if ( length(variables_to_tabulate_by_list)==2){
    
    # extract the variables from list of variables
    variable_to_tabulate_by_one<- unlist(variables_to_tabulate_by_list)[[1]]
    variable_to_tabulate_by_two<- unlist(variables_to_tabulate_by_list)[[2]]
    
    #convert the variables which data will be tabulated to a more human readable name, using lookup
    renamed_variable_to_tabulate_by_one<-names(which(rename_lookup[[database_name]]==variable_to_tabulate_by_one))
    renamed_variable_to_tabulate_by_two<-names(which(rename_lookup[[database_name]]==variable_to_tabulate_by_two))
    if (length(renamed_variable_to_tabulate_by_one)==0){
      message(str_glue("{variable_to_tabulate_by_one} does not exist. Table cannot be created. "))
      return(tibble(`Table could not be made`= str_glue("{variable_to_tabulate_by_one} doesn't exist.")))
    }
    if (length(renamed_variable_to_tabulate_by_two)==0){
      message(str_glue("{renamed_variable_to_tabulate_by_two} does not exist. Table cannot be created. "))
      return(tibble(`Table could not be made`= str_glue("{variable_to_tabulate_by_two} doesn't exist.")))
    }
    
    #allow the variable to be used as a column name
    renamed_variable_to_tabulate_by_one_col_name<- sym(renamed_variable_to_tabulate_by_one) 
    renamed_variable_to_tabulate_by_two_col_name<- sym(renamed_variable_to_tabulate_by_two) 
    
    summary_table <- df_to_create_summary_table |>
      #seperate multi select values
      separate_rows(!!renamed_variable_to_tabulate_by_one_col_name,sep = " {~@~} ") |>
      separate_rows(!!renamed_variable_to_tabulate_by_two_col_name,sep = " {~@~} ") |>
      convert_columns_to_factors(database_name) |> 
      # use count to get a table
      tabyl(!!renamed_variable_to_tabulate_by_one_col_name,
            !!renamed_variable_to_tabulate_by_two_col_name,
            show_missing_levels =TRUE,
            show_na=TRUE)|> 
      rename(any_of(c(`Not available`= "NA_"))) |>
      #add row and column totals
      adorn_totals('both')
  } else {
    message(str_glue("TOO MANY VARIABLES INCLUDED FOR {database_name}"))
    message(deparse(variables_to_tabulate_by_list))
    return(tibble(`Table could not be made`= "Too many variables"))
  }
  
  return(summary_table)
  
}

#function to convert month and level of harm columns to factors (depending on database)
convert_columns_to_factors<-function(df_without_factors, database_name){

  #convert month and harm level to ordered factors 
  if (database_name=="LFPSE"){
    #relevel factor of columns
    df_with_factors<- df_without_factors |>
      mutate(
        `Largest physical harm (across all patients in incident)`= factor(
          `Largest physical harm (across all patients in incident)`, 
          levels= c("No physical harm", "Low physical harm",
                    "Moderate physical harm","Severe physical harm", "Fatal")),
        `Largest psychological harm (across all patients in incident)`= factor(
          `Largest psychological harm (across all patients in incident)`,
          levels= c("No psychological harm",
                    "Low psychological harm",
                    "Moderate psychological harm",
                    "Severe psychological harm")),
        `Month`= factor(`Month`, levels=month.abb),
        `Year` = factor(`Year`,
                        levels = sort(unique(`Year`))),
        `Month - Year` = factor(zoo::as.yearmon(`Month - Year`), 
                                levels = sort(unique(`Month - Year`)))
        )
    
  } else if (database_name=="NRLS"){
    df_with_factors<- df_without_factors |>
      mutate( `Month`= factor(`Month`, levels=month.abb),
              `Year` = factor(`Year`,
                              levels = sort(unique(`Year`))),
             `PD09 Degree of harm (severity)` = 
               factor(`PD09 Degree of harm (severity)`,
                      levels= c("No Harm", "Low", "Moderate", "Severe","Death")),
             `Month - Year` = factor(zoo::as.yearmon(`Month - Year`), 
                                     levels = sort(unique(`Month - Year`)))
      )
    
  } else if (database_name=="STEIS"){
    df_with_factors<- df_without_factors |>
      mutate( `Month`= factor(`Month`, levels=month.abb),
              `Year` = factor(`Year`,
                              levels = sort(unique(`Year`))),
             `Month - Year` = factor(zoo::as.yearmon(`Month - Year`), 
                                     levels = sort(unique(`Month - Year`)))
      )
  }else{
    print("database name not found")
  }

  return(df_with_factors)

}





#this function adds a summary table to a sheet
add_summary_table_to_sheet<- function(wb,
                                      sheet, 
                                      summary_table,
                                      table_start_row, 
                                      table_start_col){
  
  #add summary table to sheet
  writeData(wb, sheet, summary_table, startRow = table_start_row, startCol = table_start_col, keepNA = TRUE, na.string = "Not available")
    
  setColWidths(wb,
               sheet = sheet,
               cols = table_start_col:(table_start_col + ncol(summary_table) - 1 ),
               widths = 20)
  
  # style table - header 
  addStyle(
    wb,
    sheet = sheet,
    summaryTableTopBottomStyle,
    rows = table_start_row,
    cols = table_start_col:(table_start_col + ncol(summary_table) - 1 )
  )
  
  # style table- row titles 
  addStyle(
    wb,
    sheet = sheet,
    rowTitleStyle,
    rows = (table_start_row + 1):(nrow(summary_table) + table_start_row - 1),
    cols = table_start_col
  )
  
  #style table - main body
  addStyle(
    wb,
    sheet = sheet,
    bodyStyleNoBorder,
    rows = (table_start_row + 1):(nrow(summary_table) + table_start_row - 1),
    cols = (table_start_col + 1):(table_start_col + ncol(summary_table) - 1 ),
    gridExpand = T
  )
  
  #style table- header and footer
  addStyle(
    wb,
    sheet = sheet,
    summaryTableTopBottomStyle,
    rows = nrow(summary_table) + table_start_row ,
    cols = table_start_col:(table_start_col + ncol(summary_table) - 1 ),
    gridExpand = T
  )
  

}

#this function adds a data table to a sheet and styles it 
add_data_table_to_sheet<- function(wb,
                                   sheet,
                                   data_table,
                                   table_start_row){
  
  # Write data
  writeData(wb, sheet, data_table, startRow = table_start_row)
  
  # set column widths
  setColWidths(wb,
               sheet = sheet,
               cols = 1:ncol(data_table),
               widths = 35)
  
  # set row heights - header row
  
  setRowHeights(wb,
                sheet = sheet,
                rows = table_start_row:table_start_row,
                heights = 34)
  
  # set row heights - body
  setRowHeights(wb,
                sheet = sheet,
                rows = (table_start_row + 1):(nrow(data_table) + table_start_row),
                heights = 150)
  
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
    rows = (table_start_row+1):(nrow(data_table) + table_start_row),
    cols = 1:ncol(data_table),
    gridExpand = T
  )

  
}


