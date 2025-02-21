#function to find the minimum number in a vector, and return NA if all values are NA
# This is required to find the minimum physical or psychological harm level
min_safe<- function(vec){
   ifelse(length(vec[!is.na(vec)]) == 0, NA_real_, min(vec,na.rm=TRUE))
 }


#' add summary sheet
#'
#'searches environment for list_of_tables_to_create_lfpse, nrls or steis, depending on database_name
#'creates summary tables from the columns specified by this object
#'
#' @param wb workbook (workbook class)
#' @param title title of sheet
#' @param database_name name of database (i.e. lfpse, steis or nrls)
#' @param sheet sheet name
#'
#' @return workbook with summary tables added
add_summary_sheet <- function(wb, title, database_name, sheet) {

  list_of_tables_to_create <- get(str_glue("list_of_tables_to_create_{tolower(database_name)}"))
  
  addWorksheet(wb, sheet, gridLines = FALSE) 
  
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
  
  #add caveats to lfpse tab
  if (database_name == "LFPSE"){
  
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
  
  #get data
  df_unsampled <- get(str_glue("{tolower(database_name)}_for_release_unsampled"))
  
  # write number of incidents
  writeData(
    wb,
    sheet,
    paste("Number of Incidents retrieved by search strategy", nrow(df_unsampled), sep = ": "),
    startCol = 1,
    startRow = content_start_row
  )
  
    
  #set start row for summary tables
  table_start_row = content_start_row + 2 
  
  #Add text style
  addStyle(wb, sheet = sheet,  textStyle, rows = 1:(table_start_row - 1), cols = 1)
  
  
  # loop through list- each item of list is one table
  for (variables_to_tabulate_by_list in list_of_tables_to_create) {
    
    summary_table<- create_summary_table(df_unsampled,variables_to_tabulate_by_list, database_name)

    #add summary table to sheet
    writeData(wb, sheet, summary_table, startRow = table_start_row)
    
    # style table - header 
    addStyle(
      wb,
      sheet = sheet,
      summaryTableTopBottomStyle,
      rows = table_start_row,
      cols = 1:ncol(summary_table)
    )
    
    # style table- row titles 
    addStyle(
      wb,
      sheet = sheet,
      rowTitleStyle,
      rows = (table_start_row + 1):(nrow(summary_table) + table_start_row - 1),
      cols = 1
    )
    #style table - main body
    addStyle(
      wb,
      sheet = sheet,
      bodyStyleNoBorder,
      rows = (table_start_row + 1):(nrow(summary_table) + table_start_row - 1),
      cols = 2:(ncol(summary_table)),
      gridExpand = T
    )
    
    addStyle(
      wb,
      sheet = sheet,
      summaryTableTopBottomStyle,
      rows = nrow(summary_table) + table_start_row ,
      cols = 1:(ncol(summary_table)),
      gridExpand = T
    )
    
    # increment start row to allow next table to be further down on page
    table_start_row <- table_start_row + nrow(summary_table) + 3
    
  }
  
  setColWidths(wb,
               sheet = sheet,
               cols = 1:15,
               widths = 20)
  
  return(wb)
}

#' add data sheet
#' 
#' add data to sheet and style it
#'
#' @param wb workbook (workbook class)
#' @param title title of sheet
#' @param database_name name of database (i.e. lfpse, steis or nrls)
#' @param sheet sheet name
#'
#' @return workbook with data added
add_data_sheet <- function(wb, title, database_name, sheet) {
  
  df_sampled <- get(str_glue("{tolower(database_name)}_for_release_sampled"))
  #get data
  df_unsampled <- get(str_glue("{tolower(database_name)}_for_release_unsampled"))
  
  addWorksheet(wb, sheet, gridLines = FALSE)
  
  # set column widths
  setColWidths(wb,
               sheet = sheet,
               cols = 1:ncol(df_sampled),
               widths = 35)
  
  # set row heights - header row
  
  setRowHeights(wb,
                sheet = sheet,
                rows = 8:8,
                heights = 34)
  
  # set row heights - body
  setRowHeights(wb,
                sheet = sheet,
                rows = 9:(nrow(df_sampled) + 8),
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
    rows = 8,
    cols = 1:ncol(df_sampled)
  )
  
  # Add body style
  addStyle(
    wb,
    sheet = sheet,
    bodyStyle,
    rows = 9:(nrow(df_sampled) + 8),
    cols = 1:ncol(df_sampled),
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
  
  
  # write number of incidents
  writeData(
    wb,
    sheet,
    paste("Number of Incidents retrieved by search strategy", nrow(df_unsampled), sep = ": "),
    startCol = 1,
    startRow = 5
  )
  
  if(database_name=="LFPSE"){
    number_of_incidents<- df_sampled %>% count(Reference) %>% nrow()
    number_of_patients<-df_sampled %>% count(Reference, `Patient no.`) %>% nrow()
    info_string<- paste0(str_glue("Number of Incidents in Sample: {number_of_incidents} ({number_of_patients} Patients )"))
    writeData(
      wb,
      sheet,
      x= info_string,
      startCol = 1,
      startRow = 6
      )
  }else{
    writeData(
      wb,
      sheet,
      paste("Number of Incidents in Sample", nrow(df_sampled), sep = ": "),
      startCol = 1,
      startRow = 6
    )
  }
  # Write data
  writeData(wb, sheet, df_sampled, startRow = 8)
  return(wb)
}

create_summary_table<-function(df_to_create_summary_table,
                               variables_to_tabulate_by_list, 
                               database_name){
  
  #work out if table needs to have one or 2 variables. 
  if (length(variables_to_tabulate_by_list) == 1) {
    
    # extract the variable from list of variables
    variable_to_tabulate_by_one<- unlist(variables_to_tabulate_by_list)[[1]]
    
    #convert the variable which data will be tabulated to a more human readable name, using lookup
    renamed_variable_to_tabulate_by_one<-names(which(rename_lookup[[database_name]]==variable_to_tabulate_by_one))
    
    #allow the variable to be used as a column name
    renamed_variable_to_tabulate_by_one<- sym(renamed_variable_to_tabulate_by_one) 
  
    summary_table <- df_to_create_summary_table |>
      #separate rows if there are multi select options present
      separate_rows(!!renamed_variable_to_tabulate_by_one,sep = " {~@~} ") |>
      #use count to tabulate
      count(!!renamed_variable_to_tabulate_by_one,.drop= FALSE)|>
      #  add row totals and percentage column
      mutate(percent = scales::percent(n / sum(n))) %>%
      adorn_totals('row')
    
    
  } else if ( length(variables_to_tabulate_by_list)==2){
    
    # extract the variables from list of variables
    variable_to_tabulate_by_one<- unlist(variables_to_tabulate_by_list)[[1]]
    variable_to_tabulate_by_two<- unlist(variables_to_tabulate_by_list)[[2]]
    
    #convert the variables which data will be tabulated to a more human readable name, using lookup
    renamed_variable_to_tabulate_by_one<-names(which(rename_lookup[[database_name]]==variable_to_tabulate_by_one))
    renamed_variable_to_tabulate_by_two<-names(which(rename_lookup[[database_name]]==variable_to_tabulate_by_two))
    
    #allow the variable to be used as a column name
    renamed_variable_to_tabulate_by_one<- sym(renamed_variable_to_tabulate_by_one) 
    renamed_variable_to_tabulate_by_two<- sym(renamed_variable_to_tabulate_by_two) 
    
    
    summary_table <- df_to_create_summary_table |>
      #separate rows if there are multi select options present
      separate_rows(!!renamed_variable_to_tabulate_by_one, sep = " {~@~} ") |>
      separate_rows(!!renamed_variable_to_tabulate_by_two, sep = " {~@~} ") |>
      # use count to get a table
      count(!!renamed_variable_to_tabulate_by_one,!!renamed_variable_to_tabulate_by_two,.drop= FALSE)%>% 
      #pivot so variable 2 is columns
      pivot_wider(names_from = !!renamed_variable_to_tabulate_by_two,
                  values_from = n) %>%
      #add row and column totals
      adorn_totals('both')
    
  } else{
    message("TOO MANY VARIABLES INCLUDED- ONLY THE FIRST TWO WILL BE USED")
  }
  
  return(summary_table)
  
}
