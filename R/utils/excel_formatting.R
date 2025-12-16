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
      note<- c( note, "Note: The data here has been aggregated for the patients within an incident, selecting the largest physical harm level across patients")
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