#function to find the minimum number in a vector, and return NA if all values are NA
min_safe<- function(vec){
  ifelse(length(vec[!is.na(vec)]) == 0, NA_real_, min(vec,na.rm=TRUE))
}

#' add summary sheet
#'
#'searches environment for summary_categories_lfpse, nrls or steis, depending on database_name
#'creates summary tables from the columns specified by this object
#'
#' @param wb workbook (workbook class)
#' @param i name of dataset (string)
#' @param title title of sheet
#' @param database_name name of database (i.e. lfpse, steis or nrls)
#' @param sheet sheet name
#'
#' @return workbook with summary tables added
add_summary_sheet <- function(wb, i, title, database_name, sheet) {
  
  summary_categories_list <- get(str_glue("summary_categories_{database_name}"))
  
  df <- get(i) #get dataset from environment
  # add extra columns to df, depending on database name - refactor harm levels
  if (database_name == "lfpse") {
    
    
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
  addWorksheet(wb, sheet, gridLines = FALSE) 
  
  # set column widths
  setColWidths(wb, sheet = sheet, cols = 1, widths = 50)
  
  #Add text style
  addStyle(wb, sheet = sheet,  textStyle, rows = 1:6, cols = 1:1)
  
  # Write title
  writeData( wb, 
             sheet, 
             paste(sheet, "Confidential", sep = " - "),
             startCol = 1,
             startRow = 1
  )
  
  #write subtitle
  writeData(wb, sheet, title, startCol = 1, startRow = 3)
  
  # write number of incidents
  writeData(
    wb,
    sheet,
    paste("Number of Incidents in sample", nrow(df), sep = ": "),
    startCol = 1,
    startRow = 5
  )
  
  # write number of incidents
  writeData(
    wb,
    sheet,
    paste("Note: These summary tables are counts of the data after sampling has occured", nrow(df), sep = ": "),
    startCol = 1,
    startRow = 6
  )
  
  
  #set start row for summary tables
  start_row = 8
  
  # loop through list- each item of list is one table
  for (category in summary_categories_list) {
    #work out if table has one or 2 variables
    if (length(category) == 1) {
      one_variable = TRUE
      category[[2]] = category[[1]] #to allow code to work the same for 1 or 2 categories, add a category with the same column as the first
    } else{
      one_variable = FALSE
    }
    
    #work out counts
    summary_table <- df %>%
      count(!!category[[1]], !!category[[2]])
    
    # if 2 variables add row and column totals
    if (!one_variable) {
      summary_table <- summary_table %>%
        pivot_wider(names_from = !!category[[2]],
                    values_from = n) %>%
        adorn_totals('both')
      
    } else{
      # if one variable, add row totals and percentage column
      summary_table <- summary_table %>%
        mutate(percent = scales::percent(n / sum(n))) %>%
        adorn_totals('row')
    }
    
    print(summary_table)
    #add summary table to sheet
    writeData(wb, sheet, summary_table, startRow = start_row)
    
    # style table - header
    addStyle(
      wb,
      sheet = sheet,
      headerStyle,
      rows = start_row,
      cols = 1:(ncol(summary_table))
    )
    # style table- row titles
    addStyle(
      wb,
      sheet = sheet,
      rowTitleStyle,
      rows = (start_row + 1):(nrow(summary_table) + start_row),
      cols = 1
    )
    #style table - main body
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
    # increment start row to allow next table to be further down on page
    start_row <- start_row + nrow(summary_table) + 3
    
  }
  return(wb)
}

#' add data sheet
#' 
#' add data to sheet and style it
#'
#' @param wb workbook (workbook class)
#' @param i name of dataset (string)
#' @param title title of sheet
#' @param sheet sheet name
#'
#' @return workbook with data added
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