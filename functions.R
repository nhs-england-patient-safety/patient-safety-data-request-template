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
#' @param title title of sheet
#' @param database_name name of database (i.e. lfpse, steis or nrls)
#' @param sheet sheet name
#'
#' @return workbook with summary tables added
add_summary_sheet <- function(wb, title, database_name, sheet) {
  
  summary_categories_list <- get(str_glue("summary_categories_{database_name}"))
  
  df <- get(str_glue("{database_name}_for_release_for_summary"))
  
  addWorksheet(wb, sheet, gridLines = FALSE) 
  
  # set column widths
  setColWidths(wb, sheet = sheet, cols = 1, widths = 50)
  
  #Add text style
  addStyle(wb, sheet = sheet,  textStyle, rows = 1:7, cols = 1:1)
  
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
    paste("Number of Incidents", nrow(df), sep = ": "),
    startCol = 1,
    startRow = 5
  )
  
  note<- c("Note: The data here has been aggregated for the patients within an incident, selecting the largest harm level accross patients",
           "Note: Where a question can have multiple answers, these have been seperated out so will sum to a larger number than the number of incidents.")
  
  
  # write number of incidents
  writeData(
    wb,
    sheet,
    note,
    startCol = 1,
    startRow = 6,
  )
  
  
  #set start row for summary tables
  start_row = 9
  
  # loop through list- each item of list is one table
  for (category in summary_categories_list) {
    #work out if table has one or 2 variables
    if (length(category) == 1) {
      one_variable = TRUE
      category[[2]] = category[[1]] #to allow code to work the same for 1 or 2 categories, add a category with the same column as the first
    } else{
      one_variable = FALSE
    }
    
    #work out if there is multi-select options in category 1 or 2
    
    cat_1_multi <- df %>% mutate(cat_1_delim=str_detect(!!category[[1]], " \\{~@~\\} ")) %>% filter(cat_1_delim) %>% summarise(sum(cat_1_delim)>0) %>% pull()
    cat_2_multi <- df %>% mutate(cat_2_delim=str_detect(!!category[[1]], " \\{~@~\\} ")) %>% filter(cat_2_delim) %>% summarise(sum(cat_2_delim)>0) %>% pull()
    
    summary_table <- df
    
    #separate rows if there are multi select options present
    if (cat_1_multi){
      summary_table <- summary_table %>%
        separate_rows(!!category[[1]], sep = " {~@~} ") 
    }
    if (cat_2_multi){
      summary_table <- summary_table %>%
        separate_rows(!!category[[2]], sep = " {~@~} ") 
    }
    
    summary_table <- summary_table |>
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
      heights = 44
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
#' @param title title of sheet
#' @param database_name name of database (i.e. lfpse, steis or nrls)
#' @param sheet sheet name
#'
#' @return workbook with data added
add_data_sheet <- function(wb, title, database_name, sheet) {
  
  df <- get(str_glue("{database_name}_for_release_incident_level"))
  
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
    paste("Number of Incidents in sample", nrow(df), sep = ": "),
    startCol = 1,
    startRow = 5
  )
  
  # Write data
  writeData(wb, sheet, df, startRow = 7)
  return(wb)
}