#function to find the minimum number in a vector, and return NA if all values are NA
# This is required to find the minimum physical or psychological harm level
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

  summary_categories_list <- get(str_glue("summary_categories_{tolower(database_name)}"))
  
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
  for (table_variables in summary_categories_list) {

    #work out if table has one or 2 variables. 
    if (length(table_variables) == 1) {
      
      variable_one<-sym(names(which(rename_lookup[[database_name]]==unlist(table_variables)[[1]])))
      
      df_for_summary<- df_unsampled 
      
      cat_1_multi <- df_for_summary %>% 
        mutate(cat_1_delim=str_detect(!!variable_one, " \\{~@~\\} ")) %>% 
        filter(cat_1_delim) %>% 
        summarise(sum(cat_1_delim)>0) %>%
        pull()
    
      #separate rows if there are multi select options present
      if (cat_1_multi){
        df_for_summary <- df_for_summary %>%
          separate_rows(!!variable_one,sep = " {~@~} ") 
      }
      
      summary_table <- df_for_summary |>
          count(!!variable_one,.drop= FALSE)|>
        #  add row totals and percentage column
          mutate(percent = scales::percent(n / sum(n))) %>%
          adorn_totals('row')
      
      
      } else if ( length(table_variables)==2){

        variable_one<-sym(names(which(rename_lookup[[database_name]]==unlist(table_variables)[[1]])))
        variable_two<-sym(names(which(rename_lookup[[database_name]]==unlist(table_variables)[[2]])))
        
        df_for_summary<- df_unsampled 
        #work out if there is multi-select options in table_variables 1 or 2
        
        cat_1_multi <- df_for_summary %>% 
          mutate(cat_1_delim=str_detect(!!variable_one, " \\{~@~\\} ")) %>% 
          filter(cat_1_delim) %>% 
          summarise(sum(cat_1_delim)>0) %>%
          pull()
        cat_2_multi <- df_for_summary %>% 
          mutate(cat_2_delim=str_detect(!!variable_two, " \\{~@~\\} ")) %>%
          filter(cat_2_delim) %>% 
          summarise(sum(cat_2_delim)>0) %>% 
          pull()
        
        #separate rows if there are multi select options present
        if (cat_1_multi){
          df_for_summary <- df_for_summary %>%
            separate_rows(!!variable_one,sep = " {~@~} ") 
        }
        if (cat_2_multi){
          df_for_summary <- df_for_summary %>%
            separate_rows(!!variable_one, sep = " {~@~} ") 
        }
        
        summary_table <- df_for_summary |>
          count(!!variable_one,!!variable_two,.drop= FALSE)%>% 
          pivot_wider(names_from = !!variable_two,
                      values_from = n) %>%
          adorn_totals('both')
      
    } else{
       message("TOO MANY VARIABLES INCLUDED- ONLY THE FIRST TWO WILL BE USED")
      }
    

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