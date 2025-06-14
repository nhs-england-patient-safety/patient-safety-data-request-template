file_list <- apropos('for_release_unsampled_incident_level')

#there's no need to carry on if there are no objects for release
if(is_empty(file_list)){
  stop("There's no data to write in Excel")
}

# Formatting Excel workbook
message('Formatting Excel workbook...')


# Create a new workbook
wb <- createWorkbook()

title <- basename(here())


# Create cover sheet ------------------------------------------------------

cover_sheet_name <- "Search strategy"

# To do - cover sheet
addWorksheet(wb, cover_sheet_name , gridLines = FALSE)

metadata <- c(
  "Reference:",
  "",
  "Datasets used:",
  "",
  "Extraction date:",
  "",
  "Date range:",
  "",
  "NRLS categorical criteria:",
  "",
  "StEIS categorical criteria:",
  "",
  "LFPSE categorical criteria:",
  "",
  "Sampling strategy:",
  "",
  "Free text filters:",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "Neonate or paediatric filter:",
  ""
)

ref_no <- substr(title, 5, 8)

datasets_used <- file_list |>
  str_extract("^([^_])+") |> 
  toupper() |> 
  str_replace("STEIS", "StEIS") |> 
  paste(collapse = "; ")

extraction_date <- format(Sys.Date(), "%d-%b-%y")

date_type_text <- 
  if(date_type == 'reported'){
    'reported'
  } else if (date_type == 'occurring'){
    'reported as occurring'
  }

date_range <- glue('Incidents {date_type_text} between {format(as.Date(start_date), "%d-%b-%y")} and {format(as.Date(end_date), "%d-%b-%y")}')


if(sum(!is.na(text_terms)) > 0) {
  text_terms_pretty <- text_terms
  for (group in 1:length(text_terms)){
    for (term in 1:length(text_terms[[group]])){
      prettier_term <- make_text_terms_pretty(text_terms[[group]][term])
      text_terms_pretty[[group]][term] <- prettier_term
    }
  }
} else  {
  text_terms_pretty <- "No text filters"
}

metadata_answers <- c(
  ref_no,
  "",
  datasets_used,
  "",
  extraction_date,
  "",
  date_range,
  "",
  expanded_categorical_filter_nrls,
  "",
  expanded_categorical_filter_steis,
  "",
  expanded_categorical_filter_lfpse,
  "",
  deparse(sampling_strategy),
  "",
  "Free text search based the following terms (case insensitive):",
  deparse(text_terms_pretty,width.cutoff = 500),
  deparse(text_filter, width.cutoff = 500),
  "",
  "Notes:",
  "'%' represents a boundary",
  "'~' represents an optional space that can be filled by any character",
  "",
  is_neopaed)

addStyle(wb, "Search strategy", textStyle, rows = 2:50, cols = 2)
addStyle(wb, "Search strategy", textStyle, rows = 18, cols = 5)
addStyle(wb, "Search strategy", textStyle, rows = 22, cols = 5)
writeData(wb, "Search strategy", metadata, startRow = 2, startCol = 2)
writeData(wb, "Search strategy", metadata_answers, startRow = 2, startCol = 5)


neopaed_logic<-c()
or_vector<-c()

if (is_neopaed != "none"){
  writeData(wb, "Search strategy", "Neopaed logic:", startRow = 28, startCol = 2)
  
}

if (is_neopaed %in% c("either","neonate")){

  neopaed_logic<-
    c(neopaed_logic,
      "Neonate logic:",
      "NRLS:",
      str_glue("Neonate by age: Age is between 0 and 28 days"),
      str_glue("Neonate by specialty: Specialty is Neonatology"),
      str_glue("Neonate by text: Specialty is 'Obstetrics and gynaecology' or PD04 is 'A paediatrics specialty' or PD20 is 'Yes' and text contains {make_text_terms_pretty(neonatal_terms)}"),
      "",
      "LFPSE:",
      str_glue("Neonate by age: Age (or age category) is between 0 and 28 days"),
      str_glue("Neonate by specialty: Specialty contains {make_text_terms_pretty(neonatal_specialty_terms)}"),
      str_glue("Neonate by text:  Specialty does not contain {make_text_terms_pretty(adult_specialty_terms)} and text contains {make_text_terms_pretty(neonatal_terms)}"),
      "",
      ""
      )
  
  or_vector<- 
    c(or_vector,
      "",
      "",
      "",
      "OR",
      "OR",
      "",
      "",
      "",
      "OR",
      "OR",
      "",
      "")
  
}

if (is_neopaed %in% c("either","paed")){
  neopaed_logic<-
    c(neopaed_logic,   
      "Paediatric logic:",
      "NRLS:",
      str_glue("Paediatric by age: Age is between 28 days and 18 years"),
      str_glue("Paediatric by specialty: Specialty is 'Child and adolescent mental health' (with unknown age), 'Community paediatrics' or 'Paedodontics'"),
      str_glue("Paediatric by text: PD04 is 'A paediatrics specialty' or PD20 is 'Yes' and text contains {make_text_terms_pretty(paediatric_terms)}"),
      "",
      "LFPSE:",
      str_glue("Paediatric by age: Age (or age category) is  between 28 days and 18 years"),
      str_glue("Paediatric by specialty: Specialty contains {make_text_terms_pretty(paediatric_specialty_terms)}"),
      str_glue("Paediatric by text:  Specialty does not contain {make_text_terms_pretty(adult_specialty_terms)} and text contains {make_text_terms_pretty(paediatric_terms)}")
)

  
  or_vector<- 
    c(or_vector,
      "",
      "",
      "",
      "OR",
      "OR",
      "",
      "",
      "",
      "OR",
      "OR")
  
}


writeData(wb, "Search strategy", neopaed_logic, startRow = 28, startCol = 5)
writeData(wb, "Search strategy", or_vector, startRow = 28, startCol = 4)

addStyle(wb, "Search strategy", textStyle, rows = 28, cols = 5)
addStyle(wb, "Search strategy", textStyle, rows = 40, cols = 5)
addStyle(wb, "Search strategy", createStyle(halign = 'right'), rows=1:50, cols=4)





# Add worksheets ----------------------------------------------------------

#Loop through each of the databases (using the "for_release_unsampled_incident_level" objects)
# We will create a sheet with the summary tables for this database
# If required, also add an incident data sheet
# Each sheet will have a header section, created by calling add_header_section()
# add_summary_table_to_sheet() or add_data_table_to_sheet() will add data or summary tables to the sheet as needed
for (i in file_list) {

  #get database name from i
  database_name <- toupper(str_split_i(i, "_", 1))

  # same as database name apart from capitals in StEIS
  sheet_base_name <- database_name |> 
    str_replace("STEIS", "StEIS") 

  
  ## CREATE SUMMARY SHEET
    
  #create sheet name using sheet base name 
  summary_sheet_name <-  str_glue("{sheet_base_name} - Summary")

  #use the database name to get the data required to make summary tables (both sampled and unsampled)
  df_unsampled_incident_level <- get(str_glue("{tolower(database_name)}_for_release_unsampled_incident_level"))
  df_sampled_incident_level <- get(str_glue("{tolower(database_name)}_for_release_sampled_incident_level"))

  #use the database name to get the list of tables to create (created in params file)
  #We use incident level for the summary tables. We have aggregated or removed the patient level incidents to allow us to count by incident.
  list_of_tables_to_create <- get(str_glue("list_of_tables_to_create_{tolower(database_name)}"))
    
  #add a worksheet for the summary tables  
  addWorksheet(wb, 
               sheet = summary_sheet_name, 
               gridLines = FALSE) 
    
  #add a header section to the sheet. this adds the header section to the workbook and returns the row number where the data should begin
  table_start_row<- add_header_to_sheet(wb, 
                                        title, 
                                        database_name, 
                                        sheet = summary_sheet_name,
                                        summary_sheet= TRUE,
                                        number_of_rows_sampled = nrow(df_sampled_incident_level),
                                        number_of_rows_unsampled = nrow(df_unsampled_incident_level))
    
    # loop through list, to create required tables, and add them to the sheet
    for (variables_to_tabulate_by_list in list_of_tables_to_create) {
      
      #create summary table for given variables (unsampled data)
      summary_table_unsampled<- create_summary_table(df_unsampled_incident_level,
                                                     variables_to_tabulate_by_list, 
                                                     database_name)
      
      #add headers for sampled/unsampled tables where sampling took place
      if (nrow(df_unsampled_incident_level)!=nrow(df_sampled_incident_level)){
        table_start_row <- add_text_to_summary_sheets(wb, 
                                                      sheet = summary_sheet_name,
                                                      content_start_row = table_start_row,
                                                      text_to_add = "sampled_table_headers")
      }

      #add this summary table to the sheet, and adds styling
      add_summary_table_to_sheet(wb,
                                 sheet = summary_sheet_name, 
                                 summary_table_unsampled,
                                 table_start_row,
                                 table_start_col = 1)
      
      
      #if the sampled and unsampled data have different lengths, then create and add a summary table for the sampled data
      if (nrow(df_unsampled_incident_level)!=nrow(df_sampled_incident_level)){
      
        message("Data has been sampled. Printing summary tables for both sampled and unsampled data")
        
        #create summary table for given variables (sampled data)
        summary_table_sampled<- create_summary_table(df_sampled_incident_level,
                                                     variables_to_tabulate_by_list, 
                                                     database_name)
        
        #add this summary table to the sheet, and adds styling
        add_summary_table_to_sheet(wb,
                                   sheet = summary_sheet_name,
                                   summary_table_sampled, 
                                   table_start_row,
                                   #this is printed to the right of the unsampled dataframe
                                   table_start_col = ncol(summary_table_unsampled)+2)
      }
      
      # increment start row to allow next table to be further down on page
      table_start_row <- table_start_row + nrow(summary_table_unsampled) + 3
      
    }
  
  # create group and text term tally tables if there are text terms in search strategy and this is set to do so in params
  if(sum(!is.na(text_terms)) > 0 && include_term_tally_table == "yes"){
    
    # header for the group/term tally table
    table_start_row <- add_text_to_summary_sheets(wb, 
                                                  sheet = summary_sheet_name,
                                                  content_start_row = table_start_row,
                                                  text_to_add = "term_tally_table_heading"
                                                  )
    
    # create unsampled group tally table
    group_tally_table_unsampled <- create_term_tally_table(df_unsampled_incident_level, 
                                                           cols_to_use = "group_columns")
    
    #add headers for sampled/unsampled tables if needed
    if (nrow(df_unsampled_incident_level)!=nrow(df_sampled_incident_level)){
      table_start_row <- add_text_to_summary_sheets(wb, 
                                                    sheet = summary_sheet_name,
                                                    content_start_row = table_start_row,
                                                    text_to_add = "sampled_table_headers")
    }
      
    # add this table to the summary sheet
    add_summary_table_to_sheet(wb,
                               sheet = summary_sheet_name,
                               group_tally_table_unsampled,
                               table_start_row,
                               table_start_col = 1,
                               Total_row = FALSE)
    
    #if the sampled and unsampled data have different lengths, then repeat for sampled data
    if (nrow(df_unsampled_incident_level)!=nrow(df_sampled_incident_level)){
      group_tally_table_sampled <- create_term_tally_table(df_sampled_incident_level, 
                                                           cols_to_use = "group_columns")
      add_summary_table_to_sheet(wb,
                                 sheet = summary_sheet_name,
                                 group_tally_table_sampled, 
                                 table_start_row,
                                 #this is printed to the right of the unsampled dataframe
                                 table_start_col = ncol(group_tally_table_unsampled)+3,
                                 Total_row = FALSE)
    }
    
    # increment start row to allow next table/content to be further down on page
    table_start_row <- table_start_row + nrow(group_tally_table_unsampled) + 3
    

    # create unsampled term tally table
    term_tally_table_unsampled <- create_term_tally_table(df_unsampled_incident_level, 
                                                          cols_to_use = "term_columns")
    
    # add this table to the summary sheet
    add_summary_table_to_sheet(wb,
                               sheet = summary_sheet_name,
                               term_tally_table_unsampled,
                               table_start_row,
                               table_start_col = 1,
                               Total_row = FALSE)
    
    #if the sampled and unsampled data have different lengths, then repeat sampled data
    if (nrow(df_unsampled_incident_level)!=nrow(df_sampled_incident_level)){
      term_tally_table_sampled <- create_term_tally_table(df_sampled_incident_level, 
                                                          cols_to_use = "term_columns")
      add_summary_table_to_sheet(wb,
                                 sheet = summary_sheet_name,
                                 term_tally_table_sampled, 
                                 table_start_row,
                                 #this is printed to the right of the unsampled dataframe
                                 table_start_col = ncol(term_tally_table_unsampled)+3,
                                 Total_row = FALSE)
    }
    
    # increment start row to allow next table/content to be further down on page
    table_start_row <- table_start_row + nrow(term_tally_table_unsampled) + 3
  }
  
    ## CREATE INCIDENT LEVEL DATA IF REQUIRED
  
    #if incident level data is required, then we add a sheet with this data  
    if (incident_level_required=="yes"){
      
      #We use patient level for the incidents. LFPSE contains patient level columns which we want to print.
      #use the database name to get the data required to make the data table (both sampled and unsampled)
      df_sampled_pt_level <- get(str_glue("{tolower(database_name)}_for_release_sampled_pt_level"))
      df_unsampled_pt_level <- get(str_glue("{tolower(database_name)}_for_release_unsampled_pt_level"))
      
      data_sheet_name<- str_glue("{sheet_base_name} - Data")
      
      #add a worksheet for the data     
      addWorksheet(wb, 
                   sheet = data_sheet_name, 
                   gridLines = FALSE)
      
      #add a header section to the sheet. this adds the header section to the workbook and returns the row number where the data should begin
      table_start_row<- add_header_to_sheet(wb, 
                                            title, 
                                            database_name, 
                                            sheet= data_sheet_name,
                                            summary_sheet = FALSE,
                                            number_of_rows_sampled = nrow(df_sampled_pt_level),
                                            number_of_rows_unsampled = nrow(df_unsampled_pt_level))
      
      #add this incident data to the sheet, and add styling
      add_data_table_to_sheet(wb, 
                              sheet = data_sheet_name,
                              data_table= df_sampled_pt_level, 
                              table_start_row = table_start_row)
      
    }else{
      message("incident_level_required not valid. Only the summary sheet has been added.")
  }
  
}




# set date formats

options(openxlsx.datetimeFormat = "dd-mmm-yyyy")
options(openxlsx.dateFormat = "dd-mmm-yyyy")

# save workbook

workbook_title <- paste(title,
                        "output",
                        format(Sys.time(), "%Y-%m-%d_%H%M%S.xlsx"),
                        sep = "_"
)

tf <- tempfile(fileext = ".xlsx")

saveWorkbook(wb, 
             file = tf,
             overwrite = T)

if (write_to_sp) {
  source('microsoft365R.R')
} else {
  saveWorkbook(wb, file=str_glue("output/{workbook_title}"))
}
