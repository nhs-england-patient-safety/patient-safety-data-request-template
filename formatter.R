file_list <- apropos('for_release_incident_level')

#there's no need to carry on if there are no objects for release
if(is_empty(file_list)){
  stop("There's no data to write in Excel")
}

# Formatting Excel workbook
print('Formatting Excel workbook...')

# Create a new workbook
wb <- createWorkbook()

# Create text style
textStyle <- createStyle(
  fontSize = 11,
  fontName = "Arial",
  textDecoration = "bold",
  valign = "center",
  halign = "left"
)

# Create header style
headerStyle <- createStyle(
  fontSize = 11,
  fontName = "Arial",
  border = "TopBottomLeftRight",
  borderStyle = "thick",
  wrapText = TRUE,
  textDecoration = "bold",
  valign = "center",
  halign = "left"
)

# Create body style
bodyStyle <- createStyle(
  fontSize = 11,
  fontName = "Arial",
  border = "TopBottomLeftRight",
  borderStyle = "thin",
  wrapText = TRUE,
  valign = "center",
  halign = "left"
)

rowTitleStyle <- createStyle(
  fontSize = 11,
  fontName = "Arial",
  border = "TopBottomLeftRight",
  borderStyle = "thin",
  wrapText = TRUE,
  textDecoration = "bold",
  valign = "center",
  halign = "left"
)
title <- basename(here())

# To do - cover sheet
# include summary tables - one for each dataset or one with all?
addWorksheet(wb, "Search strategy", gridLines = FALSE)

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
  "Free text filters:"
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

metadata_answers <- c(
  ref_no,
  "",
  datasets_used,
  "",
  extraction_date,
  "",
  date_range,
  "",
  deparse(nrls_categorical,width.cutoff = 500),
  "",
  deparse(steis_categorical, width.cutoff = 500),
  "",
  deparse(lfpse_categorical, width.cutoff = 500),
  "",
  deparse(text_terms,width.cutoff = 500),
  deparse(text_filter, width.cutoff = 500)
)

addStyle(wb, "Search strategy", textStyle, rows = 2:24, cols = 2)
writeData(wb, "Search strategy", metadata, startRow = 2, startCol = 2)
writeData(wb, "Search strategy", metadata_answers, startRow = 2, startCol = 5)

# Add worksheets

for (i in file_list) {
  
  database_name <- toupper(str_split_i(i, "_", 1))

  sheet_base_name <- str_extract(i, "^([^_])+") |> 
    toupper() |> 
    str_replace("STEIS", "StEIS") 

    wb<-add_summary_sheet(wb, title, database_name, str_glue("{sheet_base_name} - Summary"))
    
    if (incident_level_required=="yes"){
      wb<-add_data_sheet(wb, title, database_name, str_glue("{sheet_base_name} - Data"))
    }else{
      print("incident_level_required not valid. Only the summary sheet has been added.")
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

source('microsoft365R.R')