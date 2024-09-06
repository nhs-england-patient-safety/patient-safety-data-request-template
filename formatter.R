# Formatting Excel workbook
print('Formatting Excel workbook...')

file_list <- apropos('for_release')

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

ref_no<- str_extract(title, "Ref-[0-9]{4,4}")


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

text_terms_pretty <- text_terms |>
  str_replace_all(pattern = "\\|", " OR ") |>
  str_replace_all(pattern = "\\\\b", "%") |>
  str_replace_all(pattern = "\\(\\?i\\)", "")



metadata_answers <- c(
  ref_no,
  "",
  datasets_used,
  "",
  extraction_date,
  "",
  date_range,
  "",
  nrls_full_string,
  "",
  steis_full_string,
  "",
  lfpse_full_string,
  "",
  "Free text search based the following terms (including misspellings and variations):",
  text_terms_pretty,
  "",
  "Notes:",
  "'%' represents a boundary",
  "'~' represents an optional space that can be filled by any character"
)

addStyle(wb, "Search strategy", textStyle, rows = 2:24, cols = 2)
addStyle(wb, "Search strategy", textStyle, rows = 16, cols = 5)
addStyle(wb, "Search strategy", textStyle, rows = length(metadata_answers) - 1, cols = 5)
writeData(wb, "Search strategy", metadata, startRow = 2, startCol = 2)
writeData(wb, "Search strategy", metadata_answers, startRow = 2, startCol = 5)

# Add worksheets

for (i in file_list) {
  
  sheet <- str_extract(i, "^([^_])+") |> 
    toupper() |> 
    str_replace("STEIS", "StEIS") 
  
  df <- get(i)
  
  addWorksheet(wb, sheet, gridLines = FALSE)
  
  # set column widths
  setColWidths(wb,
               sheet = sheet,
               cols = 1:ncol(df),
               widths = 35
  )
  
  # set row heights - header row
  
  setRowHeights(wb,
                sheet = sheet,
                rows = 7:7,
                heights = 34
  )
  
  # set row heights - body
  setRowHeights(wb,
                sheet = sheet,
                rows = 8:(nrow(df) + 7),
                heights = 150
  )
  
  
  # Add text style
  addStyle(wb,
           sheet = sheet,
           textStyle,
           rows = 1:6,
           cols = 1:1
  )
  
  # Add header style
  addStyle(wb,
           sheet = sheet,
           headerStyle,
           rows = 7,
           cols = 1:ncol(df)
  )
  
  # Add body style
  addStyle(wb,
           sheet = sheet,
           bodyStyle,
           rows = 8:(nrow(df) + 7),
           cols = 1:ncol(df),
           gridExpand = T
  )
  
  # Write text
  writeData(wb, sheet, paste(sheet, "Confidential", sep = " - "), startCol = 1, startRow = 1)
  writeData(wb, sheet, title, startCol = 1, startRow = 3)
  writeData(wb, sheet, paste("Number of Incidents", nrow(df), sep = ": "), startCol = 1, startRow = 5)
  
  # Write data
  writeData(wb, sheet, df, startRow = 7)
  
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
