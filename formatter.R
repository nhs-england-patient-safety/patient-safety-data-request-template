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
  str_replace_all(pattern = "\\(\\?i\\)", "") |>
  str_replace_all(pattern =  "\\(\\?:\\|\\\\W\\)", "~")



metadata_answers <- c(
  ref_no,
  "",
  datasets_used,
  "",
  extraction_date,
  "",
  date_range,
  "",
  get0("nrls_full_string", ifnotfound = NA),
  "",
  get0("steis_full_string", ifnotfound = NA),
  "",
  get0("lfpse_full_string", ifnotfound = NA),
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
  database_name <- str_split_i(i, "_", 1)
  if (type_of_output =="data"){
    wb<-add_data_sheet(wb, i, title)
  }else{
    wb<-add_summary_sheet(wb, i, title, database_name)
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
