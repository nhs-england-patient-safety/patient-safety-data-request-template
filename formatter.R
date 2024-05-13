# Formatting Excel workbooks

library(openxlsx)
library(here)
library(tidyverse)

file_list <- list.files(here("csv"), full.names = T)

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
  "Categorical criteria:",
  "",
  "Free text filters:"
)

addStyle(wb, "Search strategy", textStyle, rows = 2:24, cols = 2)
writeData(wb, "Search strategy", metadata, startRow = 2, startCol = 2)

# Add worksheets

for (i in file_list) {
  sheet <- tools::file_path_sans_ext(basename(i))

  df <- read_csv(i)
  
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

saveWorkbook(wb, here("output", paste(title,
  "output",
  format(Sys.time(), "%Y-%m-%d_%H%M%S.xlsx"),
  sep = "_"
)), overwrite = T)
