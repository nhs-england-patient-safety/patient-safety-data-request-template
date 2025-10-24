#This script stores the styles that we use to create the excel output file in formatter.R

# Create text style
textStyle <- openxlsx::createStyle(
  fontSize = 11,
  fontName = "Arial",
  textDecoration = "bold",
  valign = "center",
  halign = "left"
)

# Create header style
headerStyle <- openxlsx::createStyle(
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
bodyStyle <- openxlsx::createStyle(
  fontSize = 11,
  fontName = "Arial",
  border = "TopBottomLeftRight",
  borderStyle = "thin",
  wrapText = TRUE,
  valign = "center",
  halign = "left"
)


# Create header style
summaryTableTopBottomStyle<- openxlsx::createStyle(
  fontSize = 11,
  fontName = "Arial",
  border = "TopBottom",
  borderStyle = "thin",
  wrapText = TRUE,
  textDecoration = "bold",
  valign = "center",
  halign = "left",
  numFmt = "#,##0"
)


# Create body style
bodyStyleNoBorder <- openxlsx::createStyle(
  fontSize = 11,
  fontName = "Arial",
  wrapText = TRUE,
  valign = "center",
  halign = "left",
  numFmt = "#,##0"
)

rowTitleStyle <- openxlsx::createStyle(
  fontSize = 11,
  fontName = "Arial",
  wrapText = TRUE,
  textDecoration = "bold",
  valign = "center",
  halign = "left"
)