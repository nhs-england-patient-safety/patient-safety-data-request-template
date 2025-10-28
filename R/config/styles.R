#This script stores the styles that we use to create the excel output file in formatter.R

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


# Create header style
summaryTableTopBottomStyle<- createStyle(
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
bodyStyleNoBorder <- createStyle(
  fontSize = 11,
  fontName = "Arial",
  wrapText = TRUE,
  valign = "center",
  halign = "left",
  numFmt = "#,##0"
)

rowTitleStyle <- createStyle(
  fontSize = 11,
  fontName = "Arial",
  wrapText = TRUE,
  textDecoration = "bold",
  valign = "center",
  halign = "left"
)