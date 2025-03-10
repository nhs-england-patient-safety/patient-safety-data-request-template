library(testthat)

## Connections.R
# test: table retrieval of database search functions
test_that(
  "NRLS reference tables load correctly", {
    if (search_nrls) {
      expect_gt(nrow(sascodes), 0) #ensure table isn't empty (#rows >0)
      expect_true(all(c("REFERENCECODE", "SASCODE", "OPTIONTEXT") %in% colnames(sascodes)))
    }
  }
)

test_that(
  "LFPSE reference tables load correctly", {
    if (search_lfpse) {
      expect_gt(nrow(QuestionReference), 0) #ensure table isn't empty (#rows >0)
      expect_gt(nrow(ResponseReference), 0) #ensure table isn't empty (#rows >0)
    }
  }
)

#test: creation of codes table as expected
test_that(
  "Codes table is processed correctly", {
    if(search_nrls) {
      expect_true(all(c("col_name", "SASCODE", "OPTIONTEXT") %in% colnames(codes)))
      expect_false(any(is.na(codes$col_name)))
    }
  }
 )

##Flow
#test: if date_type doesn't equal "reported" or "occurring" (i.e. NA?) -> expected outcome

#test: if a script doesn't exist, an error message appears
test_that("Handles missing scripts gracefully", {  
    search_nrls <- TRUE  
    file.rename("nrls.R", "nrls_backup.R")  # Temporarily rename file to simulate missing script   
    expect_error(source("flow.R"), "cannot open the connection")
    file.rename("nrls_backup.R", "nrls.R") # Restore script
  }
)

##Formatter
#test: the data_sheet_name is in the workbook created and is not empty
test_that("data_sheet_name exists and is not empty in the generated workbook", {
  expect_true(exists("wb"), info = "The workbook object 'wb' should exist")
  sheets <- getSheetNames(wb)
  expect_true(data_sheet_name %in% sheets, info = paste("Sheet", data_sheet_name, "should exist in the workbook"))
  loaded_data <- read.xlsx(wb, sheet = data_sheet_name)
  expect_gt(nrow(loaded_data), 0, info = paste("Sheet", data_sheet_name, "should contain data"))
})