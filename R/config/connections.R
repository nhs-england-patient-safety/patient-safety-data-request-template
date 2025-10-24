# connections

library(DBI)

# nrls ####
if (search_nrls) {
  con_nrls <- DBI::dbConnect(odbc::odbc(),
                        Driver = "ODBC Driver 17 for SQL Server",
                        Server = Sys.getenv("psims_server"),
                        database = Sys.getenv("nrls_database"),
                        uid = Sys.getenv("nrls_uid"),
                        pwd = Sys.getenv("nrls_pwd")
                        )
  
  # reading categorical reference tables
  sascodes <- dplyr::tbl(con_nrls, dbplyr::in_schema("art", "sascodes")) |>
    dplyr::collect()
  
  codes_ex_rm04 <- sascodes |>
    # fixing in03 lvl1 which is incorrect in the table
    dplyr::mutate(OPTIONTEXT = stringr::str_replace_all(OPTIONTEXT, "Treatment  ", "Treatment ")) |>
    dplyr::group_by(REFERENCECODE) |>
    dplyr::mutate(
      n_levels = max(DEPTH) + 1,
      col_name = ifelse(n_levels > 1,
                        paste0(REFERENCECODE, "_LVL", DEPTH + 1),
                        REFERENCECODE
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::distinct(col_name, SASCODE, OPTIONTEXT) |>
    # removing these vals which aren't required
    dplyr::filter(!col_name %in% c(
      "PD01-B",
      "DV08",
      "IN01-A-0",
      "IN02-A-01",
      "IN02-A-02",
      "DE10",
      "DE11",
      # these cols have duplicates but don't appear in the data
      "IN06",
      "MD03",
      "PD10_LVL1",
      "PD10_LVL2"
    ))
  
  codes_rm04 <- data.frame(
    col_name = rep("RM04", times = 4),
    SASCODE = c(3, 4, 8, 9, 10, 50, 51, 60),
    OPTIONTEXT = c("LRMS", "Comm - Pharmacy", "nww - eForm", "www - eForm", "OA - eForm", "GPOOH", "GP - eForm", "ASB")
  )
  
  codes <- dplyr::bind_rows(codes_ex_rm04, codes_rm04)
  
  
  # download nrls colnames file, and save to data folder
  site_url <- "https://nhs.sharepoint.com/sites/MED/ps2/it/mit"
  site <- Microsoft365R::get_sharepoint_site(site_url = site_url, tenant = "nhs")
  reslib <- site$get_drive("Restricted Library")
  reslib$download_file("NRLS/nrls lookup.xlsx",dest="Data/nrls_lookup_file.xlsx", overwrite = TRUE)
  
  nrls_lookup<- openxlsx::read.xlsx("Data/nrls_lookup_file.xlsx") |>
    dplyr::select(Code, Label)
  
  
  
  } 

# lfpse ####
if (search_lfpse) {
    
    con_lfpse <- DBI::dbConnect(odbc::odbc(),
                           Driver = "ODBC Driver 17 for SQL Server",
                           Server = Sys.getenv("psims_server"),
                           database = Sys.getenv("lfpse_database"),
                           uid = Sys.getenv("lfpse_uid"),
                           pwd = Sys.getenv("lfpse_pwd")
    )
    
    # reading categorical reference tables
    QuestionReference <- dplyr::tbl(con_lfpse, "QuestionReference") |> dplyr::collect()
    ResponseReference <- dplyr::tbl(con_lfpse, "ResponseReference") |> dplyr::collect()
    }



