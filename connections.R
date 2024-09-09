# connections

library(DBI)


if (search_nrls) {
  # nrls ####
  con_nrls <- dbConnect(odbc::odbc(),
                        Driver = "ODBC Driver 17 for SQL Server",
                        Server = Sys.getenv("psims_server"),
                        database = Sys.getenv("nrls_database"),
                        uid = Sys.getenv("nrls_uid"),
                        pwd = Sys.getenv("nrls_pwd")
                        )
  
  # reading categorical reference tables
  sascodes <- tbl(con_nrls, in_schema("art", "sascodes")) |>
    collect()
  
  codes_ex_rm04 <- sascodes |>
    # fixing in03 lvl1 which is incorrect in the table
    mutate(OPTIONTEXT = str_replace_all(OPTIONTEXT, "Treatment  ", "Treatment ")) |>
    group_by(REFERENCECODE) |>
    mutate(
      n_levels = max(DEPTH) + 1,
      col_name = ifelse(n_levels > 1,
                        paste0(REFERENCECODE, "_LVL", DEPTH + 1),
                        REFERENCECODE
      )
    ) |>
    ungroup() |>
    distinct(col_name, SASCODE, OPTIONTEXT) |>
    # removing these vals which aren't required
    filter(!col_name %in% c(
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
  
  codes <- bind_rows(codes_ex_rm04, codes_rm04)
  
  } else if (search_lfpse) {
    # lfpse ####
    
    con_lfpse <- dbConnect(odbc::odbc(),
                           Driver = "ODBC Driver 17 for SQL Server",
                           Server = Sys.getenv("psims_server"),
                           database = Sys.getenv("lfpse_database"),
                           uid = Sys.getenv("lfpse_uid"),
                           pwd = Sys.getenv("lfpse_pwd")
    )
    
    # reading categorical reference tables
    QuestionReference <- tbl(con_lfpse, "QuestionReference") |> collect()
    ResponseReference <- tbl(con_lfpse, "ResponseReference") |> collect()
    }



