# connections

library(DBI)

# nrls ####

con_nrls <- dbConnect(odbc::odbc(),
                      Driver = "ODBC Driver 17 for SQL Server",
                      Server = Sys.getenv("psims_server"),
                      database = Sys.getenv("nrls_database"),
                      uid = Sys.getenv("nrls_uid"),
                      pwd = Sys.getenv("nrls_pwd")
)

# lfpse ####

con_lfpse <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("psims_server"),
                       database = Sys.getenv("lfpse_database"),
                       uid = Sys.getenv("lfpse_uid"),
                       pwd = Sys.getenv("lfpse_pwd")
)