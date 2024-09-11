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


# download nrls colnames file, and save to data folder
site_url <- "https://nhsengland.sharepoint.com/sites/MED/ps2/it/mit"
site <- get_sharepoint_site(site_url = site_url, tenant = "nhsengland") # need to specify tenant, for it to work for non-nhse team members.
reslib <- site$get_drive("Restricted Library")
reslib$download_file("Data Requests/Copy of AF Metadata v2.1 Draft.xls",dest="Data/lookup_file.xls", overwrite = TRUE)

nrls_colname_lookup <- read_excel("data/lookup_file.xls", sheet="Datasets & Variables") %>%
  distinct(NAME, LABEL) %>%
  filter(NAME != LABEL) %>%
  filter(NAME != "", LABEL!="")
