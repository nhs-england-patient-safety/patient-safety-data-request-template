# flow
tic <- Sys.time()

if (search_nrls) {
  source("nrls.R")
} else if (search_lfpse) {
  source("lfpse.R")
} else if (search_steis) {
  source("steis.R")
} else {
  source("formatter.R")
}
