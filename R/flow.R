# flow
tic <- Sys.time()

  if (search_nrls) {
    source('R/processors/nrls.R')
  } else if (search_lfpse) {
    source('R/processors/lfpse.R')
  } else if (search_steis) {
    source('R/processors/steis.R')
  } else {
    source('R/output/formatter.R')
  }
