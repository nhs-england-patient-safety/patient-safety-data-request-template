# flow
tic <- Sys.time()


#not a function- but this creates the object "date_filter" from date_type
date_filter <- if (date_type == 'occurring') {
  expr(occurred_date)
} else if (date_type == 'reported') {
  expr(reported_date)
}




  if (search_nrls) {
    source('nrls.R')
  } else if (search_lfpse) {
    source('lfpse.R')
  } else if (search_steis) {
    source('steis.R')
  } else {
    source('formatter.R')
  }
