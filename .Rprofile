source("renv/activate.R")

run_data_request <- function(path = "params.qmd", echo = FALSE) {
  stopifnot(file.exists(path))
  rfile <- tempfile(fileext = ".R")
  
  if (!file.exists(rfile) || file.mtime(rfile) < file.mtime(path)) {
    knitr::purl(input = path, output = rfile, documentation = 0)
  }
  
  source(rfile, echo = echo)
}
