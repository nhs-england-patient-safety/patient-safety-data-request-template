source("renv/activate.R")

# source data request pipeline
run_data_pipeline <- function(path = "params.qmd") {
  
  # confirm file exists
  stopifnot(file.exists(path))
  
  # create a temporary params script
  rfile <- tempfile(fileext = ".R")
  
  # check if temporary file exists and is older than quarto notebook
  if (!file.exists(rfile) || file.mtime(rfile) < file.mtime(path)) {
    
    # extract code chunks from notebook and create temporary script
    knitr::purl(input = path, output = rfile, documentation = 0)
  }
  
  # source temporary params script
  source(rfile)
}
