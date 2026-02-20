if (write_to_sp) {
  
  library(Microsoft365R)
  
  # write to sharepoint
  message('Saving to Sharepoint...')
  
  site_url <- "https://nhs.sharepoint.com/sites/MED/ps2/it/mit"
  
  site <- suppressMessages(get_sharepoint_site(site_url = site_url, tenant="nhs"))
  
  # Write to a folder within Restricted Library / Data Requests
  
  reslib <- site$get_drive("Restricted Library")
  
  dr <- reslib$get_item("Data Requests")
  
  dr$upload(
    src = tf,
    dest = str_glue('{title}/{workbook_title}')
  )
  
  message(str_glue('Workbook saved at https://nhs.sharepoint.com/sites/MED/ps2/it/mit/ResLib/Data%20Requests/{title}'))
  
  toc <- Sys.time()
  
  time_diff <- (toc-tic)
  
  message(str_glue("Request completion time: {round(time_diff[[1]], 2)} {attr(time_diff, 'units')}"))
}
