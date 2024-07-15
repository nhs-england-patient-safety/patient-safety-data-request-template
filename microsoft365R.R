# write to sharepoint
print('Saving to Sharepoint...')

site_url <- "https://nhsengland.sharepoint.com/sites/MED/ps2/it/mit"

site <- get_sharepoint_site(site_url = site_url)

# Write to a folder within Restricted Library / Data Requests

reslib <- site$get_drive("Restricted Library")

dr <- reslib$get_item("Data Requests")

dr$upload(
  src = tf,
  dest = glue('{title}/{workbook_title}')
)

print(glue('Workbook saved at https://nhsengland.sharepoint.com/:f:/r/sites/MED/ps2/it/mit/ResLib/Data%20Requests/{title}'))
toc <- Sys.time()
print(glue('Query time (s): {toc-tic}'))