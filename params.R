library(tidyverse)
library(dbplyr)
library(janitor)
library(here)
library(openxlsx)
library(glue)
library(Microsoft365R)

# datasets to be searched (T/F)
search_nrls <- T
search_lfpse <- T
search_steis <- F

# connect to (relevant) data bases and bring corresponding look ups 
source("connections.R")

# date filter (type is occurring/reported)
start_date <- "2024-01-01"
end_date <- "2024-01-01"
date_type <- "occurring"

# TODO: cols to extract (all/default)
cols_to_extract <- "default"

# nrls categorical filters (wrap in expr() or set to 0)
nrls_categorical <- 0
# lfpse categorical filters (wrap in expr() or set to 0)
lfpse_categorical <- 0

# steis categorical filters (wrap in expr() or set to 0)
steis_categorical <- 0
steis_filename <- ''

# text terms
#example below- not real example
# text_terms <- list(group_A= c("(?i)\\bfall", "(?i)\\bslip", "(?i)\\bfall") ,
#                    group_B = c("(i)\\bslips trips and falls"),
#                    group_C= c("(?i)\\bwet floor"))
# text_filter <- expr((match_group_A | match_group_B) & match_group_C)
text_terms<- list()
text_filter<- expr(0)

# sampling strategy (default/FOI/none)
# TODO: custom
sampling_strategy <- "default"

source("flow.R")
