#########################################################
#  Script: 0.4_Resolve_NPI_duplicates.R
#  Purpose: NPI data from CMS
#  Data file: 
#  Author: Bridget Balkaran
#  Project: 161103897-1
#  Project Manager: Martine Maculaitis
#  Date Created: 9/12/19
#  Date Edited: 9/12/19
########################################################


library(tidyverse)


load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med Claims for Site Identification.RData")
load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/NPI_registry.RData")

# take billing npi first, then attending npi, then facility NPI

med <- med %>% mutate(NPI = case_when(is.na(billing_pr_npi) ~ attending_npi,
                                      is.na(billing_pr_npi) &
                                        is.na(attending_npi) ~ facility_npi,
                                      TRUE ~ billing_pr_npi))
med$NPI %>% is.na() %>% table()
NPIs <- NPIregistry %>% filter(NPI %in% med$NPI)

NPIs$provider_taxonomy_desc <- NPIs$provider_taxonomy_desc %>% 
  str_replace(pattern = "/ ", replacement = " ") %>%
  str_replace(pattern = "/", replacement = " ") %>%
  str_replace_all(pattern = " & ", replacement = " and ") 
