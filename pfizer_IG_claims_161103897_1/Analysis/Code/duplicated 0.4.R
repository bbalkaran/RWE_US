#########################################################
#  Script: 0.4_Resolve_NPI_duplicates.R
#  Purpose: NPI data from CMS
#  Data file: 
#  Author: Bridget Balkaran
#  Project: 161103897-1
#  Project Manager: Martine Maculaitis
#  Date Created: 11/6/19
#  Date Edited: 11/6/19
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
NPIs <- NPIregistry %>% filter(NPI %in% med$NPI) %>% na.omit() %>% distinct()

NPIs$provider_taxonomy_desc <- NPIs$provider_taxonomy_desc %>% 
  str_replace(pattern = "/ ", replacement = " ") %>%
  str_replace(pattern = "/", replacement = " ") %>%
  str_replace(pattern = "/", replacement = " ") %>%
  str_replace_all(pattern = " & ", replacement = " and ") %>%
  str_remove_all(pattern = ",") 



NPIs <- NPIs %>% distinct()

b <- NPIs %>% group_by(NPI) %>% filter(n()>1)
b$provider_taxonomy_code %>% n_distinct()
b$provider_taxonomy_desc %>% n_distinct()

# collapse these into broader categories 
b$provider_taxonomy_code %>% unique()
b$provider_taxonomy_desc %>% unique()

Collapse_these <- b %>% ungroup %>% select(-NPI, -`Entity Type Code`) %>% distinct()
colnames(Collapse_these) <-c("Provider Taxonomy Code", "Provider Taxonomy Description")

openxlsx::write.xlsx(Collapse_these, 
                     "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/NPI Taxonomy Descriptions to Collapse.xlsx")


NPIs <- NPIs %>% mutate(provider_taxonomy_desc = 
                  case_when(provider_taxonomy_code == "207L00000X" ~ "Allopathic and Osteopathic Physicians Anesthesiology",
                            provider_taxonomy_code == "2084P0800X" ~ "Allopathic and Osteopathic Physicians Psychiatry",
                            provider_taxonomy_code == "207RH0002X" ~ "Internal Medicine Hospice and Palliative Medicine",
                            provider_taxonomy_code == "2084A0401X" ~ "Allopathic and Osteopathic Physicians Psychiatry and Neurology",
                            provider_taxonomy_code == "207QH0002X" ~ 
                              "Allopathic and Osteopathic Physicians Family Medicine Hospice and Palliative Medicine",
                            provider_taxonomy_code == "2084H0002X" ~ 
                              "Allopathic and Osteopathic Physicians Psychiatry and Neurology Hospice and Palliative Medicine",
                            TRUE ~ provider_taxonomy_desc))

save(NPIs, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/NPI_registry_deduped.RData")

