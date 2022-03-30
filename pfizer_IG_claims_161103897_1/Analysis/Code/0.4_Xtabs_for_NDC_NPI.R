#########################################################
#  Script: 0.4_Xtabs_for_NDC_NPI.R
#  Purpose: NPI data from CMS
#  Data file: 
#  Author: Bridget Balkaran
#  Project: 161103897-1
#  Project Manager: Martine Maculaitis
#  Date Created: 9/12/19
#  Date Edited: 9/12/19
########################################################

library(tidyverse)
library(readxl)
library(lubridate)
library(data.table)
library(magrittr)
library(multidplyr)
library(doParallel)
library(jsonlite)
library(descr)

load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/IG_NDC_NPI.RData")
load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/NPI_registry.RData")
NPI_xwalk <- fromJSON("https://data.cms.gov/resource/j75i-rw8y.json")%>% mutate_if(is.character, str_trim)

dat1 <- NPIregistry %>% select(NPI, `Entity Type Code`, provider_taxonomy_code)%>% rename(attending_npi = NPI) %>%
  right_join(IG_NDC2) %>% mutate_if(is.character,str_trim) %>%
  left_join(select(NPI_xwalk, medicare_specialty_code, medicare_prov_supplier_desc,
                   provider_taxonomy_code, provider_taxonomy_desc)) %>% unique() %>% 
  rename(attending_npi_taxonomy = provider_taxonomy_desc,
         attenting_npi_med_sp_desc = medicare_prov_supplier_desc)

CrossTable(dat1$NewPt_id, dat1$Visit_id)

table(dat1$attenting_npi_med_sp_desc)

table( dat1$attenting_npi_med_sp_desc, dat1$claim_type_code) %>% kable()



dat2 <- NPIregistry %>% select(NPI, `Entity Type Code`, provider_taxonomy_code)%>% rename(billing_pr_npi = NPI) %>%
  right_join(IG_NDC2) %>% mutate_if(is.character,str_trim) %>%
  left_join(select(NPI_xwalk, medicare_specialty_code, medicare_prov_supplier_desc,
                   provider_taxonomy_code, provider_taxonomy_desc)) %>% unique() %>% 
  rename(billing_npi_taxonomy = provider_taxonomy_desc,
         billing_npi_med_sp_desc = medicare_prov_supplier_desc)

table(dat2$billing_npi_med_sp_desc)

table(dat2$billing_npi_med_sp_desc, dat2$claim_type_code) 



dat3 <- NPIregistry %>% select(NPI, `Entity Type Code`, provider_taxonomy_code)%>% rename(facility_npi = NPI) %>%
  right_join(IG_NDC2) %>% mutate_if(is.character,str_trim) %>%
  left_join(select(NPI_xwalk, medicare_specialty_code, medicare_prov_supplier_desc,
                   provider_taxonomy_code, provider_taxonomy_desc)) %>% unique() %>% 
  rename(facility_npi_taxonomy = provider_taxonomy_desc,
         facility_npi_med_sp_desc = medicare_prov_supplier_desc)

table(dat3$facility_npi_med_sp_desc)

table(dat3$facility_npi_med_sp_desc, dat3$claim_type_code) 


