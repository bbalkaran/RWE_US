#########################################################
#  Script: 0.5_Xtabs_for_J_NDC_NPI.R
#  Purpose: NPI data from CMS
#  Data file: 
#  Author: Bridget Balkaran
#  Project: 161103897-1
#  Project Manager: Martine Maculaitis
#  Date Created: 10/9/19
#  Date Edited: 10/10/19
########################################################

library(tidyverse)
library(data.table)
library(magrittr)
library(jsonlite)


load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/J_NDC_IG_NPI.RData")


load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/NPI_registry.RData") # NPI registry
NPIregistry <- NPIregistry %>% select(NPI, provider_taxonomy_code) %>% data.table()

# split NPI registry
NPIregistry_1 <- NPIregistry[1:1000000,] %>% unique()
NPIregistry_2 <- NPIregistry[1000001:2000000,] %>% unique()
NPIregistry_3 <- NPIregistry[2000001:3000000,] %>% unique()
NPIregistry_4 <- NPIregistry[3000001:4000000,] %>% unique()
NPIregistry_5 <- NPIregistry[4000001:5000000,] %>% unique()
NPIregistry_6 <- NPIregistry[5000001:6000000,] %>% unique()
NPIregistry_7 <- NPIregistry[6000001:6792363,] %>% unique()




####################################
# attending NPI 
# select out and split attending npi
attending_1 <- J_NDC[1:1000000,9] %>% unique()
attending_2 = J_NDC[1000001:2000000,9] %>% unique()
attending_3 = J_NDC[2000001:2707494,9] %>% unique()




# filter NPI registry based on NPIs in attending data
NPIregistry_list <- list(NPIregistry_1, NPIregistry_2, NPIregistry_3,
                         NPIregistry_4, NPIregistry_5, NPIregistry_6, 
                         NPIregistry_7)
attendingNPI_registry <- lapply(NPIregistry_list, function(x){
  x %>% filter(NPI %in% attending_1 | NPI %in% attending_2 | NPI %in% attending_3)
})

attending_NPI_registry <- plyr::ldply(attendingNPI_registry, data.table)# final attending NPI registry
gc()
rm(attendingNPI_registry)
#####################
# Billing NPI
billing_1 <- J_NDC[1:1000000,8] %>% unique()
billing_2 = J_NDC[1000001:2000000,8] %>% unique()
billing_3 = J_NDC[2000001:2707494,8] %>% unique()


# filter NPI registry based on NPIs in billing data

billingNPI_registry <- lapply(NPIregistry_list, function(x){
  x %>% filter(NPI %in% billing_1 | NPI %in% billing_2 | NPI %in% billing_3)
})

billing_NPI_registry <- plyr::ldply(billingNPI_registry, data.table)# final billing NPI registry
gc()
rm(billingNPI_registry)

#######################################
#  facility npi

facility_1 <- J_NDC[1:1000000,11] %>% unique()
facility_2 = J_NDC[1000001:2000000,11] %>% unique()
facility_3 = J_NDC[2000001:2707494,11] %>% unique()


# filter NPI registry based on NPIs in billing data

facilityNPI_registry <- lapply(NPIregistry_list, function(x){
  x %>% filter(NPI %in% facility_1 | NPI %in% facility_2 | NPI %in% facility_3)
})

facility_NPI_registry <- plyr::ldply(facilityNPI_registry, data.table)# final facility NPI registry
gc()
rm(facilityNPI_registry)


rm(NPIregistry, NPIregistry_1, NPIregistry_2, NPIregistry_3,
   NPIregistry_4, NPIregistry_5, NPIregistry_6,
   NPIregistry_7, NPIregistry_list)
rm(attending_1, attending_2, attending_3, billing_1,
   billing_2, billing_3, facility_1, facility_2, 
   facility_3)


####################################
# add in NPI xwalk
NPI_xwalk <- fromJSON("https://data.cms.gov/resource/j75i-rw8y.json") %>% mutate_if(is.character, str_trim)

attending_NPI_registry <- attending_NPI_registry %>%
  left_join(NPI_xwalk) # not a 1:1 merge, a single taxonomy can refer to multiple decs
billing_NPI_registry <- billing_NPI_registry %>% left_join(NPI_xwalk)
facility_NPI_registry <- facility_NPI_registry %>% left_join(NPI_xwalk)

rm(NPI_xwalk)
gc()

################################


dat1 <- J_NDC
  
  
  NPIregistry %>% select(NPI, `Entity Type Code`, provider_taxonomy_code)%>% rename(attending_npi = NPI) %>%
  right_join(J_NDC) %>% mutate_if(is.character,str_trim) %>%
  left_join(select(NPI_xwalk, medicare_specialty_code, medicare_prov_supplier_desc,
                   provider_taxonomy_code, provider_taxonomy_desc)) %>% unique() %>% 
  rename(attending_npi_taxonomy = provider_taxonomy_desc,
         attenting_npi_med_sp_desc = medicare_prov_supplier_desc)

CrossTable(dat1$NewPt_id, dat1$Visit_id)

table(dat1$attenting_npi_med_sp_desc)

table( dat1$attenting_npi_med_sp_desc, dat1$claim_type_code) 



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


