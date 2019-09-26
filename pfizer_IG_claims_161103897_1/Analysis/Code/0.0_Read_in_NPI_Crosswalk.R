#########################################################
#  Script: 0.0_Read_in_NPI_Crosswalk.R
#  Purpose: NPI data from CMS
#  Data file: 
#  Author: Bridget Balkaran
#  Project: 161103897-1
#  Project Manager: Martine Maculaitis
#  Date Created: 9/12/19
#  Date Edited: 9/12/19
########################################################


library(jsonlite)
library(data.table)

NPI_xwalk <- fromJSON("https://data.cms.gov/resource/j75i-rw8y.json")
NPI_xwalk3 <- NPI_xwalk %>% na.locf()




Dir <- list.files("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/NPPES_Data_Dissemination_September_2019/split/")
paths<- lapply(Dir, function(x){paste("C:/Users/balkaranb//OneDrive - Kantar/Projects/Pfizer IG/NPPES_Data_Dissemination_September_2019/split/", x, sep = "")})
npi_registry <- lapply(paths, function(x){ fread(file = x,select = c("NPI", "Entity Type Code", "NPI Deactivation Reason Code", "NPI Deactivation Date",
                                                         "NPI Reactivation Date", "Healthcare Provider Taxonomy Code_1", "Healthcare Provider Taxonomy Code_2"))})

NPIregistry <- plyr::ldply(npi_registry, data.table)
NPIregistry[NPIregistry == ""]<- NA
NPIregistry <-NPIregistry[-1,]

NPIregistry <- NPIregistry %>% rename( provider_taxonomy_code = `Healthcare Provider Taxonomy Code_1`)

NPI_xwalk2 <- NPI_xwalk %>% na.omit() %>% select(provider_taxonomy_code, provider_taxonomy_desc) 

NPIregistry <- NPIregistry  %>% select(-`Healthcare Provider Taxonomy Code_2`) %>% left_join(NPI_xwalk2)
save(NPIregistry, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/NPI_registry.RData")