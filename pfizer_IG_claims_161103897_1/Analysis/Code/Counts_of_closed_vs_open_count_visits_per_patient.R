#########################################################
#  Script: Counts of closed vs open 
#  Purpose: NPI data from CMS
#  Data file: 
#  Author: Bridget Balkaran
#  Project: 161103897-1
#  Project Manager: Martine Maculaitis
#  Date Created: 11/6/19
#  Date Edited: 11/6/19
########################################################


library(tidyverse)
library(readr)
library(magrittr)
library(data.table)
library(skimr)
`%notin%` = function(x,y) !(x %in% y)
beneficiary_file <- read_csv("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Pfizer IG/Data/beneficiary_file.csv")
#load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med Claims for Site Identification.RData")
load("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Visits.RData")
med$service_from %<>% as.Date()
med$service_to %<>% as.Date()
med$date_of_service %<>% as.Date()
med <- med %>% select(-Visit_id)


med$service_from %>% summary()
med %>% filter(service_from < "2015-11-02") %>% select(service_from) %>%  unique() %>% View()
med <- med %>% filter(service_from >= "2000-01-01")
med %>% filter(service_from > "2019-09-11") %>% select(service_from) %>%  unique() %>% View()
med <- med %>% filter(service_from < "2019-09-11")

med$patient_id %>% n_distinct() # distinct patients in med claims 219602
med <- med %>% filter(!is.na(patient_id))

# b <- med %>% filter(patient_id %notin% beneficiary_file$patient_id) # filter out patients not in beneficiary file
# b$patient_id %>% n_distinct() #61482 distinct patients not in beneficiary file 113896
# rm(b)
# gc()
c <- med %>% filter(patient_id %in% beneficiary_file$patient_id) # patients in beneficiary file 
c$patient_id %>% n_distinct() #107212
d <- c %>% filter( service_from >= beneficiary_file$eligibility_start_date &
                                        service_from <= beneficiary_file$eligibility_end_date)
d$patient_id %>% n_distinct()

d <- d %>% mutate(NPI = case_when( (is.na(billing_pr_npi) &
                                    is.na(attending_npi)) ~ facility_npi,
                                 is.na(billing_pr_npi) ~ attending_npi,
                                 TRUE ~ billing_pr_npi))

pts_dos <- d %>% select(patient_id, claim_id, claim_type_code, NPI, service_from, 
                        place_of_service, procedure, ndc_code, Visit_type) %>% distinct()


pts_dos <- pts_dos %>% arrange(patient_id, service_from) 

df <- pts_dos %>% arrange(patient_id, service_from) %>%
  group_by(factor(patient_id)) %>%
  mutate(previous = lag(service_from, 1)) %>%
  mutate(tae.1 = service_from - lag(service_from, 1))%>% 
  ungroup() %>%
  mutate(consecutive_flag = case_when(tae.1 == 1 ~ 1)) %>%
  mutate(consecutive_flag = case_when(tae.1 == 1 ~ 1,
                                      lead(tae.1 == 1)~1))



df4 <- df %>% group_by(patient_id, service_from) %>% 
  fill(consecutive_flag) %>% 
  fill(consecutive_flag, .direction = "up") # RAN THIS OVERNIGHT TAKES A LONG TIME!!

df4_2 <- df4 %>% 
  filter(!(Visit_type == "non_IG_visit" & is.na(consecutive_flag))) # drop out non-IG visits

df5 <- df4_2 %>% group_by(patient_id, service_from) %>% 
  arrange(service_from) %>% 
  slice(1) %>% ungroup() 

try <- df5
 try <- try %>% group_by(patient_id, service_from) %>% 
  fill(consecutive_flag) %>% 
  fill(consecutive_flag, .direction = "up")


try2 <-  try %>% group_by(patient_id) %>% 
  filter(consecutive_flag == 1) 
  
  
 try3 <-  try2 %>% mutate(new_dos = case_when(is.na(previous) & is.na(tae.1) ~ service_from,
                                              tae.1 > 1 ~ service_from)) %>%
   fill(new_dos)

rm(c,d, df, df4, df4_2, df5)
gc()
pts_dos2 <- pts_dos %>% left_join(try3) 
pts_dos2 <- pts_dos2 %>% group_by(patient_id, service_from) %>% fill(new_dos)

pts_dos2 <- pts_dos2 %>% mutate(dos = case_when(is.na(new_dos) ~ service_from,
                                                    TRUE ~ new_dos)) 

rm(try, try2, try3)
gc()
pts_dos3 <-  pts_dos2 %>% ungroup() %>% 
  distinct() %>% group_by(patient_id) %>% mutate(pt_id = group_indices()) 
#%>%   arrange(patient_id, pt_id, dos)
  
# try splitting by patient

pts <-group_split(pts_dos3)


visit_ids <- function(.data) {
  .data %>% group_by(dos) %>% mutate(Visit_id = group_indices())
}

# <- pts[[2]]

#visit_ids(e)

#e %>% group_by(dos) %>% mutate(Visit_id = group_indices())

pts2 <-  map(pts, visit_ids) # now have a data frame for each patient and their visit ids 



rm(pts_dos, pts_dos2, pts_dos3)
gc()

#save(df4, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Patient Visits Wide.RData")



load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/NPI_registry_deduped.RData")


NPIs <- NPIs %>% select(NPI, provider_taxonomy_desc) %>% distinct()

join_taxonomy <- function(.data){
  .data %>% left_join(select(NPIs, NPI, provider_taxonomy_desc))
}

join_taxonomy(e) %>% View()

pts2 <- map(pts2,join_taxonomy)


distinct_visit_taxonomy <- function(.data){
.data%>%  select(patient_id, Visit_id, 
         claim_type_code, 
         provider_taxonomy_desc) %>%
    distinct()
}
pts3 <- map(pts2, distinct_visit_taxonomy)

# try <- rbind(pts3[[1]], pts3[[2]], pts3[[3]])
# 
# rm(pts)

pt_visit_NPI <- plyr::ldply(pts3, data.table)

save(pt_visit_NPI, 
     file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Visits_and_taxonomies.RData")

