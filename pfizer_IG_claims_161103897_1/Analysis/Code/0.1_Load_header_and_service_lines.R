#########################################################
#  Script: 0.1_Clean_ennrollment_file.R
#  Purpose: NPI data from CMS
#  Data file: 
#  Author: Bridget Balkaran
#  Project: 161103897-1
#  Project Manager: Martine Maculaitis
#  Date Created: 9/12/19
#  Date Edited: 9/12/19
########################################################

# The Claims Header file has the admin and discharge date in it.  That makes it a whole lot easier.  You only need to include for the first round of analysis, for each IG treatment claim:
#   •	The IG treatment claim
# •	The header file for that IG treatment claim
# •	The header file and service line file for any claim that falls on the same date as the IG treatment 
# •	The header file only for any claim where discharge date is after admin date AND admin date <  IG treatment date < discharge date


library(tidyverse)
library(data.table)
library(doParallel)
cluster <- makeCluster(detectCores())



Dir <- list.files()
paths<- lapply(Dir, function(x){paste("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/split/", x, sep = "")})
header <- lapply(paths, function(x) {
  fread(file = x, select = c("patient_id","claim_id", "claim_type_code", "admit_type_code", "discharge_status_code", "admission_date", "discharge_date",
                             "claim_date", "D1", "billing_pr_npi", "attending_npi", "referring_pr_npi", "facility_npi", 
                             "facility_name", "facility_adr_zip", "patient_gender", "patient_state", "patient_zip", "patient_dob"))})


header2 <- plyr::ldply(header, data.table)

save(header2, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Header.RData")







Dir <- list.files("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Sample_split/")
paths<- lapply(Dir, function(x){paste("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Sample_split/", 
                                      x, sep = "")})
service <- parLapply(cl= cluster, paths, function(x) {data.table::fread(file = x, select = c("claim_id", "service_line", "service_from", 
                                                                                             "service_to", "ndc_code", "procedure",  
                             "date_of_service", "place_of_service", "rendering_prov_npi",
                             "service_facility_npi", "ordering_pr_npi", "referring_pr_npi"))})
stopCluster(cluster)
service2 <- plyr::ldply(service, data.table)

service_npis <- select(service2, claim_id, rendering_prov_npi, service_facility_npi, ordering_pr_npi, referring_pr_npi)
service2 <- service2 %>% select(-rendering_prov_npi, -service_facility_npi, -ordering_pr_npi, -referring_pr_npi)

save(service2, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Service2.RData")


save(service_npis, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Service_NPIs.RData")



#  how many more octagam and panzyga with 2019, 