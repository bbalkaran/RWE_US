#########################################################
#  Script: Clean_Claims.R
#  Purpose: Clean claims files for analysis 
#  Data file: 
#  Author: Bridget Balkaran
#  Project:161103482-2
#  Project Manager: Janelle Cambron-Mellet
#  Date Created: 7/26/19
#  Date Edited: 8/19/19
########################################################




library(tidyverse)
library(magrittr)
library(kollekt)
library(lubridate)
load("~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/PhaseI_clean.RData")
bridging_file <- read_delim("~/Kantar/Arunajadai, Srikesh (KH) - KHDICT/LINKER/komodo/komodo_bridge_06202019.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)
bridging_file <- bridging_file %>% mutate(client_patient_id = `Komodo ID`)

dat_clean$MGYES %>% table() #12,866 experience migraine, all
dat_clean$EPISODIC_CAT %>% table()
dat_clean$MGDX %>% table() # 7120 dx with migraine ~55%
dat_clean$eli_not_eli %>% table()
dat_clean$male_mig_female %>% table()
dat_clean$eligible_comb %>% table()
exp_mg_pts <- dat_clean$zkey



add_NHWS_Date <- genNHWSquery("US", "2017", vars = "MGYES") %>% 
  runNHWSquery()
Index_date<- add_NHWS_Date %>% filter(zKey %in% dat_clean$zkey & source == "us_2017") %>% 
  rename(zkey = zKey)
# 2.claims -----
Enrollment_File <- read_csv("~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Enrollment_File.csv")
Mx_Header <- read_csv("~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Medical_Claims_Header.csv")
Mx_Service_Lines <- read_csv("~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Medical_Claims_Service_Lines.csv")
Rx_Claims <- read_csv("~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Pharmacy_Claims.csv")

# reformat header... only run these once: using magrittr here
Mx_Header$admission_date %<>% as.Date('%Y-%m-%d')
Mx_Header$discharge_date %<>% as.Date('%Y-%m-%d')
Mx_Header$claim_date %<>% as.Date('%Y-%m-%d')
Mx_Header$file_date %<>% as.Date('%Y-%m-%d')
Mx_Header$patient_dob %<>% as.Date('%Y-%m-%d')
Mx_Header$received_date %<>% as.Date('%Y-%m-%d')

#reformat service line .... only run these once
Mx_Service_Lines$service_to %<>% as.Date('%m/%d/%Y')
Mx_Service_Lines$service_from %<>%  as.Date('%m/%d/%Y')
Mx_Service_Lines$file_date %<>%  as.Date('%m/%d/%Y')
Mx_Service_Lines$date_of_service %<>%  as.Date('%m/%d/%Y')
Mx_Service_Lines$received_date %<>%  as.Date('%m/%d/%Y')





# 2A. Clean enrollment file -----
Enrollment_File <- Enrollment_File %>% filter(has_closed_medical_benefit== T & has_closed_pharmacy_benefit == T) # all closed 2,516





# 3. patients in phase I
Mx_Header <- Mx_Header %>% filter(client_patient_id %in% Enrollment_File$client_patient_id) %>% left_join(bridging_file) %>% 
  filter(zkey %in% exp_mg_pts)
Mx_Service_Lines <- Mx_Service_Lines %>% filter(client_patient_id %in% Enrollment_File$client_patient_id) %>% left_join(bridging_file) %>% 
  filter(zkey %in% exp_mg_pts)
Rx_Claims <- Rx_Claims %>% filter(client_patient_id %in% Enrollment_File$client_patient_id) %>% left_join(bridging_file)%>% 
  filter(zkey %in% exp_mg_pts)



#count who has dx codes 

ICD_fields <- list("d1","da", "d2", "d3", "d4","d5", "d6", "d7",
                   "d8", "d9", "d10", "d11", "d12", "d13", "d14",
                   "d15", "d16", "d17", "d18","d19",
                   "d20", "d21", "d22", "d23", "d24", "d25", "d26")
get_migraine_ICDs <- lapply(ICD_fields, function(x) {pull(Mx_Header, x)}) %>% unlist()

migraine_ICD10 <- grep("G43", get_migraine_ICDs, value = T, ignore.case = T)
migraine_ICD9 <- grep("^[3][4][6]", get_migraine_ICDs, value = T)
migraine_ICDs <- c(migraine_ICD10, migraine_ICD9)

Triptan_and_Ergot_Meds <- read.csv("~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Medications_Lists/Triptan_and_Ergot_Meds.csv", stringsAsFactors = F)
triptans <- grep("triptan", Triptan_and_Ergot_Meds$Kantar.Grouping, value = T)
ergots <- grep('ergot', Triptan_and_Ergot_Meds$Kantar.Grouping, value = T, ignore.case = T)
inclusion_meds <- Triptan_and_Ergot_Meds %>% filter(Kantar.Grouping %in% triptans | Kantar.Grouping %in% ergots) # not all NDCs 11 digits, need to cross-ref w/ OHDSI

project_path <- file.path(getPath('KHDICT'),'OHDSI')
file_name <- file.path(project_path,'CONCEPT.csv')
scan(file_name, what = character())
OHDSI <- read_delim("~/Kantar/Arunajadai, Srikesh (KH) - KHDICT/OHDSI/CONCEPT.csv",
                    "\t", escape_double = FALSE, trim_ws = TRUE)

OHDSI <- OHDSI %>% filter(vocabulary_id == "NDC")

ndcs <- c(grep("dihydroergot", OHDSI$concept_name, value = T, ignore.case = T),
          grep("almotriptan", OHDSI$concept_name, value = T, ignore.case = T),
          grep("Eletriptan", OHDSI$concept_name, value = T, ignore.case = T),
          grep("Ergotamine", OHDSI$concept_name, value = T, ignore.case = T),
          grep("Frovatriptan", OHDSI$concept_name, value = T, ignore.case = T),
          grep("Sumatriptan", OHDSI$concept_name, value = T, ignore.case = T),
          grep("Naratriptan", OHDSI$concept_name, value = T, ignore.case = T),
          grep("Rizatriptan", OHDSI$concept_name, value = T, ignore.case = T),
          grep("Zolmitriptan", OHDSI$concept_name, value = T, ignore.case = T))

migraine_NDCs <- OHDSI %>% filter(concept_name %in% ndcs)
dropthese <- grep("belladonna", migraine_NDCs$concept_name, value = T, ignore.case = T)
migraine_NDCs <- migraine_NDCs %>% filter(!concept_name %in% dropthese )




Mx_Header <- Mx_Header %>% mutate(Dx_in_claims = case_when(d1 %in% migraine_ICDs | da %in% migraine_ICDs |
                                     d2 %in% migraine_ICDs | d3 %in% migraine_ICDs |
                                     d4 %in% migraine_ICDs | d5 %in% migraine_ICDs |
                                     d6 %in% migraine_ICDs | d7 %in% migraine_ICDs |
                                     d8 %in% migraine_ICDs | d9 %in% migraine_ICDs |
                                    d10 %in% migraine_ICDs | d11 %in% migraine_ICDs|
                                    d12 %in% migraine_ICDs | d13 %in% migraine_ICDs|
                                    d14 %in% migraine_ICDs | d15 %in% migraine_ICDs|
                                    d16 %in% migraine_ICDs | d17 %in% migraine_ICDs|
                                    d18 %in% migraine_ICDs | d19 %in% migraine_ICDs|
                                    d20 %in% migraine_ICDs | d21 %in% migraine_ICDs|
                                    d22 %in% migraine_ICDs | d23 %in% migraine_ICDs|
                                    d24 %in% migraine_ICDs | d25 %in% migraine_ICDs|
                                   d26 %in% migraine_ICDs ~ 1,
                                   TRUE ~ 0))

Rx_Claims <- Rx_Claims %>% mutate(NDC_in_claims = case_when(ndc11 %in% inclusion_meds$code | 
                                                               ndc11 %in% migraine_NDCs$concept_code ~ 1,
                                                             TRUE ~ 0))


# Mx_Header <- Mx_Header %>% group_by(client_patient_id) %>% arrange(claim_date) %>% filter(Dx_in)
#                                     mutate(date_diff = claim_date - lag(claim_date)) %>% 
#                                     arrange(client_patient_id, claim_date, date_diff) %>% View()



concordance <- dat_clean %>% select(zkey, MGYES, MGDX, eli_not_eli, episodic, eligible_comb, male_mig_female) %>% 
  left_join(select(Mx_Header, zkey, client_patient_id, claim_date, Dx_in_claims))
yes_dx <- concordance  %>%filter(Dx_in_claims == 1) %>% select(zkey) %>% distinct()

# 2017 claims only
concordance2 <- concordance %>% filter(claim_date >= "2017-01-01" & claim_date <= "2017-12-31")
concordance2_a <- concordance2 %>% select(zkey, MGYES, MGDX, Dx_in_claims, episodic, eli_not_eli, eligible_comb, male_mig_female) %>% 
  distinct() %>% group_by(zkey) %>% slice(which.max(Dx_in_claims)) %>% ungroup() # n = 395
concordance2_a$Dx_in_claims %>% table()
concordance2_a$eli_not_eli %>% table()
concordance2_a$episodic %>% table()
concordance2_a$eligible_comb %>% table()
concordance2_a$male_mig_female %>% table()


#wiggle room arounf NHWS date
concordance3 <- concordance %>% left_join(select(Index_date, zkey, START_TIME)) 
concordance3$NHWSdate <- concordance3$START_TIME %>% date()
concordance4  <- concordance3 %>% mutate(lowerDate =  NHWSdate - years(1),
                        upperDate = NHWSdate + years(1)) %>%
  filter(claim_date >= lowerDate & claim_date <= upperDate)%>% 
  group_by(zkey) %>% slice(which.max(Dx_in_claims)) %>% ungroup() # n= 477

concordance4$Dx_in_claims %>% table()
concordance4_a$eli_not_eli %>% table()
concordance4_a$episodic %>% table()
concordance4_a$eligible_comb %>% table()
concordance4_a$male_mig_female %>% table()




# no time window 

concordance5 <- concordance3 %>% filter(! claim_date > NHWSdate) %>% group_by(zkey) %>% slice(which.max(Dx_in_claims)) %>% ungroup()

