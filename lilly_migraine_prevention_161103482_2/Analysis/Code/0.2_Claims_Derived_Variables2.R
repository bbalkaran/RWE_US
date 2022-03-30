#########################################################
#  Script: Claims_Derived_Variables2.R
#  Purpose: identify ICD codes and patients for Migraind realted complications. 
#  Data file: 
#  Author: Bridget Balkaran
#  Project:161103482-2
#  Project Manager: Janelle Cambron-Mellet
#  Date Created: 7/26/19
#  Date Edited: 7/29/19
########################################################


# 1. workspace -----
library(tidyverse)
library(openxlsx)
library(lubridate)
library(magrittr)
options(scipen = 999)

#3. All cause related visits ---------------------------------------------------------------------


# read in and merge claims -------------------------------------------------------------------
Enrollment_File <- read_csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Enrollment_File.csv")
Mx_Header <- read_csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Medical_Claims_Header.csv")
Mx_Service_Lines <- read_csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Medical_Claims_Service_Lines.csv")
load("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/CohortB1.RData")

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

Enrollment_File <- Enrollment_File %>% filter(has_closed_medical_benefit== T & has_closed_pharmacy_benefit == T) # all closed 2,516
Mx_Header <- Mx_Header %>% filter(client_patient_id %in% Enrollment_File$client_patient_id)
Mx_Service_Lines <- Mx_Service_Lines %>% filter(claim_id %in% Mx_Header$claim_id)
CohortB1 <- CohortB1 %>% select(zkey, client_patient_id, NHWSindex_date, preNHWS, postNHWS)

# filter migraine complications -----
Mx_Header <- Mx_Header %>% filter(client_patient_id %in% CohortB1$client_patient_id)
Mx_Service_Lines <- Mx_Service_Lines %>% filter(claim_id %in% Mx_Header$claim_id)

Mx_Header <- Mx_Header %>% inner_join(CohortB1)
Mx_Service_Lines <- Mx_Service_Lines %>% inner_join(CohortB1)


Med_2017 <- Mx_Header %>% left_join(Mx_Service_Lines, by = c("client_patient_id", "claim_id", "file_date", "received_date"))
Med_2017 <- Med_2017 %>% distinct()

Med_2017 <-  Med_2017 %>% filter((claim_date >= preNHWS.x & claim_date <= postNHWS.x) |
                                   (date_of_service >= preNHWS.x & date_of_service <= postNHWS.x))


Med_2017 <- Med_2017 %>% mutate( place_of_service = case_when(place_of_service == 0 ~ NA_real_,
                                                              TRUE ~ place_of_service)) 


#-----------------------

All_visits <- Med_2017 %>% select(client_patient_id,
                                               claim_date, claim_type_code,
                                               date_of_service, 
                                               claim_id, place_of_service) %>% 
  mutate(year = case_when(is.na(date_of_service)  ~ year(claim_date),
                          TRUE ~ year(claim_date))) %>% distinct() %>%
  group_by(client_patient_id, claim_id, year, claim_type_code, place_of_service) %>% tally()
All_visits$place_of_service %>%  unique() 

# counts of visits per 
# label place of service:   [22 11 24 NA 81 23 12 21 41 19 20  2 72 49 
#                               14 50 85 53 31 77 17 60 15 32 83 57 71

# 2 = telehealth  -> outpatient event
# 11 = office -> physician office visit              
# 12 = home                                          
# 14 = group home  --> outpatient event 
# 15 = mobile unit -> outptient event 
# 17 = walk-in retail health clinic -> outpatient event 
# 19 = off campus opupatient hospital 
# 20  = urgent care facility -> outpatient event
# 21 = inpatient hospital -> hospitalization
# 22 = on campus OP hospital -> outpatient event
# 23 = Emergency room -> Emergency room Visits
# 24 = ambulatory surgical center -> outpatient event
# 31 = skilled nursing facility -> outpatient event
# 32 = nursing facility - outpatient event 
# 41 = Ambulance 
# 49 = independent clinic -> outpatient event 
# 50 = federally qualified health center --> physician office visit
# 53 = Community mental health center -> outpatient event
# 57 = non-residential substance abuse treatment facility -> outpatient event 
# 60 = mass immunization center -> physician office visit 
# 71 = public health clinic -> physician office visit
# 72 = rural health clinic -> physician office visit
# 77 = unassigned 
# 81 = Independent laboratory -> outpatient event
# 83 = unassigned 
# 85  = unassigned 

All_visits_count <- All_visits %>%
  mutate(Physician_Visits = case_when(place_of_service == 11 | 
                                        place_of_service == 50|
                                                place_of_service == 71 |
                                                place_of_service == 72 ~ 1,
                                              TRUE ~ 0),
         Outpatient_Visits = case_when(place_of_service == 2| 
                                         place_of_service == 14 |
                                         place_of_service == 15 | 
                                         place_of_service == 17 |
                                                 place_of_service == 19 |
                                         place_of_service == 20 |
                                         place_of_service == 21|
                                                 place_of_service == 22 |
                                                 place_of_service == 24 |
                                         place_of_service == 31|
                                                 place_of_service == 32 | 
                                         place_of_service == 41 |
                                         place_of_service == 49 |
                                         place_of_service == 53 |
                                         place_of_service == 57 |
                                         place_of_service == 60 |
                                                 place_of_service == 81 ~ 1,
                                               TRUE ~ 0),
         ER_Visits = case_when(place_of_service == 23 ~ 1,
                                       TRUE ~ 0),
         Home_event = case_when(place_of_service == 12 ~ 1,
                                        TRUE ~ 0),
         Hospitalizations = case_when(place_of_service == 21 | 
                                                (is.na(place_of_service) & claim_type_code == "I") ~ 1,
                                              TRUE ~ 0),
         All_Physician_Visit_Costs = case_when(Physician_Visits == 1 ~ 267*n,
                                           TRUE~ 0),
         All_Outpatient_Costs = case_when(Outpatient_Visits == 1 ~ 884*n,
                                      TRUE ~ 0),
         All_ER_Costs = case_when(ER_Visits == 1 ~ 1016*n,
                              TRUE ~ 0),
         All_Home_event_Costs = case_when(Home_event == 1 ~ 1757*n,
                                      TRUE ~ 0),
         All_Hospitalization_Costs = case_when(Hospitalizations == 1 ~ 14892*n,
                                           TRUE ~ 0),
         Total_Costs = All_Physician_Visit_Costs + All_Outpatient_Costs + All_ER_Costs + All_Hospitalization_Costs + All_Home_event_Costs)


#Mean expenditure per event by event type, United States, 2017
#Year	      Office-based physician visits	             Outpatient events	   Emergency room visits	  Inpatient stays	        Home health events
# 2017	          267	                                    884	                       1,016	              14,892	               1,757

load("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/CohortB1.RData")

# total costs per year
Yearly_costs <- aggregate(All_visits_count$Total_Costs, by = list(All_visits_count$client_patient_id), sum) %>%
  rename(client_patient_id = Group.1, 
         Total_cost = x)

Costs_per_year <- aggregate(All_visits_count$year, by = list(All_visits_count$client_patient_id), n_distinct) %>%
  rename(client_patient_id = Group.1,
         Years = x) %>% left_join(Yearly_costs) %>%
  mutate(All_cost_per_year = Total_cost/Years)

CohortB1 <- CohortB1 %>% left_join(select(Costs_per_year, client_patient_id, All_cost_per_year)) # 239 have cost info

# physician visits per year, and cost per year
Phys_visits <- aggregate(All_visits_count$Physician_Visits, by = list(All_visits_count$client_patient_id), sum) %>%
  rename(client_patient_id = Group.1, 
         Physician_office_visits = x) 

Phys_visits_per_year <- aggregate(All_visits_count$year, by = list(All_visits_count$client_patient_id), n_distinct) %>%
  rename(client_patient_id = Group.1,
         Years = x) %>% left_join(Phys_visits) %>%
  mutate(All_Phys_visits_per_year = Physician_office_visits/Years,
         All_Phys_visits_cost_per_year = All_Phys_visits_per_year*267)
CohortB1 <- CohortB1 %>% left_join(select(Phys_visits_per_year, client_patient_id, All_Phys_visits_per_year, All_Phys_visits_cost_per_year))

# outpatient visits per year 
OP_visits <- aggregate(All_visits_count$Outpatient_Visits, by = list(All_visits_count$client_patient_id), sum) %>%
  rename(client_patient_id = Group.1, 
         Outpatient_visits = x) 

OP_visits_per_year <- aggregate(All_visits_count$year, by = list(All_visits_count$client_patient_id), n_distinct) %>%
  rename(client_patient_id = Group.1,
         Years = x) %>% left_join(OP_visits) %>%
  mutate(All_OP_visits_per_year = Outpatient_visits/Years,
         All_OP_visits_cost_per_year = All_OP_visits_per_year*884)
CohortB1 <- CohortB1 %>% left_join(select(OP_visits_per_year, client_patient_id, All_OP_visits_per_year, All_OP_visits_cost_per_year))

# ER visits per year 
ER_visits <- aggregate(All_visits_count$ER_Visits, by = list(All_visits_count$client_patient_id), sum) %>%
  rename(client_patient_id = Group.1, 
         er_visits = x) 

ER_visits_per_year <- aggregate(All_visits_count$year, by = list(All_visits_count$client_patient_id), n_distinct) %>%
  rename(client_patient_id = Group.1,
         Years = x) %>% left_join(ER_visits) %>%
  mutate(All_ER_visits_per_year = er_visits/Years,
         All_ER_visits_cost_per_year = All_ER_visits_per_year*1016)
CohortB1 <- CohortB1 %>% left_join(select(ER_visits_per_year, client_patient_id, All_ER_visits_per_year, All_ER_visits_cost_per_year))

# hospitalizations
hosp_visits <- aggregate(All_visits_count$Hospitalizations, by = list(All_visits_count$client_patient_id), sum) %>%
  rename(client_patient_id = Group.1, 
         Hosp_visits = x) 

Hospitalizations_per_year <- aggregate(All_visits_count$year, by = list(All_visits_count$client_patient_id), n_distinct) %>%
  rename(client_patient_id = Group.1,
         Years = x) %>% left_join(hosp_visits) %>%
  mutate(All_Hosp_visits_per_year = Hosp_visits/Years,
         All_Hosp_visits_cost_per_year = All_Hosp_visits_per_year* 14,892)
CohortB1 <- CohortB1 %>% left_join(select(Hospitalizations_per_year, client_patient_id, All_Hosp_visits_per_year, All_Hosp_visits_cost_per_year))

CohortB1 <- CohortB1 %>% 
  mutate(TimeOnACD = MGYRACD + (MGMOACD/12),
         TimeOnACF = MGYRACF + (MGMOACF/12),
         TimeOnACO = MGYRACO + (MGMOACO/12),
         TimeOnAL  = MGYRAL  + (MGMOAL/12),
         TimeOnAM  = MGYRAM  + (MGMOAM/12),
         TimeOnAX  = MGYRAX  + (MGMOAX/12),
         TimeOnAY  = MGYRAY  + (MGMOAY/12),
         TimeOnBT  = MGYRBT  + (MGMOBT/12),
         TimeOnCM  = MGYRCM  + (MGMOCM/12),
         TimeOnCTS = MGYRCTS + (MGMOCTS/12),
         TimeOnCY  = MGYRCY  + (MGMOCY/12),
         TimeOnDP  = MGYRDP  + (MGMODP/12),
         TimeOnDR  = MGYRDR  + (MGMODR/12),
         TimeOnDYM = MGYRDYM + (MGMODYM/12),
         TimeOnFC  = MGYRFC  + (MGMOFC/12),
         TimeOnFN  = MGYRFN  + (MGMOFN/12),
         TimeOnFR  = MGYRFR  + (MGMOFR/12),
         TimeOnHC  = MGYRHC  + (MGMOHC/12),
         TimeOnHX  = MGYRHX  + (MGMOHX/12),
         TimeOnIB  = MGYRIB  + (MGMOIB/12),
         TimeOnICA = MGYRICA + (MGMOICA/12),
         TimeOnID =  MGYRID  + (MGMOID/12),
         TimeOnIJ  = MGYRIJ  + (MGMOIJ/12),
         TimeOnIM  = MGYRIM  + (MGMOIM/12),
         TimeOnIS  = MGYRIS  + (MGMOIS/12),
         TimeOnJW  = MGYRJW  + (MGMOJW/12),
         TimeOnMD  = MGYRMD  + (MGMOMD/12),
         TimeOnMGL = MGYRMGL + (MGMOMGL/12),
         TimeOnMGT = MGYRMGT + (MGMOMGT/12),
         TimeOnMLT = MGYRMLT + (MGMOMLT/12),
         TimeOnMX  = MGYRMX  + (MGMOMX/12),
         TimeOnNA  = MGYRNA  + (MGMONA/12),
         TimeOnNDR = MGYRNDR + (MGMONDR/12),
         TimeOnNE  = MGYRNE  + (MGMONE/12),
         TimeOnNX  = MGYRNX  + (MGMONX/12),
         TimeOnON  = MGYRON  + (MGMOON/12),
         TimeOnPRN = MGYRPRN + (MGMOPRN/12),
         TimeOnRL  = MGYRRL  + (MGMORL/12),
         TimeOnRZ  = MGYRRZ  + (MGMORZ/12),
         TimeOnSDZ = MGYRSDZ + (MGMOSDZ/12),
         TimeOnSPX = MGYRSPX + (MGMOSPX/12),
         TimeOnSU  = MGYRSU  + (MGMOSU/12),
         TimeOnSVD = MGYRSVD + (MGMOSVD/12),
         TimeOnTC  = MGYRTC  + (MGMOTC/12),
         TimeOnTI  = MGYRTI  + (MGMOTI/12),
         TimeOnTP  = MGYRTP  + (MGMOTP/12),
         TimeOnTX  = MGYRTX  + (MGMOTX/12),
         TimeOnVC  = MGYRVC  + (MGMOVC/12),
         TimeOnZCT = MGYRZCT + (MGMOZCT/12),
         TimeOnZE  = MGYRZE  + (MGMOZE/12),
         TimeOnZLM = MGYRZLM + (MGMOZLM/12),
         TimeOnZM =  MGYRZM  + (MGMOZM/12),
         TimeOnZMT = MGYRZMT + (MGMOZMT/12),
         TimeOnZS  = MGYRZS  + (MGMOZS/12)) 
#
save(CohortB1, file ="C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/CohortB1.RData")
