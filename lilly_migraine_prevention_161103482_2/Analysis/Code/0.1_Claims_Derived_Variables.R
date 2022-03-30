#########################################################
#  Script: Claims_Derived_Variables.R
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

#3. Migraine related visits ---------------------------------------------------------------------

Migraine_complications <- read.xlsx("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Potential_Migraine_Complications_ICDcodes.xlsx")



Migraine_complications$concept_code <- Migraine_complications$concept_code %>% str_remove_all(pattern = "\\.")
Migraine_complications <- Migraine_complications %>% select(concept_name, concept_code)

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
Mx_Header <- Mx_Header %>% filter(d1 %in% Migraine_complications$concept_code |
                                    da %in% Migraine_complications$concept_code |
                                    d2 %in% Migraine_complications$concept_code |
                                    d3 %in% Migraine_complications$concept_code |
                                    d4 %in% Migraine_complications$concept_code |
                                    d5 %in% Migraine_complications$concept_code |
                                    d6 %in% Migraine_complications$concept_code |
                                    d7 %in% Migraine_complications$concept_code |
                                    d8 %in% Migraine_complications$concept_code |
                                    d9 %in% Migraine_complications$concept_code |
                                    d10 %in% Migraine_complications$concept_code |
                                    d11 %in% Migraine_complications$concept_code |
                                    d12 %in% Migraine_complications$concept_code |
                                    d13 %in% Migraine_complications$concept_code |
                                    d14 %in% Migraine_complications$concept_code |
                                    d15 %in% Migraine_complications$concept_code |
                                    d16 %in% Migraine_complications$concept_code |
                                    d17 %in% Migraine_complications$concept_code |
                                    d18 %in% Migraine_complications$concept_code |
                                    d19 %in% Migraine_complications$concept_code |
                                    d20 %in% Migraine_complications$concept_code |
                                    d21 %in% Migraine_complications$concept_code |
                                    d22 %in% Migraine_complications$concept_code |
                                    d23 %in% Migraine_complications$concept_code |
                                    d24 %in% Migraine_complications$concept_code |
                                    d25 %in% Migraine_complications$concept_code |
                                    d26 %in% Migraine_complications$concept_code) 

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

Migraine_related_visits <- Med_2017 %>% select(client_patient_id,
                                               claim_date, claim_type_code,
                                               date_of_service, 
                                               claim_id, place_of_service) %>% 
  mutate(year = case_when(is.na(date_of_service)  ~ year(claim_date),
                          TRUE ~ year(claim_date))) %>% distinct() %>%
  group_by(client_patient_id, claim_id, year, claim_type_code, place_of_service) %>% tally()
Migraine_related_visits$place_of_service %>%  unique() 

 # counts of visits per 
# label place of service:  24 NA 11 12 22 21 23 81 19 14 41 77 72 32 85 71

# 11 = office -> physician office visit              
# 12 = home                                          
# 14 = group home  --> outpatient event 
# 19 = off campus opupatient hospital 
# 21 = inpatient hospital -> hospitalization
# 22 = on campus OP hospital -> outpatient event
# 23 = Emergency room -> Emergency room Visits
# 24 = ambulatory surgical center -> outpatient event
# 32 = nursing facility - ???
# 41 = Ambulance 
# 71 = public health clinic -> physician office visit
# 72 = rural health clinic -> physician office visit
# 77 = unassigned 
# 81 = Independent laboratory -> outpatient event
# 85  = unassigned 

Migraine_related_visits_count <- Migraine_related_visits %>%
  mutate(Related_Physician_Visits = case_when(place_of_service == 11 | 
                                                place_of_service == 71 |
                                                place_of_service == 72 ~ 1,
                                              TRUE ~ 0),
         Related_Outpatient_Visits = case_when(place_of_service == 14 |
                                                 place_of_service == 19 | 
                                                 place_of_service == 22 |
                                                 place_of_service == 24 |
                                                 place_of_service == 32 | 
                                                 place_of_service == 81 ~ 1,
                                               TRUE ~ 0),
         Related_ER_Visits = case_when(place_of_service == 23 ~ 1,
                                       TRUE ~ 0),
         Related_Home_event = case_when(place_of_service == 12 ~ 1,
                                       TRUE ~ 0),
         Related_Hospitalizations = case_when(place_of_service == 21 | 
                                                (is.na(place_of_service) & claim_type_code == "I") ~ 1,
                                              TRUE ~ 0),
         Physician_Visit_Costs = case_when(Related_Physician_Visits == 1 ~ 267*n,
                                           TRUE~ 0),
         Outpatient_Costs = case_when(Related_Outpatient_Visits == 1 ~ 884*n,
                                      TRUE ~ 0),
         ER_Costs = case_when(Related_ER_Visits == 1 ~ 1016*n,
                              TRUE ~ 0),
         Home_event_Costs = case_when(Related_Home_event == 1 ~ 1757*n,
                                      TRUE ~ 0),
         Hospitalization_Costs = case_when(Related_Hospitalizations == 1 ~ 14892*n,
                                           TRUE ~ 0),
         Total_Costs = Physician_Visit_Costs + Outpatient_Costs + ER_Costs + Hospitalization_Costs + Home_event_Costs)


#Mean expenditure per event by event type, United States, 2017
#Year	      Office-based physician visits	             Outpatient events	   Emergency room visits	  Inpatient stays	        Home health events
# 2017	          267	                                    884	                       1,016	              14,892	               1,757

load("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/CohortB1.RData")

# total costs per year
Yearly_costs <- aggregate(Migraine_related_visits_count$Total_Costs, by = list(Migraine_related_visits_count$client_patient_id), sum) %>%
  rename(client_patient_id = Group.1, 
         Total_cost = x)

Costs_per_year <- aggregate(Migraine_related_visits_count$year, by = list(Migraine_related_visits_count$client_patient_id), n_distinct) %>%
  rename(client_patient_id = Group.1,
         Years = x) %>% left_join(Yearly_costs) %>%
  mutate(cost_per_year = Total_cost/Years)

CohortB1 <- CohortB1 %>% left_join(select(Costs_per_year, client_patient_id, cost_per_year)) # 239 have cost info

# physician visits per year, and cost per year
Phys_visits <- aggregate(Migraine_related_visits_count$Related_Physician_Visits, by = list(Migraine_related_visits_count$client_patient_id), sum) %>%
  rename(client_patient_id = Group.1, 
         Physician_office_visits = x) 

Phys_visits_per_year <- aggregate(Migraine_related_visits_count$year, by = list(Migraine_related_visits_count$client_patient_id), n_distinct) %>%
  rename(client_patient_id = Group.1,
         Years = x) %>% left_join(Phys_visits) %>%
  mutate(Phys_visits_per_year = Physician_office_visits/Years,
         Phys_visits_cost_per_year = Phys_visits_per_year*267)
CohortB1 <- CohortB1 %>% left_join(select(Phys_visits_per_year, client_patient_id, Phys_visits_per_year, Phys_visits_cost_per_year))

# outpatient visits per year 
OP_visits <- aggregate(Migraine_related_visits_count$Related_Outpatient_Visits, by = list(Migraine_related_visits_count$client_patient_id), sum) %>%
  rename(client_patient_id = Group.1, 
         Outpatient_visits = x) 

OP_visits_per_year <- aggregate(Migraine_related_visits_count$year, by = list(Migraine_related_visits_count$client_patient_id), n_distinct) %>%
  rename(client_patient_id = Group.1,
         Years = x) %>% left_join(OP_visits) %>%
  mutate(op_visits_per_year = Outpatient_visits/Years,
         OP_visits_cost_per_year = op_visits_per_year*884)
CohortB1 <- CohortB1 %>% left_join(select(OP_visits_per_year, client_patient_id, op_visits_per_year, OP_visits_cost_per_year))

# ER visits per year 
ER_visits <- aggregate(Migraine_related_visits_count$Related_ER_Visits, by = list(Migraine_related_visits_count$client_patient_id), sum) %>%
  rename(client_patient_id = Group.1, 
         er_visits = x) 

ER_visits_per_year <- aggregate(Migraine_related_visits_count$year, by = list(Migraine_related_visits_count$client_patient_id), n_distinct) %>%
  rename(client_patient_id = Group.1,
         Years = x) %>% left_join(ER_visits) %>%
  mutate(ER_visits_per_year = er_visits/Years,
         ER_visits_cost_per_year = ER_visits_per_year*1016)
CohortB1 <- CohortB1 %>% left_join(select(ER_visits_per_year, client_patient_id, ER_visits_per_year, ER_visits_cost_per_year))

# hospitalizations
hosp_visits <- aggregate(Migraine_related_visits_count$Related_Hospitalizations, by = list(Migraine_related_visits_count$client_patient_id), sum) %>%
  rename(client_patient_id = Group.1, 
         Hosp_visits = x) 

Hospitalizations_per_year <- aggregate(Migraine_related_visits_count$year, by = list(Migraine_related_visits_count$client_patient_id), n_distinct) %>%
  rename(client_patient_id = Group.1,
         Years = x) %>% left_join(hosp_visits) %>%
  mutate(Hosp_visits_per_year = Hosp_visits/Years,
         Hosp_visits_cost_per_year = Hosp_visits_per_year* 14,892)
CohortB1 <- CohortB1 %>% left_join(select(Hospitalizations_per_year, client_patient_id, Hosp_visits_per_year, Hosp_visits_cost_per_year))


############### PICK UP HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# 4. prescription costs ----
library(lubridate)
NADAC <- 
  read.csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/NADAC__National_Average_Drug_Acquisition_Cost_.csv", 
           sep=";", stringsAsFactors=FALSE)
NADAC <- NADAC %>% rename( ndc_description = ï..NDC.Description,
                           ndc = NDC, 
                           NADAC_per_unit = NADAC_Per_Unit,
                           pricing_unit = Pricing_Unit,
                           as_of_date = As.of.Date)
NADAC$as_of_date <- NADAC$as_of_date %>% mdy()
NADAC$NADAC_per_unit <- NADAC$NADAC_per_unit %>% as.double()
#take most recent as of date
Drug_costs<- NADAC %>% select(ndc, ndc_description, pricing_unit, NADAC_per_unit, as_of_date) %>%
  group_by(ndc) %>% slice(which.max(as_of_date))


#mydf %>% group_by(myids) %>% slice(which.max(mydates))
Rx_Claims <- read_csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Pharmacy_Claims.csv")
Triptan_and_Ergot_Meds <- read.csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Medications_Lists/Triptan_and_Ergot_Meds.csv", stringsAsFactors = F)
triptans <- grep("triptan", Triptan_and_Ergot_Meds$Kantar.Grouping, value = T)
ergots <- grep('ergot', Triptan_and_Ergot_Meds$Kantar.Grouping, value = T, ignore.case = T)
inclusion_meds <- Triptan_and_Ergot_Meds %>% filter(Kantar.Grouping %in% triptans | Kantar.Grouping %in% ergots) # not all NDCs 11 digits, need to cross-ref w/ OHDSI

project_path <- file.path(getPath('KHDICT'),'OHDSI')
file_name <- file.path(project_path,'CONCEPT.csv')
scan(file_name, what = character())
OHDSI <- read_delim("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - KHDICT/OHDSI/CONCEPT.csv",
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
# get hcpcs for identifying injectables
HCPCS <- read.csv("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Lilly Migraine Prevention Phase II/Analysis/Data/Medications_Lists/Kantar_migraine med list_Acute_HCPCS.csv", stringsAsFactors=FALSE)
HCPCS <- HCPCS %>% rename(Jcode = ï..code)
CohortB1 %>% filter(procedure  %in% HCPCS$Jcode) %>% View() #1 patient with a HCPCS code, ketolorac
# check which NDC can match to HCPCS Give an example---

# calculate RX costs 
Pharm <- CohortB1 %>% filter(!is.na(p.date_of_service)) %>% 
  select(client_patient_id, p.date_of_service, p.ndc11) %>% 
  rename(ndc = p.ndc11) %>% arrange(client_patient_id, p.date_of_service) 


Costs_Migraine_drugs <- NADAC %>% 
  select(ndc, ndc_description, pricing_unit, NADAC_per_unit)  %>%
  filter(ndc %in% migraine_NDCs$concept_code) %>% distinct() 

Pharm2 <- Pharm %>% select(-client_patient_id, -p.date_of_service) %>% left_join(Drug_costs)
Pharm2 <- Pharm2 %>% rename(p.ndc11 = ndc) %>% ungroup()
Rx_claims2 <- Rx_Claims

colnames(Rx_claims2) <- paste("p", colnames(Rx_claims2), sep = ".")
Rx_claims3 <-  Rx_claims2%>% ungroup() %>% 
  rename(client_patient_id = p.client_patient_id) %>% 
  filter(client_patient_id %in% CohortB1$client_patient_id & p.ndc11 %in% CohortB1$p.ndc11) %>%
  select(client_patient_id, p.date_of_service, p.ndc11, p.quantity_dispensed, p.fill_number, p.days_supply, p.unit_of_measure)%>% 
  left_join(select(Pharm2, p.ndc11, ndc_description, pricing_unit, NADAC_per_unit, as_of_date))  %>%
  mutate(Rx_cost = p.quantity_dispensed*NADAC_per_unit) %>% 
  select(client_patient_id, p.ndc11, p.date_of_service, p.quantity_dispensed, p.unit_of_measure, p.fill_number,
         NADAC_per_unit, ndc_description, pricing_unit, as_of_date, Rx_cost) %>% distinct()
Rx_claims3$p.date_of_service <- Rx_claims3$p.date_of_service %>% mdy()
Rx_claims3 <- Rx_claims3 %>% left_join(select(CohortB1, client_patient_id, preNHWS, postNHWS))
Rx_claims3 <- Rx_claims3 %>% filter(p.date_of_service >= preNHWS & p.date_of_service <= postNHWS)
Rx_claims3$Years <- Rx_claims3$p.date_of_service %>% year()

Rx_Yearly_costs <- aggregate(Rx_claims3$Rx_cost, by = list(Rx_claims3$client_patient_id), sum) %>%
  rename(client_patient_id = Group.1, 
         Rx_Total_cost = x) 
Rx_Costs_per_year <- aggregate(Rx_claims3$Years, by = list(Rx_claims3$client_patient_id), n_distinct) %>%
  rename(client_patient_id = Group.1,
         Years = x) %>% left_join(select(Rx_Yearly_costs, client_patient_id, Rx_Total_cost)) %>%
  mutate(Rx_cost_per_year = Rx_Total_cost/Years)

CohortB1 <- CohortB1 %>% left_join(select(Rx_Costs_per_year, client_patient_id, Rx_cost_per_year)) # 230 have cost info

CohortB1$cost_per_year <- CohortB1$cost_per_year %>% round(digits = 2)
CohortB1$Rx_cost_per_year <- CohortB1$Rx_cost_per_year %>% round(digits = 2)

library(haven)
Lilly_migraine <- read_sav("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Lilly Migrane Prevention/Analysis/Data/Lilly migraine data 0v1 05-October-2018(LABELLED)10112018_new.sav")
# b2 <- Lilly_migraine %>% select(zKey, WPPRD, WPWRK, WPMIS) %>% 
#   rename(zkey = zKey) %>% as_tibble()
# b2<- b2 %>% zap_labels()
# b2 <- b2 %>% zap_widths()
# b2 <- b2 %>% zap_label()
# b2 <- b2 %>% zap_formats()
# var_label(b2) <- NULL

# b2 <- Lilly_migraine %>% select(zKey, MGTPACD, MGTPACF, MGTPACO, MGTPAL, 
#                                 MGTPAM, MGTPAX, MGTPAY, MGTPBT,
#                                 MGTPCM, MGTPCTS, MGTPCY, MGTPDP,
#                                 MGTPDR, MGTPDYM, MGTPFC, MGTPFN,
#                                 MGTPFR, MGTPHC, MGTPHX, MGTPIB, 
#                                 MGTPICA, MGTPID, MGTPIJ, MGTPIM,
#                                 MGTPIS, MGTPJW, MGTPMD, MGTPMGL,
#                                 MGTPMGT, MGTPMLT, MGTPMX, MGTPNA,
#                                 MGTPNDR, MGTPNE, MGTPNX, MGTPON, 
#                                 MGTPPRN, MGTPRL, MGTPRZ, MGTPSDZ,
#                                 MGTPSPX, MGTPSU, MGTPSVD, MGTPTC,
#                                 MGTPTI, MGTPTP, MGTPTX, MGTPVC,
#                                 MGTPZCT, MGTPZE, MGTPZLM, MGTPZM,
#                                 MGTPZM, MGTPZMT, MGTPZS) %>% rename(zkey = zKey)
# 
# CohortB1 <- CohortB1 %>% left_join(b2)

b2 <- Lilly_migraine %>% select(zKey, MGSAIM) %>% rename(zkey = zKey)

b2 <- Lilly_migraine %>% select(zKey, MGSADP, MGSADR, MGSADYM, MGSAIS) %>% rename(zkey = zKey)
CohortB1 <- CohortB1 %>% left_join(b2)

b2 <- Lilly_migraine %>% select(zKey, MGMSW, MGMSH) %>% rename(zkey = zKey)
CohortB1 <- CohortB1 %>% left_join(b2)

CohortB1 <- CohortB1 %>%
    mutate(wage = case_when(DEAGE_R == 18 & DEAGE_R <= 19 & DESEX == "Female " ~ 402,
                            DEAGE_R >= 20 & DEAGE_R <= 24 & DESEX == "Female " ~ 514,
                            DEAGE_R >= 25 & DEAGE_R <= 34 & DESEX == "Female " ~ 724,
                            DEAGE_R >= 35 & DEAGE_R <= 44 & DESEX == "Female " ~ 860,
                            DEAGE_R >= 45 & DEAGE_R <= 54 & DESEX == "Female " ~ 855,
                            DEAGE_R >= 55 & DEAGE_R <= 64 & DESEX == "Female " ~ 856,
                            DEAGE_R >= 65 & DESEX == "Female " ~ 782,
                            DEAGE_R == 18 & DEAGE_R <= 19 & DESEX == "Male " ~ 459,
                            DEAGE_R >= 20 & DEAGE_R <= 24 & DESEX == "Male " ~ 570,
                            DEAGE_R >= 25 & DEAGE_R <= 34 & DESEX == "Male " ~ 821,
                            DEAGE_R >= 35 & DEAGE_R <= 44 & DESEX == "Male " ~ 1062,
                            DEAGE_R >= 45 & DEAGE_R <= 54 & DESEX == "Male " ~ 1103,
                            DEAGE_R >= 55 & DEAGE_R <= 64 & DESEX == "Male " ~ 1098,
                            DEAGE_R >= 65 & DESEX == "Male " ~ 1016)) %>%
    #https://www.bls.gov/opub/reports/womens-earnings/2017/pdf/home.pdf  DATA COMES FROM HERE
    mutate(Hourly_wage = wage/40) %>%  # weekly earnings/ hours worked per week) 
    mutate(presenteeism_wpwrk = (WPPRD/10)*WPWRK) %>% # work productivity scale/ 10 * hours worked per week
    mutate(Indirect_cost_ABS = Hourly_wage*WPMIS*50) %>% # hourly wage *work hours missed absenteeism* 50 work weeks per year
    mutate(Indirect_cost_pres = Hourly_wage*presenteeism_wpwrk*50)  %>% # hourly wage * work hours missed presenteeism *50 work weeks per year
    mutate(Indirect_cost_PRES = case_when(is.na(Indirect_cost_pres)== T & 
                                            is.na(Indirect_cost_ABS) == F ~ 0.00, # Some patients only have ABS, can't add values to NA 
                                          TRUE ~ Indirect_cost_pres)) %>%          # change to 0s to add for totals
    mutate(Total_indirect_cost = Indirect_cost_ABS + Indirect_cost_PRES) %>%
  select(-wage, -Hourly_wage, -presenteeism_wpwrk, -Indirect_cost_pres)




save(CohortB1, file ="C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/CohortB1.RData")
