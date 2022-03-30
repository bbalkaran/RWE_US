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

# 1. workspace -----

library(tidyverse)
library(magrittr)
library(openxlsx)
library(lubridate)
library(odbc)
library(dbplyr)
library(DBI)
library(kollekt)
options(scipen = 999)

# 2. medical claims -----
Enrollment_File <- read_csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Enrollment_File.csv")
Mx_Header <- read_csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Medical_Claims_Header.csv")
Mx_Service_Lines <- read_csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Medical_Claims_Service_Lines.csv")
Rx_Claims <- read_csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Pharmacy_Claims.csv")

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



# 2B. Clean header file -----

Mx_Header <- Mx_Header %>% filter(client_patient_id %in% Enrollment_File$client_patient_id)


Mx_H_2017 <- Mx_Header %>% filter(claim_date >= "2017-01-01" & claim_date <= "2017-12-31" )
Mx_H_2017$client_patient_id %>% n_distinct()
  # 415 patients


Mx_Service_Lines <- Mx_Service_Lines %>% filter(claim_id %in% Mx_H_2017$claim_id)
Mx_Service_Lines$client_patient_id %>% n_distinct() # 415
Mx_Service_Lines$service_to %>% range(na.rm = T)
Mx_Service_Lines <- Mx_Service_Lines %>% filter(service_from >= "2017-01-01" & service_from <= "2017-12-31" )
Mx_Service_Lines$client_patient_id %>% n_distinct() # 415
# # check with Jack's file 
# JR_Med <- read_csv("~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Migraine_Prev_cohort_Medical_JR.csv", 
#                    col_types = cols(X1 = col_skip(), 
#                                     claim_date = col_date(format = "%Y-%m-%d"),
#                                     file_date = col_date(format = "%Y-%m-%d")))
# JR_Med$client_patient_id %>% n_distinct() # 68 patients
# # all same patients?
# JR_Med %>% filter(!client_patient_id  %in% Migraine_Header_2017$client_patient_id)
# Migraine_Header_2017 %>% filter(!client_patient_id %in% JR_Med$client_patient_id) # yep, all same
# 
# # check different claim
# JR_Med %>% filter(!claim_id %in% Migraine_Header_2017$claim_id) %>% View() # only one different
# Migraine_Header_2017 %>% filter(!claim_id %in% JR_Med$claim_id) # all same


# 3. Pharmacy claims -----
Rx_Claims <- read_csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Pharmacy_Claims.csv")


# reformat  claims ... only run these once
Rx_Claims$patient_dob %<>%  as.Date('%m/%d/%Y')
Rx_Claims$date_prescription_written %<>% as.Date('%m/%d/%Y')
Rx_Claims$date_of_service %<>%  as.Date('%m/%d/%Y')
Rx_Claims$extract_date %<>%  as.Date('%m/%d/%Y')
Rx_Claims$date_authorized %<>%  as.Date('%m/%d/%Y')
Rx_Claims$received_date %<>%  as.Date('%m/%d/%Y')

Rx_Claims <- Rx_Claims %>% filter(is.na(reject_code_1) & is.na(reject_code_2) & 
                                    is.na(reject_code_3) & is.na(reject_code_4) & is.na(reject_code_5))# 129,752 claims
Rx_Claims <- Rx_Claims %>% filter(transaction_type == "PAID") # 108,367 claims
Rx_Claims$client_patient_id %>% n_distinct() # 680


         
# Inclusion_Rx_Claims <- Rx_Claims%>% filter(client_patient_id %in% Enrollment_File$client_patient_id) %>%
#   filter(ndc11 %in% inclusion_meds$code | ndc11 %in% migraine_NDCs$concept_code) 
Rx_Claims <- Rx_Claims %>% filter(date_of_service >= "2017-01-01" & date_of_service <= "2017-12-31") # 427 claims, 73 patients; without OHDSI NCDs
Rx_Claims$client_patient_id %>% n_distinct()    # 461




# 4. add in bridging file to claims-----

Bridge <- read_delim("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - KHDICT/LINKER/komodo/komodo_bridge_06202019.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

Bridge <- Bridge %>% mutate(client_patient_id = `Komodo ID`)


Mx_H_2017<- Mx_H_2017 %>% left_join(Bridge) 
Med_2017 <- Mx_H_2017 %>% full_join(Mx_Service_Lines)
Med_2017$client_patient_id %>% n_distinct() # 415

Rx_Claims <- Rx_Claims %>% left_join(Bridge) 
colnames(Rx_Claims) <- paste("p", colnames(Rx_Claims), sep = ".")
Rx_Claims <- Rx_Claims %>% rename(client_patient_id = p.client_patient_id,
                                  zkey = p.zkey)
Rx_Claims$client_patient_id %>% n_distinct() # 461
Rx_Claims$zkey %>% n_distinct()




# 5 determine linked patients -----
z <- dbConnect(odbc::odbc(), driver = "MySQL ODBC 5.3 ANSI Driver", 
               database = "NHWS", uid = "bridgetb", pwd = "bridgetb", server = "10.176.50.61", 
               pORt = 3306)
us_date <- dbGetQuery(z, "select * from char_all
                                 where variable like 'start_time'
                                 or variable like 'date'")
arrange_and_transpose_str <- function(dat){
  dat %>% arrange(zKey) %>% spread(variable, strvalue)
}

us_date <- arrange_and_transpose_str(us_date)
us_date <- us_date %>% mutate(START_TIME  = case_when(is.na(START_TIME) ~ DATE,
                                                      TRUE ~ START_TIME))
us_date <- us_date %>% filter(source == "us_2017")
us_date$START_TIME <- date(as.Date(us_date$START_TIME))
us_date <- us_date %>% select(-DATE, -uniqueid)
us_date  <- us_date %>% rename(zkey = zKey)


load("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/PhaseI_clean.RData")

dat_clean <- dat_clean %>% filter(zkey %in% Bridge$zkey)
dat_clean$source <- dat_clean$source %>% as.character() %>% str_trim("both")
dat_clean <- dat_clean %>% left_join(us_date, by = c("zkey", "source"))
rm(us_date)

rm(Bridge)

drop2 <- c("p.timestamp_authorized","p.group_id", 
           "p.diagnosis_code", "p.diagnosis_code_type",          
           "p.ndc11_original", "p.quantity_prescribed_original",
           "p.is_service", "p.prior_authorization_type_code",
           "p.is_compound_drug", "p.level_of_service",             
           "p.coupon_number", "p.coupon_type",                 
           "p.coupon_value_amount", "p.pharmacy_submitted_cost",       
           "p.patient_pay","p.copay_coinsurance",            
           "p.reject_code_1", "p.reject_code_2" ,"p.reject_code_3", 
           "p.reject_code_4","p.reject_code_5", "p.extract_date" )

Rx_Claims <- Rx_Claims %>% select(-drop2)

drop3 <- c("orig_claim_id", "assignment_benefits_indicator",   
           "release_of_information_indicator",   "diagnosis_e_code_1", 
           "diagnosis_e_code_2", "diagnosis_group", "p1", "p2",                              
           "p3", "p4",  "p5", "p6", "p7", "p8", "p9", "p10",                             
           "p11", "p12", "p13", "p14", "p15", "p16",                             
           "p17", "p18", "p19", "p20", "p21", "p22",                             
           "p23", "p24", "p25", "payment_type", "patient_amount_paid",
           "revenue_center_code", "revenue_code")

Med_2017 <- Med_2017 %>% select(-drop3)

b <- dat_clean %>% left_join(Med_2017)
c <- dat_clean %>% full_join(Rx_Claims)


b <- b %>% mutate(NHWSindex_date = START_TIME) 
b$client_patient_id %>% is.na() %>% table() 
b <- b %>% filter(!is.na(client_patient_id))
b$client_patient_id %>% n_distinct() #395
b$zkey %>% n_distinct() #395
b <- b %>% mutate(preNHWS = (NHWSindex_date - months(6)),
                  postNHWS = (NHWSindex_date + months(6)))
b <- b %>% filter((claim_date >= preNHWS & claim_date <= postNHWS) |
                    (date_of_service >= preNHWS & date_of_service <= postNHWS))
b$client_patient_id %>% n_distinct() #383

# allow 1 year windowaround NHWS date 
c <- c %>% mutate(NHWSindex_date = START_TIME) 
c$client_patient_id %>% is.na() %>% table() 
c <- c %>% filter(!is.na(client_patient_id))
c$client_patient_id %>% n_distinct() #461
c$zkey %>% n_distinct() #461
c <- c %>% mutate(preNHWS = (NHWSindex_date - months(6)),
             postNHWS = (NHWSindex_date + months(6)))
c <- c %>% filter(p.date_of_service >= preNHWS & p.date_of_service <= postNHWS)



NHWS_Med_Pharm <- b %>% full_join(c)
NHWS_Med_Pharm$client_patient_id %>% n_distinct() #538
NHWS_Med_Pharm$zkey %>% n_distinct() # 538
NHWS_Med_Pharm <- NHWS_Med_Pharm %>% group_by(client_patient_id, source) %>% filter(row_number(NHWSindex_date)==1) %>% 
  arrange(client_patient_id, source) 
CohortA2 <- NHWS_Med_Pharm #538


save(CohortA2,file = "C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Data to merge.RData")


# 493 patients  if we use claims bound within a 1 year window around NHWS date

# go to 0.1 for COhort A, any claim linked with NHWS, ignore code below


# df <- df %>%
#   group_by(card_id) %>%
#   arrange(purchase_date) %>%
#   mutate(diff = purchase_date - lag(purchase_date, default = first(purchase_date))) %>%
#   mutate(diff = round(diff/86400, digits = 2))
# 
# 
# Linked_Migraine_Header_2017 <- Linked_Migraine_Header_2017 %>% group_by(client_patient_id) %>% arrange(claim_date) %>% 
#   mutate(date_diff = claim_date - lag(claim_date)) %>% 
#   arrange(client_patient_id, claim_date, date_diff) 
# 
# Linked_Migraine_Header_2017$claim <- "m"
# Linked_Migraine_Header_2017 %>% select(client_patient_id, claim_id, claim_date, date_diff) %>% View()
# 
# 
# # linked Rx patients
# Linked_Inclusion_Rx_Claims <- Linked_Inclusion_Rx_Claims %>% group_by(client_patient_id) %>% arrange(date_of_service) %>% 
#   mutate(p.date.diff = date_of_service - lag(date_of_service)) %>% 
#   arrange(client_patient_id, date_of_service, p.date.diff)
# 
# Linked_Inclusion_Rx_Claims$claim <- "p"
# 
# 
# # merge header and Rx
# 
# Linked_Claims <- Linked_Migraine_Header_2017 %>% 
#   select(client_patient_id, zkey,  source, claim_id, claim_date) %>% #date_diff, claim) %>% 
#   full_join(select(Linked_Inclusion_Rx_Claims, client_patient_id, zkey, source, claim_id, 
#                   date_of_service)) %>% #p.date.diff, claim)) %>% 
#   arrange(client_patient_id, zkey, source, claim_date, date_of_service)
# write.xlsx(Linked_Claims, file= "~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Linked_Claims.xlsx")
# 
# 
# Linked_Claims_Inclusion_Identifier <- read_excel("C:/Users/balkaranb/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Linked_Claims_Inclusion_Identifier.xlsx", 
#                                                  col_types = c("numeric", "numeric", "text", 
#                                                                "text", "text", "date", "text", "text", 
#                                                                "date", "text", "numeric", "text"))
# 
# MG <- Linked_Claims_Inclusion_Identifier %>% filter(Keep == 1) # 541 claims
# MG$client_patient_id %>% n_distinct() # 73 patients
# MG$zkey %>% n_distinct() # 73 patients
# 
# 
# 
# #save(MG, file = "~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/PhaseII.RData")
# 
# load("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/PhaseII.RData")
# load("~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/PhaseII.RData")

