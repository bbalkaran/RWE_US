#########################################################
#  Script: 0.2_Xtabs_for_NPI.R
#  Purpose: NPI data from CMS
#  Data file: 
#  Author: Bridget Balkaran
#  Project: 161103897-1
#  Project Manager: Martine Maculaitis
#  Date Created: 9/12/19
#  Date Edited: 9/12/19
########################################################

library(tidyverse)
library(data.table)
library(magrittr)
library(openxlsx)

`%notin%` = function(x,y) !(x %in% y)
load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med_merged.RData")
med$discharge_date %<>% as.Date()
med$claim_date %<>% as.Date()
med$date_of_service %<>% as.Date()
med$service_from %<>% as.Date()
med$service_to %<>% as.Date()

med <- med %>% select(patient_id, claim_id, claim_type_code,
                      admit_type_code, discharge_status_code, admission_date,
                      discharge_date,  date_of_service, 
                      service_from,
                      service_to, attending_npi, billing_pr_npi, facility_npi,
                      place_of_service, 
                      procedure, ndc_code, Visit_type )

med <- med %>% distinct()


med <- med %>% filter(!is.na(patient_id)) #remove records with no patient id
med$service_from %>% is.na() %>% table() # 403,524 records with no service from 

#med <- med %>% filter(!is.na(service_from) #date_of_service)) 
# remove recorxs with no date of servive, # = 35469022 service lines



# look at range of dates: remove outliers
    # date of service
    # USE service from !!!!!!!!!!!!!!!!!!!! 11/11/19
med$service_from %>% summary()
med %>% filter(service_from < "2015-11-02") %>% select(service_from) %>%  unique() %>% View()
med <- med %>% filter(service_from >= "2000-01-01")
med %>% filter(service_from > "2018-07-28") %>% select(service_from) %>%  unique() %>% View()
med <- med %>% filter(service_from <= "2019-09-11")
# dates of service from 1914, 1915, 1920, 1970, 1990, 1992
# remove these --> likely DOB 

med$patient_id %>% n_distinct()
  med$claim_id %>% n_distinct()
# patinets = 219,602
# claims =7411319



# count those with P and  I claims 
df2 <- med %>% select(patient_id, claim_type_code) %>%
  group_by(patient_id) %>%
  count(claim_type_code)

claims <- df2 %>% spread( claim_type_code, n)

claims2 <- claims %>% 
  mutate(all_I = case_when(is.na(P) & is.na(`<NA>`) & I >0  ~ 1,
                           TRUE ~ 0))

Inst_claims_only <- claims2 %>% filter(all_I == 1) %>% 
  select(patient_id) %>% distinct() # patients with inst claims only
Inst_claims_only$patient_id %>% n_distinct() 
#save for later when comparing hosp vs non hosp
# 70,428 with inst claims only  

med_Inst <- med %>% filter(patient_id %in% Inst_claims_only$patient_id) %>%############################# save for NPIcollapsed == "Hospital"
  mutate(NPIcollapsed = "Hospital")
med_Inst$patient_id %>% n_distinct()
#70482
med_Inst$claim_id %>% n_distinct()
#966831

med <- med %>% filter(!patient_id %in% med_Inst$patient_id)

med$patient_id %>% n_distinct()
# 149,120 patients
med$claim_id %>% n_distinct()
# 6444489 claims

med_Inst <- med_Inst %>% filter(!Visit_type == "non_IG_visit")
med_Inst$patient_id %>% n_distinct()
#69259
med_Inst$claim_id %>% n_distinct()
#961072
rm(claims, claims2, df2, Inst_claims_only)
gc()



##############
# count patients with dischage codes as hosp

discharge_codes <- med %>% filter(!is.na(discharge_status_code)) %>%################################ save for NPIcollapsed == "Hospital"
  mutate(NPIcollapsed = "Hospital")
discharge_codes$patient_id %>% n_distinct
#39553
discharge_codes$claim_id %>% n_distinct
#1067873

med <- med %>% filter(!claim_id %in% discharge_codes$claim_id)
med$patient_id %>% n_distinct()
# 129308
med$claim_id %>% n_distinct()
#5376616

discharge_codes <- discharge_codes %>% filter(!Visit_type == "non_IG_visit")
discharge_codes$patient_id %>% n_distinct
#39488
discharge_codes$claim_id %>% n_distinct
#1065456

###############################
# count patients with admit type codes as hosp 
admit_codes <- med %>% filter(!is.na(admit_type_code)) %>%################################ save for NPIcollapsed == "Hospital"
  mutate(NPIcollapsed = "Hospital")
admit_codes$patient_id %>% n_distinct()
#324
admit_codes$claim_id %>% n_distinct()
#7903

med <- med %>% filter(!claim_id %in% admit_codes$claim_id)
med$patient_id %>% n_distinct()
#129133
med$claim_id %>% n_distinct()
# 5368713

admit_codes <- admit_codes %>% filter(!Visit_type == "non_IG_visit")
admit_codes$patient_id %>% n_distinct()
#318
admit_codes$claim_id %>% n_distinct()
# 7742
###################################
# count patients with place_of_service == 19|21|22|23|24|
hosp_pos_codes <- med %>% filter(place_of_service == 19|
                                   place_of_service == 21|
                                   place_of_service == 22|
                                   place_of_service == 23 |
                                   place_of_service == 24) %>%################################ save for NPIcollapsed == "Hospital"
  mutate(NPIcollapsed = "Hospital")
hosp_pos_codes$patient_id %>% n_distinct
#3715
hosp_pos_codes$claim_id %>% n_distinct()
#55266

med <- med %>% filter(!claim_id %in% hosp_pos_codes$claim_id)
med$patient_id %>% n_distinct()
# 126672
med$claim_id %>% n_distinct()
#5313447


hosp_pos_codes <- hosp_pos_codes %>%  filter(!Visit_type == "non_IG_visit")
hosp_pos_codes$patient_id %>% n_distinct
#3670
hosp_pos_codes$claim_id %>% n_distinct()
#54921
######


#                                                                                        
# hosp2<- data.table(select(med, patient_id, claim_id, claim_type_code, service_from, place_of_service,
#                           Visit_type))
# 
# 
# 
# # id consecutive dates
# df <- hosp2 %>% arrange(patient_id, service_from) %>%
#   group_by(factor(patient_id)) %>%
#   mutate(previous = lag(service_from, 1)) %>%
#   mutate(tae.1 = service_from - lag(service_from, 1))%>% 
#   ungroup() %>%
#   mutate(consecutive_flag = case_when(tae.1 == 1 ~ 1)) %>%
#   mutate(consecutive_flag = case_when(tae.1 == 1 ~ 1,
#                                       lead(tae.1 == 1)~1))
# # filter consecutive dates
# df2 <- df %>% group_by(patient_id) %>% 
#   filter(consecutive_flag == 1)
# 
# # only pts with consecutive dates!
# df3 <- df %>% filter(patient_id %in% df2$patient_id)
# 
# # fill these 
# df4 <- df3 %>% group_by(patient_id, service_from) %>% 
#   fill(consecutive_flag) %>% 
#   fill(consecutive_flag, .direction = "up")  %>%
#   group_by(patient_id, service_from, consecutive_flag)
# 
# df_4 <- df4 %>%  mutate(new_date = 
#                           case_when(is.na(previous) & 
#                                       consecutive_flag == 1 ~ service_from)) # RAN THIS OVERNIGHT TAKES A LONG TIME!!
# 
# table(df4$place_of_service,df4$claim_type_code, useNA = "ifany") %>% as.matrix()
# # check claim type and POS for iding consecutive visits. most of these are non-hospital
# # select out those without POS and 
# 
# save(df4, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Consectutive visits identifier.RData")
#  
# gc()
# rm(hosp2)
# gc()
# rm(df)

# select out those with continuous dates and with an IG visit --> counting these as hospitalizations
# df5 <- df4 %>% group_by(patient_id, service_from) %>% 
#   mutate(consecutive_dates_w_IG = case_when((Visit_type == "IG_J_NDC_Visit" | 
#                                               Visit_type == "IG_NDC_Visit") & 
#                                               consecutive_flag == 1 ~ "IG hospitalization",
#                                             (Visit_type == "Unspedified Jcode visit" &
#                                                consecutive_flag == 1) ~ "IG hospitalization",
#                                             consecutive_flag == 1 ~ "Hospitalization",
#                                             TRUE ~ "Other Visit"))
# 
# 
# df6 <- df5 %>% filter(!consecutive_dates_w_IG == "Other Visit")
# 
# df5 %>% group_by(consecutive_dates_w_IG) %>% summarise(n_patients= n_distinct(patient_id))
# 
# ID_IG_hospitalizatons <- df5 %>% select(patient_id, consecutive_dates_w_IG) %>%
#   group_by(patient_id) %>%
#   count(consecutive_dates_w_IG) %>%
#   spread( consecutive_dates_w_IG, n) 
# 
# 
# Patients_hosp <- ID_IG_hospitalizatons %>% filter(!is.na(Hospitalization)) %>%
#   filter(!is.na(`IG hospitalization`) & is.na(`Other Visit`)) 
# #7,612 patients with hospitalizations based on consecutive dates and IG claims
# 
# save(Patients_hosp, ID_IG_hospitalizatons, df5, 
#      file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Institutional_claims_2.RData")

# remove these non-IG visits
med <- med %>% filter(!Visit_type == "non_IG_Visit")
med$patient_id %>% n_distinct()
#126318
med$claim_id %>% n_distinct()
# 5305878 patients



#save(med, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med Claims for Site Identification.RData")
# go to 0.3_Xtabs_for_med_NPI.R


########################## add in NPI taxonomies
load("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/NPI_registry_deduped.RData")

med$attending_npi %>% is.na() %>% table()
med$billing_pr_npi %>% is.na() %>% table()
med$facility_npi %>% is.na() %>% table()

med <- med %>% mutate(NPI = case_when( (is.na(billing_pr_npi) &
                                          is.na(attending_npi)) ~ facility_npi,
                                       is.na(billing_pr_npi) ~ attending_npi,
                                       TRUE ~ billing_pr_npi))
med$NPI %>% is.na() %>% table()
# missing NPI Identifier
med_no_NPI <- med %>% filter(is.na(NPI)) # 1308293
med_no_NPI$place_of_service %>% table() # id by pos
med_no_NPI$patient_id %>% n_distinct()
#19901
med_no_NPI$claim_id %>% n_distinct()
#1125090

med <- med %>% filter(!claim_id %in% med_no_NPI$claim_id)
med$patient_id %>% n_distinct()
#121556
med$claim_id %>% n_distinct()
# 4180788

med_no_NPI <- med_no_NPI %>% mutate(NPIcollapsed =
                                      case_when(place_of_service == 0 ~ NA_character_,
                                                place_of_service == 1 ~ "POS Pharmacy",
                                                place_of_service == 3 ~ "POS School",
                                                place_of_service == 11 ~ "POS Office",
                                                place_of_service == 12 ~ "POS Home",
                                                place_of_service == 14 ~ "POS Group Home",
                                                place_of_service == 15 ~ "POS Mobile Unit",
                                                place_of_service == 17 ~ "POS Walk-in Retail Health Clinic",
                                                place_of_service == 18 ~ "POS Place of Employment-Worksite",
                                                place_of_service == 20 ~ "POS Urgent Care Facility",
                                                place_of_service == 31 ~ "POS Skilled Nursing Facility",
                                                place_of_service == 32 ~ "POS Nursing Facility",
                                                place_of_service == 41 ~ NA_character_,
                                                place_of_service == 49 ~ "POS independent Clinic",
                                                place_of_service == 52 ~ NA_character_,
                                                place_of_service == 60 ~ NA_character_,
                                                place_of_service == 61 ~ "POS Comprehensive Inpatient Rehabilitation Facility",
                                                place_of_service == 65 ~ NA_character_,
                                                place_of_service == 81 ~ "POS Independent Laboratory",
                                                place_of_service == 99 ~ NA_character_))
med_no_NPI <- med_no_NPI %>% filter(!is.na(NPIcollapsed))
med_no_NPI$patient_id %>% n_distinct()
#18201
med_no_NPI$claim_id %>% n_distinct()
#1089999

med_no_NPI$attending_npi %>% is.na() %>% table()
med_no_NPI$billing_pr_npi %>% is.na() %>% table()
med_no_NPI$facility_npi %>% is.na() %>% table()

###########################


attending_npi <-  med$attending_npi %>% unique()
billing_npi <- med$billing_pr_npi %>% unique()
facility_npi <- med$facility_npi %>% unique()
NPIreg <- NPIs %>% filter(NPI %in% attending_npi |
                                   NPI %in% billing_npi |
                                   NPI %in% facility_npi) %>% 
  select(NPI, provider_taxonomy_code, provider_taxonomy_desc) %>% na.omit()
rm(NPIs)
gc()

NPIreg <- NPIreg %>% distinct()

med2 <- med %>% left_join(NPIreg)

################## remove patients with mismatching txs

# table NPI x products at patient level
pts <- med2 %>% select(patient_id,claim_id, service_from, NPI, provider_taxonomy_desc, procedure, ndc_code )



# bring in OHDSI for identifying J codes and NDCs  
OHDSI <- read_delim("C:/Users/balkaranb/Kantar/Arunajadai, Srikesh (KH) - KHDICT/OHDSI/CONCEPT.csv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)
OHDSI_NDC <- OHDSI %>% filter(vocabulary_id == "NDC")
OHDSI_HCPCS <- OHDSI %>% filter(vocabulary_id == "HCPCS")

pts_Jcodes <- pts %>% select(procedure) %>% distinct()
pts_NDC <- pts %>% select(ndc_code) %>% distinct()

pts_combos<- pts %>% select(procedure, ndc_code) %>% distinct() #### these will be colum names for X tab

pts_combos <- pts_combos %>% mutate(procedure = 
                        case_when(!procedure %in% IG_jcodes ~ NA_character_,
                                  TRUE ~ procedure))



# NDC and J inclusion codes
IG_NDC <- read.xlsx("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Pfizer_IG_Open_Claims_Inclusion_Codes.xlsx")
IG_Codes <- IG_NDC %>% select(-X3)
rm(IG_NDC)
IG_Codes <- IG_Codes %>%
  mutate(ndc         = str_remove_all(NDC, "XX|-"), 
         ndc_length  = str_length(ndc),
         ndc9        = str_trunc(ndc, 9, "right", ellipsis = ""),
         ndc9_length = str_length(ndc9))
IG_NDC <- IG_Codes[["ndc"]]
IG_jcodes <- IG_Codes[-1,1] %>% 
  as.vector() %>% 
  na.omit()

NDC_Names <- read.xlsx("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/2019-08-26 Inclusion codes for IG - NDC, J, HCPCS, etc..xlsx")
NDC_Names$NDC <- NDC_Names$NDC %>% str_remove_all(pattern = "XX") %>% 
  str_remove_all(pattern = "-")

NDC_Names <- NDC_Names[-1,1:3]

NDC_Names <- NDC_Names[1:62, c("IG.Product", "NDC")] %>% na.omit() %>% 
  rename(ndc_code = NDC)

NDC_Names$IG.Product <-  NDC_Names$IG.Product %>% str_remove_all(" \\d\\dg") %>% 
  str_remove_all(" \\dg") %>%
  str_remove_all(" S/D 5*") %>%
  str_remove_all("Less IGA") %>%
  str_remove_all("\\.") %>%
  str_remove_all("\\d\\d") %>%
  str_remove_all(" \\d") %>% 
  str_remove_all(" \\%") %>%
  str_remove_all("\\%") %>%
  str_remove_all(" Liquid") %>%
  str_trim("right")




J_Names <- read.xlsx("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/2019-08-26 Inclusion codes for IG - NDC, J, HCPCS, etc..xlsx",
                     sheet = 2)


##  get codes for Jcodes
# jcodes <- OHDSI_HCPCS %>% filter(concept_code %in% IG_jcodes) 
# NDCs <- OHDSI_NDC %>% filter(concept_code %in% IG_NDC)



# Start with J code names 
J_Names <- J_Names %>% filter(J_Code %in% pts_combos$procedure) %>% 
  rename(procedure = J_Code)



pts_combos <- pts_combos %>% left_join(select(J_Names, procedure, Product_Name))

# then NDC names 

try <- pts_combos %>% left_join(NDC_Names) %>% 
  rename(NDC_product = IG.Product)

get_these_from_OHDSI <- try %>% filter(!is.na(ndc_code) & is.na(NDC_product)) %>% select(ndc_code) %>% unique()


NDC_codes_from_OHDSI <- OHDSI_NDC %>% select(concept_code, concept_name) %>% 
  filter(concept_code %in% get_these_from_OHDSI$ndc_code)


NDC_codes_from_OHDSI <- NDC_codes_from_OHDSI %>% 
  mutate(ndc_name = word(concept_name, -1),
         ndc_name = case_when(ndc_name == "liquid" ~ "Privigen",
                              TRUE ~ ndc_name)) %>% 
  rename(ndc_code = concept_code) 

NDC_codes_from_OHDSI$ndc_name <- NDC_codes_from_OHDSI$ndc_name %>% 
  str_remove_all(pattern = "\\[") %>% 
  str_remove_all(pattern = "\\]")

NDC_codes_from_OHDSI <- NDC_codes_from_OHDSI %>% filter(ndc_name == "Bivigam" | 
                                               ndc_name == "Carimune" |
                                               ndc_name == "Cuvitru"|
                                               ndc_name == "Flebogamma"|
                                               ndc_name == "Gammagard"|
                                               ndc_name == "Gammaked"|
                                               ndc_name == "Gammaplex"|
                                               ndc_name == "Gamunex" |
                                               ndc_name == "Octagam"|
                                               ndc_name == "Panzyga"|
                                               ndc_name == "Privigen")


NDC_codes_from_OHDSI <- NDC_codes_from_OHDSI %>% 
  rename(IG.Product = ndc_name)

NDC_Names <- NDC_Names %>% full_join(select(NDC_codes_from_OHDSI, ndc_code, IG.Product))

pts_combos <- pts_combos %>% left_join(NDC_Names) %>% 
  select(procedure, Product_Name, ndc_code, IG.Product)

pts_combos <- pts_combos %>% 
  filter(!is.na(Product_Name)& !is.na(IG.Product))


rm(get_these_from_OHDSI, IG_Codes, IG_Jcodes,  J_Names,
   NDC_codes_from_OHDSI, NDC_J_Names, NDC_Names,
   OHDSI, OHDSI_HCPCS, OHDSI_NDC, try, med, 
   IG_NDC, ndc_code, attending_npi, billing_npi, facility_npi)

gc()

# now name treatments in med claims
Patient_Treatments_By_NPI <-  pts %>% left_join(pts_combos)
Patient_Treatments_By_NPI$patient_id %>% n_distinct() # 131,578
Patient_Treatments_By_NPI$J_NDC_treatment <- paste(Patient_Treatments_By_NPI$Product_Name, 
                                                   Patient_Treatments_By_NPI$IG.Product, sep = " + ")

# unique combos and treatments 
treatments <- Patient_Treatments_By_NPI$J_NDC_treatment %>% 
  unique()





Patient_Treatments_By_NPI$J_NDC_type <- paste(Patient_Treatments_By_NPI$J_code_type, 
                                              Patient_Treatments_By_NPI$NDC_code_type, sep = " + ")
treatments_types <- Patient_Treatments_By_NPI %>% #select(J_NDC_treatment, J_NDC_type) %>% 
  distinct()

Patient_Treatments_By_NPI <- Patient_Treatments_By_NPI %>% distinct()

med2 <- med2 %>% left_join(select(Patient_Treatments_By_NPI, 
                                patient_id, claim_id, service_from, 
                                NPI, procedure, ndc_code, J_NDC_treatment))
med2 <- med2 %>% filter(!J_NDC_treatment == "NA + NA")
med2$patient_id %>% n_distinct()
#74755
med2$claim_id %>% n_distinct()
#2071439

med2 <- med2 %>% mutate(J_NDC_treatment2 = case_when(J_NDC_treatment == "Privigen + NA" |
                                                  J_NDC_treatment == "Privigen + Privigen" |
                                                  J_NDC_treatment == "IG IV, lyophilized, NOS + Privigen" |
                                                  J_NDC_treatment == "IG IV, non-lyophilized, NOS + Privigen" |
                                                  J_NDC_treatment == "NA + Privigen" ~ "Privigen",
                                                J_NDC_treatment == "Hyqvia + HyQvia" |
                                                  J_NDC_treatment == "Hyqvia + NA" |
                                                  J_NDC_treatment == "NA + HyQvia" ~ "Hyqvia",
                                                J_NDC_treatment == "Gamunex-c/ Gammaked + Gamunex" | 
                                                  J_NDC_treatment == "IG IV, lyophilized, NOS + Gamunex"|
                                                  J_NDC_treatment == "IG IV, non-lyophilized, NOS + Gamunex" |
                                                  J_NDC_treatment == "NA + Gamunex" |
                                                  J_NDC_treatment == "IV IG + Gamunex" ~ "Gamunex",
                                                J_NDC_treatment == "Gammaplex + Gammaplex" |
                                                  J_NDC_treatment == "Gammaplex + NA"|
                                                  J_NDC_treatment == "IG IV, lyophilized, NOS + Gammaplex"|
                                                  J_NDC_treatment == "IG IV, non-lyophilized, NOS + Gammaplex" |
                                                  J_NDC_treatment == "NA + Gammaplex" ~ "Gammaplex", 
                                                J_NDC_treatment == "Gammagard + Gammagard"|
                                                  J_NDC_treatment == "Gammagard + NA"|
                                                  J_NDC_treatment == "IG IV, lyophilized, NOS + Gammagard" |
                                                  J_NDC_treatment == "IG IV, non-lyophilized, NOS + Gammagard" |
                                                  J_NDC_treatment == "NA + Gammagard"|
                                                  J_NDC_treatment == "IV IG + Gammagard" ~ "Gammagard",
                                                J_NDC_treatment == "Flebogamma/ Flebogamma DIF + Flebogamma" |
                                                  J_NDC_treatment == "Flebogamma/ Flebogamma DIF + NA" |
                                                  J_NDC_treatment == "IG IV, non-lyophilized, NOS + Flebogamma"|
                                                  J_NDC_treatment == "NA + Flebogamma"|
                                                  J_NDC_treatment == "IG IV, lyophilized, NOS + Flebogamma" ~ "Flebogamma",
                                                J_NDC_treatment == "Cuvitru + Cuvitru" |
                                                  J_NDC_treatment == "Cuvitru + NA"|
                                                  J_NDC_treatment == "NA + Cuvitru" ~ "Cuvitru",
                                                J_NDC_treatment == "Bivigam + Bivigam" |
                                                  J_NDC_treatment == "Bivigam + NA"|
                                                  J_NDC_treatment == "IG IV, lyophilized, NOS + Bivigam"|
                                                  J_NDC_treatment == "IG IV, non-lyophilized, NOS + Bivigam" ~ "Bivigam",
                                                J_NDC_treatment == "Gamunex-c/ Gammaked + Gammaked"|
                                                  J_NDC_treatment == "IG IV, lyophilized, NOS + Gammaked" ~ "Gammaked",
                                                J_NDC_treatment == "IG IV, lyophilized, NOS + Carimune" ~ "Carimune",
                                                J_NDC_treatment == "Hizentra + Hizentra" |
                                                  J_NDC_treatment == "Hizentra + NA"|
                                                  J_NDC_treatment == "Hizentra + NA"|
                                                  J_NDC_treatment == "NA + Hizentra"~ "Hizentra",
                                                J_NDC_treatment == "Gamunex-c/ Gammaked + NA" ~ "Gamunex-c/ Gammaked",
                                                J_NDC_treatment == "IG IV, lyophilized, NOS + Octagam"|
                                                  J_NDC_treatment == "IG IV, non-lyophilized, NOS + Octagam"|
                                                  J_NDC_treatment == "NA + Octagam"|
                                                  J_NDC_treatment == "Octagam + NA"|
                                                  J_NDC_treatment == "Octagam + Octagam" ~ "Octagam",
                                                J_NDC_treatment == "IG IV, non-lyophilized, NOS + Panzyga"~ "Panzyga",
                                                J_NDC_treatment == "IG IV, lyophilized, NOS + NA"|
                                                  J_NDC_treatment == "IG IV, non-lyophilized, NOS + NA"|
                                                  J_NDC_treatment == "IV IG + NA"~ "NOC",
                                                J_NDC_treatment == "Vivaglobin + NA" ~ "Vivaglobin",
                                                TRUE ~ J_NDC_treatment))
drop_txs <- c("NA + NA", "Gammagard + Gamunex" , "Privigen + Gammagard",
              "Gammagard + Flebogamma","Flebogamma/ Flebogamma DIF + Carimune",
              "Gammaplex + Gammaked" ,"Privigen + Hizentra","Gamunex-c/ Gammaked + Privigen",        
              "Gammagard + Privigen","Gamunex-c/ Gammaked + Gammagard" ,"Hyqvia + Gamunex",
              "Gamunex-c/ Gammaked + Gammaplex" ,"Gammagard + Gammaked","Gammagard + Hizentra",                  
              "Privigen + Flebogamma","Gamunex-c/ Gammaked + Flebogamma","Privigen + Gamunex",                    
              "Vivaglobin + Gamunex","Privigen + Gammaked", "Flebogamma/ Flebogamma DIF + Gammagard",
              "Octagam + Gamunex", "Gammagard + Carimune", "Flebogamma/ Flebogamma DIF + Octagam",  
              "Gamunex-c/ Gammaked + Octagam","Gammagard + Octagam", "Gammaplex + Octagam",                   
              "Gammagard + HyQvia","Hizentra + Gammagard","Privigen + Octagam",
              "Flebogamma/ Flebogamma DIF + Privigen","Octagam + Gammagard",
              "Octagam + Gammaplex","Octagam + Flebogamma","Gammaplex + Gammagard",                 
              "IG IV, non-lyophilized, NOS + Hizentra","Privigen + Bivigam","Gamunex-c/ Gammaked + Bivigam",         
              "Hizentra + Bivigam","Hizentra + Gammaplex","Bivigam + Privigen",
              "Gammaplex + Privigen","Octagam + Privigen","Octagam + Carimune",                    
              "IG IV, non-lyophilized, NOS + Cuvitru","Flebogamma/ Flebogamma DIF + Gamunex",  
              "Gammagard + Gammaplex", "Gammaplex + Hizentra","Hizentra + Panzyga",                    
              "Hizentra + Privigen","Privigen + Panzyga", "Octagam + Bivigam" ,                    
              "Gamunex-c/ Gammaked + Carimune","Gammaplex + Gamunex","Cuvitru + Hizentra",                    
              "Gammagard + Bivigam" ,"Gamunex-c/ Gammaked + Hizentra","Gammaplex + Carimune" ,                 
              "Vivaglobin + HyQvia","IG IV, non-lyophilized, NOS + HyQvia" ,"Hizentra + Cuvitru",                    
              "Bivigam + Gammagard", "Bivigam + Gamunex","Hyqvia + Gammagard","Octagam + Panzyga",                     
              "Hyqvia + Hizentra", "Gammaplex + HyQvia", "Privigen + Carimune", "Bivigam + Carimune", 
              "Flebogamma/ Flebogamma DIF + Gammaked", "Hizentra + HyQvia")

med2 <- med2 %>% filter(!J_NDC_treatment2 %in% drop_txs)
med2$patient_id %>% n_distinct()
# 74693
med2$claim_id %>% n_distinct()
# 2069301


# ID by NPI taxonomy #####################

med2 <- med2 %>% mutate(NPIcollapsed = 
                          case_when(provider_taxonomy_desc == "Hospitals General Acute Care Hospital" ~ "Hospital",
                                    provider_taxonomy_desc == "Agencies Public Health or Welfare" ~ NA_character_,
                                    provider_taxonomy_desc ==  "Agencies Voluntary or Charitable" ~ NA_character_,
                                    provider_taxonomy_desc ==  "Allopathic &Osteopathic Physicians Dermatology Procedural Dermatology" ~ NA_character_, 
                                    provider_taxonomy_desc ==  "Allopathic and Osteopathic Physicians Advanced Heart Failure and Transplant Cardiology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Allergy and Immunology" ~ "Allergy and Immunology",
                                    provider_taxonomy_desc ==  "Allopathic and Osteopathic Physicians Allergy and Immunology Allergy" ~ "Allergy and Immunology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Allergy and Immunology Clinical and Laboratory Immunology" ~ "Allergy and Immunology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Anesthesiology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Anesthesiology Critical Care Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Anesthesiology Pain Medicine" ~ NA_character_, 
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Anesthesiology Pediatric Anesthesiology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Colon and Rectal Surgery" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Dermatology" ~ NA_character_, 
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Dermatology Dermapathology" ~  NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Dermatology Pediatric Dermatology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Dermatology MOHS-Micrographic Surgery" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Emergency Medicine Emergency Medical Services" ~ 'Hospital',
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Emergency Medicine Medical Toxicology" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Emergency Medicine Pediatric Emergency Medicine" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Family Medicine" ~ "Family Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Family Medicine Adolescent Medicine" ~ "Family Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Family Medicine Adult Medicine" ~ "Family Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Family Medicine Geriatric Medicine" ~ "Family Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Family Medicine Hospice and Palliative Medicine" ~ "Family Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians General Practice" ~ "General Practice",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Hospitalist" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Addiction Medicine" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Adolescent Medicine" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Allergy and Immunology" ~ "Allergy and Immunology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Cardiovascular Disease" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Bariatric Medicine" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Cardiovascular Disease" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Clinical and Laboratory Immunology" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Clinical Cardiatric Electrophysiology" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Colon and Rectal Surgery" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Critical Care Medicine" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Endocrinology Diabetes and Metabolism" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Gastroenterology" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Hematology" ~ "Hematology and Oncology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Hematology and Oncology"  ~ "Hematology and Oncology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Hepatology" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Infectious Disease" ~ "Infectious Disease",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Interventional Cardiology" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Medical Oncology" ~ "Hematology and Oncology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Nephrology" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Pulmonary Disease" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Rheumatology" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Sleep Medicine" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Transplant Hepatology" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Neurological Surgery" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Neuromusculoskeletal Medicine and OMM" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Neuromusculoskeletal Medicine Sports Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Nuclear Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Nuclear Medicine Nuclear Imaging and Therapy" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Obstetrics and Gynecology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Obstetrics and Gynecology Gynecologic Oncology" ~ "Hematology and Oncology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Obstetrics and Gynecology Gynecology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Obstetrics and Gynecology Maternal and Fetal Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Obstetrics and Gynecology Obstetrics" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Obstetrics and Gynecology Reproductive Endocrinology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Ophthalmic Plastic and Reconstructive Surgery" ~ NA_character_, 
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Ophthalmology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Ophthalmology Dental Providers Dentist" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Ophthalmology Dental Providers Dentist Oral and Maxillofacial Surgery" ~ NA_character_, 
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Ophthalmology Glaucoma Specialist"~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Oral and Maxillofacial Surgery" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Orthopedic Surgery" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Orthopedic Surgery Adult Reconstructive Orthopedic Surgery" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Orthopedic Surgery Foot and Ankle Surgery" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Orthopedic Surgery Hand Surgery" ~ NA_character_, 
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Orthopedic Surgery Orthopedic Surgery of the Spine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Orthopedic Surgery Orthopedic Trauma" ~ NA_character_, 
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Orthopedic Surgery Sports Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology" ~ NA_character_, 
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology Facial Plastic Surgery" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology Otolaryngic Allergy" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology Otology & Neurotology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology Pediatric Otolaryngology" ~ NA_character_, 
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology Plastic Surgery within the Head and Neck" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pain Medicine Interventional Pain Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pain Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Anatomic Pathology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Anatomic Pathology and Clinical Pathology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Clinical Pathology" ~ NA_character_, 
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Clinical Pathology Laboratory Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Cytopathology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Dermapathology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Hematology" ~ NA_character_, 
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Immunopathology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Molecular Genetic Pathology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Neuropathology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics"  ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Adolescent Medicine" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Developmental–Behavioral Pediatrics" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Neonatal-Perinatal Medicine" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Neurodevelopmental Disabilities" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Allergy and Immunology" ~ "Allergy and Immunology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Cardiology" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatric Critical Care Medicine" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Emergency Medicine" ~ "Hosptial",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Endocrinology" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatric Gastroenterology" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Hematology-Oncology" ~ "Hematology and Oncology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Infectious Diseases" ~ "Infectious Disease",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Nephrology" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Pulmonology" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Rheumatology" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Transplant Hepatology" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Sleep Medicine" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Physical Medicine and Rehabilitation" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Physical Medicine and Rehabilitation Hospice and Palliative Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Physical Medicine and Rehabilitation Neuromuscular Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Physical Medicine and Rehabilitation Pain Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Physical Medicine and Rehabilitation Pediatric Rehabilitation Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Physical Medicine and Rehabilitation Spinal Cord Injury Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Physical Medicine and Rehabilitation Sports Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Physicians Plastic Surgery" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Plastic Surgery Surgery of the Hand" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Preventive Medicine Public Health and General Preventive Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Preventive Medicine Undersea and Hyperbaric Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Child and Adolescent Psychiatry" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Clinical Neurophysiology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Diagnostic Neuroimaging" ~NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Neurocritical Care" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Neurology" ~ "Neurology", 
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Neurology with Special Qualifications in Child Neurology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Neuromuscular Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Pain Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Sleep Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Vascular Neurology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Radiology Diagnostic Radiology" ~ "Radiology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Radiology Radiation Oncology" ~ "Hematology and Oncology",
                                    provider_taxonomy_desc == "Radiology Vascular and Interventional Radiology" ~ "Radiology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Surgery" ~ "Hospital",
                                    provider_taxonomy_desc ==  "Allopathic and Osteopathic Physicians Pediatrics Surgery Pediatric Surgery" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Surgery Plastic and Reconstructive Surgery" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Surgery Surgery of the Hand" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Surgery Surgical Critical Care" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Surgery Surgical Oncology" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Surgery Vascular Surgery" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Thoracic Surgery (Cardiothoracic Vascular Surgery)" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Transplant Surgery" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Urology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Urology Pediatric Urology" ~ NA_character_,
                                    provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic-Center Ambulatory Surgical" ~ "Hospital",
                                    provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic-Center Radiology Mammography" ~ "Hospital",
                                    provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic-Center Radiology Mobile Mammography" ~ "Hospital",
                                    provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic Center Mental Health" ~ "Hospital",
                                    provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic Center Multi-Specialty" ~ "Hospital",
                                    provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic Center Physical Therapy" ~ "Hospital",
                                    provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic Center Radiology" ~ "Hospital",
                                    provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic Center Rehabilitation" ~ "Hospital",
                                    provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic Center Rehabilitation Comprehensive Outpatient Rehabilitation Facility (CORF)" ~ "Hospital",
                                    provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic Center Rural Health" ~ "Hospital",
                                    provider_taxonomy_desc == "Ambulatory Health Care Facilities End-Stage Renal Disease (ESRD) Treatment" ~ "Hospital",
                                    provider_taxonomy_desc == "Ambulatory Health Care Facilities Federally Qualified Health Center (FQHC)" ~ "Hospital",
                                    provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist" ~ NA_character_,
                                    provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist Addiction (Substance Abuse Disorder)"~ NA_character_,
                                    provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist Clinical" ~ NA_character_,
                                    provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist Clinical Child and Adolescent" ~ NA_character_,
                                    provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist Cognitive and Behavioral" ~ NA_character_,
                                    provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist Counseling" ~ NA_character_,
                                    provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist Health" ~ NA_character_,
                                    provider_taxonomy_desc == "Behavioral Health and Social Service Providers Social Worker Clinical" ~ NA_character_,
                                    provider_taxonomy_desc == "Chiropractic Providers Chiropractor" ~ NA_character_,
                                    provider_taxonomy_desc == "Chiropractic Providers Chiropractor Occupational Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Chiropractic Providers Chiropractor Rehabilitation" ~ NA_character_,
                                    provider_taxonomy_desc == "Dietary and Nutritional Service Providers Dietician Registered" ~ NA_character_,
                                    provider_taxonomy_desc == "Dietary and Nutritional Service Providers Dietician Registered Nutrition Pediatric" ~ NA_character_,
                                    provider_taxonomy_desc == "Eye and Vision Service Providers Optometrist" ~ NA_character_,
                                    provider_taxonomy_desc == "Eye and Vision Service Providers Optometrist Corneal and Contact Management" ~ NA_character_,
                                    provider_taxonomy_desc == "Eye and Vision Service Providers Technician Technologist Optician" ~ NA_character_,
                                    provider_taxonomy_desc == "Female Pelvic Medicine and Reconstructive Surgery" ~ NA_character_,
                                    provider_taxonomy_desc == "Group Single-Specialty" ~ NA_character_,
                                    provider_taxonomy_desc == "Hospital Units Medicare Defined Swing Bed Unit" ~ "Hospital",
                                    provider_taxonomy_desc == "Hospital Units Psychiatric Unit" ~ "Hospital",
                                    provider_taxonomy_desc == "Hospital Units Rehabilitation Unit" ~ "Hospital",
                                    provider_taxonomy_desc == "Hospitals General Acute Care Hospital Children" ~ "Hospital",
                                    provider_taxonomy_desc == "Hospitals General Acute Care Hospital Critical Access" ~ "Hospital",
                                    provider_taxonomy_desc == "Hospitals Long Term Care Hospital" ~ "Hospital",
                                    provider_taxonomy_desc == "Hospitals Psychiatric Hospital" ~ "Hospital",
                                    provider_taxonomy_desc == "Hospitals Rehabilitation Hospital" ~ "Hospital",
                                    provider_taxonomy_desc == "Hospitals Special(ty) Hospital" ~ "Hospital",
                                    provider_taxonomy_desc == "Internal Medicine Hospice and Palliative Medicine" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Laboratories Clinical Medical Laboratory" ~ "Laboratory",
                                    provider_taxonomy_desc == "Laboratories Physiological Laboratory" ~ "Laboratory",
                                    provider_taxonomy_desc == "Nursing and Custodial Care Facilities Nursing Facility" ~ "Nursing Facility",
                                    provider_taxonomy_desc == "Nursing and Custodial Care Facilities Skilled Nursing Facility" ~ "Nursing Facility",
                                    provider_taxonomy_desc == "Physician Assistants  and Advanced Practice Nursing Providers Clinical Nurse Specialist" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Clinical Nurse Specialist Acute Care" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Clinical Nurse Specialist Adult Health" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Clinical Nurse Specialist Family Health" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Clinical Nurse Specialist Oncology" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Clinical Nurse Specialist Oncology Pediatrics" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Clinical Nurse Specialist Pediatrics" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Anesthetist Certified Registered" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == 'Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner' ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Acute Care"  ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Adult Health" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Family" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Gerontology" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Neonatal" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Obstetrics and Gynecology" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Pediatrics" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Perinatal" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Primary Care" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Psychiatric Mental Health" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Women’s Health" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Physician Assistant" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Physician Assistant Medical" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Physician Assistant Surgical" ~ "Hospital",
                                    provider_taxonomy_desc == "Podiatric Medicine and Surgery Service Providers Podiatrist" ~ NA_character_,
                                    provider_taxonomy_desc == "Podiatric Medicine and Surgery Service Providers Podiatrist Foot and Ankle Surgery" ~ NA_character_,
                                    provider_taxonomy_desc == "Podiatric Medicine and Surgery Service Providers Podiatrist Primary Podiatric Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Respiratory Developmental Rehabilitative and Restorative Service Providers Occupational Therapist" ~ NA_character_,
                                    provider_taxonomy_desc == "Respiratory Developmental Rehabilitative and Restorative Service Providers Occupational Therapist Feeding Eating &Swallowing" ~ NA_character_,
                                    provider_taxonomy_desc == "Respiratory Developmental Rehabilitative and Restorative Service Providers Occupational Therapist Hand" ~ NA_character_,
                                    provider_taxonomy_desc == "Respiratory Developmental Rehabilitative and Restorative Service Providers Occupational Therapist Pediatrics" ~ NA_character_,
                                    provider_taxonomy_desc == "Respiratory Developmental Rehabilitative and Restorative Service Providers Physical Therapist" ~ NA_character_,
                                    provider_taxonomy_desc == "Respiratory Rehabilitative and Restorative Service Providers Physical Therapist Sports" ~ NA_character_,
                                    provider_taxonomy_desc == "Speech Language and Hearing Service Providers" ~ NA_character_,
                                    provider_taxonomy_desc == "Speech Language and Hearing Service Providers Audiologist" ~ NA_character_,
                                    provider_taxonomy_desc == "Transportation Services Ambulance" ~ NA_character_,
                                    provider_taxonomy_desc == "Transportation Services Ambulance Air Transport" ~ NA_character_,
                                    provider_taxonomy_desc == "Transportation Services Ambulance Land Transport" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Emergency Medicine" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Family Medicine Sports Medicine"  ~ "Family Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Geriatric Medicine" ~ "Internal Medicine",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Nuclear Medicine Nuclear Cardiology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology Otolaryngology/Facial Plastic Surgery" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology Otology &Neurotology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pain Medicine Pain Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Blood Banking and Transfusion Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Adolescent Medicine" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Critical Care Medicine"  ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Gastroenterology" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Physical Medicine and Rehabilitation Pediatric Rehabilitation Medicine" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Physical Medicine and Rehabilitation Sports Medicine" ~ NA_character_,
                                    provider_taxonomy_desc ==  "Allopathic and Osteopathic Physicians Plastic Surgery" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Plastic Surgery Plastic Surgery Within the Head and Neck" ~"Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Plastic Surgery Surgery of the Hand" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Preventive Medicine  Preventive Medicine Occupational Environmental Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Preventive Medicine Public Health and General Preventive Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Preventive Medicine Undersea and Hyperbaric Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology" ~ "Neurology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Child and Adolescent Psychiatry" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Clinical Neurophysiology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Diagnostic Neuroimaging" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Neurocritical Care" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Neurology" ~ "Neurology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Neurology with Special Qualifications in Child Neurology" ~ "Neurology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Neuromuscular Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Pain Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Psychosomatic Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Sleep Medicine" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Vascular Neurology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Radiology Diagnostic Radiology" ~ "Radiology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Radiology Radiation Oncology" ~ "Radiology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Radiology Vascular and Interventional Radiology" ~ "Radiology",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Surgery" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Surgery Pediatric Surgery" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Surgery Plastic and Reconstructive Surgery" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Surgery Surgery of the Hand" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Surgery Surgical Critical Care" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Surgery Surgical Oncology" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Surgery Vascular Surgery" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Thoracic Surgery (Cardiothoracic Vascular Surgery)" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Transplant Surgery" ~ "Hospital",
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Urology" ~ NA_character_,
                                    provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Urology Pediatric Urology" ~ "Pediatrics",
                                    provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist Prescribing (Medical)" ~ NA_character_,
                                    provider_taxonomy_desc == "Chiropractic Providers Chiropractor Orthopedic" ~ NA_character_,
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Clinical Nurse Specialist" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Midwife Certified Nurse" ~ "Physician Assistant and Advanced Practice Nursing",
                                    provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Pediatrics Critical Care" ~ "Physician Assistant and Advanced Practice Nursing",
                                    TRUE ~ provider_taxonomy_desc))

table(med2$NPIcollapsed, med2$place_of_service)
rm(Patient_Treatments_By_NPI, NPIreg, pts, pts_combos, pts_Jcodes, pts_NDC)

# rejoin admit codes, discharge codes, hospital_POS_codes, med_inst, med_no_NPI,  

Mx <- med2 %>% full_join(admit_codes) %>% full_join(discharge_codes) %>% 
  full_join(med_Inst) %>% full_join(med_no_NPI) 

mxDME <- Mx %>% filter(NPIcollapsed ==  "Suppliers Durable Medical Equipment and Medical Supplies" |
                      NPIcollapsed == "Suppliers Durable Medical Equipment and Medical Supplies Oxygen Equipment and Supplies")
mxDMEpts <- mxDME %>% select(patient_id) %>% distinct() 
mxDME2 <- mxDME %>% distinct()
########################## bring in pharmacy claims 



rx <- readRDS('C:/Users/BalkaranB/Kantar/Huynh, Stephanie (KHNYG HO) - Pfizer - Immunoglobulin/Data/Clean_Rx/IG_rx_claims_2.RData')
load("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/NPI_registry_deduped.RData")
rx <- rx %>% filter(transaction_type == "PAID")
rx_sample$reject_code_1 %>% table() # E9 = missing provider ID, all other reject codes all missing
rx <- rx %>% filter(is.na(reject_code_1))
rx$ndc11 <- rx$ndc11 %>% str_remove_all("XX")

rx$pharmacy_npi %>% is.na() %>% table(useNA = "ifany") %>% prop.table()
rx$pharmacist_npi %>% is.na() %>% table(useNA = "ifany") %>% prop.table()
rx$primary_care_npi %>% is.na() %>% table(useNA = "ifany") %>% prop.table()
rx$prescriber_npi %>% is.na() %>% table(useNA = "ifany") %>% prop.table()
 # least to most missing -> prescriber npi, pharmacy npi, primary care npi, pharmacist npi
rx <- rx %>% mutate(NPI = case_when(is.na(prescriber_npi) & is.na(pharmacy_npi) ~ pharmacist_npi,
                              is.na(pharmacist_npi) ~ pharmacy_npi, 
                              TRUE ~ NA_real_))

rx <- rx %>% select(patient_id, claim_id, date_of_service, prescription_id, ndc11,
              place_of_service, pharmacy_npi, pharmacist_npi, prescriber_npi, 
              primary_care_npi, fill_number,
              days_supply, quantity_dispensed, unit_of_measure, IG_visit, NPI)

NPIs <- NPIs %>% filter(NPI %in% rx$NPI) %>% distinct()

rx2 <- rx %>% filter(patient_id %in% mxDMEpts$patient_id)

rx2$patient_id %>% n_distinct()
#1252
rx2$claim_id %>% n_distinct()
#38751

rx3 <- rx2 %>% left_join(mxDME2, by = "patient_id") %>% 
  group_by(patient_id) %>% 
  mutate(Window30d = service_from + days(30)) %>% 
  filter(date_of_service.x >= service_from & date_of_service.x <= Window30d) %>% 
  mutate(daysToPharm = date_of_service.x - service_from) 

rx3 <- rx3 %>% 
  select(patient_id, NPI.x, claim_id.x, claim_id.y, date_of_service.x, ndc11,
         place_of_service.x, fill_number, days_supply,
         quantity_dispensed, service_from, place_of_service.y, J_NDC_treatment2,
         NPIcollapsed, Window30d, daysToPharm) %>% distinct()  %>% 
  rename(NPI = NPI.x)
rx3$NPI <- rx3$NPI %>% as.character()

rx3$patient_id %>% n_distinct()

rx3 <- rx3 %>% left_join(select(NPIs, NPI, provider_taxonomy_desc))
rx3$provider_taxonomy_desc %>% table()
# Suppliers Durable Medical Equipment and Medical Supplies                     all pharmacy sites 
# 3922
# Suppliers Durable Medical Equipment and Medical Supplies Oxygen Equipment and Supplies 
# 2 
# Suppliers Pharmacy 
# 2 
# Suppliers Pharmacy Home Infusion Therapy Pharmacy 
# 54
# Suppliers Pharmacy Mail Order Pharmacy 
# 4 
# Suppliers Pharmacy Specialty Pharmacy 
# 4


rx3 <- rx3 %>% select(patient_id, claim_id.x, claim_id.y, date_of_service.x, 
               ndc11, place_of_service.x, fill_number, days_supply, quantity_dispensed,
               service_from, J_NDC_treatment2, daysToPharm, NPIcollapsed) %>% 
  rename( p.claim_id = claim_id.x,
          p.ndc = ndc11,
          p.place_of_service = place_of_service.x,
          claim_id = claim_id.y,
          p.date_of_service = date_of_service.x) %>% distinct() %>% ungroup()
rx3$NPIcollapsed %>% table()



##########################
# bring pharmacy claims back into Med
IVIG <- Mx %>% left_join(rx3)
IVIG$patient_id %>% n_distinct()
# 180154
IVIG$claim_id %>% n_distinct()
#  5193569


df <- IVIG %>% filter(!is.na(NPIcollapsed)) 
df$patient_id %>% n_distinct()
# 164431
df$claim_id %>% n_distinct()
#4389544

df2 <- df %>% select(patient_id, NPIcollapsed) %>%
  group_by(patient_id) %>%
  count(NPIcollapsed)

df3 <- df2 %>% spread( NPIcollapsed, n)

df3 <- df3 %>% mutate(Site_designation = 
                 case_when(is.na( `Agencies Home Health`) & is.na(`Agencies Hospice Care Community Based`) &
                           is.na(`Allergy and Immunology`) & is.na(`Family Medicine`) &
                             is.na(`General Practice`) & is.na(`Hematology and Oncology`) &
                             !is.na(Hospital) & is.na(`Infectious Disease`) & is.na(`Internal Medicine`) &
                             is.na(Laboratory) & is.na(Neurology) & is.na(Pediatrics) &
                             is.na(`Physician Assistant and Advanced Practice Nursing`) &
                             is.na(`POS Comprehensive Inpatient Rehabilitation Facility`) &
                             is.na(`POS Group Home`) & is.na(`POS Home`) & is.na(`POS independent Clinic`) &
                             is.na(`POS Mobile Unit`) & is.na(`POS Nursing Facility`) & is.na(`POS Office`) &
                             is.na(`POS Pharmacy`) & is.na(`POS Place of Employment-Worksite`) & 
                             is.na(`POS School`) & is.na(`POS Skilled Nursing Facility`) &
                             is.na(`POS Urgent Care Facility`) & is.na(`POS Walk-in Retail Health Clinic`) &
                             is.na(Radiology) & is.na(`Suppliers Durable Medical Equipment and Medical Supplies`) &
                             is.na(`Suppliers Durable Medical Equipment and Medical Supplies Oxygen Equipment and Supplies`) &
                             is.na(`Suppliers Pharmacy`) & is.na(`Suppliers Pharmacy Clinic Pharmacy`) &
                             is.na(`Suppliers Pharmacy Community Retail Pharmacy`) & is.na(`Suppliers Pharmacy Compounding Pharmacy`) &
                             is.na(`Suppliers Pharmacy Home Infusion Therapy Pharmacy`) & is.na(`Suppliers Pharmacy Institutional Pharmacy`) &
                             is.na(`Suppliers Pharmacy Long-term Care Pharmacy`) & is.na(`Suppliers Pharmacy Mail Order Pharmacy`) &
                             is.na(`Suppliers Pharmacy Managed Care Organization Pharmacy`) & is.na(`Suppliers Pharmacy Specialty Pharmacy`) &
                             is.na(`Suppliers Prosthetic Orthotic Supplier`) ~ "Hospital",
                           ((!is.na( `Agencies Home Health`) | !is.na(`Agencies Hospice Care Community Based`)|
                           !is.na(`Allergy and Immunology`) | !is.na(`Family Medicine`) |
                             !is.na(`General Practice`) | !is.na(`Hematology and Oncology`) |
                              !is.na(`Infectious Disease`) | !is.na(`Internal Medicine`) |
                             !is.na(Laboratory) | !is.na(Neurology) | !is.na(Pediatrics) |
                             !is.na(`Physician Assistant and Advanced Practice Nursing`) |
                             !is.na(`POS Comprehensive Inpatient Rehabilitation Facility`) |
                             !is.na(`POS Group Home`) | !is.na(`POS Home`) | !is.na(`POS independent Clinic`) |
                             !is.na(`POS Mobile Unit`) | !is.na(`POS Nursing Facility`) | !is.na(`POS Office`) |
                             !is.na(`POS Pharmacy`) | !is.na(`POS Place of Employment-Worksite`) | 
                             !is.na(`POS School`) | !is.na(`POS Skilled Nursing Facility`) |
                             !is.na(`POS Urgent Care Facility`) | !is.na(`POS Walk-in Retail Health Clinic`) |
                             !is.na(Radiology) | !is.na(`Suppliers Durable Medical Equipment and Medical Supplies`) |
                             !is.na(`Suppliers Durable Medical Equipment and Medical Supplies Oxygen Equipment and Supplies`) |
                             !is.na(`Suppliers Pharmacy`) | !is.na(`Suppliers Pharmacy Clinic Pharmacy`) |
                             !is.na(`Suppliers Pharmacy Community Retail Pharmacy`) | !is.na(`Suppliers Pharmacy Compounding Pharmacy`) |
                             !is.na(`Suppliers Pharmacy Home Infusion Therapy Pharmacy`) | !is.na(`Suppliers Pharmacy Institutional Pharmacy`) |
                             !is.na(`Suppliers Pharmacy Long-term Care Pharmacy`) | !is.na(`Suppliers Pharmacy Mail Order Pharmacy`) |
                             !is.na(`Suppliers Pharmacy Managed Care Organization Pharmacy`) | !is.na(`Suppliers Pharmacy Specialty Pharmacy`) |
                             !is.na(`Suppliers Prosthetic Orthotic Supplier`)) &
                                      is.na(Hospital)) ~ "Non-Hospital",
                          ((!is.na( `Agencies Home Health`) | !is.na(`Agencies Hospice Care Community Based`)|
                           !is.na(`Allergy and Immunology`) | !is.na(`Family Medicine`) |
                             !is.na(`General Practice`) | !is.na(`Hematology and Oncology`) | !is.na(`Infectious Disease`) | !is.na(`Internal Medicine`) |
                             !is.na(Laboratory) | !is.na(Neurology) | !is.na(Pediatrics) |
                             !is.na(`Physician Assistant and Advanced Practice Nursing`) |
                             !is.na(`POS Comprehensive Inpatient Rehabilitation Facility`) |
                             !is.na(`POS Group Home`) | !is.na(`POS Home`) | !is.na(`POS independent Clinic`) |
                             !is.na(`POS Mobile Unit`) | !is.na(`POS Nursing Facility`) | !is.na(`POS Office`) |
                             !is.na(`POS Pharmacy`) | !is.na(`POS Place of Employment-Worksite`) | 
                             !is.na(`POS School`) | !is.na(`POS Skilled Nursing Facility`) |
                             !is.na(`POS Urgent Care Facility`) | !is.na(`POS Walk-in Retail Health Clinic`) |
                             !is.na(Radiology) | !is.na(`Suppliers Durable Medical Equipment and Medical Supplies`) |
                             !is.na(`Suppliers Durable Medical Equipment and Medical Supplies Oxygen Equipment and Supplies`) |
                             !is.na(`Suppliers Pharmacy`) | !is.na(`Suppliers Pharmacy Clinic Pharmacy`) |
                             !is.na(`Suppliers Pharmacy Community Retail Pharmacy`) | !is.na(`Suppliers Pharmacy Compounding Pharmacy`) |
                             !is.na(`Suppliers Pharmacy Home Infusion Therapy Pharmacy`) | !is.na(`Suppliers Pharmacy Institutional Pharmacy`) |
                             !is.na(`Suppliers Pharmacy Long-term Care Pharmacy`) | !is.na(`Suppliers Pharmacy Mail Order Pharmacy`) |
                             !is.na(`Suppliers Pharmacy Managed Care Organization Pharmacy`) | !is.na(`Suppliers Pharmacy Specialty Pharmacy`) |
                             !is.na(`Suppliers Prosthetic Orthotic Supplier`)) & !is.na(Hospital))  ~ "Hospital/ Non-Hospital"))

df3$Site_designation %>% table()
df3$Site_designation %>% table() %>% prop.table()
#   Hospital      Hospital/ Non-Hospital           Non-Hospital 


IVIG <- df3 
#  102353                   8380                  53698 
