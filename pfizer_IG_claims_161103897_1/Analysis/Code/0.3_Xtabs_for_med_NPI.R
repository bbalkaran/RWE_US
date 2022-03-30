#########################################################
#  Script: 0.3_Xtabs_for_med_NPI.R
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
library(DescTools)



load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med Claims for Site Identification.RData")
load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/NPI_registry.RData")

med$attending_npi %>% is.na() %>% table()
med$billing_pr_npi %>% is.na() %>% table()
med$facility_npi %>% is.na() %>% table()



# take billing npi first, then attending npi, then facility NPI

med <- med %>% mutate(NPI = case_when( (is.na(billing_pr_npi) &
                                          is.na(attending_npi)) ~ facility_npi,
                                       is.na(billing_pr_npi) ~ attending_npi,
                                      TRUE ~ billing_pr_npi))
med$NPI %>% is.na() %>% table()
# missing NPI Identifier
med_no_NPI <- med %>% filter(is.na(NPI)) # 1308293
med_no_NPI$attending_npi %>% is.na() %>% table()
med_no_NPI$billing_pr_npi %>% is.na() %>% table()
med_no_NPI$facility_npi %>% is.na() %>% table()

#med <- med %>% filter(!is.na(NPI)) # 778376
med$patient_id %>% n_distinct() #139940 patients
med$claim_id %>% n_distinct() # 6228910 claims



attending_npi <-  med$attending_npi %>% unique()
billing_npi <- med$billing_pr_npi %>% unique()
facility_npi <- med$facility_npi %>% unique()




# select out attending and billing NPIs from NPI registry

NPIreg <- NPIregistry %>% filter(NPI %in% attending_npi |
                         NPI %in% billing_npi |
                           NPI %in% facility_npi) %>% 
  select(NPI, provider_taxonomy_code, provider_taxonomy_desc) %>% na.omit()
rm(NPIregistry)
gc()



#Dedup
# 2344375
med <- med %>% distinct()


### medical claims with Idnentifying NPI
med2 <- med %>% left_join(NPIreg)
# not a 1:1 match this will be larger than the original dataset

rm(med_no_NPI)

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

NDC_codes_from_OHDSI <- NDC_codes_from_OHDSI %>% 
  rename(IG.Product = ndc_name)

NDC_Names <- NDC_Names %>% full_join(select(NDC_codes_from_OHDSI, ndc_code, IG.Product))

pts_combos <- pts_combos %>% left_join(NDC_Names) %>% 
  select(procedure, Product_Name, ndc_code, IG.Product)


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

write.xlsx(treatments, "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/J code and NDC Combinations.xlsx")
################ pick up here###################################################################### add 
# SC and IV combinations? 

Patient_Treatments_By_NPI <- Patient_Treatments_By_NPI %>% 
  mutate(J_code_type = case_when(Product_Name == "Bivigam" | 
                                   Product_Name == "Carimune" |
                                   Product_Name == "Flebogamma/ Flebogamma DIF" |
                                   Product_Name == "Gamunex-c/ Gammaked" |
                                   Product_Name == "Gammagard" |
                                   Product_Name == "Gammaplex" |
                                   Product_Name == "Octagam" |
                                   Product_Name == "Privigen" | 
                                   Product_Name == "IG IV, non-lyophilized, NOS" |
                                   Product_Name == "IV IG" | 
                                   Product_Name == "IG IV, lyophilized, NOS" ~ "IV",
                                 Product_Name == "Hizentra" |
                                   Product_Name == "Hyqvia" |
                                   Product_Name == "Cuvitru" |
                                   Product_Name == "Vivaglobin" ~ "SC"),
         NDC_code_type = case_when(IG.Product == "Bivigam" |
                                     IG.Product == "Carimune" |
                                     IG.Product == "Flebogamma" | 
                                     IG.Product == "Gamunex" |
                                     IG.Product == "Gammagard"|
                                     IG.Product == "Gammaked" | 
                                     IG.Product == "Gammaplex" | 
                                     IG.Product == "Octagam" |
                                     IG.Product == "Octagam"|
                                     IG.Product == "Privigen" | 
                                     IG.Product == "Panzyga"  ~ "IV",
                                   IG.Product == "Hizentra" |
                                     IG.Product == "Hizentra" |
                                     IG.Product == "Hizentra" | 
                                     IG.Product == "Hizentra" |
                                     IG.Product == "HyQvia"| 
                                     IG.Product == "HyQvia" |
                                     IG.Product == "HyQvia" | 
                                     IG.Product == "HyQvia" |
                                     IG.Product == "HyQvia" |
                                     IG.Product == "Cuvitru" |
                                     IG.Product == "Cuvitru" | 
                                     IG.Product == "Cuvitru" |
                                     IG.Product == "Cuvitru"  ~ "SC"))


Patient_Treatments_By_NPI$J_NDC_type <- paste(Patient_Treatments_By_NPI$J_code_type, 
                                                   Patient_Treatments_By_NPI$NDC_code_type, sep = " + ")
treatments_types <- Patient_Treatments_By_NPI %>% #select(J_NDC_treatment, J_NDC_type) %>% 
   distinct()

b <- treatments_types %>% select(patient_id, claim_id, service_from, J_NDC_treatment, J_NDC_type) %>% distinct()

b2 <- b %>% group_by(J_NDC_treatment, J_NDC_type) %>% summarise(N_patients = n_distinct(patient_id),
                                                                N_claims = n_distinct(claim_id))
  
b2 <- b2 %>% ungroup() %>% arrange(desc(N_patients),desc(N_claims)) 

b2 <- b2 %>% filter(! J_NDC_treatment == "NA + NA")

write.xlsx(treatments_types, "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/J code and NDC Combinations and Types.xlsx")

bA <- b %>% mutate(J_NDC_treatment2 = case_when(J_NDC_treatment == "Privigen + NA" |
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
b3 <- bA %>% filter(! J_NDC_treatment2 == "NA + NA") %>% group_by(J_NDC_treatment2) %>% summarise(N_patients = n_distinct(patient_id),
                                                        N_claims = n_distinct(claim_id)) %>% 
   ungroup() %>% arrange(desc(N_patients),desc(N_claims)) 


b4 <- bA %>% filter(! J_NDC_treatment2 == "NA + NA") %>% 
   select(patient_id,claim_id,service_from, J_NDC_treatment2) %>%  
   arrange(patient_id, service_from, J_NDC_treatment2)


tmp <- b4 %>% 
   group_by(patient_id, J_NDC_treatment2) %>% 
   slice(c(1, n())) %>% distinct() %>%
   ungroup() %>% 
   group_by(patient_id) %>%
   mutate(previous = lag(service_from, 1)) %>%
   mutate(tae.3 = as.duration(service_from %--% lag(service_from, 1))) %>% 
   mutate(tae.3 = tae.3/60/60/24/7) %>%
   ungroup() %>% group_by(patient_id, J_NDC_treatment2) %>%
   mutate(last_date = lead(service_from,1)) 

tmp2 <- tmp %>% select(patient_id, J_NDC_treatment2, service_from, last_date)

tmp3 <- tmp2 %>% group_by(patient_id, J_NDC_treatment2) %>% slice(1)%>% group_by(patient_id) %>% filter(n()>1)

tmp4 <- tmp3 %>% mutate(tx_interval = interval(service_from, last_date),
                                 tx_interval = tx_interval/60/60/24/7,
                        Occurences = n())

# figure out N of patients on more than 1 tx 
tmp4$patient_id %>% n_distinct() # 49,190 patients with more than one treatment
tmp4.5 <- tmp4 %>% filter(Occurences == 2)
tmp4.5$patient_id %>% n_distinct() #32134
tmp5 <- tmp4 %>% filter(Occurences ==3)
tmp5$patient_id %>% n_distinct() # 11346
tmp6 <- tmp4 %>% filter(Occurences == 4)
tmp6$patient_id %>% n_distinct() # 3929
tmp7 <- tmp4 %>% filter(Occurences == 5)
tmp7$patient_id %>% n_distinct()
tmp8 <- tmp4 %>% filter(Occurences == 6)
tmp8$patient_id %>% n_distinct() #365
tmp9 <- tmp4 %>% filter(Occurences == 7)
tmp9$patient_id %>% n_distinct() # 95
tmp10 <- tmp4 %>% filter(Occurences ==8)
tmp10$patient_id  %>% n_distinct() # 33
tmp11 <- tmp4 %>% filter(Occurences == 9)
tmp11$patient_id %>% n_distinct() #5
tmp12 <- tmp4 %>% filter(Occurences == 10)
tmp12$patient_id %>% n_distinct() #3
tmp13 <- tmp4 %>% filter(Occurences == 11)
tmp13$patient_id %>% n_distinct() #0
tmp14 <- tmp4 %>% filter(Occurences == 12)
tmp14$patient_id %>% n_distinct() #2
tmp15 <- tmp4 %>% filter(Occurences == 13)
tmp15$patient_id %>% n_distinct() #1
tmp16 <- tmp4 %>% filter(Occurences == 14)
tmp16$patient_id %>% n_distinct() #0
tmp17 <- tmp4 %>% filter(Occurences == 15)
tmp17$patient_id %>% n_distinct() #0
tmp18 <- tmp4 %>% filter(Occurences == 16)
tmp18$patient_id %>% n_distinct() #0
tmp19 <- tmp4 %>% filter(Occurences == 17)
tmp19$patient_id %>% n_distinct() #1

# figure out which patients have overlapping treatments
# 2 tx
tmp4.5 <- tmp4.5 %>% mutate(second_tx = lead(J_NDC_treatment2,1),
                            service_from2 = lead(service_from,1),
                            last_date2 = lead(last_date, 1)) %>% 
   group_by(patient_id) %>% slice(1)


tmp4.5 <- tmp4.5 %>% ungroup() %>% mutate(range1 = interval(service_from, last_date),
                  range2 = interval(service_from2, last_date2),
                  overlap = int_overlaps(range1, range2))
tmp4.5 %>% select(J_NDC_treatment2, second_tx) %>% distinct() %>% View()
tmp4.5 %>% filter(overlap == T) #9497

# 3 tx 
tmp5 <- tmp5 %>% mutate(second_tx = lead(J_NDC_treatment2,1),
                            service_from2 = lead(service_from,1),
                            last_date2 = lead(last_date, 1),
                        third_tx = lead(J_NDC_treatment2,2),
                        service_from3 = lead(service_from,2),
                        last_date3 = lead(service_from,2)) %>%
   group_by(patient_id) %>% slice(1)
tmp5 <- tmp5 %>% ungroup() %>% mutate(range1 = interval(service_from, last_date),
                                          range2 = interval(service_from2, last_date2),
                                      range1_2 = interval(service_from2, last_date2),
                                      range2_2 = interval(service_from3, last_date3),
                                          overlap = int_overlaps(range1, range2),
                                      overlap2 = int_overlaps(range1_2, range2_2))
tmp5 %>% select(J_NDC_treatment2, second_tx, third_tx) %>% distinct() %>% View()
tmp5 %>% filter(overlap == T)
tmp5 %>% filter(overlap2 == T)

# 4 tx
tmp6 <- tmp6 %>% mutate(second_tx = lead(J_NDC_treatment2,1),
                        service_from2 = lead(service_from,1),
                        last_date2 = lead(last_date, 1),
                        third_tx = lead(J_NDC_treatment2,2),
                        service_from3 = lead(service_from,2),
                        last_date3 = lead(service_from,2),
                        fourth_tx = lead(J_NDC_treatment2, 3),
                        service_from4 = lead(service_from,3),
                        last_date4 = lead(last_date,3)) %>%
   group_by(patient_id) %>% slice(1)
tmp6 <- tmp6 %>% ungroup() %>% mutate(range1 = interval(service_from, last_date),
                                      range2 = interval(service_from2, last_date2),
                                      range1_2 = interval(service_from2, last_date2),
                                      range2_2 = interval(service_from3, last_date3),
                                      range1_3 = interval(service_from3, last_date3),
                                      range2_3 = interval(service_from4,last_date4),
                                      overlap = int_overlaps(range1, range2),
                                      overlap2 = int_overlaps(range1_2, range2_2),
                                      overlap3 = int_overlaps(range1_3, range2_3))
tmp6 %>% select(J_NDC_treatment2, second_tx, third_tx) %>% distinct() %>% View()
tmp6 %>% filter(overlap == T)
tmp6 %>% filter(overlap2 == T)
tmp6 %>% filter(overlap3 == T)

Phys_visits <- aggregate(Migraine_related_visits_count$Related_Physician_Visits, by = list(Migraine_related_visits_count$client_patient_id), sum) %>%
   rename(client_patient_id = Group.1, 
          Physician_office_visits = x) 



b4 %>% aggregate()
b5 <- b %>% filter(! J_NDC_treatment == "NA + NA") %>% select(patient_id, claim_id)
b5 <- b4 %>% head(n = 500) %>% group_by(patient_id) %>% filter(n()>1) 
  
write.xlsx(b2, "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/J code and NDC Combination Counts.xlsx")
write.xlsx(b3, "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/J code and NDC Combination Counts Collapsed.xlsx")
b$patient_id %>% n_distinct()


treatments_types$patient_id %>% n_distinct()
treatments_types$claim_id %>% n_distinct()
#139,940

# count treatmet types by NPI 

try <- treatments_types %>% select(patient_id, claim_id, service_from, J_NDC_treatment, J_NDC_type) %>% 
   group_by(patient_id, claim_id) %>% filter(n()>1) 

df <- Patient_Treatments_By_NPI %>% 
  group_by(provider_taxonomy_desc, J_NDC_treatment) %>% summarise(n_claims = n_distinct(claim_id))
# remove those with no taxonomy description
df2 <- df %>% filter(!is.na(provider_taxonomy_desc))
df3 <- df2 %>% arrange(provider_taxonomy_desc, desc(n_claims))
write.xlsx(as.data.frame(df3), "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Med Claims by NPI by Claims Long.xlsx")
ClaimsXtx <- df2 %>% spread(J_NDC_treatment, n_claims)
write.xlsx(as.data.frame(ClaimsXtx), "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Med Claims by NPI by Claims Wide.xlsx")


#############################################################################################################################



#  Patient X NPI 

claims <- med2 %>% select(claim_id, NPI, provider_taxonomy_desc, procedure, ndc_code ) %>% 
  
  
claims_combos <- claims  %>% select(procedure, ndc_code) %>% distinct() # same as pts combos 

claims$claim_id %>% n_distinct() #4294478


df <- Patient_Treatments_By_NPI %>% 
  group_by(provider_taxonomy_desc, J_NDC_treatment) %>% summarise(n_patients = n_distinct(patient_id),
                                                                  n_claims = n_distinct(claim_id))
# remove those with no taxonomy description
df2 <- df %>% filter(!is.na(provider_taxonomy_desc))

df2 <- df2 %>% arrange(provider_taxonomy_desc, desc(n_patients))


openxlsx::write.xlsx(as.data.frame(df2), "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Med Claims  NPI by Tx by Patients Long.xlsx")


###########################################
# 
# # now 2018 only 
# 
# df <- Patient_Treatments_By_NPI %>% filter(date_of_service >= "2018-01-01" & date_of_service <= "2018-12-31") %>%
#   group_by(provider_taxonomy_desc, J_NDC_treatment) %>% summarise(n_patients = n_distinct(patient_id))
# # remove those with no taxonomy description
# df2 <- df %>% filter(!is.na(provider_taxonomy_desc))
# 
# df2 <- df2 %>% arrange(provider_taxonomy_desc, desc(n_patients))
# 
# openxlsx::write.xlsx(as.data.frame(df2), "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Med Claims  NPI by Tx by Patients Long 2018.xlsx")
# 
# 
# df <- Patient_Treatments_By_NPI %>% filter(date_of_service >= "2018-01-01" & date_of_service <= "2018-12-31") %>%
#   group_by(provider_taxonomy_desc, J_NDC_treatment) %>% summarise(n_claims = n_distinct(claim_id))
# # remove those with no taxonomy description
# df2 <- df %>% filter(!is.na(provider_taxonomy_desc))
# 
# df2 <- df2 %>% arrange(provider_taxonomy_desc, desc(n_claims))
# 
# openxlsx::write.xlsx(as.data.frame(df2), "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Med Claims  NPI by Tx by Claims Long 2018.xlsx")
# 
# 
# 

