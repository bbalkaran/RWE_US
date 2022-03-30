#########################################################
#  Script: 0.6_Remove_Patients_with_Mismatched_treatmens.R
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

#Dedup
# 2344375
med <- med %>% distinct()



#products at patient level
pts <- med %>% select(patient_id,claim_id, service_from, procedure, ndc_code )



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



# now name treatments in med claims
Patient_Treatments_By_NPI <-  pts %>% left_join(pts_combos)
Patient_Treatments_By_NPI$patient_id %>% n_distinct() # 131,578
Patient_Treatments_By_NPI$J_NDC_treatment <- paste(Patient_Treatments_By_NPI$Product_Name, 
                                                   Patient_Treatments_By_NPI$IG.Product, sep = " + ")

# unique combos and treatments 
treatments <- Patient_Treatments_By_NPI$J_NDC_treatment %>% 
  unique()

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

bA$patient_id %>% n_distinct() #139940

# remove claims with mismatched treatments

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

med2 <- bA %>% filter(!J_NDC_treatment2 %in% drop_txs)
med2$patient_id %>% n_distinct() # 139917

med <- med %>% inner_join(med2)
med$patient_id %>% n_distinct() #139917, 23 patients dropped 

save(med, 
     file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med Claims for Site Identification clean txs.RData")
