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
library(xlsx)



load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med Claims for Site Identification.RData")
load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/NPI_registry.RData")

med$attending_npi %>% is.na() %>% table()
med$billing_pr_npi %>% is.na() %>% table()
med$facility_npi %>% is.na() %>% table()



# take billing npi first, then attending npi, then facility NPI

med <- med %>% mutate(NPI = case_when(is.na(billing_pr_npi) ~ attending_npi,
                                      is.na(billing_pr_npi) &
                                        is.na(attending_npi) ~ facility_npi,
                                      TRUE ~ billing_pr_npi))
med$NPI %>% is.na() %>% table()
# missing NPI Identifier
med_no_NPI <- med %>% filter(is.na(NPI)) # 1308293

med <- med %>% filter(!is.na(NPI)) # 778376
med$patient_id %>% n_distinct() #131,578 patients
med$claim_id %>% n_distinct() # 4,294,478 claims




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

### medical claims with Idnentifying NPI
med2 <- med %>% left_join(NPIreg)
# not a 1:1 match this will be larger than the original dataset

rm(med_no_NPI)

# table NPI x products at patient level
pts <- med2 %>% select(patient_id,claim_id, date_of_service, NPI, provider_taxonomy_desc, procedure, ndc_code )



# bring in OHDSI for identifying J codes and NDCs  
OHDSI <- read_delim("~/Kantar/Arunajadai, Srikesh (KH) - KHDICT/OHDSI/CONCEPT.csv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)
OHDSI_NDC <- OHDSI %>% filter(vocabulary_id == "NDC")
OHDSI_HCPCS <- OHDSI %>% filter(vocabulary_id == "HCPCS")

pts_Jcodes <- pts %>% select(procedure) %>% distinct()
pts_NDC <- pts %>% select(ndc_code) %>% distinct()

pts_combos<- pts %>% select(procedure, ndc_code) %>% distinct() #### these will be colum names for X tab

 
 
 
 # NDC and J inclusion codes
 IG_NDC <- read_excel("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Pfizer_IG_Open_Claims_Inclusion_Codes.xlsx")
 IG_Codes <- IG_NDC %>% select(-...3)
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
 
 NDC_J_Names <- read_excel("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/2019-08-26 Inclusion codes for IG - NDC, J, HCPCS, etc..xlsx", 
                           col_types = c("text", "text", "text", 
                                         "skip", "skip", "skip", "skip", 
                                         "skip", "skip"))
 NDC_J_Names$NDC <- NDC_J_Names$NDC %>% str_remove_all(pattern = "XX") %>% 
   str_remove_all(pattern = "-")
 NDC_J_Names <- NDC_J_Names[1:64,]
 

 
 ##  get codes for Jcodes
 jcodes <- OHDSI_HCPCS %>% filter(concept_code %in% IG_jcodes$`J Codes`) 
 NDCs <- OHDSI_NDC %>% filter(concept_code %in% IG_NDC)
 
 
 
 
 NDC_Names <- NDC_J_Names %>% filter(NDC %in% IG_NDC |
                                       NDC %in% ndc_codes) %>%
   select(-`J-Code`) %>% rename(ndc_name = `IG Product`,
                                ndc_code = NDC) # bind these to names for identifying J and NDC code comibantions from J_NDC_ids

 
 
 # Start with J code names 
 J_Names <- NDC_J_Names %>% filter(`J-Code` %in% pts_combos$procedure) %>% 
   select(-NDC) %>% na.omit() %>% rename(J_Code_Name = `IG Product`,
                                         procedure = `J-Code`)
 J_Names <- J_Names %>% arrange(procedure)
 J_Names <- J_Names[-14,]
 
J_Names <-  J_Names %>% mutate(J_Code_Name = case_when( procedure == "J1561" ~ "Gamunex-c/ Gammaked",
                                             procedure == "J1566" ~ "IG IV Lyophillized NOS",
                                             procedure == "J1599" ~ "IG IV non-lyophilized NOS",
                                             TRUE ~ J_Code_Name)) %>% distinct()


pts_combos <- pts_combos %>% left_join(J_Names)
# then NDC names 

try <- pts_combos %>% left_join(NDC_Names)

get_these_from_OHDSI <- try %>% filter(!is.na(ndc_code) & is.na(ndc_name)) %>% select(ndc_code) %>% unique()

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

NDC_Names <- NDC_Names %>% full_join(select(NDC_codes_from_OHDSI, ndc_code, ndc_name))

pts_combos <- pts_combos %>% left_join(NDC_Names)
rm(get_these_from_OHDSI, IG_Codes, IG_Jcodes,  J_Names,
   NDC_codes_from_OHDSI, NDC_J_Names, NDC_Names,
   OHDSI, OHDSI_HCPCS, OHDSI_NDC, try, med, 
   IG_NDC, ndc_code, attending_npi, billing_npi, facility_npi)

gc()

# now name treatments in med claims
Patient_Treatments_By_NPI <-  pts %>% left_join(pts_combos)
Patient_Treatments_By_NPI$patient_id %>% n_distinct() # 131,578
Patient_Treatments_By_NPI$J_NDC_treatment <- paste(Patient_Treatments_By_NPI$J_Code_Name, 
                                                   Patient_Treatments_By_NPI$ndc_name, sep = "+")

# count treatmet types by NPI 


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
  group_by(provider_taxonomy_desc, J_NDC_treatment) %>% summarise(n_patients = n_distinct(patient_id))
# remove those with no taxonomy description
df2 <- df %>% filter(!is.na(provider_taxonomy_desc))

df2 <- df2 %>% arrange(provider_taxonomy_desc, desc(n_patients))


openxlsx::write.xlsx(as.data.frame(df2), "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Med Claims  NPI by Tx by Patients Long.xlsx")


###########################################

# now 2018 only 

df <- Patient_Treatments_By_NPI %>% filter(date_of_service >= "2018-01-01" & date_of_service <= "2018-12-31") %>%
  group_by(provider_taxonomy_desc, J_NDC_treatment) %>% summarise(n_patients = n_distinct(patient_id))
# remove those with no taxonomy description
df2 <- df %>% filter(!is.na(provider_taxonomy_desc))

df2 <- df2 %>% arrange(provider_taxonomy_desc, desc(n_patients))

openxlsx::write.xlsx(as.data.frame(df2), "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Med Claims  NPI by Tx by Patients Long 2018.xlsx")


df <- Patient_Treatments_By_NPI %>% filter(date_of_service >= "2018-01-01" & date_of_service <= "2018-12-31") %>%
  group_by(provider_taxonomy_desc, J_NDC_treatment) %>% summarise(n_claims = n_distinct(claim_id))
# remove those with no taxonomy description
df2 <- df %>% filter(!is.na(provider_taxonomy_desc))

df2 <- df2 %>% arrange(provider_taxonomy_desc, desc(n_claims))

openxlsx::write.xlsx(as.data.frame(df2), "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Med Claims  NPI by Tx by Claims Long 2018.xlsx")




