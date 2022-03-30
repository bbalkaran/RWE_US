#########################################################
#  Script: All_IG_NDC_codes.R
#  Purpose: list of NDC codes for Wing Yu
#  Data file: Med_merged.RData
#  Author: Bridget Balkaran
#  Project: 161103897-1
#  Project Manager: Martine Maculaitis
#  Date Created: 1/16/20
#  Date Edited: 1/16/20
########################################################

library(tidyverse)
library(data.table)
library(magrittr)
library(openxlsx)

NDC_inclusion_codes <- read_excel("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Pfizer IG/2019-08-26 Inclusion codes for IG - NDC, J, HCPCS, etc..xlsx", 
                                  sheet = "Sheet3", col_types = c("text", "text", "skip", "skip")) 
NDC_inclusion_codes$NDC <- NDC_inclusion_codes$NDC %>% str_remove_all("-") %>% 
  str_remove_all("XX")
NDC_inclusion_codes <- NDC_inclusion_codes[-1,]
NDC_inclusion_codes <- NDC_inclusion_codes[-29,]


OHDSI <- read.delim("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - KHDICT/OHDSI_NEW/CONCEPT.csv", stringsAsFactors=FALSE)
OHDSI <- OHDSI %>% filter(vocabulary_id == "NDC")


med_NDC <- as.list(NDC_inclusion_codes$NDC)
NDC_from_OHDSI <- lapply(med_NDC, function(x) {grep(x, OHDSI$concept_code, ignore.case = T, value = T)})
NDC_from_OHDSI <- NDC_from_OHDSI %>% unlist()
load("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med_merged.RData")
med <- med %>% select(ndc_code) %>% filter(!is.na(ndc_code)) %>% distinct()

NDCs_all <- OHDSI %>% select(concept_name, concept_code) %>% filter(concept_code %in% NDC_from_OHDSI)


NDCs_all_med <- NDCs_all %>% filter(concept_code %in% med$ndc_code) %>% as_tibble()


NDCs_all_rx <- NDCs_all %>% filter(concept_code %in% rx_sample2$ndc11)

NDC_codes <- NDCs_all_med %>% full_join(NDCs_all_rx) %>% distinct()


write.xlsx(NDC_codes, file = "C:/Users/BalkaranB/OneDrive - Kantar/Projects/Pfizer IG/IG_NDC_codes.xlsx")

rm()

J_codes <- read_excel("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Pfizer IG/2019-08-26 Inclusion codes for IG - NDC, J, HCPCS, etc..xlsx", 
                                  sheet = "Sheet2") 
OHDSI <- read.delim("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - KHDICT/OHDSI_NEW/CONCEPT.csv", stringsAsFactors=FALSE)
OHDSI <- OHDSI %>% filter(vocabulary_id == "HCPCS")
med_Jcodes <- as.list(J_codes$J_Code)
J_from_OHDSI <- lapply(med_Jcodes, function(x) {grep(x, OHDSI$concept_code, ignore.case = T, value = T)})
J_from_OHDSI <- unlist(J_from_OHDSI)

Js_all <- OHDSI %>% select(concept_name, concept_code) %>% filter(concept_code %in% J_from_OHDSI)

load("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med_merged.RData")
med <- med %>% select(procedure) %>% filter(!is.na(procedure)) %>% distinct()
J_code <- Js_all %>% filter(concept_code %in% med$procedure)
write.xlsx(J_code, file = "C:/Users/BalkaranB/OneDrive - Kantar/Projects/Pfizer IG/J_codes.xlsx")

