#########################################################
#  Script: Migraine_related_visits.R
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
library(xlsx)
library(data.table)
library(kollekt)

project_path <- file.path(getPath('KHDICT'),'OHDSI')


file_name <- file.path(project_path,'CONCEPT.csv')
scan(file_name, what = character())
OHDSI <- read_delim("~/Kantar/Arunajadai, Srikesh (KH) - KHDICT/OHDSI/CONCEPT.csv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

OHDSI <- OHDSI %>% filter(vocabulary_id == "ICD10CM" |  vocabulary_id == "ICD9CM")

# create vectors for list of complications
Migraine_ICD_text <- grep("migraine", OHDSI$concept_name, value = T, ignore.case = T)

Migraine_complications_text <- list(grep("status migrainosus", OHDSI$concept_name, value = T, ignore.case = T),
                                    grep("cerebral infarction", OHDSI$concept_name, value = T, ignore.case = T),
                                    grep("with aura", OHDSI$concept_name, value = T, ignore.case = T),
                                    grep("depressive", OHDSI$concept_name, value = T, ignore.case = T),
                                    grep("bipolar", OHDSI$concept_name, value = T, ignore.case = T),
                                    grep(" panic |^Panic ", OHDSI$concept_name, value = T, ignore.case = T),
                                    grep("vertigo", OHDSI$concept_name, value = T, ignore.case = T),
                                    grep("insomnia", OHDSI$concept_name, value = T, ignore.case = T),
                                    grep("serotonin syndrome", OHDSI$concept_name, value = T, ignore.case = T),
                                    grep("gastric ulcer", OHDSI$concept_name, value = T, ignore.case = T),
                                    grep("serotonin syndrome", OHDSI$concept_name, value = T, ignore.case = T))

Migraine_complcations_ICD <- list(
  grep("T50.995", OHDSI$concept_code, value = T, ignore.case = T),
  grep("R10.9$",OHDSI$concept_code, value = T, ignore.case = T),
  grep("T39.395A ",OHDSI$concept_code, value = T, ignore.case = T))

# create xlxs file to send to lilly
Migraine_OHDSI <- OHDSI %>% filter(concept_name %in% unlist(Migraine_complications_text))
Migraine_OHDSI_2 <- OHDSI %>% filter(concept_code %in% unlist(Migraine_complcations_ICD))

Migraine_complications <- Migraine_OHDSI %>% full_join(Migraine_OHDSI_2) %>%
  as_tibble() %>% distinct()
Migraine_complications <- Migraine_complications %>% select(-standard_concept, -valid_start_date, -valid_end_date, -invalid_reason)%>% arrange(concept_name)
#write.xlsx(Migraine_complications, file = "../Data/Potential_Migraine_Complications_ICDcodes.xlsx")