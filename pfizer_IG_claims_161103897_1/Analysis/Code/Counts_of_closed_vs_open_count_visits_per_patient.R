#########################################################
#  Script: Counts of closed vs open 
#  Purpose: NPI data from CMS
#  Data file: 
#  Author: Bridget Balkaran
#  Project: 161103897-1
#  Project Manager: Martine Maculaitis
#  Date Created: 11/6/19
#  Date Edited: 11/6/19
########################################################


library(tidyverse)
library(readr)
library(skimr)
`%notin%` = function(x,y) !(x %in% y)
beneficiary_file <- read_csv("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Pfizer IG/Data/beneficiary_file.csv")
load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med Claims for Site Identification.RData")

med$patient_id %>% n_distinct() # distinct patients in med claims

b <- med %>% filter(patient_id %notin% beneficiary_file$patient_id) # filter out patients not in beneficiary file
b$patient_id %>% n_distinct() #61482 distinct patients not in beneficiary file

c <- med %>% filter(patient_id %in% beneficiary_file$patient_id) # patients in beneficiary file 


d <- c %>% filter(patient_id == beneficiary_file$patient_id & (date_of_service >= beneficiary_file$eligibility_start_date &
                                        date_of_service <= beneficiary_file$eligibility_end_date))
pts_dos <- med %>% select(patient_id, claim_id,  date_of_service) %>% distinct()


pts_dos <- pts_dos %>% arrange(patient_id, date_of_service) %>% select(patient_id, date_of_service)

df <- pts_dos %>% arrange(patient_id, date_of_service) %>%
  group_by(factor(patient_id)) %>%
  mutate(previous = lag(date_of_service, 1)) %>%
  mutate(tae.1 = date_of_service - lag(date_of_service, 1))%>% 
  ungroup() %>%
  mutate(consecutive_flag = case_when(tae.1 == 1 ~ 1)) %>%
  mutate(consecutive_flag = case_when(tae.1 == 1 ~ 1,
                                      lead(tae.1 == 1)~1))



df4 <- df %>% group_by(patient_id, date_of_service) %>% 
  fill(consecutive_flag) %>% 
  fill(consecutive_flag, .direction = "up") # RAN THIS OVERNIGHT TAKES A LONG TIME!!

df5 <- df4 %>% group_by(patient_id, date_of_service) %>% 
  arrange(date_of_service) %>% 
  slice(1) %>% ungroup() 

try <- df5
 try <- try %>% group_by(patient_id, date_of_service) %>% 
  fill(consecutive_flag) %>% 
  fill(consecutive_flag, .direction = "up")


try2 <-  try %>% group_by(patient_id) %>% 
  filter(consecutive_flag == 1) 
  
  
 try3 <-  try2 %>% mutate(new_dos = case_when(is.na(previous) & is.na(tae.1) ~ date_of_service,
                                              tae.1 > 1 ~ date_of_service)) %>%
  fill(new_dos)

pts_dos2 <- pts_dos %>% left_join(try3) 
pts_dos2 <- pts_dos2 %>% group_by(patient_id, date_of_service) %>% fill(new_dos)

pts_dos2 <- pts_dos2 %>% mutate(new_dos = case_when(is.na(new_dos) ~ date_of_service,
                                                    TRUE ~ new_dos)) 

rm(try, try2, try3)
gc()
pts_dos3 <-  pts_dos2 %>% select(patient_id, claim_id, date_of_service, new_dos) %>%
  distinct() %>% group_by(patient_id) %>% mutate(pt_id = group_indices())%>% 
  
  
  
pts_dos_4 <- pts_dos3 %>%  ungroup() %>%
  group_by(pt_id, dos)  %>% mutate(Visit_id = group_indices())




#save(df4, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Patient Visits Wide.RData")


med2 <- med %>% left_join(pts_dos3)

pts_dos_wide <- pts_dos3 %>% distinct() %>% spread(Visit_id, date_of_service)


pts_dos_small %>% group_keys()

library(dplyr)
pts_dos_small %>% 

df$num <- sequence(rle(df$cat)$lengths)

DT[, id := seq_len(.N), by = cat]

df %>% group_by(IDFAM) %>% mutate(count = sequence(n()))


