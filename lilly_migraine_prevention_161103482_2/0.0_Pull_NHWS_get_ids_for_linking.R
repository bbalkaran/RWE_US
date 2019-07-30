#########################################################
#  Script: 0.0_Pull_NHWS_get_ids_for_linking.R
#  Purpose: build linked cohort
#  Author: Bridget Balkaran
#  Project: 161103482-2 Lilly Migrane Prevention Linked
#  Project Manager: Janelle Cambron-Mellott, Dena Jaffe
#  Date Created: 7/1/19
#  Date Edited: 7/1/19
########################################################


# 1. workspace-----
library(tidyverse)
library(readxl)
Komodo <- read_excel("C:/Users/balkaranb/OneDrive - Kantar/Projects/Linked Projects Info/2019-06-20 zkey to Komodo ID bridging file.xlsx")

names(Komodo) <- names(Komodo) %>% str_replace_all(pattern = " ",
                                            replacement = "_")



library(odbc)




years <- c("2015", "2016", "2017")

query0 <- "Select * From NHWS.num_all_US_2017 where variable  = 'MCS' or variable = 'PCS' 
                                                 or variable  = 'SF6D_R2' or variable = 'EQ5DINDEX' 
                                                 or variable  = 'TRADTMI' or variable = 'RUNL6Q'
                                                 or variable  = 'RUER6Q'or variable  = 'RUHP6Q'
                                                 or variable  = '


query <- lapply(years, years_loop)
query_exe <- function(connection, x){
  q<- dbSendQuery(con, x); dbFetch(q)
}
year_data <- lapply(query, function(x){dbGetQuery(z, x)})

arrange_and_transpose <- function(dat){
  dat %>% arrange(zKey) %>% spread(variable, numvalue)

