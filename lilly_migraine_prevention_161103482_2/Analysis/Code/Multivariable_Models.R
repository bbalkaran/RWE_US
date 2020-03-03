#########################################################
#  Script: MV_models_Prevention_Eligibility.R
#  Purpose: multivariable models
#  Data file: Cohort1B.RData
#  Author: Bridget Balkaran
#  Project:161103482-2
#  Project Manager: Janelle Cambron-Mellot
#  Date Created: 7/26/19
#  Date Edited: 8/19/19
########################################################


# 1.WOrkspace-----

library(lme4)
library(lmtest)
library(lmerTest)
library(glmmTMB)
library(DHARMa)
library(MASS)
library(car)
library(emmeans)
library(magrittr)
library(tidyverse)
options(scipen=999)
load("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Lilly Migraine Prevention Phase II/lilly_migraine_prevention_161103482_2/Analysis/Data/CohortB1.RData")
CohortB1$All_Phys_visits_per_year %<>% floor()
CohortB1$Phys_visits_per_year %<>% floor()
CohortB1$All_OP_visits_per_year %<>% floor()
CohortB1$op_visits_per_year %<>% floor()
CohortB1$All_ER_visits_per_year %<>% floor()
CohortB1$ER_visits_per_year %<>% floor()
CohortB1$All_Hosp_visits_per_year %<>% floor()
CohortB1$Hosp_visits_per_year %<>% floor()

CohortB1$All_Phys_visits_per_year2 <- floor(CohortB1$All_Phys_visits_per_year/2) 
CohortB1$Phys_visits_per_year2 <- floor(CohortB1$Phys_visits_per_year/2)
CohortB1$All_OP_visits_per_year2 <- floor(CohortB1$All_OP_visits_per_year/2)
CohortB1$op_visits_per_year2 <- floor(CohortB1$op_visits_per_year/2)
CohortB1$All_ER_visits_per_year2 <- floor(CohortB1$All_ER_visits_per_year/2)
CohortB1$ER_visits_per_year2 <- floor(CohortB1$ER_visits_per_year/2)
CohortB1$All_Hosp_visits_per_year2 <- floor(CohortB1$All_Hosp_visits_per_year/2)
CohortB1$Hosp_visits_per_year2 <- floor(CohortB1$Hosp_visits_per_year/2)

CohortB1 <- CohortB1 %>% mutate(PNDX_R = 
                             case_when(PNDX_R == "0" ~ "No",
                                       PNDX_R == "1" ~ "Yes",
                                       TRUE ~ PNDX_R))

# prevention eligible -----
# MCS, PCS, SF6D, EQ5D-----
norm_dependent_vars <- c("MCS", "PCS", "SF6D_R2", "EQ5DINDEX")
norm_eligibility_lst2 <- lapply(norm_dependent_vars, function(x) {
  fit <- glm(paste(x,'~', 'eli_not_eli+Age+Gender+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
  CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol'), data=CohortB1,
             family = gaussian(link = "identity"))
  summary(lsmeans(fit, 'eli_not_eli', data=CohortB1))
  summary(fit)})
norm_eli_results <- Map(cbind, norm_eligibility_lst, outcome = seq_along(norm_dependent_vars))
norm_eligibility_lst2 <- lapply(norm_dependent_vars, function(x) {
  fit <- glm(paste(x,'~', 'eli_not_eli+Age+Gender+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
  CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol'), data=CohortB1,
             family = gaussian(link = "identity")))
  summary(fit)})
norm_eli_results2 <- Map(cbind, norm_eligibility_lst2, outcome = seq_along(norm_dependent_vars))


# HCRU and WPAI-----
skewed_dependent_vars <- c("WPPCTWRK", "WPIMPAIR", "WPWRKIMP", "WPACTIMP", "MGMSW", 
                           "MGMSH","TRADTMI",  "RUNL6Q",
                           "RUER6Q",  "RUHP6Q")
skew_eligibility_lst <- lapply(skewed_dependent_vars, function(x) {
  fit <- glm.nb(paste(x,'~', 'eli_not_eli+Age+Gender+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
  CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol'), data=CohortB1)
  summary(emmeans(fit, 'eli_not_eli', data=CohortB1, type = "response"))})
skew_eli_results <- Map(cbind, skew_eligibility_lst, outcome = seq_along(skewed_dependent_vars))

skew_eligibility_lst2 <- lapply(skewed_dependent_vars, function(x) {
  fit <- glm.nb(paste(x,'~', 'eli_not_eli+Age+Gender+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
  CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol'), data=CohortB1)
  summary(fit)})
skew_eli_results2 <- Map(cbind, skew_eligibility_lst2, outcome = seq_along(skewed_dependent_vars))

# 
# 
# 
# # cost-----
# #CohortB1$Dir.Cost.Total
# CohortB1$All_cost_per_year <- CohortB1$All_cost_per_year +1
# CohortB1$cost_per_year <- CohortB1$cost_per_year + 1
# CohortB1$Rx_cost_per_year <- CohortB1$Rx_cost_per_year+1 
# 
# cost_dependent_vars <- c( "Dir.Cost.Total","Total_indirect_cost")
# cost_eligibility_lst <- lapply(cost_dependent_vars, function(x) {
#   fit <- glm(paste(x,'~', 'eli_not_eli+Age+Gender+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
#   CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol'), data=CohortB1, 
#              family = gaussian(link = "inverse"))
#   summary(emmeans(fit, 'eli_not_eli', data=CohortB1, type = "response"))})
# cost_eli_results <- Map(cbind, cost_eligibility_lst, outcome = seq_along(cost_dependent_vars))
# cost_eligibility_lst2 <- lapply(cost_dependent_vars, function(x) {
#   fit <- glm(paste(x,'~', 'eli_not_eli+Age+Gender+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
#   CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol'), data=CohortB1,
#                 family = Gamma(link = "inverse"))
#   summary(fit)})
# cost_eli_results2 <- Map(cbind, cost_eligibility_lst2, outcome = seq_along(cost_dependent_vars))

#migraine frequency -----

CohortB1$episodic <-CohortB1$episodic %>% 
  relevel(ref = "Patients with episodic migraine (e.g., <4, 4-7, and 8-14 headache days per month)" )


norm_frequency_lst <- lapply(norm_dependent_vars, function(x) {
  fit <- glm(paste(x,'~', 'episodic+Age+Gender+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
  CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol'), data=CohortB1,
             family = gaussian(link = "identity"))
  summary(lsmeans(fit, 'episodic', data=CohortB1))})
norm_frequency_results <- Map(cbind, norm_frequency_lst, outcome = seq_along(norm_dependent_vars))
norm_frequency_lst2 <- lapply(norm_dependent_vars, function(x) {
  fit <- glm(paste(x,'~', 'episodic+Age+Gender+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
  CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol'), data=CohortB1,
             family = gaussian(link = "identity"))
summary(fit)})
norm_frequenct_results2 <- Map(cbind, norm_frequency_lst2, outcome = seq_along(norm_dependent_vars))


# HCRU and WPAI
skew_frequency_lst <- lapply(skewed_dependent_vars, function(x) {
  fit <- glm.nb(paste(x,'~','episodic+Age+Gender+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
  CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol'), data=CohortB1)
  summary(emmeans(fit, 'episodic', data=CohortB1, type = "response"))})
skew_frequency_results <- Map(cbind, skew_frequency_lst, outcome = seq_along(skewed_dependent_vars))

skew_frequency_lst2 <- lapply(skewed_dependent_vars, function(x) {
  
  fit <- glm.nb(paste(x,'~', 'episodic+Age+Gender+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
  CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol'), data=CohortB1)
 
   summary.glm(fit)})
skew_frequency_results2 <- Map(cbind, skew_frequency_lst2, outcome = seq_along(skewed_dependent_vars))

mod <- glm.nb(WPIMPAIR ~episodic+Age+Gender+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
  CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol, data=CohortB1)
summary.glm(mod)

# gender -----
norm_gender_lst <- lapply(norm_dependent_vars, function(x) {
  fit <- glm(paste(x,'~', 'Gender+Age+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
  CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol'), data=CohortB1,
             family = gaussian(link = "identity"))
  summary(lsmeans(fit, 'Gender', data=CohortB1))})
norm_gender_results <- Map(cbind, norm_gender_lst, outcome = seq_along(norm_dependent_vars))
norm_gender_lst2 <- lapply(norm_dependent_vars, function(x) {
  fit <- glm(paste(x,'~', 'Gender+Age+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
  CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol'), data=CohortB1,
             family = gaussian(link = "identity"))
summary(fit)})
norm_gender_results2 <- Map(cbind, norm_gender_lst2, outcome = seq_along(norm_dependent_vars))


# HCRU and WPAI
skew_gender_lst <- lapply(skewed_dependent_vars, function(x) {
  fit <- glm.nb(paste(x,'~', 'Gender+Age+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
  CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol'), data=CohortB1)
  summary(emmeans(fit, 'Gender', data=CohortB1, type = "response"))})
skew_gender_results <- Map(cbind, skew_gender_lst, outcome = seq_along(skewed_dependent_vars))
skew_gender_lst2 <- lapply(skewed_dependent_vars, function(x) {
  fit <- glm.nb(paste(x,'~', 'Gender+Age+RaceEthnic+CCIcat+ALLDX_R+AXDX_R+PNDX_R+HDTPCT+
  CNDX_R +DPDX_R+FMDX_R+Anticonvulsant+Antidepressant+FDC+NSAID+Alcohol'), data=CohortB1)
  summary(fit)})
skew_gender_results2 <- Map(cbind, skew_gender_lst2, outcome = seq_along(skewed_dependent_vars))
