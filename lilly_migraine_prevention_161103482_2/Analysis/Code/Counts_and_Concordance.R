#########################################################
#  Script: Counts_and_Concordance.R
#  Purpose: Concordance Analysis
#  Data file: PhaseII.RData
#  Author: Bridget Balkaran
#  Project:161103482-2
#  Project Manager: Janelle Cambron-Mellet
#  Date Created: 8/19/19
#  Date Edited: 8/19/19
########################################################

# 1. workspace -----

library(tidyverse)
library(magrittr)
library(openxlsx)
library(kollekt)
options(scipen = 999)

load("~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/PhaseII.RData")
load("~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/PhaseI.RData")

dat <- dat %>% rename(zkey = zKey)
linked <- dat %>% filter(zkey %in% MG$zkey)

try2 <- try %>% left_join(select(MG, client_patient_id, zkey, Criteria))


linked <- linked %>% select(zkey, source, MGDX, eligible_comb, eli_not_eli, male_mig_female, episodic, EPISODIC_CAT, preventive,
               # HRQol
               MCS, PCS, SF6D_R2, EQ5DINDEX, 
               # HRU
               TRADTMI, RUER6Q, RUHP6Q, RUNL6Q, 
               # WPAI
               WPPCTWRK, WPIMPAIR, WPACTIMP, WPWRKIMP,
               # Diagnosis
               MGDXY, MGDXDR, 
               # Migraines
               MGFQ1M, MGFQ6M, MGSV, MGSVRX, MGSVNRX, MGMC, MGTPHD, MGHIT, 
               # Symptoms 
               MGAUR, MGMSP, MGNAU, MGTHR, MGASY,
               MGMOV, MGLS, MGSND, MGSPT, MGHRS, 
               # treatment
               MGRXEVR, 
               MGRXACD, MGRXACF, MGRXACO, MGRXAL, MGRXAM, MGRXAX,
               MGRXAY, MGRXBT, MGRXCM, MGRXCTS, MGRXCY, MGRXDP, MGRXDR, MGRXDYM, MGRXFC, MGRXFC, 
               MGRXFN, MGRXFR, MGRXHC, MGRXHX, MGRXIB, MGRXICA, MGRXID, MGRXIJ, MGRXIM, MGRXIS, MGRXJW,
               MGRXMD, MGRXMGL, MGRXMGT, MGRXMLT, MGRXMX, MGRXNA, MGRXNDR, MGRXNE, MGRXNX, MGRXON, 
               MGRXOT, MGRXPRN, MGRXRL, MGRXRZ, MGRXSDZ, MGRXSPX, MGRXSU, MGRXSVD, MGRXTC, MGRXTI, 
               MGRXTP, MGRXTX, MGRXVC, MGRXZCT, MGRXZE, MGRXZLM, MGRXZM, MGRXZMT, MGRXZS, 
                  #  days using Rx in past month
               MGDYACD, MGDYACF,
               MGDYACO, MGDYAL, MGDYAM, MGDYAX, MGDYAY, MGDYBT, MGDYCM, MGDYCTS, MGDYCY, MGDYDP, MGDYDR,
               MGDYDYM, MGDYFC, MGDYFN, MGDYFR, MGDYHC, MGDYHX, MGDYIB, MGDYICA, MGDYID, MGDYIJ, MGDYIM,
               MGDYIS, MGDYJW, MGDYMD, MGDYMGL, MGDYMGT, MGDYMLT, MGDYMX, MGDYNA, MGDYNDR, MGDYNE, MGDYNX,
               MGDYON, MGDYOT, MGDYPRN, MGDYRL, MGDYRZ, MGDYSDZ, MGDYSPX, MGDYSU, MGDYSVD, MGDYTC, MGDYTI, 
               MGDYTP, MGDYTX, MGDYVC, MGDYZCT, MGDYZE, MGDYZLM, MGDYZM, MGDYZMT, MGDYZS, 
                  # HCP who prescribed med
               MGDRACD, MGDRACF,
               MGDRACO, MGDRAL, MGDRAM, MGDRAX, MGDRAY, MGDRBT, MGDRCM, MGDRCTS, MGDRCY, MGDRDP, 
               MGDRDR, MGDRDYM, MGDRFC, MGDRFN, MGDRFR, MGDRHC, MGDRHX, MGDRIB, MGDRICA, MGDRID, MGDRIJ,
               MGDRIM, MGDRIS, MGDRJW, MGDRMD, MGDRMGL, MGDRMGT, MGDRMLT, MGDRMX, MGDRNA,
               MGDRNDR, MGDRNE, MGDRNX, MGDRON, MGDROT, MGDRPRN, MGDRRL, MGDRRZ, MGDRSDZ,MGDRSPX, MGDRSU,
               MGDRSVD, MGDRTC, MGDRTI, MGDRTP, MGDRTX, MGDRVC, MGDRZCT, MGDRZE, MGDRZLM, MGDRZM, MGDRZMT, MGDRZS,
              
                  # Satisfaction wit treatment
               MGSAACD, MGSAACF, MGSAACO, MGSAAL, MGSAAM, MGSAAX, MGSAAY, MGSABT, MGSACM, MGSACTS, MGSACY,
               MGSAFC, MGSAFN, MGSAFR, MGSAHC, MGSAHX, MGSAIB, MGSAICA, MGSAID, MGSAIJ, MGSAJW, MGSAMD, MGSAMGL, 
               MGSAMGT, MGSAMLT, MGSAMX, MGSANA, MGSANDR, MGSANE, MGSANX, MGSAON, MGSAOT, MGSAPRN, 
               MGSARL, MGSARZ, MGSASDZ, MGSASPX, MGSASU, MGSASVD, MGSATC, MGSATI, MGSATP, MGSATX, MGSAVC,
               MGSAZCT, MGSAZE, MGSAZLM, MGSAZM, MGSAZMT, MGSAZS, 
                 # length of time on current treatment
               MGYRACD, MGMOACD, MGYRACF, MGMOACF,  MGYRACO, MGMOACO, MGYRAL, MGMOAL, MGYRAL, MGMOAL,
               MGYRAM, MGMOAM, MGYRAX, MGMOAX, MGYRAY, MGMOAY, MGYRBT, MGMOBT, MGYRCM, MGMOCM, MGYRCTS, MGMOCTS,
               MGYRCY, MGMOCY, MGYRDP, MGMODP, MGYRDR, MGMODR, MGYRDYM, MGMODYM, MGYRFC, MGMOFC, MGYRFN, MGMOFN,
               MGYRFR, MGMOFR, MGYRHC, MGMOHC, MGYRHX, MGMOHX, MGYRIB, MGMOIB, MGYRICA, MGMOICA, MGYRID, MGMOID, 
               MGYRIJ, MGMOIJ, MGYRIM, MGMOIM, MGYRIS, MGMOIS, MGYRJW, MGMOJW, MGYRMD, MGMOMD, MGYRMGL, MGMOMGL, 
               MGYRMGT, MGMOMGT, MGYRMLT, MGMOMLT, MGYRMX, MGMOMX, MGYRNA, MGMONA, MGYRNDR, MGMONDR, MGYRNE, MGMONE,
               MGYRNX, MGMONX, MGYRON, MGMOON, MGYROT, MGMOOT, MGYRPRN, MGMOPRN, MGYRRL, MGMORL, MGYRRZ,
               MGMORZ, MGYRSDZ, MGMOSDZ, MGYRSPX, MGMOSPX, MGYRSU, MGMOSU, MGYRSVD, MGMOSVD, MGYRTC, MGMOTC,
               MGYRTI, MGMOTI, MGYRTP, MGMOTP, MGYRTX, MGMOTX, MGYRVC, MGMOVC, MGYRZCT, MGMOZCT, 
               MGYRZE, MGMOZE, MGYRZLM, MGMOZLM, MGYRZM, MGMOZM, MGYRZMT, MGMOZMT, MGYRZS, MGMOZS, 
              #treatment
               MGPR,MGRE, MGDRRC,
                # demographics
               DEAGE_R, Age, DESEX, gender, RaceEthnic, DEETHNIC, DEMAR, marital, married, DEEDU, Education, 
               DEPEMP_R, Income, DEINC_R, INHV, INTYPPRI, CCI, CCIcat,
                # comorbidities
               GDDX, ALLDX, APDX, AXDX, AMDX, cancer, TMDX, BCADX, 
               VCADX, CCADX, LKDX, LMPDX, MSTDX, LCADX, OVDX, PCADX, SCADX,
               SLCADX, UCADX, OCADX, ANDX, GTDX, LUDX, OSTDX,
               OPDX, OSDX, PARDX, RHEUMDX, HDTPCT, CNDX, DPDX,
               DB2DX, DB1DX, EPDX, FMDX, HDDX, CEDX, BPDX, CDDX, UCDX,
               UNADX, OSDX, SLPAPDX, STDX, TIADX,
                # health habits
               Smoke, HHSMK, Alcohol, HHALCQ, Exercise, HHEX,
               BMI_R, BMI_CAT)
               
               
# get neurologist visits DONE 
# recalculate costs?? check with shonda
# recreate employment var
# recreate Income var
# recode insurance
# cannot find aneurysm
# check how smoke is created
# check how can cancer is calculated
# double check on non-cardiac chest pain
# add in HHSMKEV



