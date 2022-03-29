mood <- c("29383 29600 29601 29602 29603 29604 29605 29606 29610 29611 29612 29613 29614 29615 29616 29620 29621 29622 29623 29624 29625 29626 29630 29631 29632 29633 29634 29635 29636 29640 29641 29642 29643 29644 29645 29646 29650 29651 29652 29653 29654 29655 29656 29660 29661 29662 29663 29664 29665 29666 2967 29680 29681 29682 29689 29690 29699 3004 311")
mood <- strsplit(mood," ")[[1]]
anxiety <- c("29384 30000 30001 30002 30009 30010 30020 30021 30022 30023 30029 3003 3005 30089 3009 3080 3081 3082 3083 3084 3089 30981 3130 3131 31321 31322 3133 31382 31383")
anxiety <- strsplit(anxiety," ")[[1]]
impulse_control <- c('31230 31231 31232 31233 31234 31235 31239')
impulse_control <- strsplit(impulse_control," ")[[1]]

setwd("~/Downloads/ccs_dx_icd10cm_2019_1/")
dat <- fread("ccs_dx_icd10cm_2019_1.csv",header = TRUE)
id_anxiety <- which(dat$`'CCS CATEGORY DESCRIPTION'`=='Anxiety disorders')
id_mood <- which(dat$`'CCS CATEGORY DESCRIPTION'`=='Mood disorders')
id_icd <- which(dat$`'CCS CATEGORY DESCRIPTION'`=='Impulse control disorders NEC')

mood <- c(mood,gsub("'","",dat$`'ICD-10-CM CODE'`[id_mood]))
anxiety <- c(anxiety,gsub("'","",dat$`'ICD-10-CM CODE'`[id_anxiety]))
impulse_control <- c(impulse_control,gsub("'","",dat$`'ICD-10-CM CODE'`[id_icd]))

w <- .internal_ohdsi_env$concept
w <- w[vocabulary_id %in% c("ICD9CM","ICD10CM")]
w$concept_code <- gsub("\\.","",w$concept_code)

mood1 <- w[concept_code %in% mood]
anx1  <- w[concept_code %in% anxiety]
icd1  <- w[concept_code %in% impulse_control]

mood1 <- mood1[,c("concept_code","concept_name","vocabulary_id")]
anx1  <- anx1[,c("concept_code","concept_name","vocabulary_id")]
icd1  <- icd1[,c("concept_code","concept_name","vocabulary_id")]
mood1$condition <- "Mood Disorders"
anx1$condition <- "Anxiety Disorders"
icd1$condition <- "Impulse Control Disorders"
codes <- rbindlist(list(mood1,anx1,icd1))

setwd("~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/12_06_2019/12_10_2019")
write.table(codes,file="subgroyp_codes.txt",sep="\t",row.names = FALSE)
