#  NHWS
fname <- file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Data','adhd_data.RData')
load(fname)
NHWS <- adhd_data[cohort=="ADHD"]
# Linkable
# Get Link ID
fname <- file.path(getPath('KHDICT'),'LINKER','hv','2015-2018 Master File.txt')
nhws_link   <- fread(fname)
nhws_link <- nhws_link[HVID != "0"]
#ehr_link    <- fread("~/OneDrive - Kantar/KHDICT/LINKER/2015-2018 Linked EHR IDs.txt")
#ehr_link$HVID <- tolower(ehr_link$HVID)

linkable <- nhws_link[zKey %in% NHWS$zKey]
linkable <- linkable[,c('zKey','HVID')]
linkable <- unique(linkable)

setwd(file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Data'))
# Get claims data
md_claims_1 <- fread("ADHD Medical Claims.csv",
                   colClasses = list(character = c('ndc_code','record_id','claim_id','hvid')))
md_claims_2 <- fread("ADHD Medical Claims - Part 2.csv",
                   colClasses = list(character = c('ndc_code','record_id','claim_id','hvid')))
md_claims <- rbind(md_claims_1,md_claims_2)

rx_claims_1 <- fread("ADHD Pharmacy Claims.csv",
                   colClasses = list(character = c('ndc_code','record_id','claim_id','hvid')))
rx_claims_2 <- fread("ADHD Pharmacy Claims - Part 2.csv",
                     colClasses = list(character = c('ndc_code','record_id','claim_id','hvid')))
rx_claims <- rbind(rx_claims_1,rx_claims_2)
length(unique(md_claims$hvid))
length(unique(rx_claims$hvid))
md_claims <- md_claims[hvid %in% linkable$HVID]
rx_claims <- rx_claims[hvid %in% linkable$HVID]
length(unique(md_claims$hvid))
length(unique(rx_claims$hvid))
common <- intersect(md_claims$hvid, rx_claims$hvid)
length(common)


#  Diagnosis Concordance
icd  <- searchCodes(search = 'label',vocabulary = c("ICD9CM", "ICD10CM"), pattern = list(pattern = c("attention deficit","attention-deficit")))
icd1 <- gsub("\\.","",icd$concept_code)
pt_icd <- unique(md_claims$hvid[md_claims$diagnosis_code %in% icd1])
non_adhd_icd <- setdiff(md_claims$diagnosis_code, icd1)
non_adhd_icd <- searchCodes(search = 'code', vocabulary = c("ICD9CM","ICD10CM"), pattern = list(pattern = non_adhd_icd))
#write.xlsx(icd,          file="purdue_data_mgmt.xlsx",sheetName = "ADHD ICD",row.names = FALSE)
#write.xlsx(non_adhd_icd, file="purdue_data_mgmt.xlsx",sheetName = "NON-ADHD ICD",row.names = FALSE, append = TRUE)

# Treatment Concordance
# From Drug List
drugs <- fread("adhd_drug_list", sep ="\n", header = FALSE)
drugs <- unique(sapply(strsplit(drugs$V1, " "), function(z){z[1]}))
ndc_from_list_1 <- drugSearch(drugs = drugs, indication = c("ADD","ADHD"), indication_case_ignore = FALSE)
ndc_from_list_2 <- drugSearch(drugs = drugs, indication = c("attention deficit","attention-deficit"))
ndc_from_list <- combineDrugSearch(list(ndc_from_list_1,ndc_from_list_2))
ndc_from_list <- ndc_from_list[drug_name != 'Sodium chloride']
#ndc_from_list <- drugSearch(drugs = drugs)
# ndc_details_drug <- getNDCmap(ndc_from_list$concept_code)
# # For Indication
# load("~/TEMP/FDALABEL/2019-04-20/fda_data.RData")
# id <- grep("ADHD|ADD|attention deficit|Attention Deficit",fda_label$indications_and_usage)
# drugs <- fda_label[id]
# ndc_for_ind <- fda_table[spl_set_id %in% drugs$set_id]
# ndc_for_ind <- unlist(ndc_for_ind$package_ndc)
# ndc_for_ind <- sapply(ndc_for_ind, ndc211)
# ndc_details_ind <- getNDCmap(ndc_for_ind)
# # Common to both methods
# common <- intersect(ndc_details_drug$ndc_code, ndc_details_ind$ndc_code)
# dlist  <- setdiff(ndc_details_drug$ndc_code, ndc_details_ind$ndc_code)
# ilist  <- setdiff(ndc_details_ind$ndc_code, ndc_details_drug$ndc_code)
# # MERGE
# ndc_details <- unique(rbind(ndc_details_drug, ndc_details_ind))
# ndc_details$SEARCH <- " "
# ndc_details$SEARCH[ndc_details$ndc_code %in% common] <- "Drug and Indication Search"
# ndc_details$SEARCH[ndc_details$ndc_code %in% dlist] <- "Drug Search Only"
# ndc_details$SEARCH[ndc_details$ndc_code %in% ilist] <- "Indication Search Only"
# DRUG Concordance
pt_ndc <- unique(rx_claims$hvid[rx_claims$ndc_code %in% ndc_from_list$ndc_code])
non_adhd_ndc <- setdiff(rx_claims$ndc_code, ndc_from_list$ndc_code)
non_adhd_ndc <- drugSearch(ndc = non_adhd_ndc)
#write.xlsx(ndc_details,  file="purdue_data_mgmt.xlsx",sheetName = "ADHD DRUG LIST",row.names = FALSE, append = TRUE)
#write.xlsx(non_adhd_ndc_details, file="purdue_data_mgmt.xlsx",sheetName = "NON-ADHD DRUG LIST",row.names = FALSE, append = TRUE)


NHWS <- adhd_data
NHWS$RX_USE  <- NHWS$ADHRX
NHWS$RX_USE[NHWS$ADHRX == 1 | NHWS$ADRX == 1] <- 1
pt_id_rx <- unique(NHWS$zKey[NHWS$RX_USE==1])

tab_data <- linkable
#tab_data <- tab_data[HVID %in% unique(c(md_claims$hvid,rx_claims$hvid))]
tab_data$SR <- 1
tab_data$ICD <- 0
tab_data$ICD[tab_data$HVID %in% pt_icd] <- 1
tab_data$NDC <- 0
tab_data$NDC[tab_data$HVID %in% pt_ndc] <- 1
tab_data$RX <- 0
tab_data$RX[tab_data$zKey %in% pt_id_rx]<-1
table(tab_data$SR,tab_data$ICD)
table(tab_data$SR,tab_data$NDC)
table(tab_data$RX,tab_data$NDC)

