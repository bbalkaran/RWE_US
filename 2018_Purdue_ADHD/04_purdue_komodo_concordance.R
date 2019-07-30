#  NHWS
fname <- file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Data','adhd_data.RData')
load(fname)
NHWS <- adhd_data[cohort=="ADHD"]

project <- '108_Purdue_ADHD'
readKomodo(project)
linker <- getLinkInfo(NHWS)
reassignID('NHWS',linker)
eligibility <- enrollKomodo(claim_enrollment,years = 2015:2018, m.status = 'cl2', p.status = 'cl2', 
                            min.continuous = 1, window = 'nhws', state.ignore = TRUE)
                      
# Get Drug Names
setwd(file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Data'))
drugs <- fread("adhd_drug_list", sep ="\n", header = FALSE)
drugs <- unique(sapply(strsplit(drugs$V1, " "), function(z){z[1]}))
ndc_from_list_1 <- drugSearch(drugs = drugs, indication = c("ADD","ADHD"), indication_case_ignore = FALSE)
ndc_from_list_2 <- drugSearch(drugs = drugs, indication = c("attention deficit","attention-deficit"))
ndc_from_list <- combineDrugSearch(list(ndc_from_list_1,ndc_from_list_2))
ndc_from_list <- ndc_from_list[!(drug_name %in% c('Sodium chloride','Glucose'))]
ndc_from_list$drug_name[ndc_from_list$drug_name == 'Human prescription drug'] <- 'Clonidine hydrochloride'
ndc_from_list <- ndc_from_list[,1:7]
ind <- grep("XR$",ndc_from_list$drug_name)
ndc_from_list$release_type <- "IR"
ndc_from_list$release_type[ind] <- 'ER'
ndc_from_list$ndc9 <- substr(ndc_from_list$ndc_code,1,9)
ndc_from_list <- unique(ndc_from_list,by= 'ndc9')
ndc_from_list$drug_for <- 'ADHD'
# Drugs in claims 
ndc_in_claims <- unique(claim_pharmacy$ndc9)
non_adhd_ndc  <- setdiff(ndc_in_claims, ndc_from_list$ndc9)
non_adhd_ndc  <- non_adhd_ndc[sapply(non_adhd_ndc,nchar)==9]
non_adhd_ndc  <- non_adhd_ndc[non_adhd_ndc != 'INV999999'] 
# drug search for non-adhd_drugs 
non_adhd_ndc  <- drugSearch(ndc = non_adhd_ndc,ndc9_search = TRUE)
non_adhd_ndc  <- unique(non_adhd_ndc,by = 'ndc9')
non_adhd_ndc <- non_adhd_ndc[,1:8]
non_adhd_ndc$release_type <- NA
non_adhd_ndc <- unique(non_adhd_ndc,by= 'ndc9')
non_adhd_ndc$drug_for <- 'NON-ADHD'
drug_names <- bind_rows(list(ndc_from_list, non_adhd_ndc))
drug_names <- unique(drug_names, by = 'ndc9')
# Add drug names to claims 
claim_pharmacy  <- merge(claim_pharmacy, drug_names[,c('ndc9','drug_name','release_type','drug_for')], by = 'ndc9', all.x = TRUE)
claim_pharmacy  <- claim_pharmacy[ndc9 != '']

# Classify patients as IR/ER/Combined
adhd_drug_claims <- claim_pharmacy[drug_for == 'ADHD']
adhd_drug_claims <- adhd_drug_claims[order(kid2,date_of_service)]
adhd_drug_claims$date_of_service <- as.Date(adhd_drug_claims$date_of_service, format = "%Y-%m-%d")
adhd_drug_claims[,temp1 := shift(release_type, fill = release_type[.N], type = 'lead'),  by = 'kid2']
adhd_drug_claims[,temp2 := shift(date_of_service,type = 'lead') - date_of_service,  by = 'kid2']
adhd_drug_claims[,class := ifelse(temp2 <=30, paste(sort(unique(c(release_type, temp1))), collapse= "+"),release_type)]
adhd_drug_claims[,final_class:= any(grepl('ER+IR',class)),by = 'kid2']




