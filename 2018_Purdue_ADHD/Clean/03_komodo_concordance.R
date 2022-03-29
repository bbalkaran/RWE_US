#  NHWS
fname <- file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Data','adhd_data.RData')
load(fname)
NHWS <- adhd_data[cohort=="ADHD"]

project <- '108_Purdue_ADHD'
readKomodo(project)
linker <- getLinkInfo(NHWS)
reassignID('NHWS',linker)
#eligibility <- enrollKomodo(claim_enrollment,years = 2015:2018, m.status = 'cl0', p.status = 'cl0', 
#                            min.continuous = 1, window = 'nhws', state.ignore = TRUE)


#----------------------------------------------------------------------------------------------------------------
# Raw numbers 
#----------------------------------------------------------------------------------------------------------------
N_medical <- length(unique(claim_medical$kid2))
N_pharma  <- length(unique(claim_pharmacy$kid2))
N_both    <- length(intersect(claim_medical$kid2, claim_pharmacy$kid2))
N_elig    <- length(unique(claim_enrollment$kid2))

#----------------------------------------------------------------------------------------------------------------
# Concordance of Diagnosis 
#----------------------------------------------------------------------------------------------------------------
adhd_icd <- searchCodes('label', vocabulary = c('ICD9CM','ICD10CM'), design = list(ws= c('attention','deficit'), exclude = 'concentration'))
# Additional codes based on Deedback from Jaromir's subject matter expert 
adhd_icd2 <- searchCodes('code', vocabulary = c('ICD9CM','ICD10CM'), pattern = list(pattern = "314\\.[1289]"))
adhd_icd <- rbind(adhd_icd, adhd_icd2)
adhd_icd$icd <- gsub("\\.","",adhd_icd$concept_code)
ind<- apply(claim_header[,c("da",paste0("d",1:26))],1,function(z){any(z %in% adhd_icd$icd)})
diagnosis_check <- claim_header[ind]
pt_icd <- unique(diagnosis_check$kid2)

#----------------------------------------------------------------------------------------------------------------
# Concordance by Drug 
#----------------------------------------------------------------------------------------------------------------
# Get Drug Names
setwd(file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Data'))
drugs <- fread("adhd_drug_list", sep ="\n", header = FALSE)
###  THE FOLLOWING LINES REMOVED ON FEB 26, 2020 
#corrected_drugs <- fread("adhd_drugs_AM.txt",sep ="\t", header = TRUE)
#corrected_drugs$type[corrected_drugs$type==""]<-"IR"
###  THE FOLLOWING LINES ADDED ON FEB 26, 2020 
corrected_drugs <- fread("/Users/arunajadais/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Copy of adhd_drugs AM.JA_18FEB2020.txt")
corrected_drugs <-corrected_drugs[,1:2]
#corrected_drugs[corrected_drugs[[2]]=="XR"] <- "ER"
colnames(corrected_drugs) <- c('Drug name (Those with XR will be considerd as Extended Release drugs)','type')
##
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
# CLEAN UP DRUG NAMES FOr CONSISTENCYs
ndc_from_list$drug_name[ndc_from_list$drug_name == "Apraclonidine ophthalmic"]  <- "Apraclonidine" 
ndc_from_list$drug_name[ndc_from_list$drug_name == "Atomoxetine hydrochloride"] <- "Atomoxetine"
ndc_from_list$drug_name[ndc_from_list$drug_name == "Clonidine hydrochloride"] <- "Clonidine" 
ndc_from_list$drug_name[ndc_from_list$drug_name == "Clonidine hydrochloride XR"] <- "Clonidine XR" 
ndc_from_list$drug_name[ndc_from_list$drug_name == "Dexedrine spansule capsules XR"] <- "Dexedrine XR" 
ndc_from_list$drug_name[ndc_from_list$drug_name == "Dexmethylphenidate hydrochloride extended-release XR"] <- "Dexmethylphenidate hydrochloride XR" 
ndc_from_list$drug_name[ndc_from_list$drug_name == "Guanfacine hydrochloride"] <- "Guanfacine" 
ndc_from_list$drug_name[ndc_from_list$drug_name == "Metadate er XR"] <- "Metadate XR"
ndc_from_list$drug_name[ndc_from_list$drug_name == "Metadate cd XR"] <- "Metadate XR"
ndc_from_list$drug_name[ndc_from_list$drug_name == "Methylphenidate"] <- "Methylphenidate hydrochloride" 
ndc_from_list$drug_name[ndc_from_list$drug_name == "Methylphenidate hydrochloride cd XR"] <- "Methylphenidate hydrochloride XR" 
ndc_from_list$drug_name[ndc_from_list$drug_name == "Methylphenidate hydrochloride extented release capsules XR"] <- "Methylphenidate hydrochloride XR" 
ndc_from_list$drug_name[ndc_from_list$drug_name == "Nexiclon xr XR"] <- "Nexiclon XR" 
# remove unlcear drug names
ndc_from_list <- ndc_from_list[drug_name != 'Mental']
ndc_from_list <- ndc_from_list[drug_name != 'Please check or multiple drugs']
# remove drug_names suggested to be removed by Jaromir
ndc_from_list <- ndc_from_list[-grep('Apraclonidine',drug_name, ignore.case = TRUE)]
ndc_from_list <- ndc_from_list[-grep('Clorpres',drug_name, ignore.case = TRUE)]
ndc_from_list <- ndc_from_list[-grep('Iopidine',drug_name, ignore.case = TRUE)]
ndc_from_list <- ndc_from_list[-grep('Progena',drug_name, ignore.case = TRUE)]
ndc_from_list <- ndc_from_list[-grep('Nexiclon',drug_name, ignore.case = TRUE)]

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

###  THE FOLLOWING LINES REMOVED ON FEB 26, 2020 
# # correction based on Jaromirs file in Novmeber
# temp1 <- unique(ndc_from_list[,c('drug_name','release_type')])
# temp1 <- split(temp1, temp1$release_type)
# temp2 <- split(corrected_drugs, corrected_drugs$type)
# # correction IR to ER
# ir2er <- c('Atomoxetine','Catapres-tts-1','Catapres-tts-2','Catapres-tts-3','Combipres','Cotempla','Daytrana','Intuniv','Jenloga','Kapvay','Quillichew','Strattera','Vyvanse')
# id <- unlist(sapply(ir2er, function(z){grep(z,ndc_from_list$drug_name,ignore.case = TRUE)}))
# ndc_from_list$release_type[id] <- "ER"
# er2ir <- c('Clonidine')
# id <- unlist(sapply(er2ir, function(z){grep(z,ndc_from_list$drug_name,ignore.case = TRUE)}))
# ndc_from_list$release_type[id] <- "IR"
drug_names <- bind_rows(list(ndc_from_list, non_adhd_ndc))
drug_names <- unique(drug_names, by = 'ndc9')
###  THE FOLLOWING LINES ADDED ON FEB 26, 2020 
colnames(corrected_drugs) <- c("V1","V2")
drug_names <- merge(drug_names, corrected_drugs, by.x =  "drug_name", by.y = "V1", all.x = TRUE)
drug_names[,release_type:=NULL]
drug_names <- rename(drug_names, release_type = V2)
drug_names <- drug_names[!duplicated(drug_names$ndc_code)]

# Add drug names to claims 
claim_pharmacy  <- merge(claim_pharmacy, drug_names[,c('ndc9','drug_name','release_type','drug_for')], by = 'ndc9', all.x = TRUE)
claim_pharmacy  <- claim_pharmacy[ndc9 != '']

# THose with ADHD drugs 
drug_check <- claim_pharmacy[drug_for=='ADHD']
pt_ndc     <- unique(drug_check$kid2)
pt_icd_or_ndc <- unique(c(pt_icd,pt_ndc))
N_icd <- length(pt_icd)
N_ndc <- length(pt_ndc)
N_icd_or_ndc <- length(pt_icd_or_ndc)

# Split by numbers in the claims data set 
N_icd_medical <- sum(pt_icd %in% claim_medical$kid2)
N_icd_pharma  <- sum(pt_icd %in% claim_pharmacy$kid2)
N_icd_both    <- sum(pt_icd %in% claim_medical$kid2 & pt_icd %in% claim_pharmacy$kid2)
# by drugs 
N_ndc_medical <- sum(pt_ndc %in% claim_medical$kid2)
N_ndc_pharma  <- sum(pt_ndc %in% claim_pharmacy$kid2)
N_ndc_both    <- sum(pt_ndc %in% claim_medical$kid2 & pt_ndc %in% claim_pharmacy$kid2)
# By either 
N_either_medical <- sum(pt_icd_or_ndc %in% claim_medical$kid2)
N_either_pharma  <- sum(pt_icd_or_ndc %in% claim_pharmacy$kid2)
N_either_both    <- sum(pt_icd_or_ndc %in% claim_medical$kid2 & pt_icd_or_ndc %in% claim_pharmacy$kid2)

# Subset claims data to only those that are verified by either ICD or NDC and have both medical and pharmacy claims 
pt_select <- pt_icd_or_ndc[pt_icd_or_ndc %in% claim_medical$kid2 & pt_icd_or_ndc %in% claim_pharmacy$kid2]
claim_medical <- claim_medical[kid2 %in% pt_select]
claim_pharmacy <- claim_pharmacy[kid2 %in% pt_select]
claim_header   <- claim_header[kid2 %in% pt_select]

# Impose eligibility Open and Closed
eligibility <- enrollKomodo(claim_enrollment,years = 2015:2018, m.status = 'cl0', p.status = 'cl0',
                            min.continuous = 1, window = 'nhws', state.ignore = TRUE)

# Only closed or algo-closed 
# eligibility <- enrollKomodo(claim_enrollment,years = 2015:2018, m.status = 'cl2', p.status = 'cl2',
#                             min.continuous = 1, window = 'nhws', state.ignore = TRUE)

# Choose RX claims in the elgibility window:
claim_pharmacy <- merge(claim_pharmacy, eligibility$enrollment[,c('kid2','start','end')], by = 'kid2')
claim_medical  <- merge(claim_medical, eligibility$enrollment[,c('kid2','start','end')], by = 'kid2')
N_elig_medical <- length(unique(claim_medical$kid2))
N_elig_pharma  <- length(unique(claim_pharmacy$kid2))
N_elig_both    <- length(intersect(claim_pharmacy$kid2,claim_medical$kid2))
# Claims only in contigous enrollment period
claim_pharmacy$date_of_service <- as.Date(claim_pharmacy$date_of_service, format = "%Y-%m-%d")
claim_medical$date_of_service  <- as.Date(claim_medical$date_of_service, format = "%Y-%m-%d")
claim_medical$survey_date  <- as.Date(claim_medical$survey_date, format = "%Y-%m-%d")
claim_pharmacy$survey_date  <- as.Date(claim_pharmacy$survey_date, format = "%Y-%m-%d")
claim_pharmacy <- claim_pharmacy[date_of_service >= start & date_of_service <= end]
claim_medical  <- claim_medical[date_of_service >= start & date_of_service <= end]
N_cont_medical <- length(unique(claim_medical$kid2))
N_cont_pharma  <- length(unique(claim_pharmacy$kid2))
N_cont_both    <- length(intersect(claim_pharmacy$kid2,claim_medical$kid2))

# Subset to patients who are prsent in both medical and pharmacy claims 
final_pt_list <- intersect(as.numeric(claim_medical$kid2), as.numeric(claim_pharmacy$kid2))
######
claim_pharmacy <- claim_pharmacy[as.numeric(kid2) %in% final_pt_list]
claim_medical  <- claim_medical[as.numeric(kid2) %in% final_pt_list]
claim_header   <- claim_header[as.numeric(kid2) %in% final_pt_list]




# Follow-up 
follow_pharma <- claim_pharmacy[,j= list(min_date = min(date_of_service), max_date = max(date_of_service)), by = 'kid2']
follow_pharma[,follow_up_pharma := max_date - min_date]
follow_medical <- claim_medical[,j= list(min_date = min(date_of_service), max_date = max(date_of_service)), by = 'kid2']
follow_medical[,follow_up_medical := max_date - min_date]
follow_up <- merge(follow_medical,follow_pharma, by = 'kid2', all = TRUE)
follow_up[,follow_up := max(follow_up_medical, follow_up_pharma), by = 'kid2']
mean(follow_up$follow_up >= 30)
mean(follow_up$follow_up >= 60)

# Save files 
fname <- file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Results','icd_codes.txt')
write.table(adhd_icd, file = fname, sep = "\t", row.names = FALSE)
fname <- file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Results','adhd_drugs.txt')
adhd_drugs <- unique(drug_names[drug_for=='ADHD', 'drug_name'])
adhd_drugs <- adhd_drugs[order(drug_name)]
write.table(adhd_drugs, file = fname, sep = "\t", row.names = FALSE)
