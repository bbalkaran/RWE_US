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
drug_names <- bind_rows(list(ndc_from_list, non_adhd_ndc))
drug_names <- unique(drug_names, by = 'ndc9')

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
#claim_pharmacy <- claim_pharmacy[date_of_service >= start & date_of_service <= end]
#claim_medical  <- claim_medical[date_of_service >= start & date_of_service <= end]
N_cont_medical <- length(unique(claim_medical$kid2))
N_cont_pharma  <- length(unique(claim_pharmacy$kid2))
N_cont_both    <- length(intersect(claim_pharmacy$kid2,claim_medical$kid2))

# Subset to patients who are prsent in both medical and pharmacy claims 
final_pt_list <- intersect(as.numeric(claim_medical$kid2), as.numeric(claim_pharmacy$kid2))
claim_pharmacy <- claim_pharmacy[kid2 %in% final_pt_list]
claim_medical  <- claim_medical[kid2 %in% final_pt_list]
claim_header   <- claim_header[kid2 %in% final_pt_list]

adhd_drug_claims <- claim_pharmacy[!(transaction_type %in% c('REJECTED','REVERSED'))]
adhd_drug_claims <- adhd_drug_claims[drug_for == 'ADHD']
adhd_drug_claims <- adhd_drug_claims[order(kid2,date_of_service)]
class_info <- adhd_drug_claims[,.(kid2,date_of_service,release_type,days_supply)]
class_info <- unique(class_info)
id <- duplicated(class_info[,1:4])
ind <- which(id)
ind <- class_info[ind,1:4]
class_info_2 <- merge(class_info, ind, by = names(ind))
adhd_drug_claims <- adhd_drug_claims[!(kid2 %in% class_info_2$kid2)]
class_info <- adhd_drug_claims[,.(kid2,date_of_service,release_type,days_supply)]
class_info <- unique(class_info)

### Code for similar to Jaromir's abstract
class_info[,initiate := .SD[1], .SDcols = 'release_type', by = 'kid2']
class_info[,`:=`(temp_start = .(list(date_of_service)), temp_rt = .(list(release_type))), by = 'kid2']
class_info$alt <- sapply(1:nrow(class_info), function(i){min(which(class_info$temp_rt[[i]][[1]] != class_info$release_type[i]))})
class_info$alt2 <- sapply(1:nrow(class_info), function(i){ifelse(is.finite(class_info$alt[i]),class_info$temp_start[[i]][[1]][class_info$alt[i]],NA)})
class_info$alt2 <- as.Date('1970-01-01') + class_info$alt2 
class_info$alt3 <- sapply(1:nrow(class_info), function(i){ifelse(is.finite(class_info$alt[i]),class_info$temp_rt[[i]][[1]][class_info$alt[i]],NA)})
class_info$delta <- class_info$alt2 - class_info$date_of_service
class_info[, count := sequence(.N), by = kid2]
class_info <- class_info[count==1]
class_info$class <- ifelse(is.na(class_info$delta) | class_info$delta>0,class_info$release_type,paste(class_info$release_type,class_info$alt3, sep = " + "))
class_info$class[class_info$class=='IR + ER'] <- 'ER + IR' 
class_info$index <- class_info$date_of_service
class_info$mult <- sapply(class_info$temp_rt, function(z){sapply(z,function(x){length(unique(x))})})
# merge with claims data 
claim_pharmacy <- merge(claim_pharmacy,class_info[,c('kid2','class','index','mult')], by = 'kid2', all.x = TRUE)
