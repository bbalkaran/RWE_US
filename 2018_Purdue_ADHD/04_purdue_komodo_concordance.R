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
corrected_drugs <- fread("adhd_drugs_AM.txt",sep ="\t", header = TRUE)
corrected_drugs$type[corrected_drugs$type==""]<-"IR"
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
# correction based on Jaromirs file in Novmeber
temp1 <- unique(ndc_from_list[,c('drug_name','release_type')])
temp1 <- split(temp1, temp1$release_type) 
temp2 <- split(corrected_drugs, corrected_drugs$type)
# correction IR to ER 
ir2er <- c('Atomoxetine','Catapres-tts-1','Catapres-tts-2','Catapres-tts-3','Combipres','Cotempla','Daytrana','Intuniv','Jenloga','Kapvay','Quillichew','Strattera','Vyvanse')
id <- unlist(sapply(ir2er, function(z){grep(z,ndc_from_list$drug_name,ignore.case = TRUE)}))
ndc_from_list$release_type[id] <- "ER"
er2ir <- c('Clonidine')
id <- unlist(sapply(er2ir, function(z){grep(z,ndc_from_list$drug_name,ignore.case = TRUE)}))
ndc_from_list$release_type[id] <- "IR"
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
claim_pharmacy <- claim_pharmacy[date_of_service >= start & date_of_service <= end]
claim_medical  <- claim_medical[date_of_service >= start & date_of_service <= end]
N_cont_medical <- length(unique(claim_medical$kid2))
N_cont_pharma  <- length(unique(claim_pharmacy$kid2))
N_cont_both    <- length(intersect(claim_pharmacy$kid2,claim_medical$kid2))

# Subset to patients who are prsent in both medical and pharmacy claims 
final_pt_list <- intersect(as.numeric(claim_medical$kid2), as.numeric(claim_pharmacy$kid2))
claim_pharmacy <- claim_pharmacy[kid2 %in% final_pt_list]
claim_medical  <- claim_medical[kid2 %in% final_pt_list]
claim_header   <- claim_header[kid2 %in% final_pt_list]

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

# Classify patients as IR/ER/Combined based on days between drugs 

# drug_gap <- 30
# adhd_drug_claims <- claim_pharmacy[drug_for == 'ADHD']
# adhd_drug_claims <- adhd_drug_claims[order(kid2,date_of_service)]
# adhd_drug_claims[,delta_survey :=  date_of_service - survey_date]
# class_info <- adhd_drug_claims[,.(kid2,date_of_service,release_type, delta_survey)]
# class_info <- unique(class_info)
# class_info[,`:=`(temp_dos = .(list(date_of_service)), temp_rt = .(list(release_type))), by = 'kid2']
# class_info$release_type_30 <- sapply(1:nrow(class_info), function(i){unique(class_info$temp_rt[[i]][[1]][which(class_info$temp_dos[[i]][[1]] - class_info$date_of_service[i] <= drug_gap)])})
# class_info$release_type_30 <- sapply(1:nrow(class_info), function(i){paste(sort(unique(c(class_info$release_type[i],class_info$release_type_30[[i]]))), collapse=" + ")})
# fn1 <- function(x){
#   min.x <- max(x[x<0], na.rm = TRUE)
#   max.x <- min(x[x>=0], na.rm = TRUE)
#   if(abs(min.x) < abs(max.x)){
#     out <- min.x
#   }else{
#     out <- max.x
#   }
#   return(out)
# }
# class_info[,survey_window := fn1(delta_survey), by = 'kid2']
# class_info <- class_info[delta_survey == survey_window]
# class_info <- class_info[,.(kid2,delta_survey,release_type_30)]
# class_info <- unique(class_info)

# Classify patients as IR/ER/Combined based on overlap between start of first and end of second

adhd_drug_claims <- claim_pharmacy[!(transaction_type %in% c('REJECTED','REVERSED'))]
adhd_drug_claims <- adhd_drug_claims[drug_for == 'ADHD']
adhd_drug_claims[,delta_survey :=  date_of_service - survey_date]
adhd_drug_claims[,nearest := min(delta_survey), by = "kid2"]
adhd_drug_claims <- adhd_drug_claims[order(kid2,date_of_service)]
class_info <- adhd_drug_claims[,.(kid2,date_of_service,release_type, delta_survey,days_supply,nearest)]
class_info <- unique(class_info)
id <- duplicated(class_info[,1:4])
ind <- which(id)
ind <- class_info[ind,1:4]
class_info_2 <- merge(class_info, ind, by = names(ind))
adhd_drug_claims <- adhd_drug_claims[!(kid2 %in% class_info_2$kid2)]
class_info <- adhd_drug_claims[,.(kid2,date_of_service,release_type, delta_survey,days_supply,nearest)]
class_info <- unique(class_info)

### Code for similar to Jaromir's abstract
class_info[,initiate := .SD[1], .SDcols = 'release_type', by = 'kid2']
class_info[,`:=`(temp_start = .(list(date_of_service)), temp_rt = .(list(release_type))), by = 'kid2']
class_info$switch <- sapply(1:nrow(class_info), function(i){min(class_info$temp_start[[i]][[1]][class_info$temp_rt[[i]][[1]] != class_info$release_type[i]])})
  


#### Code for concomitant overlap  
class_info$start <- class_info$date_of_service
class_info$end   <- class_info$date_of_service + class_info$days_supply
class_info[,`:=`(temp_start = .(list(start)), temp_rt = .(list(release_type))), by = 'kid2']
class_info$start2 <- sapply(1:nrow(class_info), function(i){id <- which(class_info$temp_rt[[i]][[1]] != class_info$release_type[i]);
                                                            temp <- class_info$temp_start[[i]][[1]][id]; temp <- temp[which(temp>=class_info$start[i])]; min(temp)})
class_info$start2 <- as.Date('1970-01-01') + class_info$start2
class_info$gap <- class_info$start2 - class_info$end
class_info$type15 <- ifelse(is.na(class_info$gap) | class_info$gap > 0 | class_info$gap < -15, class_info$release_type, "ER + IR")
class_info$type30 <- ifelse(is.na(class_info$gap) | class_info$gap > 0 | class_info$gap < -30, class_info$release_type, "ER + IR")
class_info$type60 <- ifelse(is.na(class_info$gap) | class_info$gap > 0 | class_info$gap < -60, class_info$release_type, "ER + IR")
class_info$type90 <- ifelse(is.na(class_info$gap) | class_info$gap > 0 | class_info$gap < -90, class_info$release_type, "ER + IR")
class_info$type00 <- ifelse(is.na(class_info$gap) | class_info$gap > 0 , class_info$release_type, "ER + IR")
fn1 <- function(x){
  min.x <- max(x[x<0], na.rm = TRUE)
  max.x <- min(x[x>=0], na.rm = TRUE)
  if(abs(min.x) < abs(max.x)){
    out <- min.x
  }else{
    out <- max.x
  }
  return(out)
}
class_info[,survey_window := fn1(delta_survey), by = 'kid2']
class_info <- class_info[delta_survey == survey_window]
class_info_15 <- unique(class_info[,.(kid2,delta_survey,type15,nearest)])
class_info_30 <- unique(class_info[,.(kid2,delta_survey,type30,nearest)])
class_info_60 <- unique(class_info[,.(kid2,delta_survey,type60,nearest)])
class_info_90 <- unique(class_info[,.(kid2,delta_survey,type90,nearest)])
class_info_00 <- unique(class_info[,.(kid2,delta_survey,type00,nearest)])
class_info_30 <- class_info_30[!duplicated(kid2)]
class_info_60 <- class_info_60[!duplicated(kid2)]
class_info_90 <- class_info_90[!duplicated(kid2)]
class_info_00 <- class_info_00[!duplicated(kid2)]

class_info <- unique(class_info_90)



# --------------------------------------------------------------------------------------
# PREP data for Analysis 
# --------------------------------------------------------------------------------------
# substitute NHWS data to those patients selected in claims 
linker2 <- linker[kid2 %in% final_pt_list]
ind <- unique(linker2, by = c('zKey2','zKey'))
NHWS <- NHWS[zKey2 %in% ind$zKey2]
ind <- unique(linker2, by = c('zKey2','kid2'))
NHWS  <- merge(NHWS,ind, by = 'zKey2')
# attach class info 
NHWS <- merge(NHWS,class_info[,.(kid2,type90)], by = 'kid2', all.x = TRUE)
NHWS$type90[is.na(NHWS$type90)] <-'No_ADHD_RX'

save(NHWS, file="NHWS_linked.RData")

#-------------------------------------
# END OF DATA PREP - Followinhg is just exploratory code - DO  NOT RUN
#-------------------------------------


# Prescription Claims 
## Total number of medications
no_of_medications <- claim_pharmacy[, j= list(no_of_medications = length(unique(drug_name))), by = c('kid2','drug_for')]
no_of_medications <- dcast(no_of_medications, kid2 ~ drug_for, value.var = 'no_of_medications')

# Medical Claims 
##  Comorbidities
comorbs <- claim_header[,c('kid2','da',paste0('d',1:26))]
comorbs[comorbs == ''] <- NA
comorbs <- melt(comorbs, id.vars = 'kid2', measure.vars = c('da',paste0('d',1:26)), na.rm = TRUE)
icd_comorbs <- comorbid_quan_deyo(comorbs,visit_name = 'kid2', icd_name = 'value',return_df = TRUE)
cci <- charlson_from_comorbid(icd_comorbs)
icd_comorbs[,-1] <- icd_comorbs[,-1] *1
icd_comorbs <- as.data.table(icd_comorbs)
icd_comorbs$cci <- cci
## Depression, Anxiety 
AXDX     <- searchCodes("label",pattern = list(pattern = c("anxiety")), vocabulary = c("ICD9CM","ICD10CM"))
AXDX     <- AXDX[domain_id == 'Condition']
DPDX     <- searchCodes("label",pattern = list(pattern = c("major depressive disorder")), vocabulary = c("ICD9CM","ICD10CM"))
DPDX     <- DPDX[domain_id == 'Condition']
dx_codes <- list(AXDX = AXDX,DPDX = DPDX)
dx_codes <- lapply(dx_codes, function(z){z$code <- gsub("\\.","",z$concept_code);z})

dummyDX <- function(data, dx_codes){
  # Set values of dx codes to zero or 1
  ind <- mclapply(dx_codes, function(z){apply(data[,c(paste0("d",1:26),'da')], 1, function(x){any(x %in% z$code)})}, mc.cores = 5)
  ind <- do.call(cbind,ind)
  ind <- as.data.table(ind*1)
  data <- cbind(data,ind)
  #  data <- data[,-paste0("D",1:27)]
  return(data)
}
comorbs2 <- claim_header %>% dummyDX(., dx_codes)
comorbs2 <- comorbs2[,c('kid2','AXDX','DPDX')]
comorbs2 <- comorbs2[,j = list(AXDX = max(AXDX, na.rm = TRUE), DPDX = max(DPDX, na.rm = TRUE)), by = 'kid2']
comorbs2$kid2 <- as.character(comorbs2$kid2)
comorbs <- merge(icd_comorbs, comorbs2, all = TRUE, by = 'kid2' )

# Length of treatment 
lot <- claim_pharmacy[drug_for=='ADHD' & transaction_type == 'PAID',.(kid2,date_of_service, days_supply,drug_name)]
lot <- lot[order(kid2,drug_name,date_of_service)]
lot <- unique(lot)
lot[,overlap:=ifelse(date_of_service < shift(date_of_service,1,type='lag', fill = 0) + days_supply,date_of_service - shift(date_of_service,1,type='lag', fill = 0),0), by = c('kid2','drug_name')]
lot[,new_days_supply := days_supply - overlap]
lot <- lot[,j = list(lot = sum(new_days_supply), min_date = min(date_of_service), max_date = max(date_of_service)), by = c('kid2','drug_name')]
lot <- lot[order(kid2,min_date,max_date)]
lot[,adj:=ifelse(min_date < shift(max_date,1,type='lag', fill = 0), shift(max_date,1,type='lag', fill = 0) - min_date ,0), by = c('kid2')]
lot[,lot2:=ifelse(lot-adj<0,0,lot-adj)]
lot <- lot[,j = list(lot2 = sum(lot2)), by = 'kid2']

# add  class info to medical and pharmacy claims 
claim_medical  <- merge(claim_medical,  class_info[,.(kid2,release_type_30)], by = 'kid2', all.x = TRUE)
claim_pharmacy <- merge(claim_pharmacy, class_info[,.(kid2,release_type_30)], by = 'kid2', all.x = TRUE)

adhd_drug_claims[,temp1 := shift(release_type, fill = release_type[.N], type = 'lead'),  by = 'kid2']
adhd_drug_claims[,temp2 := shift(date_of_service,type = 'lead') - date_of_service,  by = 'kid2']
adhd_drug_claims$class <- ''
for(i in 1:nrow(adhd_drug_claims)){
  if(!is.na(adhd_drug_claims$temp2[i]) & adhd_drug_claims$temp2[i] <= drug_gap){
    adhd_drug_claims$class[i] <- paste(sort(unique(c(adhd_drug_claims$release_type[i], adhd_drug_claims$temp1[i]))), collapse= "+")
  }else{
    adhd_drug_claims$class[i] <- adhd_drug_claims$release_type[i]
  }
}

# For some reason this is not working. explore later 
#adhd_drug_claims[,class := if(temp2 <= drug_gap){paste(sort(unique(c(release_type, temp1))), collapse= "+")}else{release_type}]
adhd_drug_claims[,final_class_ind:= sum(class == 'ER+IR') >0, by = 'kid2']
adhd_drug_claims$final_class <-''
for(i in 1:nrow(adhd_drug_claims)){
  if(adhd_drug_claims$final_class_ind[i]){
    adhd_drug_claims$final_class[i] <- "ER+IR"
  }else{
    adhd_drug_claims$final_class[i] <- adhd_drug_claims$class[i]
  }
}
counter <- unique(adhd_drug_claims[,c('kid2','final_class')])


