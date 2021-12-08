library(sjlabelled)
library(stringr)
library(table1)

#  NHWS
fname <- file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Data','adhd_data.RData')
load(fname)
NHWS <- adhd_data[cohort=="ADHD"]

project <- '108_Purdue_ADHD'
readKomodo(project)
linker <- getLinkInfo(NHWS,linker_file = "komodo_bridge_06202019.txt") 
#linker <- getLinkInfo(NHWS,linker_file = "komodo_bridge_06202019.txt")
reassignID('NHWS',linker)

fname <- file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Data','NHWS_linked.RData')
load(fname)
NHWS <- NHWS[type90 != "No_ADHD_RX"]
# Subset claims data to  for those used in NHWS linked analysis 
pt_select <- NHWS$kid2
# those with ADHD claims 
claim_medical <- claim_medical[kid2 %in% pt_select]
claim_pharmacy <- claim_pharmacy[kid2 %in% pt_select]
claim_header   <- claim_header[kid2 %in% pt_select]

#----------------------------------------------------------------------------------------------------------------
# Concordance by Drug 
#----------------------------------------------------------------------------------------------------------------
# Get Drug Names
claim_pharmacy$ndc9 <- substr(claim_pharmacy$ndc11,1,9)
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

# Combine stimulant non-stimulant information 
#drug_type <- fread("/Users/arunajadais/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/adhd_drugs AM_JA.txt")
drug_type <- fread("/Users/arunajadais/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Copy of adhd_drugs AM.JA_18FEB2020.txt")
#drug_type <- corrected_drugs[corrected_drugs[[2]]=="XR"] <- "ER"
drug_type <- drug_type[-1,1:2]
colnames(drug_type) <- c("drug_name","drug_type")
drug_names <- merge(drug_names,drug_type, by = "drug_name", all.x = TRUE)
drug_names$drug_type[drug_names$drug_name=="Adzenys XR"]   <- "XR"
drug_names$drug_type[drug_names$drug_name=="Concerta XR"]  <- "XR"
drug_names$drug_type[drug_names$drug_name=="Cotempla"]     <- "XR"
drug_names$drug_type[drug_names$drug_name=="Dexedrine XR"] <- "XR"
drug_names$drug_type[drug_names$drug_name=="Intuniv XR"]   <- "NS"
drug_names$drug_type[drug_names$drug_name=="Kapvay XR"]    <- "NS"
drug_names$drug_type[drug_names$drug_name=="Methylin XR"]  <- "XR"
drug_names$drug_type[drug_names$drug_name=="Mydayis XR"]   <- "XR"
drug_names$drug_type[drug_names$drug_name=="Quillichew"]   <- "XR"
drug_names$drug_type[drug_names$drug_name=="Ritalin XR"]   <- "XR"
drug_names$drug_type[drug_names$drug_name=="Ritalin LA"]   <- "XR"

# Add drug names to claims 
claim_pharmacy  <- merge(claim_pharmacy, drug_names[,c('ndc9','drug_name','release_type','drug_for','drug_type')], by = 'ndc9', all.x = TRUE)
claim_pharmacy  <- claim_pharmacy[ndc9 != '']
claim_pharmacy0 <- claim_pharmacy

# merge the classifdication type 
claim_medical  <- merge(claim_medical, NHWS[,c('kid2','type90')], by = 'kid2')
claim_pharmacy <- merge(claim_pharmacy, NHWS[,c('kid2','type90')], by = 'kid2')
claim_header   <- merge(claim_header, NHWS[,c('kid2','type90')], by = 'kid2')


#### create results matrix to add metrics 
trt_pat <- unique(claim_pharmacy[,c('kid2','type90')])

### 1.  Initial treatment on ?
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

### Code for similar to Jaromir's abstract for TReatment initilization 
class_info[,initiate := .SD[1], .SDcols = 'release_type', by = 'kid2']
class_info[,`:=`(temp_start = .(list(date_of_service)), temp_rt = .(list(release_type))), by = 'kid2']
class_info$alt <- sapply(1:nrow(class_info), function(i){min(which(class_info$temp_rt[[i]][[1]] != class_info$release_type[i]))})
class_info$alt2 <- sapply(1:nrow(class_info), function(i){ifelse(is.finite(class_info$alt[i]),class_info$temp_start[[i]][[1]][class_info$alt[i]],NA)})
#class_info$alt2 <- as.Date('1970-01-01') + class_info$alt2 
class_info$alt3 <- sapply(1:nrow(class_info), function(i){ifelse(is.finite(class_info$alt[i]),class_info$temp_rt[[i]][[1]][class_info$alt[i]],NA)})
class_info$delta <- as.Date(class_info$alt2) - as.Date(class_info$date_of_service)
class_info[, count := sequence(.N), by = kid2]
class_info <- class_info[count==1]
class_info$class <- ifelse(is.na(class_info$delta) | class_info$delta>0,class_info$release_type,paste(class_info$release_type,class_info$alt3, sep = " + "))
class_info$class[class_info$class=='IR + ER'] <- 'ER + IR' 
class_info$index <- class_info$date_of_service
class_info$mult <- sapply(class_info$temp_rt, function(z){sapply(z,function(x){length(unique(x))})})
claim_pharmacy <- merge(claim_pharmacy,class_info[,c('kid2','class','index','mult')], by = 'kid2', all.x = TRUE)
claim_pharmacy$date_of_service <- as.Date(claim_pharmacy$date_of_service)
claim_pharmacy[,delta_survey :=  date_of_service - survey_date]
claim_pharmacy <- merge(claim_pharmacy,NHWS[,c('kid2','survey_window')], by = 'kid2')

### 1.0 TRT initilization
init_trt <- unique(claim_pharmacy[,c('kid2','class')])
colnames(init_trt)[2] <- "Initial Treatmet"
trt_pat <- merge(trt_pat, init_trt, by = 'kid2')

# ### At time nearest to survey how many drugs 
# temp <- claim_pharmacy[,c('kid2','drug_name','drug_for','release_type','delta_survey','survey_window','type90')]
# temp <- temp[drug_for=="ADHD"]
# temp <- temp[delta_survey >= survey_window]
# temp <- temp[order(delta_survey)]
# temp <- unique(temp)
# for(i in 1:nrow(temp)){
#   temp$ind[i] <- grepl(temp$release_type[i],temp$type90[i])
# }
# temp2 <- split(temp,temp$kid2)
# for(i in 1:length(temp2)){
#   a <- temp2[[i]]
#   len <- rle(a$ind)$lengths[1]
#   a <- a[1:len,]
#   temp2[[i]] <- unique(a[,c('kid2','drug_name','type90')])
#   
# }
# temp2 <- bind_rows(temp2)
# temp2[,count:= .N, by = c("kid2","type90")]

### 2.0 Treatment duration
# Length of treatment 
lot <- claim_pharmacy[drug_for=='ADHD' & transaction_type == 'PAID',.(kid2,date_of_service, days_supply,drug_name)]
lot <- lot[order(kid2,drug_name,date_of_service)]
lot <- unique(lot)
lot$date_of_service <- as.Date(lot$date_of_service)
lot[,overlap:=ifelse(date_of_service < shift(date_of_service,1,type='lag', fill = 0) + days_supply,date_of_service - shift(date_of_service,1,type='lag', fill = 0),0), by = c('kid2','drug_name')]
lot[,new_days_supply := days_supply - overlap]
# get discontinuation 
lot <- lot[order(kid2,date_of_service)]
discont <- lot[,j = list(discont = ifelse(date_of_service > shift(date_of_service,1,type='lag', fill = 0) + days_supply + 60,shift(date_of_service,1,type='lag', fill = NA) + days_supply,NA)), by = c('kid2')]
discont <- discont[!is.na(discont)]
discont$discont <- as.Date('1970-01-01') + discont$discont
discont <- discont[order(kid2,discont)]
discont[,count:=sequence(.N), by = 'kid2']
discont2 <- discont[count==1]
lot <- merge(lot,discont2, all.x=TRUE, by = 'kid2')
lot[,discont2:= ifelse(is.na(discont),max(date_of_service),discont), by = 'kid2']
# only retain 
lot2 <- lot[date_of_service <= discont2]
lot2 <- lot2[order(kid2,drug_name,date_of_service)]
lot2 <- lot2[,j = list(lot = sum(new_days_supply), min_date = min(date_of_service), max_date = max(date_of_service)), by = c('kid2','drug_name')]
lot2 <- lot2[order(kid2,min_date,max_date)]
lot2[,adj:=ifelse(min_date < shift(max_date,1,type='lag', fill = 0), shift(max_date,1,type='lag', fill = 0) - min_date ,0), by = c('kid2')]
lot2[,lot2:=ifelse(lot-adj<0,0,lot-adj)]
lot2 <- lot2[,j = list(lot2 = sum(lot2), start = min(min_date)), by = 'kid2']
# Total duration
lot3 <- lot
lot3 <- lot3[order(kid2,drug_name,date_of_service)]
lot3 <- lot3[,j = list(lot = sum(new_days_supply), min_date = min(date_of_service), max_date = max(date_of_service)), by = c('kid2','drug_name')]
lot3 <- lot3[order(kid2,min_date,max_date)]
lot3[,adj:=ifelse(min_date < shift(max_date,1,type='lag', fill = 0), shift(max_date,1,type='lag', fill = 0) - min_date ,0), by = c('kid2')]
lot3[,lot3:=ifelse(lot-adj<0,0,lot-adj)]
lot3 <- lot3[,j = list(lot3 = sum(lot3)), by = 'kid2']
trt_pat <- merge(trt_pat, lot2, by = 'kid2')
trt_pat <- merge(trt_pat, lot3, by = 'kid2')

# Duration Until Survey 
lot4 <- merge(lot, unique(claim_pharmacy[,c('kid2','survey_date')]), by = "kid2",all.x =TRUE)
lot4 <- lot4[date_of_service < survey_date]
lot4 <- lot4[order(kid2,drug_name,date_of_service)]
lot4 <- lot4[,j = list(lot = sum(new_days_supply), min_date = min(date_of_service), max_date = max(date_of_service)), by = c('kid2','drug_name')]
lot4 <- lot4[order(kid2,min_date,max_date)]
lot4[,adj:=ifelse(min_date < shift(max_date,1,type='lag', fill = 0), shift(max_date,1,type='lag', fill = 0) - min_date ,0), by = c('kid2')]
lot4[,lot4:=ifelse(lot-adj<0,0,lot-adj)]
lot4 <- lot4[,j = list(lot4 = sum(lot4)), by = 'kid2']
trt_pat <- merge(trt_pat, lot4, by = 'kid2', all.x = TRUE)

# Duration From Survey 
lot5 <- merge(lot, unique(claim_pharmacy[,c('kid2','survey_date')]), by = "kid2",all.x =TRUE)
lot5 <- lot5[date_of_service >= survey_date]
lot5 <- lot5[order(kid2,drug_name,date_of_service)]
lot5 <- lot5[,j = list(lot = sum(new_days_supply), min_date = min(date_of_service), max_date = max(date_of_service)), by = c('kid2','drug_name')]
lot5 <- lot5[order(kid2,min_date,max_date)]
lot5[,adj:=ifelse(min_date < shift(max_date,1,type='lag', fill = 0), shift(max_date,1,type='lag', fill = 0) - min_date ,0), by = c('kid2')]
lot5[,lot5:=ifelse(lot-adj<0,0,lot-adj)]
lot5 <- lot5[,j = list(lot5 = sum(lot5)), by = 'kid2']
trt_pat <- merge(trt_pat, lot5, by = 'kid2',all.x = TRUE)

### Treatment episodes : if greater than 60 days 
lot <- claim_pharmacy[drug_for=='ADHD' & transaction_type == 'PAID',.(kid2,date_of_service, days_supply,drug_name)]
lot <- lot[order(kid2,drug_name,date_of_service)]
lot <- unique(lot)
lot$date_of_service <- as.Date(lot$date_of_service)
lot[,overlap:=ifelse(date_of_service < shift(date_of_service,1,type='lag', fill = 0) + days_supply,date_of_service - shift(date_of_service,1,type='lag', fill = 0),0), by = c('kid2','drug_name')]
lot[,new_days_supply := days_supply - overlap]
lot$end <- lot$date_of_service + lot$new_days_supply
lot <- lot[order(kid2,date_of_service)]
lot[,delta:= date_of_service - shift(end,1,type='lag'), by = "kid2"]
lot$delta[is.na(lot$delta)] <- 0
lot$episode <- (lot$delta > 60 )*1
lot <- lot[,j=list(episode = sum(episode)), by = 'kid2']
trt_pat <- merge(trt_pat,lot,by="kid2",all.x = TRUE)


#------ Number of medications (ADHD RX group )
nom <- claim_pharmacy[transaction_type == 'PAID',.(kid2,date_of_service, days_supply,drug_name, drug_for)]
nom <- merge(nom,discont2, all.x=TRUE, by = 'kid2')
nom$date_of_service <- as.Date(nom$date_of_service)
nom[,discont2:= ifelse(is.na(discont),max(date_of_service),discont), by = 'kid2']
nom <- nom[date_of_service <= discont2]
no_of_medications <- nom[, j= list(no_of_medications = length(unique(drug_name))), by = c('kid2','drug_for')]
no_of_medications <- dcast(no_of_medications, kid2 ~ drug_for, value.var = 'no_of_medications')
trt_pat <- merge(trt_pat,no_of_medications, all = TRUE, by = 'kid2')


### INITIATED ON IR 
### Started on IR and switched to ER 
kid20 <- trt_pat$kid2[trt_pat$`Initial Treatmet`=="IR"]
claims <- claim_pharmacy[kid2 %in% kid20 & drug_for=="ADHD" & transaction_type == 'PAID']
claims <- merge(claims,discont2, all.x=TRUE, by = 'kid2')
claims$date_of_service <- as.Date(claims$date_of_service )
claims[,discont2:= ifelse(is.na(discont),max(date_of_service),discont), by = 'kid2']
claims <- claims[date_of_service <= discont2]
claims <- claims[,c('kid2','drug_name','release_type','date_of_service','days_supply','drug_type')]
claims <- claims[order(kid2,date_of_service)]
claims[,end_date := date_of_service + days_supply-1]
claims1 <- unique(claims, by = c('kid2','release_type'))
claims1 <- claims1[kid2 %in% unique(claims1$kid2[duplicated(claims1$kid2)])]
claims1[,overlap := pmax(pmin(end_date, shift(end_date,1,type='lag')) - pmax(date_of_service, shift(date_of_service,1,type='lag')) + 1,0), by = 'kid2']
claims1[,switch := ifelse(overlap>0,paste(shift(drug_name,1,type='lag'),"+",drug_name), paste(shift(drug_name,1,type='lag'),"-->",drug_name)), by = 'kid2']
claims1[,switch_type0 := ifelse(overlap>0,"Added ER to IR","Switched to ER"), by = 'kid2']
claims1[,switch_type := ifelse(overlap>0,paste0("Added ",drug_type," to IR"),paste0("Switched to ",drug_type)), by = 'kid2']
claims1[,switch2 := ifelse(overlap>0 ,paste(shift(drug_type,1,type='lag'),"+",drug_type), paste(shift(drug_type,1,type='lag'),"-->",drug_type)), by = 'kid2']
claims1 <-claims1[duplicated(kid2)]
claims11 <- claims1

### Started on IR and swithed or added another IR 
kid20 <- trt_pat$kid2[trt_pat$`Initial Treatmet`=="IR"]
claims <- claim_pharmacy[kid2 %in% kid20 & drug_for=="ADHD" & transaction_type == 'PAID']
claims <- merge(claims,discont2, all.x=TRUE, by = 'kid2')
claims$date_of_service <- as.Date(claims$date_of_service )
claims[,discont2:= ifelse(is.na(discont),max(date_of_service),discont), by = 'kid2']
claims <- claims[date_of_service <= discont2]
claims <- claims[release_type == 'IR',c('kid2','drug_name','date_of_service','days_supply','drug_type')]
claims <- claims[order(kid2,date_of_service)]
claims[,end_date := date_of_service + days_supply]
claims1 <- unique(claims, by = c('kid2','drug_name'))
claims1 <- claims1[kid2 %in% unique(claims1$kid2[duplicated(claims1$kid2)])]
claims1[,overlap := pmax(pmin(end_date, shift(end_date,1,type='lag')) - pmax(date_of_service, shift(date_of_service,1,type='lag')) + 1,0), by = 'kid2']
claims1[,switch := ifelse(overlap>0,paste(shift(drug_name,1,type='lag'),"+",drug_name), paste(shift(drug_name,1,type='lag'),"-->",drug_name)), by = 'kid2']
claims1[,switch_type0 := ifelse(overlap>0,"Added IR to IR","Switched to another IR"), by = 'kid2']
claims1[,switch_type := ifelse(overlap>0,paste0("Added ",drug_type," to IR"),paste0("Switched to ",drug_type)), by = 'kid2']
claims1[,switch2 := ifelse(overlap>0,paste(shift(drug_type,1,type='lag'),"+",drug_type), paste(shift(drug_type,1,type='lag'),"-->",drug_type)), by = 'kid2']
claims1 <-claims1[duplicated(kid2)]
claims12 <- claims1

# combine to get earliest 
claims13 <- merge(claims11[,c('kid2','date_of_service','switch_type')], claims12[,c('kid2','date_of_service','switch_type')], by = 'kid2', all= TRUE)
claims13$date_of_service.x <- ifelse(is.na(claims13$date_of_service.x),as.Date('2050-01-01'),claims13$date_of_service.x)
claims13$date_of_service.y <- ifelse(is.na(claims13$date_of_service.y),as.Date('2050-01-01'),claims13$date_of_service.y)
claims13$switch_date <- as.Date('1970-01-01') + ifelse(claims13$date_of_service.x < claims13$date_of_service.y, claims13$date_of_service.x, claims13$date_of_service.y)
claims13$switch_type <- ifelse(claims13$date_of_service.x < claims13$date_of_service.y, claims13$switch_type.x, claims13$switch_type.y)
claims13 <- claims13[,c('kid2','switch_date','switch_type')]
claims13 <- unique(claims13[order(kid2,switch_date)], by = 'kid2')
claims14 <- claims13

### INITIATED ON ER
### Started on IR and switched to ER 
kid20 <- trt_pat$kid2[trt_pat$`Initial Treatmet`=="ER"]
claims <- claim_pharmacy[kid2 %in% kid20 & drug_for=="ADHD" & transaction_type == 'PAID']
claims <- merge(claims,discont2, all.x=TRUE, by = 'kid2')
claims$date_of_service <- as.Date(claims$date_of_service )
claims[,discont2:= ifelse(is.na(discont),max(date_of_service),discont), by = 'kid2']
claims <- claims[date_of_service <= discont2]
claims <- claims[,c('kid2','drug_name','release_type','date_of_service','days_supply','drug_type')]
claims <- claims[order(kid2,date_of_service)]
claims[,end_date := date_of_service + days_supply-1]
claims1 <- unique(claims, by = c('kid2','release_type'))
claims1 <- claims1[kid2 %in% unique(claims1$kid2[duplicated(claims1$kid2)])]
claims1[,overlap := pmax(pmin(end_date, shift(end_date,1,type='lag')) - pmax(date_of_service, shift(date_of_service,1,type='lag')) + 1,0), by = 'kid2']
claims1[,switch := ifelse(overlap>0,paste(shift(drug_name,1,type='lag'),"+",drug_name), paste(shift(drug_name,1,type='lag'),"-->",drug_name)), by = 'kid2']
claims1[,switch_type0 := ifelse(overlap>0,"Added IR to ER","Switched to IR"), by = 'kid2']
claims1[,switch_type := ifelse(overlap>0,paste0("Added ",drug_type," to IR"),paste0("Switched to ",drug_type)), by = 'kid2']
claims1[,switch2 := ifelse(overlap>0,paste(shift(drug_type,1,type='lag'),"+",drug_type), paste(shift(drug_type,1,type='lag'),"-->",drug_type)), by = 'kid2']
claims1 <-claims1[duplicated(kid2)]
claims21 <- claims1

### Started on IR and swithed or added another IR 
kid20 <- trt_pat$kid2[trt_pat$`Initial Treatmet`=="ER"]
claims <- claim_pharmacy[kid2 %in% kid20 & drug_for=="ADHD" & transaction_type == 'PAID']
claims <- merge(claims,discont2, all.x=TRUE, by = 'kid2')
claims$date_of_service <- as.Date(claims$date_of_service )
claims[,discont2:= ifelse(is.na(discont),max(date_of_service),discont), by = 'kid2']
claims <- claims[date_of_service <= discont2]
claims <- claims[release_type == 'ER',c('kid2','drug_name','date_of_service','days_supply','drug_type')]
claims <- claims[order(kid2,date_of_service)]
claims[,end_date := date_of_service + days_supply]
claims1 <- unique(claims, by = c('kid2','drug_name'))
claims1 <- claims1[kid2 %in% unique(claims1$kid2[duplicated(claims1$kid2)])]
claims1[,overlap := pmax(pmin(end_date, shift(end_date,1,type='lag')) - pmax(date_of_service, shift(date_of_service,1,type='lag')) + 1,0), by = 'kid2']
claims1[,switch := ifelse(overlap>0,paste(shift(drug_name,1,type='lag'),"+",drug_name), paste(shift(drug_name,1,type='lag'),"-->",drug_name)), by = 'kid2']
claims1[,switch_type0 := ifelse(overlap>0,"Added ER to ER","Switched to another ER"), by = 'kid2']
claims1[,switch_type := ifelse(overlap>0,paste0("Added ",drug_type," to IR"),paste0("Switched to ",drug_type)), by = 'kid2']
claims1[,switch2 := ifelse(overlap>0,paste(shift(drug_type,1,type='lag'),"+",drug_type), paste(shift(drug_type,1,type='lag'),"-->",drug_type)), by = 'kid2']
claims1 <-claims1[duplicated(kid2)]
claims22 <- claims1

# combine to get earliest 
claims13 <- merge(claims21[,c('kid2','date_of_service','switch_type')], claims22[,c('kid2','date_of_service','switch_type')], by = 'kid2', all= TRUE)
claims13$date_of_service.x <- ifelse(is.na(claims13$date_of_service.x),as.Date('2050-01-01'),claims13$date_of_service.x)
claims13$date_of_service.y <- ifelse(is.na(claims13$date_of_service.y),as.Date('2050-01-01'),claims13$date_of_service.y)
claims13$switch_date <- as.Date('1970-01-01') + ifelse(claims13$date_of_service.x < claims13$date_of_service.y, claims13$date_of_service.x, claims13$date_of_service.y)
claims13$switch_type <- ifelse(claims13$date_of_service.x < claims13$date_of_service.y, claims13$switch_type.x, claims13$switch_type.y)
claims13 <- claims13[,c('kid2','switch_date','switch_type')]
claims13 <- unique(claims13[order(kid2,switch_date)], by = 'kid2')
claims24 <- claims13

### Combine switching information
switch <- rbind(claims14, claims24)
trt_pat <- merge(trt_pat, switch, by = 'kid2', all = TRUE)
trt_pat$switch_type <- ifelse(is.na(trt_pat$switch_date), 'No modification', trt_pat$switch_type)
trt_pat$mod_time <- trt_pat$switch_date - trt_pat$start
trt_pat$mod_time[trt_pat$mod_time<=0] <- NA
# prep data for swicting tables
switch_tables <- list(claims11,claims12, claims21, claims22)
switch1 <- lapply(switch_tables, function(z){a <- rev(sort(table(z$switch))); b <- prop.table(a); cbind(a,b)})
switch1 <- lapply(1:length(switch1), function(i){data.frame(cbind(rownames(switch1[[i]]),switch1[[i]],i))})
switch1 <- rbindlist(switch1)
switch2 <- lapply(switch_tables, function(z){a <- rev(sort(table(z$switch2))); b <- prop.table(a); cbind(a,b)})
switch2 <- lapply(1:length(switch2), function(i){data.frame(cbind(rownames(switch2[[i]]),switch2[[i]],i))})
switch2 <- rbindlist(switch2)
write.table(switch1, file="~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/02_27_2020/switch1.txt", sep = "\t", row.names = FALSE)
write.table(switch2, file="~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/02_27_2020/switch2.txt", sep = "\t", row.names = FALSE)

# Adherence MPR 
library(AdhereR)
claims <- claim_pharmacy[drug_for=="ADHD" & transaction_type == 'PAID']
claims <- merge(claims,discont2, all.x=TRUE, by = 'kid2')
claims$date_of_service <- as.Date(claims$date_of_service )
claims[,discont2:= ifelse(is.na(discont),max(date_of_service),discont), by = 'kid2']
claims <- claims[date_of_service <= discont2]
claims <- merge(claims, trt_pat[,c('kid2','lot2')], by = 'kid2', all.x = TRUE)
claims$temp_id <- paste(claims$kid2,claims$drug_name,sep="+")
cma1 <- CMA1(data=claims,
             ID.colname="temp_id",
             event.date.colname="date_of_service",
             event.duration.colname="days_supply",
             date.format="%Y-%m-%d");

cma11 <- cma1$CMA
cma11$kid2 <- sapply(strsplit(cma11$temp_id,"\\+"),function(z){z[1]})
cma11$CMA[cma11$CMA>1]<-1
cma11<- data.table(cma11)
cma11 <- cma11[, j = list(CMA = mean(CMA, na.rm=TRUE)), by = "kid2"]
trt_pat$kid2 <- as.character(trt_pat$kid2)
trt_pat <- merge(trt_pat, cma11, by = 'kid2', all = TRUE)
fname <- file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Data','NHWS_linked.RData')
load(fname)
# add_age 
NHWS$kid2 <- as.character(NHWS$kid2)
trt_pat <- merge(trt_pat, NHWS[,c('kid2','DEAGE_R')], by = 'kid2')
trt_pat$no_mod_age  <- ifelse(trt_pat$switch_type == "No modification", trt_pat$DEAGE_R, NA)
trt_pat$yes_mod_age <- ifelse(trt_pat$switch_type != "No modification", trt_pat$DEAGE_R, NA)

# Adherence by drug class
claims$temp_id <- paste(claims$kid2,claims$drug_name,claims$release_type,sep="+")
cma2 <- CMA1(data=claims,
             ID.colname="temp_id",
             event.date.colname="date_of_service",
             event.duration.colname="days_supply",
             date.format="%Y-%m-%d");

cma22 <- cma2$CMA
cma22$kid2 <- sapply(strsplit(cma22$temp_id,"\\+"),function(z){z[1]})
cma22$release_type <- sapply(strsplit(cma22$temp_id,"\\+"),function(z){z[3]})
cma22$CMA[cma22$CMA>1]<-1
cma22<- data.table(cma22)
cma22 <- cma22[, j = list(CMA = mean(CMA, na.rm=TRUE)), by = c("kid2","release_type")]
cma22 <- cma22[!is.nan(cma22$CMA)]
cma22  <- dcast(cma22,kid2~release_type)
colnames(cma22)[2:3] <- paste0("MPR_",colnames(cma22)[2:3])
trt_pat <- merge(trt_pat, cma22, by = 'kid2', all = TRUE)

# Concurrent use ER >= 80% and IR >= 65%
# Booster ER >= 80% and IR < 65%
trt_pat$concurrent <- (trt_pat$MPR_ER >= 0.8 & trt_pat$MPR_IR >= 0.65) * 1
trt_pat$booster    <- (trt_pat$MPR_ER >= 0.8 & trt_pat$MPR_IR < 0.65) * 1

# GENERAL CHARCTERISTIC - THIS WILL USE ALL THE PATIEMTS WIT AND WITHOUT TREATMENT
#  NHWS
fname <- file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Data','adhd_data.RData')
load(fname)
NHWS <- adhd_data[cohort=="ADHD"]

project <- '108_Purdue_ADHD'
readKomodo(project)
linker <- getLinkInfo(NHWS)
reassignID('NHWS',linker)

fname <- file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Data','NHWS_linked.RData')
load(fname)
trt_pat2 <- unique(NHWS[,c('kid2','type90')])
trt_pat2$kid2 <- as.character(trt_pat2$kid2)
NHWS$kid2 <- as.character(NHWS$kid2)
trt_pat2 <- merge(trt_pat2, NHWS[,c('kid2','DEAGE_R')], by = 'kid2')
# Substance abuse disorder
sud_type <- fread("/Users/arunajadais/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/ICD_Codes 11_25_2019_JA.txt", header = TRUE)
sud_type$V4[sud_type$V4==""] <- "SUD"
sud_type$concept_code <- gsub("\\.","",sud_type$concept_code)
sad <-searchCodes(search = "code", vocabulary = c("ICD9CM","ICD10CM"),
               pattern = list(pattern="29[1-2]\\...?|30[3-5]\\...?|F1[0-9]\\....?"))
sad<-sad[domain_id=='Condition']
sad_codes <- gsub("\\.","",sad$concept_code)
id <- sapply(c(paste("^d",1:26,"$",sep="")), function(z){grep(z,colnames(claim_header))})
id <- c(id,15)
temp <- claim_header[,..id]
#different types of SUD
sud_codes1 <- split(sud_type$concept_code,sud_type$V4)
sud_codes2 <- split(sud_type$concept_code,sud_type$V5)[2:4]
temp1 <- sapply(sud_codes1,function(x){apply(temp, 1, function(z){any(z %in% x)})})
temp2 <- sapply(sud_codes2,function(x){apply(temp, 1, function(z){any(z %in% x)})})
temp0 <- data.table(cbind(temp1,temp2))
temp <- cbind(claim_header$kid2, temp0[[1]])
for(i in 2:ncol(temp0)){
  temp <- cbind(temp, temp0[[i]])
}
temp <- data.table(temp)
colnames(temp) <- c("kid2",colnames(temp0))
temp <- temp[,j = list(ND = sum(ND), SUD = sum(SUD), SUE = sum(SUE), MILD = sum(MILD), `MOD-SEV` = sum(`MOD-SEV`),remission = sum(remission)), by = 'kid2']
for(i in 2:ncol(temp)){
  temp[[i]] <- ifelse(temp[[i]]>0,1,0)
  temp[[i]] <- as.factor(temp[[i]])
  levels(temp[[i]])[1] <- 'No'
  levels(temp[[i]])[2] <- 'Yes'
}
label(temp[[2]]) <- "Nicotine Dependence"
label(temp[[3]]) <- "Substance use disorder"
label(temp[[4]]) <- "Substance use episode"
label(temp[[5]]) <- "Mild"
label(temp[[6]]) <- "Moderate-Severe"
label(temp[[7]]) <- "remission"
temp$kid2 <- as.character(temp$kid2)
trt_pat2 <- merge(trt_pat2, temp, by = "kid2", all.x = TRUE)

# Comorbidities
anx <- searchCodes(vocabulary = c("ICD9CM","ICD10CM"),pattern = list(pattern="anxiety",exclude="induced")); anx$comorbidity <- "anxiety"
bpd <- searchCodes(vocabulary = c("ICD9CM","ICD10CM"),pattern = list(pattern="bipolar")); bpd$comorbidity <- "bipolar disorder"
dp  <- searchCodes(vocabulary = c("ICD9CM","ICD10CM"),pattern = list(pattern="depression|depressive")); dp$comorbidity <- "depression"
gsd <- searchCodes(vocabulary = c("ICD9CM","ICD10CM"),pattern = list(pattern="Generalized anxiety disorder")); gsd$comorbidity <- "generalized anxiety disorder"
ha  <- searchCodes(vocabulary = c("ICD9CM","ICD10CM"),pattern = list(pattern="headache")); ha$comorbidity <- "headache"
ocd  <- searchCodes(vocabulary = c("ICD9CM","ICD10CM"),pattern = list(pattern="Obsessive-compulsive")); ocd$comorbidity <- "obsessive-compulsive disorder"
pd  <- searchCodes(vocabulary = c("ICD9CM","ICD10CM"),pattern = list(pattern="panic disorder")); pd$comorbidity <- "panic disorder"
phobia  <- searchCodes(vocabulary = c("ICD9CM","ICD10CM"),pattern = list(pattern="phobia")); phobia$comorbidity <- "phobias"
ptsd <- searchCodes(vocabulary = c("ICD9CM","ICD10CM"),pattern = list(pattern="post-traumatic stress")); ptsd$comorbidity <- "PTSD"
sad  <- searchCodes(vocabulary = c("ICD9CM","ICD10CM"),pattern = list(pattern="social phobia")); sad$comorbidity <- "social anxiety disorder"
sch  <- searchCodes(vocabulary = c("ICD9CM","ICD10CM"),pattern = list(pattern="schizophr")); sch$comorbidity <- "schizophrenia"

comorbs <- rbind(anx,bpd,dp,gsd,ha,ocd,pd,phobia,ptsd,sad,sch)
comorbs <- comorbs[domain_id=='Condition']
comorbs$concept_code <-  gsub("\\.","",comorbs$concept_code)
comorb_codes <- split(comorbs$concept_code,comorbs$comorbidity)
id <- sapply(c(paste("^d",1:26,"$",sep="")), function(z){grep(z,colnames(claim_header))})
id <- c(id,15)
for(i in 1:length(comorb_codes)){
  temp <- claim_header[,..id]
  temp <- apply(temp, 1, function(z){any(z %in% comorb_codes[[i]])})
  temp <- data.table(cbind(claim_header$kid2, temp))
  temp <- temp[,j = list(comorb = sum(V2)), by = 'V1']
  temp[,2] <- ifelse(temp[,2]>0,1,0)
  colnames(temp) <- c("kid2",names(comorb_codes)[i])
  temp[[2]] <- as.factor(temp[[2]])
  levels(temp[[2]])[1] <- 'No'
  levels(temp[[2]])[2] <- 'Yes'
  temp$kid2 <- as.character(temp$kid2)
  label(temp[[2]]) <- stri_trans_totitle(names(comorb_codes)[i])
  trt_pat2 <- merge(trt_pat2, temp, by = "kid2", all.x = TRUE)
}

# IP/OP/ER visits
cpt_codes <- list(IP = c(99217:99239, 99304:99340, 99477:99482), OP = c(99201:99215, 99341:99355, 99358:99360), ER=c(99281:99292, 99466:99476))
claim_medical[,IP:=(procedure %in% cpt_codes$IP)*1]
claim_medical[,OP:=(procedure %in% cpt_codes$OP)*1]
claim_medical[,ER:=(procedure %in% cpt_codes$ER)*1]
temp <- unique(claim_medical[,c('kid2','date_of_service','IP','OP','ER','service_to','service_from')])
temp <- temp[,j=list(IP = sum(IP),OP = sum(OP), ER = sum(ER), service_to = max(service_to), service_from = min(service_from)), by = 'kid2']
temp$kid2 <- as.character(temp$kid2)
temp$service_from <- as.Date(temp$service_from)
temp$service_to <- as.Date(temp$service_to)
temp$med_duration <- (temp$service_to - temp$service_from + 1)/dyears(1)
temp$IP <- temp$IP/temp$med_duration
temp$OP <- temp$OP/temp$med_duration
temp$ER <- temp$ER/temp$med_duration
temp[,service_to:=NULL]
temp[,service_from:=NULL]
trt_pat2 <- merge(trt_pat2, temp, by = "kid2", all.x = TRUE)
# Direct Medical Costs 
trt_pat2$kid2 <- as.numeric(trt_pat2$kid2)
#NHWS$kid2 <- as.numeric(NHWS$kid2)
#trt_pat2 <- merge(trt_pat2, NHWS[,c("kid2","DEAGE_R")], by = "kid2", all.x = TRUE)
trt_pat2 <- direct_costs_claims(trt_pat2)
label(trt_pat2$IP) <- "Inpatient"
label(trt_pat2$OP) <- "Outpatient"
label(trt_pat2$ER) <- "Emergency Room"
label(trt_pat2$Total_Direct_Cost) <- "Total Direct Cost (Annual)"

# ------------------------------------------------------------------
# treatment Duration for patients not on treatment 
# ------------------------------------------------------------------
# Length of treatment 
pt_id <- trt_pat2$kid2[trt_pat2$type90=='No_ADHD_RX']
pt_id <- pt_id[!is.na(pt_id)]
claim_pharmacy$kid2 <- as.character(claim_pharmacy$kid2)
lot <- claim_pharmacy[kid2 %in% pt_id & transaction_type == 'PAID',.(kid2,ndc11,date_of_service, days_supply)]
lot$drug_name <- substr(lot$ndc11,1,9)
lot[,ndc11 := NULL]
lot <- lot[order(kid2,drug_name,date_of_service)]
lot <- unique(lot)
lot$date_of_service <- as.Date(lot$date_of_service)
lot[,overlap:=ifelse(date_of_service < shift(date_of_service,1,type='lag', fill = 0) + days_supply,date_of_service - shift(date_of_service,1,type='lag', fill = 0),0), by = c('kid2','drug_name')]
lot[,new_days_supply := days_supply - overlap]
# Total duration
lot3 <- lot
lot3 <- lot3[order(kid2,drug_name,date_of_service)]
lot3 <- lot3[,j = list(lot = sum(new_days_supply), min_date = min(date_of_service), max_date = max(date_of_service)), by = c('kid2','drug_name')]
lot3 <- lot3[order(kid2,min_date,max_date)]
lot3[,adj:=ifelse(min_date < shift(max_date,1,type='lag', fill = 0), shift(max_date,1,type='lag', fill = 0) - min_date ,0), by = c('kid2')]
lot3[,lot3:=ifelse(lot-adj<0,0,lot-adj)]
lot3 <- lot3[,j = list(lot3 = sum(lot3)), by = 'kid2']
a <- setdiff(pt_id,lot3$kid2)
junk <- data.table(kid2=a,lot3=NA)
lot3 <- rbind(lot3,junk)
NO_ADHD_LOT  <- lot3

# Duration Until Survey 
lot4 <- merge(lot, unique(claim_pharmacy[,c('kid2','survey_date')]), by = "kid2",all.x =TRUE)
lot4 <- lot4[date_of_service < survey_date]
lot4 <- lot4[order(kid2,drug_name,date_of_service)]
lot4 <- lot4[,j = list(lot = sum(new_days_supply), min_date = min(date_of_service), max_date = max(date_of_service)), by = c('kid2','drug_name')]
lot4 <- lot4[order(kid2,min_date,max_date)]
lot4[,adj:=ifelse(min_date < shift(max_date,1,type='lag', fill = 0), shift(max_date,1,type='lag', fill = 0) - min_date ,0), by = c('kid2')]
lot4[,lot4:=ifelse(lot-adj<0,0,lot-adj)]
lot4 <- lot4[,j = list(lot4 = sum(lot4)), by = 'kid2']
NO_ADHD_LOT  <- merge(NO_ADHD_LOT , lot4, by = 'kid2', all.x = TRUE)

# Duration From Survey 
lot5 <- merge(lot, unique(claim_pharmacy[,c('kid2','survey_date')]), by = "kid2",all.x =TRUE)
lot5 <- lot5[date_of_service >= survey_date]
lot5 <- lot5[order(kid2,drug_name,date_of_service)]
lot5 <- lot5[,j = list(lot = sum(new_days_supply), min_date = min(date_of_service), max_date = max(date_of_service)), by = c('kid2','drug_name')]
lot5 <- lot5[order(kid2,min_date,max_date)]
lot5[,adj:=ifelse(min_date < shift(max_date,1,type='lag', fill = 0), shift(max_date,1,type='lag', fill = 0) - min_date ,0), by = c('kid2')]
lot5[,lot5:=ifelse(lot-adj<0,0,lot-adj)]
lot5 <- lot5[,j = list(lot5 = sum(lot5)), by = 'kid2']
NO_ADHD_LOT  <- merge(NO_ADHD_LOT , lot5, by = 'kid2',all.x = TRUE)
NO_ADHD_LOT$type90 <- "No_ADHD_RX"
# number of medications 
#------ Number of medications (ADHD RX group )
nom <- claim_pharmacy[kid2 %in% pt_id & transaction_type == 'PAID',.(kid2,date_of_service, days_supply,ndc11)]
nom$drug_name <- substr(nom$ndc11,1,9)
nom[,ndc11 := NULL]
nom$date_of_service <- as.Date(nom$date_of_service)
no_of_medications <- nom[, j= list(no_of_medications = length(unique(drug_name))), by = c('kid2')]
NO_ADHD_LOT  <- merge(NO_ADHD_LOT ,no_of_medications, all.x = TRUE, by = 'kid2')

trt_pat <- rbindlist(list(trt_pat,NO_ADHD_LOT),fill = TRUE)
# CREATE TABLES


library(table1)
trt_pat$type90 <- factor(trt_pat$type90,levels = c("IR","XR","ER + IR","NS","No_ADHD_RX"))
trt_pat2$type90 <- factor(trt_pat2$type90,levels = c("IR","XR","ER + IR","NS","No_ADHD_RX"))
# Table formatting
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=3), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.1f %%)", FREQ, PCT))))
}
trt_pat$mod_time <- as.numeric(trt_pat$mod_time)
label(trt_pat$lot2) <- "Treatment Duration (with Discontinuation) (days)"
label(trt_pat$lot3) <- "Treatment Duration (Total) (days)"
label(trt_pat$lot4) <- "Treatment Duration (Pre-survey) (days)"
label(trt_pat$lot5) <- "Treatment Duration (Post-survey) (days)"
label(trt_pat$ADHD) <- "Number of ADHD related drugs"
label(trt_pat$`NON-ADHD`) <- "Number of Non-ADHD related drugs"
label(trt_pat$switch_type) <- "Treatment Modification Type"
label(trt_pat$mod_time) <- "Time to modification (days)"
label(trt_pat$CMA) <- "Adherence (MPR)"
label(trt_pat$MPR_ER) <- "Adherence - ER (MPR)"
label(trt_pat$MPR_IR) <- "Adherence - IR (MPR)"
label(trt_pat$no_mod_age) <- "Age (Non-Modifiers)"
label(trt_pat$yes_mod_age) <- "Age (Modifiers)"
trt_pat$concurrent <- as.factor(trt_pat$concurrent)
trt_pat$booster <- as.factor(trt_pat$booster)
levels(trt_pat$concurrent) <- c("No","Yes")
levels(trt_pat$booster) <- c("No","Yes")
label(trt_pat$concurrent) <- "Concurrent Use (ER >= 0.8, IR >= 0.65)"
label(trt_pat$booster) <- "Booster (ER >= 0.8, IR < 0.65)"
varlist <- c('`Initial Treatmet`','lot2','lot3','lot4','lot5','ADHD','`NON-ADHD`','episode','switch_type','mod_time','CMA','MPR_ER','MPR_IR','concurrent','booster','no_mod_age','yes_mod_age')
frml <- as.formula(paste("~  ", paste(varlist, collapse = " + "), " | type90" , sep =''))
table1(frml, data = trt_pat, footnote = "Treatment Pattern", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra")
table1(frml, data = trt_pat[type90 != "No_ADHD_RX"], footnote = "Treatment Pattern", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra")

# Distribution of times 
grp <- unique(trt_pat$type90)
stats <- lapply(grp, function(z){round(t(sapply(c(4,6,7,8),function(i){x <-trt_pat[type90==z][[i]]; summary(x[!is.na(x)])})),0)})
stats <- lapply(1:length(grp), function(i){cbind(grp[i],stats[[i]])})
stats <- do.call(rbind,stats)
stats <- as.data.table(stats)
temp<- stats$V1
stats <- stats[,lapply(.SD,function(z){as.numeric(z)}),.SDcols = 2:7]
stats$grp <- temp
stats$Max.[c(14,15,16)] <- c(3151,2310,2112)
temp <- stats$Min.[1:12] 
temp[temp<30] <- 30
stats$Min.[1:12] <- temp
write.table(stats, file="~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/02_27_2020/times_distribution.txt", sep = "\t", row.names = FALSE)


# KM Analysis
km_data <- trt_pat[type90 != "No_ADHD_RX",c('kid2','lot3','mod_time','type90')]
km_data$time <- ifelse(is.na(km_data$mod_time),km_data$lot3,km_data$mod_time)
km_data$event <- ifelse(is.na(km_data$mod_time),0,1)
library(survival)  
km1 <- survfit(Surv(time, event) ~ type90,  type="kaplan-meier", conf.type="log", data=km_data)
survival:::survmean(km1,rmean=max(km_data$time))


# General Characteristics
ind <- which(!is.na(trt_pat2$type90))
trt_pat2 <- as.data.table(trt_pat2)
trt_pat2 <- trt_pat2[ind,]
#label(trt_pat2$sad) <- "Substance Abuse Disorder"
label(trt_pat2$PTSD) <- "PTSD"
varlist <- c('anxiety','`bipolar disorder`','depression','`generalized anxiety disorder`','headache','`obsessive-compulsive disorder`','`panic disorder`',
             'phobias','PTSD','schizophrenia','`social anxiety disorder`','ND','SUD','SUE','MILD','`MOD-SEV`','remission','IP','OP','ER','Total_Direct_Cost')
frml <- as.formula(paste("~  ", paste(varlist, collapse = " + "), " | type90" , sep =''))
table1(frml, data = trt_pat2, footnote = "Patient Characteristics", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra")

##  Get Drrug names for patients not treated by ADHD DRugs 
#  NHWS
fname <- file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Data','adhd_data.RData')
load(fname)
NHWS <- adhd_data[cohort=="ADHD"]

project <- '108_Purdue_ADHD'
readKomodo(project)
linker <- getLinkInfo(NHWS)
reassignID('NHWS',linker)
claim_pharmacy$kid2 <- as.numeric(claim_pharmacy$kid2)
pt_id <- as.numeric(pt_id)  # pt_id from code section treatment Duration for patients not on treatment 
claim_pharmacy2 <- claim_pharmacy[kid2 %in% pt_id]
claim_pharmacy2$ndc9 <- as.character(claim_pharmacy2$ndc9)
#claim_pharmacy2 <- claim_pharmacy[!(kid2 %in% trt_pat$kid2)]
claim_pharmacy2  <- merge(claim_pharmacy2, non_adhd_ndc[,c('ndc9','drug_name','drug_for')], by = 'ndc9', all.x = TRUE)
claim_pharmacy2  <- claim_pharmacy2[ndc9 != '']
claim_pharmacy2 <- unique(claim_pharmacy2[,c('kid2','drug_name')])
claim_pharmacy2<-claim_pharmacy2[!is.na(drug_name)]

# Drug list from Jessica at Purdue
fn1 <- function(x){
  #drug_check <-c('Modafinil', 'Armodafinil', 'Buprenorphine', 'Sertraline', 'Citalopram', 'escitalopram', 'fluoxetine', 'paroxetine', 'venlafaxine', 'mirtazapine', 'trazodone', 'risperidone', 'aripirazole')
  #drug_check <- paste("\\b",drug_check,"\\b")
  y <- any(sapply(drug_check,function(z){grepl(z,x,ignore.case = TRUE)}))
  return(y)
}
drug_check0 <-c('Modafinil', 'Armodafinil', 'Buprenorphine', 'Sertraline', 'Citalopram', 'escitalopram', 'fluoxetine', 'paroxetine', 'venlafaxine', 'mirtazapine', 'trazodone', 'risperidone', 'aripirazole')
drug_check <- paste("\\b",drug_check0,"\\b",sep="")
# get brand names as well 
brands <- list()
for(i in 1:length(drug_check)){
  ind <- sapply(.internal_fda_env$fda_table$generic_name,function(z){any(grepl(drug_check[i],z,ignore.case=TRUE))})
  ind <- which(ind)
  brands[[i]] <- unique(tolower(unlist(.internal_fda_env$fda_table$brand_name[ind])))
}
drug_check0 <- unique(c(drug_check0,unlist(brands)))
drug_check <- paste("\\b",drug_check0,"\\b",sep="")
claim_pharmacy2$check <- sapply(claim_pharmacy2$drug_name,fn1)
check <- claim_pharmacy2[check==TRUE]
drug_check0 <-c('Modafinil', 'Armodafinil', 'Buprenorphine', 'Sertraline', 'Citalopram', 'escitalopram', 'fluoxetine', 'paroxetine', 'venlafaxine', 'mirtazapine', 'trazodone', 'risperidone', 'aripirazole')
drug_check0 <- tolower(drug_check0)
for(i in 1:length(drug_check0)){
  brands[[i]] <- unique(c(brands[[i]],drug_check0[i]))
}
brand_index <- unlist(sapply(1:length(brands),function(i){rep(i,length(brands[[i]]))}))
gen_name    <- unlist(sapply(1:length(brands),function(i){rep(drug_check0[i],length(brands[[i]]))}))
brands2 <- unlist(brands)
check$drug_name_temp <- paste("\\b",check$drug_name,"\\b",sep="")
a <- sapply(check$drug_name_temp,function(z){unique(gen_name[unique(grep(z,brands2,ignore.case = TRUE))])})
id <- which(sapply(a,length)==0)
a[id] <- "venlafaxine"
check$drug_name_2 <- unlist(a)
pt_drug <- dcast(check,kid2~drug_name_2)
by_drug <- apply(pt_drug[,-1],2,sum)
pt_n <- apply(pt_drug[,-1],1,sum)

