# Run 03_komodo_concordance.R first 

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
class_info_15 <- unique(class_info[,.(kid2,delta_survey,type15,survey_window)])
class_info_30 <- unique(class_info[,.(kid2,delta_survey,type30,survey_window)])
class_info_60 <- unique(class_info[,.(kid2,delta_survey,type60,survey_window)])
class_info_90 <- unique(class_info[,.(kid2,delta_survey,type90,survey_window)])
class_info_00 <- unique(class_info[,.(kid2,delta_survey,type00,survey_window)])
class_info_30 <- class_info_30[!duplicated(kid2)]
class_info_60 <- class_info_60[!duplicated(kid2)]
class_info_90 <- class_info_90[!duplicated(kid2)]
class_info_00 <- class_info_00[!duplicated(kid2)]

class_info <- unique(class_info_90)
class_info$type90[class_info$kid2==201906066852] <- "IR"
class_info$type90[class_info$kid2==201906134572] <- "XR"
class_info$type90[class_info$kid2==201906066852] <- "IR"
class_info <- class_info[!is.na(type90)]

# --------------------------------------------------------------------------------------
# PREP data for Analysis 
# --------------------------------------------------------------------------------------
# substitute NHWS data to those patients selected in claims 
linker2 <- linker[as.numeric(kid2) %in% final_pt_list]
ind <- unique(linker2, by = c('zKey2','zKey'))
NHWS <- NHWS[zKey2 %in% ind$zKey2]
ind <- unique(linker2, by = c('zKey2','kid2'))
NHWS  <- merge(NHWS,ind, by = 'zKey2')
# attach class info 
NHWS <- merge(NHWS,class_info[,.(kid2,type90,survey_window)], by = 'kid2', all.x = TRUE)
NHWS$type90[is.na(NHWS$type90)] <-'No_ADHD_RX'

save(NHWS, file="NHWS_linked.RData")



