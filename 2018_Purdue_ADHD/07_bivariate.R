cat.pvalue <- function(x,y){
  if(any(table(x)<5) | any(table(y) <5)){
    fisher.test(as.table(rbind(table(x),table(y))),simulate.p.value = TRUE)$p.value
  }else{
    chisq.test(as.table(rbind(table(x),table(y))),simulate.p.value = TRUE)$p.value
  }
}

formatTable <- function(x,dat){
  len <- sapply(names(x), function(z){length(levels(dat[[z]]))})
  len[len==0] <- 1
  xnames <- names(x)
  xnames <- unlist(sapply(1:length(xnames),function(i){paste0(xnames[i],1:(len[i]+1))}))
  x <- lapply(x,function(z){round(z,3)})
  x <- lapply(x,function(z){z[z<0.001] <- "< 0.001"; z})
  x <- lapply(1:length(x),function(i){matrix(c(x[[i]],rep("",len[i]*length(x[[i]]))),ncol=length(x[[i]]),byrow = TRUE)})
  x <- do.call(rbind,x)
  x <- as.data.frame(x)
  rownames(x) <- xnames
  return(x)
}

# RUN: CODE 05_nhws_only_analysis_linked.R
# Run till Table 7 contents
NHWS2$Total_Direct_Cost_0 <- NHWS2$Total_Direct_Cost
demographics <- c('DEAGE_R','DESEX','DEETHNIC','DEMAR','DEEDU','DEPEMP_R','DEINC_R','insurance','RURAL_CODE','REGION4','TOTIN_R','HHLBS_R','cci',
                  'smoking','HHALCQ','HHSMKUP12NG','HHSMKUP12NP','HHSMKUP12EC','HHSMKTME','BMI_R','HHEX')
outcomes <- c('MCS','PCS','BP_NBS','GH_NBS','MH_NBS','PF_NBS','RE_NBS','RP_NBS','SF_NBS','VT_NBS','SF6D_R2','EQ5DINDEX','EQ5D1','EQ5D2','EQ5D3','EQ5D4','EQ5D5','EQ5D6','GAD7SCRE1','TRADTMI','speciality','RUPY6Q','RUER6Q',
                         'RUHP6Q','WPPCTWRK','WPIMPAIR','WPWRKIMP','WPACTIMP','INPOCKR','INCSTPVT',
                          'Total_Direct_Cost_0','Indirect_cost_ABS','Indirect_cost_pres','Total_indirect_cost')
gen_char <- c('anxiety','bipolar_disorder','depression','generalized_anxiety_disorder','headache','obsessive_compulsive_disorder','panic_disorder',
                         'phobias','PTSD','schizophrenia','social_anxiety_disorder','ND','SUD','SUE','MILD','MOD_SEV','remission','IP','OP','ER','Total_Direct_Cost')

NHWS_demo_outcomes <- NHWS2[,c("kid2",demographics,outcomes)]
NHWS_demo_outcomes$kid2 <- as.numeric(NHWS_demo_outcomes$kid2)
# RUN: CODE 06_trt_pattern.R
# Run till beginning of table creation
trt_pat$type90 <- factor(trt_pat$type90,levels = c("ER + IR","IR","XR","NS","No_ADHD_RX"))
trt_pat2$type90 <- factor(trt_pat2$type90,levels = c("ER + IR","IR","XR","NS","No_ADHD_RX"))
trt_pat2[,DEAGE_R:=NULL]
trt_pat2$MOD_SEV <- trt_pat2$`MOD-SEV`
trt_pat2$bipolar_disorder <- trt_pat2$`bipolar disorder`
trt_pat2$generalized_anxiety_disorder<- trt_pat2$`generalized anxiety disorder`
trt_pat2$obsessive_compulsive_disorder <- trt_pat2$`obsessive-compulsive disorder`
trt_pat2$panic_disorder <- trt_pat2$`panic disorder`
trt_pat2$social_anxiety_disorder <- trt_pat2$`social anxiety disorder`
### Combine outcomes/demo and other treatment related info
NHWS3 <- merge(NHWS_demo_outcomes,trt_pat2, by = "kid2", all.x = TRUE)
library(table1)

# RECODE INSURANCE MENTAL HEALTH COMORBIDITIES
commercial <- c("Insurance Coverage Through A Current Or Former Employer", "Insurance Coverage Through Spouse's/Partner's Employer", "Individual/Family Insurance Plan Thru A State Health Exchange",
  "Individual/Family Insurance Plan Purchased Directly By You","Insurance Coverage Through My Parent's Or Legal Guardian's Employer")
public <- c("Medicaid (Medical For California Residents)","Medicare","Veterans Administration (Va)/Champus","Tricare")
uninsured <- c("Not Sure","Not Insured")
NHWS3$insurance <- as.character(NHWS3$insurance)
NHWS3$insurance[NHWS3$insurance %in% uninsured] <- "Uninsured"
NHWS3$insurance[NHWS3$insurance %in% public] <- "Public"
NHWS3$insurance[NHWS3$insurance %in% commercial] <- "Commercial"
NHWS3$insurance <- as.factor(NHWS3$insurance)
### Comorbidity vs No Comorbidity
comorbs <- c("anxiety", "bipolar disorder", "depression", "generalized anxiety disorder","obsessive-compulsive disorder", "panic disorder",
             "phobias","PTSD","schizophrenia","social anxiety disorder")
comorbs <- NHWS3[,comorbs]
comorbs <- apply(comorbs, 1, function(z){sum(z=="Yes")>0})
id <- which(comorbs)
NHWS3$comorbid <- "No"
NHWS3$comorbid[id] <- "Yes"
gen_char <- c("comorbid",gen_char)

# BETWEEN TREATMENT GROUPS 
# Demographics 
frml <- as.formula(paste("~  ", paste(demographics, collapse = " + "), " | type90" , sep =''))
table1(frml, data = NHWS3, footnote = "Table - Demographics", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra", overall = FALSE)
DEMO <- split(NHWS3,NHWS3$type90)
cohorts <- names(DEMO)
ref_cohort <- cohorts[1]
cohorts <- cohorts[-1]
demo_compare <- list()
for(i in 1:length(demographics)){
  if(is.numeric(NHWS3[[demographics[i]]])){
    demo_compare[[i]] <- sapply(DEMO[cohorts], function(z){wilcox.test(z[[demographics[i]]],DEMO[[1]][[demographics[i]]],exact = FALSE)$p.value})
  }else{
    demo_compare[[i]] <- sapply(DEMO[cohorts], function(z){cat.pvalue(z[[demographics[i]]],DEMO[[1]][[demographics[i]]])})
  }
}
names(demo_compare) <- demographics
demo_compare <- formatTable(demo_compare,NHWS3)

# Outcomes
frml <- as.formula(paste("~  ", paste(outcomes, collapse = " + "), " | type90" , sep =''))
table1(frml, data = NHWS3, footnote = "Table - Outcomes", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra", overall = FALSE)
DEMO <- split(NHWS3,NHWS3$type90)
cohorts <- names(DEMO)
ref_cohort <- cohorts[1]
cohorts <- cohorts[-1]
out_compare <- list()
for(i in 1:length(outcomes)){
  if(is.numeric(NHWS3[[outcomes[i]]])){
    out_compare[[i]] <- sapply(DEMO[cohorts], function(z){wilcox.test(z[[outcomes[i]]],DEMO[[1]][[outcomes[i]]], exact = FALSE)$p.value})
  }else{
    out_compare[[i]] <- sapply(DEMO[cohorts], function(z){cat.pvalue(z[[outcomes[i]]],DEMO[[1]][[outcomes[i]]])})
  }
}
names(out_compare) <- outcomes
out_compare <- formatTable(out_compare,NHWS3)

# General Characteristics
frml <- as.formula(paste("~  ", paste(gen_char, collapse = " + "), " | type90" , sep =''))
table1(frml, data = NHWS3, footnote = "Table - General Characteristics", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra", overall = FALSE)
DEMO <- split(NHWS3,NHWS3$type90)
cohorts <- names(DEMO)
ref_cohort <- cohorts[1]
cohorts <- cohorts[-1]
gc_compare <- list()
for(i in 1:length(gen_char)){
  if(is.numeric(NHWS3[[gen_char[i]]])){
    gc_compare[[i]] <- sapply(DEMO[cohorts], function(z){wilcox.test(z[[gen_char[i]]],DEMO[[1]][[gen_char[i]]], exact = FALSE)$p.value})
  }else{
    gc_compare[[i]] <- sapply(DEMO[cohorts], function(z){cat.pvalue(z[[gen_char[i]]],DEMO[[1]][[gen_char[i]]])})
  }
}
names(gc_compare) <- gen_char
gc_compare <- formatTable(gc_compare,NHWS3)
write.table(demo_compare, file="~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/04_03_2020/demo1.txt", sep = "\t", row.names = TRUE)
write.table(out_compare,  file="~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/04_03_2020/out1.txt",  sep = "\t", row.names = TRUE)
write.table(gc_compare,   file="~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/04_03_2020/gc1.txt",   sep = "\t", row.names = TRUE)

# REMOVE NON-STIMULANTS FROM THE DATA 
NHWS3 <- NHWS3[NHWS3$type90 != "NS",]
NHWS3$type90 <- as.factor(as.character(NHWS3$type90))
### TREATED VS UNTREATED
NHWS3$treated <- ifelse(NHWS3$type90 == "No_ADHD_RX","Untreated","Treated")
NHWS3$treated <- as.factor(NHWS3$treated)
# Demographics 
frml <- as.formula(paste("~  ", paste(demographics, collapse = " + "), " | treated" , sep =''))
table1(frml, data = NHWS3, footnote = "Table - Demographics", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra", overall = FALSE)
DEMO <- split(NHWS3,NHWS3$treated)
cohorts <- names(DEMO)
ref_cohort <- cohorts[1]
cohorts <- cohorts[-1]
demo_compare <- list()
for(i in 1:length(demographics)){
  if(is.numeric(NHWS3[[demographics[i]]])){
    demo_compare[[i]] <- sapply(DEMO[cohorts], function(z){wilcox.test(z[[demographics[i]]],DEMO[[1]][[demographics[i]]],exact = FALSE)$p.value})
  }else{
    demo_compare[[i]] <- sapply(DEMO[cohorts], function(z){cat.pvalue(z[[demographics[i]]],DEMO[[1]][[demographics[i]]])})
  }
}
names(demo_compare) <- demographics
demo_compare <- formatTable(demo_compare,NHWS3)

# Outcomes
frml <- as.formula(paste("~  ", paste(outcomes, collapse = " + "), " | treated" , sep =''))
table1(frml, data = NHWS3, footnote = "Table - Outcomes", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra", overall = FALSE)
DEMO <- split(NHWS3,NHWS3$treated)
cohorts <- names(DEMO)
ref_cohort <- cohorts[1]
cohorts <- cohorts[-1]
out_compare <- list()
for(i in 1:length(outcomes)){
  if(is.numeric(NHWS3[[outcomes[i]]])){
    out_compare[[i]] <- sapply(DEMO[cohorts], function(z){wilcox.test(z[[outcomes[i]]],DEMO[[1]][[outcomes[i]]], exact = FALSE)$p.value})
  }else{
    out_compare[[i]] <- sapply(DEMO[cohorts], function(z){cat.pvalue(z[[outcomes[i]]],DEMO[[1]][[outcomes[i]]])})
  }
}
names(out_compare) <- outcomes
out_compare <- formatTable(out_compare,NHWS3)

# General Characteristics
frml <- as.formula(paste("~  ", paste(gen_char, collapse = " + "), " | treated" , sep =''))
table1(frml, data = NHWS3, footnote = "Table - General Characteristics", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra", overall = FALSE)
DEMO <- split(NHWS3,NHWS3$treated)
cohorts <- names(DEMO)
ref_cohort <- cohorts[1]
cohorts <- cohorts[-1]
gc_compare <- list()
for(i in 1:length(gen_char)){
  if(is.numeric(NHWS3[[gen_char[i]]])){
    gc_compare[[i]] <- sapply(DEMO[cohorts], function(z){wilcox.test(z[[gen_char[i]]],DEMO[[1]][[gen_char[i]]], exact = FALSE)$p.value})
  }else{
    gc_compare[[i]] <- sapply(DEMO[cohorts], function(z){cat.pvalue(z[[gen_char[i]]],DEMO[[1]][[gen_char[i]]])})
  }
}
names(gc_compare) <- gen_char
gc_compare <- formatTable(gc_compare,NHWS3)
write.table(demo_compare, file="~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/04_03_2020/demo2.txt", sep = "\t", row.names = TRUE)
write.table(out_compare,  file="~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/04_03_2020/out2.txt",  sep = "\t", row.names = TRUE)
write.table(gc_compare,   file="~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/04_03_2020/gc2.txt",   sep = "\t", row.names = TRUE)

### Comorbidity vs No Comorbidity
# Demographics 
frml <- as.formula(paste("~  ", paste(demographics, collapse = " + "), " | comorbid" , sep =''))
table1(frml, data = NHWS3, footnote = "Table - Demographics", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra", overall = FALSE)
DEMO <- split(NHWS3,NHWS3$comorbid)
cohorts <- names(DEMO)
ref_cohort <- cohorts[1]
cohorts <- cohorts[-1]
demo_compare <- list()
for(i in 1:length(demographics)){
  if(is.numeric(NHWS3[[demographics[i]]])){
    demo_compare[[i]] <- sapply(DEMO[cohorts], function(z){wilcox.test(z[[demographics[i]]],DEMO[[1]][[demographics[i]]],exact = FALSE)$p.value})
  }else{
    demo_compare[[i]] <- sapply(DEMO[cohorts], function(z){cat.pvalue(z[[demographics[i]]],DEMO[[1]][[demographics[i]]])})
  }
}
names(demo_compare) <- demographics
demo_compare <- formatTable(demo_compare,NHWS3)

# Outcomes
frml <- as.formula(paste("~  ", paste(outcomes, collapse = " + "), " | comorbid" , sep =''))
table1(frml, data = NHWS3, footnote = "Table - Outcomes", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra", overall = FALSE)
DEMO <- split(NHWS3,NHWS3$comorbid)
cohorts <- names(DEMO)
ref_cohort <- cohorts[1]
cohorts <- cohorts[-1]
out_compare <- list()
for(i in 1:length(outcomes)){
  if(is.numeric(NHWS3[[outcomes[i]]])){
    out_compare[[i]] <- sapply(DEMO[cohorts], function(z){wilcox.test(z[[outcomes[i]]],DEMO[[1]][[outcomes[i]]], exact = FALSE)$p.value})
  }else{
    out_compare[[i]] <- sapply(DEMO[cohorts], function(z){cat.pvalue(z[[outcomes[i]]],DEMO[[1]][[outcomes[i]]])})
  }
}
names(out_compare) <- outcomes
out_compare <- formatTable(out_compare,NHWS3)

# General Characteristics
frml <- as.formula(paste("~  ", paste(gen_char, collapse = " + "), " | comorbid" , sep =''))
table1(frml, data = NHWS3, footnote = "Table - General Characteristics", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra", overall = FALSE)
DEMO <- split(NHWS3,NHWS3$comorbid)
cohorts <- names(DEMO)
ref_cohort <- cohorts[1]
cohorts <- cohorts[-1]
gc_compare <- list()
#for(i in c(5,12:length(gen_char))){
for(i in 1:length(gen_char)){
  if(is.numeric(NHWS3[[gen_char[i]]])){
    gc_compare[[i]] <- sapply(DEMO[cohorts], function(z){wilcox.test(z[[gen_char[i]]],DEMO[[1]][[gen_char[i]]], exact = FALSE)$p.value})
  }else{
    gc_compare[[i]] <- sapply(DEMO[cohorts], function(z){cat.pvalue(z[[gen_char[i]]],DEMO[[1]][[gen_char[i]]])})
  }
}
names(gc_compare ) <- gen_char
gc_compare  <- formatTable(gc_compare,NHWS3)
write.table(demo_compare, file="~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/04_03_2020/demo3.txt", sep = "\t", row.names = TRUE)
write.table(out_compare,  file="~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/04_03_2020/out3.txt",  sep = "\t", row.names = TRUE)
write.table(gc_compare,   file="~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/04_03_2020/gc3.txt",   sep = "\t", row.names = TRUE)

