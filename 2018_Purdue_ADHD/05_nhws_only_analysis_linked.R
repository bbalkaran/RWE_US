library(kollekt)
library(table1)
library(sjlabelled)
fname <- file.path(getPath('RWE_US'),'2018_Purdue_ADHD','Data','NHWS_linked.RData')
load(fname)

mh_disorders <- c("Anxiety","Depression","Attention","Bipolar disorder","Panic disorder",
                  "Phobias","Social anxiety disorder","Generalized anxiety disorder",
                  "Obsessive Compulsive Disorder","Post-Traumatic Stress Disorder","Schizophrenia",
                  "Migraine","Headache")

# Custom Mapping
myMap <- list(
  DEMAR    = list("Married/Living with Partner" = c(1,6), "Not Married" = c(2,3,4,5), "Decline to Answer" = 99),
  DEEDU    = list("Less than high school or equivalent" = 1, "High school or equivalent" = c(2,3,4), "Associate's degree" = 5, "Bachelor's degree" = c(6,7), "Graduate degree" = 8, "Decline To Answer" = 99),
  DEPEMP_R = list("Employed full time" = c(1,2), "Employed part time" = 3, "Unemployed" = 4:9),
  DEINC_R  = list("Less than $25K" = c(1,2), "$25K to <$50K" = c(3,4), "$50K to <$75K" = 5, "$75K to <$100K" = 6, "$100K to <$150K" = c(7,8), "$150K or more" = c(9,10), "Decline to answer" = 99),
  HHSMK = list("Current smoker" = c(1,2), "Former smoker" = c(3,4)),
  DEHISP_1 = list("Non-Hispanic" = 1, "No" = 0),
  DEHISP_2 = list("Hispanic" = 1, "No" = 0),
  DEHISP_3 = list("Hispanic" = 1, "No" = 0),
  DEHISP_4 = list("Hispanic" = 1, "No" = 0),
  DEHISP_5 = list("Hispanic" = 1, "No" = 0),
  DEETHNIC = list("Non-Hispanic White" = 5, "Non-Hispanic Black" = 1, "Hispanic" = 4, "Other" = c(2,3,6,7)),
  RURAL_CODE = list("Rural" = 1, "Urban" = 2, "Decline to Answer" = 99)
)

# Data map and prep
cat_vars <- names(categorizeNumeric(999))
NHWS <- cciNHWS(NHWS)
NHWS[,GAD7SCRE1 := GADFN+GADWR+GADWDT+GADTR+GADCST+GADAI+GADPHA]
NHWS$speciality <- apply(NHWS[,.SD, .SDcols = colnames(NHWS)[grep("RU..6Q",colnames(NHWS))]],1,sum,na.rm = TRUE)
NHWS <- NHWS %>%
  mutate_all(., mapNHWS, mapping = myMap) %>%
  mutate(., insurance = combine2Stage(INHV, INTYPPRI, "Not Insured", 'Yes')) %>%
  mutate(., smoking = combine2Stage(HHSMKEV, HHSMK, "Never smoker",'Yes')) %>%
  mutate_at(., colnames(NHWS)[colnames(NHWS) %in% names(categorizeNumeric(999))],categorizeNumeric) %>%
  mutate(., year = gsub("us_","",source))
NHWS <- as.data.table(NHWS)
NHWS$AGE_CAT <- cut(NHWS$DEAGE_R,breaks = c(-Inf,20,30,40,50,Inf),right = FALSE)
NHWS$RX_USE  <- NHWS$ADHRX
NHWS$RX_USE[NHWS$ADHRX == 1 | NHWS$ADRX == 1] <- 1
NHWS <- direct_costs(NHWS)
NHWS <- indirect_costs(NHWS)
# Load addtional PRO data 
fname <- file.path(getPath("RWE_US"),"2018_Purdue_ADHD","Data","pro_extra.RData")
load(fname)
#pro_extra[,survey_date:= ifelse(is.na(DATE),START_TIME,DATE)]
#pro_extra$survey_date <- as.Date(pro_extra$survey_date)
#pro_extra <- getIndex(pro_extra)
pro_extra <- pro_extra[,c("zKey","BP_NBS","GH_NBS","MH_NBS","PF_NBS","RE_NBS","RP_NBS","SF_NBS","VT_NBS",
                          "EQ5D1","EQ5D2","EQ5D3","EQ5D4","EQ5D5")]
NHWS <- merge(NHWS,pro_extra,by.x = "zKey2",by.y = "zKey",all.x = TRUE)

library(plyr)
NHWS$AGE_CAT <- revalue(NHWS$AGE_CAT, c("[-Inf,20)"="< 20", "[50, Inf)"=">= 50"))

# Attach labels to derived variables 
varlist <- list(insurance = "Insurance Status", cci = "CCI", smoking = "Smoking Status",GAD7SCRE1 = "GAD-7 Score", speciality = "Specialist Visits: Number of visits per 6-month of follow-up")
for(i in 1:length(varlist)){
  var_lab(NHWS[[names(varlist)[i]]]) <- varlist[[i]]
}

# # Order comorbidities by prevalance
comorb <- nhwsDX()
comorb <- comorb[comorb %in% colnames(NHWS)]

# Table formatting
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=3), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.1f %%)", FREQ, PCT))))
}

# Making sure the NA values are assigned the correct value of FALSE (0 or 2 depending on initial codiing)
comorb <- comorb[!comorb %in% c('PGDDX','TBDX')]
for(cname in comorb){
  cat(cname,"\n")
  if(cname %in% names(NHWS)){
    temp <- NHWS[[cname]][!is.na(NHWS[[cname]])]
    temp <- unique(temp[temp!=1])
    if(length(temp)>0){
      NHWS[[cname]][is.na(NHWS[[cname]])] <- temp
    }
  }
}
# fatorize factor variables
NHWS2 <- factorize(NHWS)
a <- factor(NHWS2$type90,levels = c("IR","XR","ER + IR","NS","No_ADHD_RX"))
cols <- colnames(NHWS2)[!(colnames(NHWS2) %in% "type90")]
NHWS2 <- NHWS2[,..cols]
NHWS2$type90 <- a
# Table 6
varlist <- c('DEAGE_R','DESEX','DEETHNIC','DEMAR','DEEDU','DEPEMP_R','DEINC_R','insurance','RURAL_CODE','REGION4','TOTIN_R','HHLBS_R','cci',
             'smoking','HHALCQ','HHSMKUP12NG','HHSMKUP12NP','HHSMKUP12EC','HHSMKTME','BMI_R','HHEX')
frml <- as.formula(paste("~  ", paste(varlist, collapse = " + "), " | type90" , sep =''))
table1(frml, data = subset(NHWS2, subset = cohort == "ADHD"), footnote = "Table - Demographics", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra")

# Table 7
varlist <- c('MCS','PCS','BP_NBS','GH_NBS','MH_NBS','PF_NBS','RE_NBS','RP_NBS','SF_NBS','VT_NBS','SF6D_R2','EQ5DINDEX','EQ5D1','EQ5D2','EQ5D3','EQ5D4','EQ5D5','EQ5D6','GAD7SCRE1','TRADTMI','speciality','RUPY6Q','RUER6Q',
             'RUHP6Q','WPPCTWRK','WPIMPAIR','WPWRKIMP','WPACTIMP','INPOCKR','INCSTPVT','Total_Direct_Cost','Indirect_cost_ABS','Indirect_cost_pres','Total_indirect_cost')
label(NHWS2$BP_NBS) <- "Bodily Pain"
label(NHWS2$GH_NBS) <- "General Health"
label(NHWS2$MH_NBS) <- "Mental Health"
label(NHWS2$PF_NBS) <- "Physical Functioning"
label(NHWS2$RE_NBS) <- "Role Emotional"
label(NHWS2$RP_NBS) <- "Role Physical"
label(NHWS2$SF_NBS) <- "Social Functioning"
label(NHWS2$VT_NBS) <- "Vitality"
label(NHWS2$EQ5D1) <- "Mobility"
label(NHWS2$EQ5D2) <- "Self-care"
label(NHWS2$EQ5D3) <- "Usual Activities"
label(NHWS2$EQ5D4) <- "pain/Discomfort"
label(NHWS2$EQ5D5) <- "Anxiety Depression"
frml <- as.formula(paste("~  ", paste(varlist, collapse = " + "), " | type90" , sep =''))
table1(frml, data = subset(NHWS2, subset = cohort == "ADHD"), footnote = "Table - Outcomes", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra")

# Resource Utlization - 1
ru_vars <- searchNHWS('code',pattern = list(pattern  = "^RU..6Q$"))
varlist <- ru_vars$variable
varlist <- varlist [!(varlist  %in% c('RUER6Q','RUHP6Q','RUPY6Q'))]
varlist <- intersect(varlist,colnames(NHWS2))
frml <- as.formula(paste("~  ", paste(varlist, collapse = " + "), " | type90" , sep =''))
table1(frml, data = subset(NHWS2, subset = cohort == "ADHD"), footnote = "Table: Resource Utlization (Number of Visits in last 6 months)", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra")

# Resource Utlization - 2
ru_vars <- searchNHWS('code',pattern = list(pattern  = "^RU..6Q$"))
varlist <- gsub("6Q","",ru_vars$variable)
#varlist <- varlist [!(varlist  %in% c('RUER6Q','RUHP6Q','RUPY6Q'))]
varlist <- intersect(varlist,colnames(NHWS2))
frml <- as.formula(paste("~  ", paste(varlist, collapse = " + "), " | type90" , sep =''))
table1(frml, data = subset(NHWS2, subset = cohort == "ADHD"), footnote = "Table: Resource Utilization (Visit in last 6 month)", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra")

# Resource Utlization - 3
varlist <- c("RUIWB","RUIAP","RUHWB","RUHAP","RUEWB","RUEAP","RUWRT","RUNON")
varlist <- intersect(varlist,colnames(NHWS2))
frml <- as.formula(paste("~  ", paste(varlist, collapse = " + "), " | type90" , sep =''))
table1(frml, data = subset(NHWS2, subset = cohort == "ADHD"), footnote = "Table: Technology Assistance", render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra")

# Comorbidities
comorb_mh <- searchNHWS('code', pattern = list(pattern = "DX$"))
comorb_mh <- searchNHWS('label', pattern = list(pattern = mh_disorders), data = comorb_mh)
comorb_mh <- searchNHWS('label', pattern = list(pattern = c("diagnos"), exclude = c("saw","total")), data = comorb_mh)
comorb <- comorb[comorb %in% comorb_mh$variable]
comorb <- comorb[-c(1,2)]
frml <- as.formula(paste("~  ", paste(comorb, collapse = " + "), " | type90" , sep =''))
table1(frml, data = subset(NHWS2, subset = cohort == "ADHD"), footnote = "Table: Comorbidities" , render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra")

# Severity
sv_vars <-searchNHWS('code',pattern = list(pattern = "SV$"))
sv_vars <-searchNHWS('label',pattern = list(pattern = c("severity")), data = sv_vars)
sv_vars <-searchNHWS('label',pattern = list(pattern = mh_disorders), data = sv_vars)
varlist <- sv_vars$variable
var_list_1 <- varlist
sv_vars_rx <-searchNHWS('code',pattern = list(pattern = "SVRX$"))
sv_vars_rx <-searchNHWS('label',pattern = list(pattern = mh_disorders), data = sv_vars_rx)
varlist <- sv_vars_rx$variable
var_list_1 <- c(var_list_1,varlist)
sv_vars_nrx <-searchNHWS('code',pattern = list(pattern = "SVNRX$"))
sv_vars_nrx <-searchNHWS('label',pattern = list(pattern = mh_disorders), data = sv_vars_nrx)
varlist <- sv_vars_nrx$variable
var_list_1 <- c(var_list_1,varlist)
frml <- as.formula(paste("~  ", paste(var_list_1, collapse = " + "), " | type90" , sep =''))
table1(frml, data = subset(NHWS2, subset = cohort == "ADHD"), footnote = "Table: Severity" , render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra")


# Medication and OTC
otc_vars <- searchNHWS('label',pattern = list(pattern = c("OTC","counter")))
otc_vars <- searchNHWS('label',pattern = list(pattern = c("use")), data = otc_vars)
otc_vars <- searchNHWS('label',pattern = list(pattern = mh_disorders), data = otc_vars)
otc_vars <- intersect(otc_vars$variable, colnames(NHWS2))
varlist <- otc_vars
varlist <- c("RX_USE",otc_vars)
frml <- as.formula(paste("~  ", paste(varlist, collapse = " + "), " | type90" , sep =''))
table1(frml, data = subset(NHWS2, subset = cohort == "ADHD"), footnote = "Table: Medication and OTC" , render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra")

# Switching
sw_vars <- searchNHWS('code',pattern = list(pattern = "^SWTC"))
sw_vars <- intersect(sw_vars$variable, colnames(NHWS2))
varlist <- sw_vars
frml <- as.formula(paste("~  ", paste(varlist, collapse = " + "), " | type90" , sep =''))
table1(frml, data = subset(NHWS2, subset = cohort == "ADHD"), footnote = "Table: Medication switching reasons. Table shows the proprtion of times a reason ranked 1,2, or 3 by the patient." , render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra")

# Sleeping condition
sq_vars <- searchNHWS('code',pattern = list(pattern = "^SQSD"))
sq_vars <- setdiff(sq_vars$variable, c("SQSDSLB","SQSDTM"))
varlist <- sq_vars
frml <- as.formula(paste("~  ", paste(varlist, collapse = " + "), " | type90" , sep =''))
table1(frml, data = subset(NHWS2, subset = cohort == "ADHD"), footnote = "Table: Sleep" , render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra")

# HEADACHE
hac_vars <- searchNHWS('code',pattern = list(pattern = c("^HDTP")))
varlist <- hac_vars$variable
for(k in varlist){
  temp <- NHWS2[[k]]
  temp[is.na(temp)] <- 'No'
  NHWS2[[k]] <- temp
}
frml <- as.formula(paste("~  ", paste(varlist, collapse = " + "), " | type90" , sep =''))
table1(frml, data = subset(NHWS2, subset = cohort == "ADHD"), footnote = "Table: Headache" , render.continuous=my.render.cont, render.categorical=my.render.cat, topclass="Rtable1-zebra")

