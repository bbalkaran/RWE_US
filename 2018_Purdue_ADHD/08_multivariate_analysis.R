# RUN 05_nhws_only_analysis_linked till creation oh NHWS2 and just before tables
# RUN 06_trt_pattern till creation of trt_pat and trt_pat2

demo <- c('kid2','DEAGE_R','DESEX','DEETHNIC','DEMAR','DEEDU','DEPEMP_R','DEINC_R','insurance','RURAL_CODE','REGION4','TOTIN_R','HHLBS_R','cci',
  'smoking','HHALCQ','HHSMKTME','BMI_R','HHEX')
outcomes <- c('MCS','PCS','BP_NBS','GH_NBS','MH_NBS','PF_NBS','RE_NBS','RP_NBS','SF_NBS','VT_NBS','SF6D_R2','EQ5DINDEX','EQ5D6','WPPCTWRK','WPIMPAIR','WPWRKIMP','WPACTIMP','Total_indirect_cost')
varlist <- c(demo,outcomes)
NHWS2 <- as.data.table(NHWS2)
nhws2 <- NHWS2[,..varlist]

# From TRT TABLES
mh <- c('anxiety','bipolar disorder','depression','generalized anxiety disorder','obsessive-compulsive disorder','panic disorder',
        'phobias','PTSD','schizophrenia','social anxiety disorder')
temp <- trt_pat2[,..mh]
temp$mh <- apply(temp,1,function(z){any(z=='Yes')})
trt_pat2$mh <- temp$mh*1
varlist <- c('kid2','type90','mh','headache','ND','SUD','IP','OP','ER','Total_Direct_Cost')
trt2 <- trt_pat2[,..varlist]
varlist <- c('kid2','CMA')
trt1 <- trt_pat[,..varlist]
trt1$kid2 <- as.numeric(trt1$kid2)
trt <- merge(trt1,trt2,by="kid2")

# Final Data for regression
nhws2$kid2 <- as.numeric(nhws2$kid2)
reg_data <- merge(nhws2,trt,by="kid2")
colnames(reg_data)[colnames(reg_data)=="type90"] <- "Treatment"
reg_data$Treatment <- as.character(reg_data$Treatment)
# Remove NS from the data 
reg_data <- reg_data[Treatment != "NS"]
reg_data$Treatment <- factor(reg_data$Treatment,levels = c('ER + IR','IR','XR','No_ADHD_RX '))
reg_data$Treatment <- as.character(reg_data$Treatment)
reg_data$Treatment[is.na(reg_data$Treatment)] <- "No_ADHD_RX"
reg_data$Treatment <- factor(reg_data$Treatment,levels = unique(reg_data$Treatment)[c(4,1,2,3)])
reg_data$Treated <- as.character(reg_data$Treatment)
reg_data$Treated[reg_data$Treated == "No_ADHD_RX"] <- "No"
reg_data$Treated[is.na(reg_data$Treated)] <- "No"
reg_data$Treated[reg_data$Treated != "No"] <- "Yes"
reg_data$Treated <- factor(reg_data$Treated,levels = c("No","Yes"))
outcomes <- c('MCS','PCS','BP_NBS','GH_NBS','MH_NBS','PF_NBS','RE_NBS','RP_NBS','SF_NBS','VT_NBS','SF6D_R2','EQ5DINDEX','EQ5D6',
              'WPPCTWRK','WPIMPAIR','WPWRKIMP','WPACTIMP','IP','OP','ER','Total_Direct_Cost','Total_indirect_cost')
demo <- c('Treatment','Treated','DEAGE_R','DESEX','DEETHNIC','DEMAR','DEEDU','DEPEMP_R','DEINC_R','insurance','RURAL_CODE','REGION4','HHLBS_R','cci',
          'smoking','HHALCQ','BMI_R','HHEX','mh','headache','ND','SUD') #,'CMA')
# Collapse/Clean categories
insurance <- c('Not Insured', 'Insurance Coverage Through A Current Or Former Employer', "Insurance Coverage Through Spouse's/Partner's Employer", 'Individual/Family Insurance Plan Thru A State Health Exchange', 'Individual/Family Insurance Plan Purchased Directly By You', 'Medicaid (Medical For California Residents)', 'Medicare', 'Veterans Administration (Va)/Champus', 'Tricare', "Insurance Coverage Through My Parent's Or Legal Guardian's Employer", 'Not Sure')
reg_data$insurance <- as.character(reg_data$insurance)
reg_data$insurance[reg_data$insurance %in% insurance[c(1,11)]] <- "Not Insured"
reg_data$insurance[reg_data$insurance %in% insurance[c(2:5,10)]] <- "Commercial"
reg_data$insurance[reg_data$insurance %in% insurance[c(6:9)]] <- "Public"
reg_data$insurance <- factor(reg_data$insurance,levels = unique(reg_data$insurance)[c(3,2,1)])

reg_data$DEMAR <- as.character(reg_data$DEMAR)
reg_data$DEMAR[reg_data$DEMAR == "Decline to Answer"] <- "Not Married"
reg_data$DEMAR <- factor(reg_data$DEMAR,levels = unique(reg_data$DEMAR))

reg_data$DEEDU <- as.character(reg_data$DEEDU)
reg_data$DEEDU[reg_data$DEEDU %in% c("Less than high school or equivalent","Decline To Answer")] <- "High school or equivalent"
reg_data$DEEDU <- factor(reg_data$DEEDU,levels = unique(reg_data$DEEDU)[c(3,2,4,1)])

temp <- levels(reg_data$HHALCQ)
temp <- c(temp[length(temp)],temp[-7])
reg_data$HHALCQ <- as.character(reg_data$HHALCQ)
reg_data$HHALCQ <- factor(reg_data$HHALCQ,levels = temp)

reg_data$mh <- ifelse(reg_data$mh==0,"No","Yes")
reg_data$mh <- factor(reg_data$mh,levels = c("No","Yes"))


# Outcomes Range 
ranges <- sapply(outcomes, function(z){range(reg_data[[z]],na.rm = TRUE)})
normality <- sapply(outcomes, function(z){shapiro.test(reg_data[[z]])$p.value})
#  1 - 13 : Normal distribution
# 14 - 22 : Tweedie distribution

library(statmod)
library(tweedie)
demo <- c('DEAGE_R','DESEX','DEETHNIC','DEMAR','DEEDU','DEINC_R','insurance','REGION4','cci',
          'smoking','HHALCQ','BMI_R','HHEX','headache','ND','SUD')
# PRIMARY VARIABLE: TREATMENT TYPE 
PRIMARY_VARIABLE <- 'Treatment'
MODELS <- list()
TW_PARAM <- list()
NORMAILITY <- rep(NA,length(outcomes))
for(i in 1:length(outcomes)){
  frml <- as.formula(paste0(outcomes[i],"~."))
  variables <- c(outcomes[i],c(PRIMARY_VARIABLE,demo))
  if(i <= 13){
    model <- glm(frml, data = reg_data[,..variables])
    TW_PARAM[[i]] <- NA
    MODELS[[i]] <- model 
    NORMAILITY[i] <- shapiro.test(residuals(model,type="deviance"))$p.value
  }else{
    tw_param <- tweedie.profile(frml, data=reg_data[,..variables],xi.vec=seq(1.1, 1.9, by=0.1))
    model <- glm(frml, data = reg_data[,..variables], family = tweedie(var.power=tw_param$xi.max, link.power=0))
    TW_PARAM[[i]] <- tw_param
    MODELS[[i]] <- model 
    NORMAILITY[i] <- shapiro.test(qresid(model))$p.value
  }
}
MODELS_1 <- MODELS

# PRIMARY VARIABLE: TREATED 
PRIMARY_VARIABLE <- 'Treated'
MODELS <- list()
TW_PARAM <- list()
NORMAILITY <- rep(NA,length(outcomes))
for(i in 1:length(outcomes)){
  frml <- as.formula(paste0(outcomes[i],"~."))
  variables <- c(outcomes[i],c(PRIMARY_VARIABLE,demo))
  if(i <= 13){
    model <- glm(frml, data = reg_data[,..variables])
    TW_PARAM[[i]] <- NA
    MODELS[[i]] <- model 
    NORMAILITY[i] <- shapiro.test(residuals(model,type="deviance"))$p.value
  }else{
    tw_param <- tweedie.profile(frml, data=reg_data[,..variables],xi.vec=seq(1.1, 1.9, by=0.1))
    model <- glm(frml, data = reg_data[,..variables], family = tweedie(var.power=tw_param$xi.max, link.power=0))
    TW_PARAM[[i]] <- tw_param
    MODELS[[i]] <- model 
    NORMAILITY[i] <- shapiro.test(qresid(model))$p.value
  }
}
MODELS_2 <- MODELS

# PRIMARY VARIABLE: TREATMENT + MH
PRIMARY_VARIABLE <- c('Treatment','mh')
MODELS <- list()
TW_PARAM <- list()
NORMAILITY <- rep(NA,length(outcomes))
for(i in 1:length(outcomes)){
  frml <- as.formula(paste0(outcomes[i],"~."))
  variables <- c(outcomes[i],c(PRIMARY_VARIABLE,demo))
  if(i <= 13){
    model <- glm(frml, data = reg_data[,..variables])
    TW_PARAM[[i]] <- NA
    MODELS[[i]] <- model 
    NORMAILITY[i] <- shapiro.test(residuals(model,type="deviance"))$p.value
  }else{
    tw_param <- tweedie.profile(frml, data=reg_data[,..variables],xi.vec=seq(1.1, 1.9, by=0.1))
    model <- glm(frml, data = reg_data[,..variables], family = tweedie(var.power=tw_param$xi.max, link.power=0))
    TW_PARAM[[i]] <- tw_param
    MODELS[[i]] <- model 
    NORMAILITY[i] <- shapiro.test(qresid(model))$p.value
  }
}
MODELS_3 <- MODELS


# PRIMARY VARIABLE: TREATMENT * MH  (with interaction)
PRIMARY_VARIABLE <- c('Treatment','mh')
MODELS <- list()
TW_PARAM <- list()
NORMAILITY <- rep(NA,length(outcomes))
for(i in 1:length(outcomes)){
  frml <- as.formula(paste0(outcomes[i],"~.+ Treatment * mh"))
  variables <- c(outcomes[i],c(PRIMARY_VARIABLE,demo))
  if(i <= 13){
    model <- glm(frml, data = reg_data[,..variables])
    TW_PARAM[[i]] <- NA
    MODELS[[i]] <- model 
    NORMAILITY[i] <- shapiro.test(residuals(model,type="deviance"))$p.value
  }else{
    tw_param <- tweedie.profile(frml, data=reg_data[,..variables],xi.vec=seq(1.1, 1.9, by=0.1))
    model <- glm(frml, data = reg_data[,..variables], family = tweedie(var.power=tw_param$xi.max, link.power=0))
    TW_PARAM[[i]] <- tw_param
    MODELS[[i]] <- model 
    NORMAILITY[i] <- shapiro.test(qresid(model))$p.value
  }
}
MODELS_4 <- MODELS

# Processing for presentation
demo2 <- c("Treatment","Treated","mh",demo)
labels <- c("Treatment","Treated","Mental Health","Age","Gender","Ethnicity","Marital Status","Education","Income","Insurance","US Region 4","CCI",
            "Smoking Habit","Alcohol Consumption","BMI","Exercise","Headache","Nicotine Dependence","Substance Abuse Disorder")
params <- data.table(coef = "(Intercept)", variable = "Intercept", label = "")
for(i in 1:length(demo2)){
  if(is.factor(reg_data[[demo2[i]]])){
    if(!is.ordered(reg_data[[demo2[i]]])){
      coef <- paste0(demo2[i],levels(reg_data[[demo2[i]]]))
    }else{
      coef <- paste0(demo2[i],c("ref",".L",".Q",".C","^4"))
    }
    variable <- labels[i]
    label <- c(paste0(levels(reg_data[[demo2[i]]])[1]," [Reference]"),levels(reg_data[[demo2[i]]])[-1])
    temp <- cbind(coef,variable,label)
    params <- rbind(params,temp)
  }else{
    coef <- demo2[i]
    variable <- labels[i]
    label <- ""
    temp <- cbind(coef,variable,label)
    params <- rbind(params,temp)
  }
}

outcome_labels <- c("MCS","PCS","Bodily Pain","General Health","Mental Health","Physical Functioning","Role Emotional","Role Physical",
                    "Social Functioning","Vitality","SF6D","EQ5D-INDEX","EQ5D-VAS",
                    "Absenteeism","Presenteeism","Overall work productivity loss","Activity Impairement",
                    "Inpatient Visits","Outpatient Visits","ER Visits","Total Direct Cost","Total Indirect Cost")

library(purrr)
library(writexl)
library(car)
format_coeffs <- function(model,delta = FALSE){
  coeffs <- coef(summary(model))
  if(!delta){
    coeffs[,1] <- round(coeffs[,1],3)
    coeffs[,2] <- round(coeffs[,2],3)
    coeffs[,4] <- round(coeffs[,4],3)
    coeffs <- coeffs[,-3]
  }else{
    tcoef <-paste0("exp(",paste0("b",1:dim(coeffs)[1]),")")
    tnames <- paste0("b",1:dim(coeffs)[1])
    coefs2 <- do.call(rbind,lapply(tcoef,function(z){deltaMethod(model,z, parameterNames = tnames, rhs =1)}))
    coefs2 <- coefs2[,c(1,2,6,7)]
    colnames(coefs2) <- colnames(coeffs)
    rownames(coefs2) <- rownames(coeffs)
    coefs2[,4] <- coeffs[,4]
    coeffs <- coefs2
    coeffs[,1] <- round(coeffs[,1],3)
    coeffs[,2] <- round(coeffs[,2],3)
    coeffs[,4] <- round(coeffs[,4],3)
    coeffs <- as.data.frame(coeffs)[,-3]
  }
  a <- rownames(coeffs)
  coeffs <- as.data.table(coeffs)
  coeffs$coef <- a
  return(coeffs)
}


#   coeffs <- coef(summary(model))
#   cnames <- colnames(coeffs)
#   coeffs1 <- sprintf("%.1f",coeffs[,1])
#   coeffs2 <- sprintf("%.2f",coeffs[,2])
#   coeffs3 <- sprintf("%.3f",coeffs[,4])
#   a <- rownames(coeffs)
#   coeffs <- data.table(a,coeffs1, coeffs2,coeffs3)
#   colnames(coeffs) <- c("coef",cnames[-3])
#   return(coeffs)
# }

out <- list()
for(i in 1:length(outcomes)){
  cat(i,"\n")
  if(i <= 13){
    delta0 <- FALSE
  }else{
    delta0 <- TRUE
  }
  temp1 <- format_coeffs(MODELS_1[[i]], delta = delta0)
  temp2 <- format_coeffs(MODELS_2[[i]], delta = delta0)
  temp3 <- format_coeffs(MODELS_3[[i]], delta = delta0)
  temp <- list(params,temp1,temp2,temp3) %>% reduce(left_join, by = "coef")
  temp <- temp[,-1]
  colnames(temp) <- c("Variable","Level",rep(c("Estimate","SE","Pvalue"),3))
  ind <- duplicated(temp$Variable)
  temp$Variable[ind] <- ''
  out[[i]] <- temp
  
}
names(out) <- outcome_labels
write_xlsx(out,path = "~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/04_28_2020/reg_results_exp.xlsx")

## INTERACTION MODEL ONLY 
z0 <- c("Treatment:mh","TreatmentIR:mhYes","TreatmentXR:mhYes","TreatmentNo_ADHD_RX:mhYes")
z1 <- rep("Treatament x Mental Health",4)
z2 <- c("IR + XR [Reference]", "IR","XR","Untreated")
z <- cbind(z0,z1,z2)
z <- as.data.table(z)
colnames(z) <- colnames(params)

params <- rbind(params,z)
out <- list()
for(i in 1:length(outcomes)){
  cat(i,"\n")
  if(i <= 13){
    delta0 <- FALSE
  }else{
    delta0 <- TRUE
  }
  temp4 <- format_coeffs(MODELS_4[[i]], delta = delta0)
  temp <- list(params,temp4) %>% reduce(left_join, by = "coef")
  temp <- temp[,-1]
  colnames(temp) <- c("Variable","Level",rep(c("Estimate","SE","Pvalue"),1))
  ind <- duplicated(temp$Variable)
  temp$Variable[ind] <- ''
  out[[i]] <- temp
  
}
names(out) <- outcome_labels
out <- out[18:22]
write_xlsx(out,path = "~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/04_28_2020/reg_results_exp_interaction.xlsx")

# Sample Sizes
out1 <- list()
out2 <- list()
out3 <- list()
out4 <- list()
for(i in 1:length(outcomes)){
  temp1 <- length(MODELS_1[[i]]$y)
  temp2 <- length(MODELS_2[[i]]$y)
  temp3 <- length(MODELS_3[[i]]$y)
  temp <- c(temp1,temp2,temp3)
  out1[[i]] <- temp
  id <- which(!is.na(MODELS_1[[i]]$data[[outcomes[[i]]]]))
  temp1 <- table(MODELS_1[[i]]$data$Treatment[id])
  id <- which(!is.na(MODELS_2[[i]]$data[[outcomes[[i]]]]))
  temp2 <- table(MODELS_2[[i]]$data$Treated[id])
  id <- which(!is.na(MODELS_3[[i]]$data[[outcomes[[i]]]]))
  #temp3 <- table(MODELS_3[[i]]$data$Treatment[id],MODELS_3[[i]]$data$mh[id])
  temp3 <- table(MODELS_3[[i]]$data$mh[id])
  out2[[i]] <- temp1
  out3[[i]] <- temp2
  out4[[i]] <- temp3
  
}
names(out1) <- outcome_labels
out1 <- do.call(rbind,out1)
names(out2) <- outcome_labels
out2 <- do.call(rbind,out2)
names(out3) <- outcome_labels
out3 <- do.call(rbind,out3)
names(out4) <- outcome_labels
out4 <- do.call(rbind,out4)
out <- cbind(out1[,1,drop=FALSE],out2,out3,out4)
write.table(out, file= "~/OneDrive - Kantar/RWE_US/2018_Purdue_ADHD/Results/04_28_2020/sample_size.txt", sep = "\t")



pvals <- sapply(MODELS,function(z){coef(summary(z))[,4]})
id <- Reduce(intersect,lapply(pvals,names))
pvals2 <- sapply(pvals, function(z){z[names(z) %in% id]})
ind <- apply(pvals2,1,function(z){any(z<0.05)})
ind <- apply(pvals2,1,function(z){mean(z<0.05)})

