library(StatMatch)
library(mice)
library(miceadds)
library(mitools)
library(kollekt)
library(psych)

model_path <- "/Volumes/My Passport for Mac/KOMODO_LINKED_2020/"
data_path <- "/Volumes/My Passport for Mac/KOMODO_IBD/"
outcomes <- c('PCS','MCS','SF6D_R2','EQ5DINDEX','WPPCTWRK','WPIMPAIR','WPWRKIMP','WPACTIMP','WPPRD','WPWRK','WPMIS','employed')
mcid          <- c(3,3,0.18,0.033); names(mcid) <- outcomes[1:4]

ibd_icd  <- searchCodes(search = 'code',vocabulary = c("ICD9CM", "ICD10CM"), pattern = list(pattern = c("^556","^555","^K50","^K51")))
ibd_icd <- gsub("\\.","",ibd_icd$concept_code)
train_id <- getErrorGroup(ibd_icd)


train_error <- getTrainError(model_path,  outcomes = outcomes, train_group =  train_id$ind_ccs, match_adjust = TRUE)
fused <- getFused(data_path, train_error = train_error, mcid = mcid)
save(fused, train_error, file = paste0(data_path,"RData/fused_data.RData"))
# Input data 
input <- fread(paste0(data_path,"RData/pred_mat.txt"))
input$age_grp <- cut(input$age,breaks = c(15,44,64,79,Inf))
input <- input[,c("kid2","age_grp")]
fused2 <- mergeFuse(fused1,input)
# Splits by groups of interest 
fused_gender  <- splitFuse(fused2,"gender")
fused_age     <- splitFuse(fused2,"age_grp")
fused_employed <- splitFuse(fused2,"pred_employed")

mu_overall    <- displayMean(fused2)
mu_overall_gender <- lapply(reFormatFused(fused_gender), displayMean)
mu_overall_age    <- lapply(reFormatFused(fused_age), displayMean)
mu_employed    <- lapply(reFormatFused(fused_employed), displayMean2)
rho_pro           <- displayRho(fused2)
rho_age           <- displayRho(fused2,external = "age")
prop_overall <- displayProp(fused2)