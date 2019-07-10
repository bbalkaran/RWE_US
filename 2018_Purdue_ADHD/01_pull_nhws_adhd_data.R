library(kollekt)

# LOAD NHWS DATA
setwd(getPath('NHWS_US'))
ptm <- proc.time()
load("nhws_2015.RData")
load("nhws_2016.RData")
load("nhws_2017.RData")
load("nhws_2018.RData")
load("nhws_char.RData")
proc.time() - ptm


mh_disorders <- c("Anxiety","Depression","Attention","Bipolar disorder","Panic disorder",
                  "Phobias","Social anxiety disorder","Generalized anxiety disorder",
                  "Obsessive Compulsive Disorder","Post-Traumatic Stress Disorder","Schizophrenia",
                  "Migraine","Headache")

# Variable Selection
my_vars <- c("ADHYES","ADYES","MCS", "PCS", "SF6D_R2", "EQ5DINDEX", "EQ5D6","TRAD", "TRADTMI",
            "WPPCTWRK", "WPIMPAIR", "WPWRKIMP", "WPACTIMP","ADHRX","ADRX","GAD7SCRE","SQSDTM","INPOCKR","INCSTPVT",
            "GADFN","GADWR","GADWDT","GADTR","GADCST","GADAI","GADPHA",
            "HHSMKUP12NG","HHSMKUP12NP","HHSMKUP12EC","HHSMKUP12NC","HHSMKUP12NA","HHSMKTME",
            "RUIWB","RUIAP","RUHWB","RUHAP","RUEWB","RUEAP","RUWRT","RUNON",
            "TOTIN_R","HHLBS_R")

ru_vars <- searchNHWS('code',pattern = list(pattern  = "^RU..6Q$"))
ru_vars_2 <- gsub("6Q","",ru_vars$variable)
sq_vars <- searchNHWS('code',pattern = list(pattern = "^SQSD"))
sw_vars <- searchNHWS('code',pattern = list(pattern = "^SWTC"))
otc_vars <- searchNHWS('label',pattern = list(pattern = c("OTC","counter")))
otc_vars <- searchNHWS('label',pattern = list(pattern = c("use")), data = otc_vars)
otc_vars <- searchNHWS('label',pattern = list(pattern = mh_disorders), data = otc_vars)
hac_vars <- searchNHWS('code',pattern = list(pattern = c("^HDTP")))
# severity variables
sv_vars_rx <-searchNHWS('code',pattern = list(pattern = "SVRX$"))
sv_vars_rx <-searchNHWS('label',pattern = list(pattern = mh_disorders), data = sv_vars_rx)
sv_vars_nrx <-searchNHWS('code',pattern = list(pattern = "SVNRX$"))
sv_vars_nrx <-searchNHWS('label',pattern = list(pattern = mh_disorders), data = sv_vars_nrx)
sv_vars <-searchNHWS('code',pattern = list(pattern = "SV$"))
sv_vars <-searchNHWS('label',pattern = list(pattern = c("severity")), data = sv_vars)
sv_vars <-searchNHWS('label',pattern = list(pattern = mh_disorders), data = sv_vars)

# Generate and run query
#adhd_query <- genNHWSquery("US", 2015:2018, vars = my_vars, cci = TRUE, dx = TRUE, demog = TRUE)
#adhd_data  <- runNHWSquery(adhd_query)
my_vars <- c(my_vars, ru_vars$variable, ru_vars_2, sq_vars$variable, otc_vars$variable, hac_vars$variable, sv_vars$variable, sv_vars_nrx$variable, sv_vars_rx$variable, sw_vars$variable)
adhd_data_1 <- selectNHWSdf("US", years = 2015:2018, vars = my_vars)
adhd_data_2 <- selectNHWSdf("US", years = 2015:2018, cci = TRUE, dx = TRUE, demog = TRUE)
common_id <- intersect(colnames(adhd_data_1), colnames(adhd_data_2))
adhd_data <- merge(adhd_data_1, adhd_data_2, by = common_id, all = TRUE)
fname <- file.path(getPath("RWE_US"),"2018_Purdue_ADHD","Data","adhd_data_raw.RData")
save(adhd_data, file = fname)
# ADHD and NON ADHD cohort selection
unloadNHWSdata()
# ADHD
adhd <- adhd_data[ADYES == 1 | ADDX == 1 | ADHYES == 1 | ADHDX == 1]
adhd <- getIndex(adhd)
adhd <- adhd[(ADYES == 1 & ADDX == 1) | (ADHYES == 1 & ADHDX == 1)]
adhd$cohort <- 'ADHD'
# NON ADHD
non_adhd <- adhd_data[!(zKey %in% adhd$zKey)]
non_adhd <- getIndex(non_adhd)
non_adhd$cohort <- "Non-ADHD"
# Combine Data
adhd_data <- list(adhd, non_adhd)
adhd_data <- rbindlist(adhd_data)
fname <- file.path(getPath("RWE_US"),"2018_Purdue_ADHD","Data","adhd_data.RData")
save(adhd_data, file = fname)



