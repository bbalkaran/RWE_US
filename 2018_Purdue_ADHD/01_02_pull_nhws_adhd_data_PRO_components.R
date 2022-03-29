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

my_vars <- c("EQ5D1","EQ5D2","EQ5D3","EQ5D4","EQ5D5","EQ5D6",'BP_NBS','GH_NBS','MH_NBS','PF_NBS','RE_NBS','RP_NBS','SF_NBS','VT_NBS')
pro_extra <- selectNHWSdf("US", years = 2015:2018, vars = my_vars, cci = FALSE, dx = FALSE, demog = FALSE)
fname <- file.path(getPath("RWE_US"),"2018_Purdue_ADHD","Data","pro_extra.RData")
save(pro_extra, file = fname)
