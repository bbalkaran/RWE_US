library(data.table)
library(dplyr)
library(kollekt)
library(stringr)
library(icd)
library(pryr)
library(touch)

type       <- "unlinked"   # either "linked" or "unlinked"
data_path  <- "/Volumes/My Passport for Mac/KOMODO_IBD/"
pre_period <- 365

# I N D E X   D A T E S
surveyIndex(data_path)



