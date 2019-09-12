#########################################################
#  Script: 0.0_Read_in_NPI_Crosswalk.R
#  Purpose: NPI data from CMS
#  Data file: 
#  Author: Bridget Balkaran
#  Project: 161103897-1
#  Project Manager: Martine Maculaitis
#  Date Created: 9/12/19
#  Date Edited: 9/12/19
########################################################


library(jsonlite)

NPI_xwalk <- fromJSON("https://data.cms.gov/resource/j75i-rw8y.json")