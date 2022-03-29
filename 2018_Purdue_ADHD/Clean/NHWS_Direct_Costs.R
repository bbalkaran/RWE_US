# Calculate direct costs based on HCRU and age group
# annualized costs -> multiply NHWS variables * 2 
# Dir.Cost.TRADTMI == annual direct costs based on number of traditional provider visits
# Dir.Cost.ER == annual direct costs based on number of ER visits
# Dir.Cost.HP == annual direct costs based on number of hospitalizations 

direct_costs<- function(data){
  data <- data %>%
    mutate(Dir.Cost.TRADTMI = case_when(DEAGE_R >= 18 & DEAGE_R <= 44 ~ TRADTMI*2*1029,
                            DEAGE_R >= 45 & DEAGE_R <= 64  ~ TRADTMI*2*1520,
                            DEAGE_R >= 65 ~ TRADTMI*2*1888),
           Dir.Cost.ER = case_when(DEAGE_R >= 18 & DEAGE_R <= 44 ~ RUER6Q*2*1638,
                                   DEAGE_R >= 45 & DEAGE_R <= 64  ~ RUER6Q*2*2062,
                                   DEAGE_R >= 65 ~ RUER6Q*2*1173),
           Dir.Cost.HP = case_when(DEAGE_R >= 18 & DEAGE_R <= 44 ~ RUHP6Q*2*15632,
                                   DEAGE_R >= 45 & DEAGE_R <= 64  ~ RUHP6Q*2*23615,
                                   DEAGE_R >= 65 ~ RUHP6Q*2*18656),
           Dir.Cost.Total = Dir.Cost.TRADTMI + Dir.Cost.ER + Dir.Cost.HP)
  data$Total_Direct_Cost <- remove_all_labels(data$Dir.Cost.Total)
  return(data)
  
}


# DATA COMES FROM HERE: https://meps.ahrq.gov/mepstrends/hc_use/

# Select statistic: Mean expenditure per person with espense
# select data view: cross-sectional
# year: whatever year you want, most recent as of writing this is 2016, 2016 used here
# group by (columns): age groups ( 18- 44, 45 - 64, 65+)
# group by (rows): check:  physician based office visits  = TRADTMI
                     # :  Emergency room visits = RUER6Q
                     # : Inpatient stays = RUHP6Q



direct_costs_claims<- function(data){
  data <- data %>%
    mutate(Dir.Cost.OP = case_when(DEAGE_R >= 18 & DEAGE_R <= 44 ~ OP*1029,
                                        DEAGE_R >= 45 & DEAGE_R <= 64  ~ OP*1520,
                                        DEAGE_R >= 65 ~ OP*1888),
           Dir.Cost.ER = case_when(DEAGE_R >= 18 & DEAGE_R <= 44 ~ ER*1638,
                                   DEAGE_R >= 45 & DEAGE_R <= 64  ~ ER*2062,
                                   DEAGE_R >= 65 ~ ER*1173),
           Dir.Cost.IP = case_when(DEAGE_R >= 18 & DEAGE_R <= 44 ~ IP*15632,
                                   DEAGE_R >= 45 & DEAGE_R <= 64  ~ IP*23615,
                                   DEAGE_R >= 65 ~ IP*18656),
           Dir.Cost.Total = Dir.Cost.OP + Dir.Cost.ER + Dir.Cost.IP)
  data$Total_Direct_Cost <- remove_all_labels(data$Dir.Cost.Total)
  return(data)
}

