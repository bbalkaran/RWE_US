# Calculate Indirect Costs of Absenteeism, Presenteeim, and Total Indirect Costs

indirect_costs<- function(data){
  data <- data %>%
    mutate(wage = case_when(DEAGE_R == 18 & DEAGE_R <= 19 & DESEX == 2 ~ 402,
                            DEAGE_R >= 20 & DEAGE_R <= 24 & DESEX == 2 ~ 514,
                            DEAGE_R >= 25 & DEAGE_R <= 34 & DESEX == 2 ~ 724,
                            DEAGE_R >= 35 & DEAGE_R <= 44 & DESEX == 2 ~ 860,
                            DEAGE_R >= 45 & DEAGE_R <= 54 & DESEX == 2 ~ 855,
                            DEAGE_R >= 55 & DEAGE_R <= 64 & DESEX == 2 ~ 856,
                            DEAGE_R >= 65 & DESEX == 2 ~ 782,
                            DEAGE_R == 18 & DEAGE_R <= 19 & DESEX == 1 ~ 459,
                            DEAGE_R >= 20 & DEAGE_R <= 24 & DESEX == 1 ~ 570,
                            DEAGE_R >= 25 & DEAGE_R <= 34 & DESEX == 1 ~ 821,
                            DEAGE_R >= 35 & DEAGE_R <= 44 & DESEX == 1 ~ 1062,
                            DEAGE_R >= 45 & DEAGE_R <= 54 & DESEX == 1 ~ 1103,
                            DEAGE_R >= 55 & DEAGE_R <= 64 & DESEX == 1 ~ 1098,
                            DEAGE_R >= 65 & DESEX == 1 ~ 1016)) %>%
    #https://www.bls.gov/opub/reports/womens-earnings/2017/pdf/home.pdf  DATA COMES FROM HERE
    mutate(Hourly_wage = wage/40) %>%  # weekly earnings/ hours worked per week) 
    mutate(presenteeism_wpwrk = (WPPRD/10)*WPWRK) %>% # work productivity scale/ 10 * hours worked per week
    mutate(Indirect_cost_ABS = Hourly_wage*WPMIS*50) %>% # hourly wage *work hours missed absenteeism* 50 work weeks per year
    mutate(Indirect_cost_pres = Hourly_wage*presenteeism_wpwrk*50) # hourly wage * work hours missed presenteeism *50 work weeks per year
  library(sjlabelled)
  data$Indirect_cost_ABS <- remove_all_labels(data$Indirect_cost_ABS)
  data$Indirect_cost_pres <- remove_all_labels(data$Indirect_cost_pres)
  data <- data %>% 
    mutate(Indirect_cost_PRES = case_when(is.na(Indirect_cost_pres)== T & 
                                            is.na(Indirect_cost_ABS) == F ~ 0.00, # Some patients only have ABS, can't add values to NA 
                                          TRUE ~ Indirect_cost_pres)) %>%          # change to 0s to add for totals
    mutate(Total_indirect_cost = Indirect_cost_ABS + Indirect_cost_PRES) 
  data$Total_indirect_cost[is.na(data$Total_indirect_cost)] <- 0
  #data <- data %>% select(-wage, -Hourly_wage, -presenteeism_wpwrk, -Indirect_cost_pres)
  return(data)
  
}

