# Format data for prediction model 
diagnosis  <- processDX(data_path)
procedures <- processPS(data_path)
treatments <- processRX(data_path)
pred_mat   <- getPredMat(diagnosis, procedures, treatments, index)


model_path <- "/Volumes/My Passport for Mac/KOMODO_LINKED_2020/"
model_vars <- readRDS(file = paste0(model_path,"RData/pred_vars.rds"))
# Variables in Data but not present in the model 
NIM <- setdiff(colnames(pred_mat), model_vars)
NIM <- NIM[-1]
mu_NIM <- pred_mat[,..NIM]
mu_NIM <- apply(mu_NIM,2,mean)
# Remove NIM variables 
set(pred_mat, , NIM, NULL)

# Add variables in model not present in data to the model 
VIM <- setdiff(model_vars, colnames(pred_mat))
set(pred_mat, , VIM, 0)
model_vars <- c("kid2",model_vars)
pred_mat <- pred_mat[,..model_vars]

# Write data 
write.table(pred_mat, file = paste0(data_path,"RData/pred_mat.txt"), sep="\t", row.names = FALSE)

