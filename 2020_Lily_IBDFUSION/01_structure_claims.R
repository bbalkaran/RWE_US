# Get Index Dates Processed in Step 00
index <- readRDS(paste0(data_path, "RData/index_dates.rds"))

# Read and Structure: Header, Service Lines, and Pharmacy  Data 
structureHeader(data_path, index)
structureService(data_path, index)
structurePharmacy(data_path, index)

# Get Value Mapping
header_map <- readUnique(data_path,"header") %>% 
              convICD(.) %>%
              chronicCCS(.) %>%
              filter(.,ccs != "")
saveRDS(header_map,file = paste0(data_path,"RData/processedData/header/header_map.rds"))

service_map <- readUnique(data_path,"service") %>% 
               getProcedures(.) 
saveRDS(service_map,file = paste0(data_path,"RData/processedData/service/service_map.rds"))

pharmacy_map <- readUnique(data_path,"pharmacy") %>%
                prepPharmacy(.,atc_class = 5)
saveRDS(pharmacy_map,file = paste0(data_path,"RData/processedData/pharmacy/pharmacy_map.rds"))

# final merge 
mergeFilter(data_path, type = "header",   pre_period = pre_period, combine = TRUE)
mergeFilter(data_path, type = "service",  pre_period = pre_period, combine = TRUE)
mergeFilter(data_path, type = "pharmacy", pre_period = pre_period, combine = TRUE)

