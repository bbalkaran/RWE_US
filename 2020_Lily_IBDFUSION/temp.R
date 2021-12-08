surveyIndex <- function(folder_root){
  if(any(grepl("header_sample.csv",list.dirs(folder_root)))){
    type <- "unlinked"
  }else{
    type <- "linked"
  }
  save_path <- paste0(folder_root,"RData/")
  if(type == "linked"){
    index <- fread("~/OneDrive - Kantar/KHDICT/LINKER/reconciled_bridge_04162020.txt")
    index <- getIndex2(index)
    index$client_patient_id <- as.character(index$client_patient_id)
    index$kid2 <- as.character(index$kid2)
    index$index <- as.Date(index$index)
    index$age <- year(index$index) - index$yob
    index$zKey <- as.character(index$zKey)
  }else{
    index <- fread(paste0(save_path, "index_dates.txt"))
    colnames(index)[colnames(index) == "patient_gender"] <- "gender"
    colnames(index)[colnames(index) == "patient_dob"] <- "yob"
    colnames(index)[colnames(index) == "pseudo_survey_date"] <- "index"
    index$patient_id <- as.character(index$patient_id)
    index$index <- as.Date(index$index)
    index$age <- year(index$index) - index$yob
    colnames(index)[colnames(index) == "patient_id"] <- "client_patient_id"
  }
  saveRDS(index, file = paste0(save_path, "index_dates.rds"))
}

blockRead <- function(folder_path, index, type, save_path){
  callingFn <- as.character(sys.call(-1))[1]
  if(!is.null(callingFn)){
    choices <- c("header","service","pharmacy")
    choice  <- choices[sapply(choices,function(z){grepl(z,callingFn, ignore.case = TRUE)})]
  }
  if(choice == "service"){
    cat("retrive HIPPS and other non-clinical codes . . . ")
    hipps <- fread("~/OneDrive - Kantar/KHDICT/HIPPS/HIPPSextV9/508_Compliant_Version_of_Master List.csv", header = TRUE)
    exclude <- c(paste0("G",8006:9140), paste0(c(str_pad(1:999, 4, pad = "0"),1000:6020),"F"), hipps$`HIPPS code`)
  }
  if(type=="linked"){
    files <- list.files(folder_path, full.names = TRUE)
    files <- files[grep(choice, files, ignore.case = TRUE)]
    if(choice == "header"){
      variables <- c('client_patient_id','claim_date',paste0('d',1:26),'da')
    }
    if(choice == "service"){
      variables <- c('client_patient_id','date_of_service','procedure') 
    }
    if(choice == "pharmacy"){
      variables <-  c('client_patient_id','date_of_service','ndc11')
    }
  }else{
    if(choice == "pharmacy"){
      choice2 <- "rx"
    }else{
      choice2 <- choice
    }
    files <- list.files(folder_path, full.names = TRUE)
    files <- files[grep(choice2, files, ignore.case = TRUE)]
    if(choice == "header"){
      variables <- c('patient_id','claim_date',paste0('D',1:26),'DA')
    }
    if(choice == "service"){
      variables <- c('patient_id','date_of_service','procedure')
    }
    if(choice == "pharmacy"){
      variables <-  c('patient_id','date_of_service','ndc11')
    }
  }
  if(length(files)>1){
    perblock <- 5
    blocks <- length(files) %/% perblock 
    extra  <- length(files) %% perblock 
    grp <- c(rep(1:blocks, rep(perblock,blocks)),rep(blocks+1, extra))
    files <- split(files,grp)
  }else{
    files <- list(files)
  }
  unique_values <- NULL
  cat(paste0("Reading ",choice," File(s) \n"))
  for(nblock in 1:length(files)){
    block_data <- list()
    cat("Processing block: ",nblock,"/",length(files),"\n")
    for(nfile in 1:length(files[[nblock]])){
      cat("- - - Processing file: ",nfile,"/",length(files[[nblock]])," in block ",nblock," \n")
      #claims_files<- fread(files[[nblock]][[nfile]], header = TRUE, select = variables, fill = TRUE)
      claims_files<- fread(files[[nblock]][[nfile]], header = TRUE, select = variables)
      if("patient_id" %in% colnames(claims_files)){
        colnames(claims_files)[colnames(claims_files) == "patient_id"] <- "client_patient_id"
      }
      if(choice == "header"){
        if(nrow(claims_files) > 500000){
          id <- seq(1,nrow(claims_files),by=500000)
          id <- unique(c(id,nrow(claims_files)))
          temp0 <- list()
          cat("- - - - - - Header File: Wide --> Long . . . ")
          for(i in 2:length(id)){
            temp <- claims_files[id[i-1]:(id[i]-1)]
            temp <- melt(temp, id=1:2, measure=variables[-c(1:2)])
            temp <- temp[temp$value!='']
            temp0[[i-1]] <- temp
          }
          rm(claims_files)
          rm(temp)
          claims_files <- rbindlist(temp0)
        }else{
          #claims_files <- melt(claims_files, id=1:4, measure=c(paste0("d",1:26),"da"))
          claims_files <- melt(claims_files, id=1:2, measure=c(paste0("D",1:26),"DA"))
        }
        claims_files[,variable := NULL]
      }
      claims_files$client_patient_id <- as.character(claims_files$client_patient_id)
      # Excluding HIPPS codes in procedure
      if(choice == "service"){
        claims_files <- claims_files[!(procedure %in% exclude)]
      }
      if(is.character(claims_files$client_patient_id)){
        ind  <- which(validUTF8(claims_files$client_patient_id))
        claims_files <- claims_files[ind]
      }
      if(choice == "header"){
        colnames(claims_files)[colnames(claims_files) == "claim_date"] <- "date_of_service"
        index_vars <- c('client_patient_id','age','gender','index')
      }else{
        index_vars <- c('client_patient_id','index')
      }
      claims_files$date_of_service   <- as.Date(claims_files$date_of_service)
      cat("Done \n")
      cat("- - - - - - Merging with Index dates . . . ")
      claims_files <- merge(claims_files,index[,..index_vars],by.x = "client_patient_id", by.y = "client_patient_id", allow.cartesian=TRUE)
      claims_files <- claims_files[date_of_service < index]
      cat("Done \n")
      if(choice == "header"){
        colnames(claims_files)[colnames(claims_files) == "value"] <- "icd"
        unique_values <- unique(c(unique_values,claims_files$icd))
      }
      if(choice == "service"){
        unique_values <- unique(c(unique_values,claims_files$procedure))
      }
      if(choice == "pharmacy"){
        unique_values <- unique(c(unique_values,claims_files$ndc11))
        claims_files$ndc9 <- substr(claims_files$ndc11,1,9)
      }
      block_data[[nfile]] <- claims_files
    }
    block_data <- rbindlist(block_data)
    saveRDS(block_data, file = paste0(save_path,choice,"_A_",nblock,".rds"))
    rm(block_data)
    gc()
  }
  saveRDS(unique_values, file = paste0(save_path,"unique_values.rds"))
}

combineDF <- function(folder_path, series, choice){
  files <- list.files(folder_path, full.names = TRUE)
  ind   <- which(sapply(strsplit(files,paste0("_",series,"_")),length)==2)
  files <- files[ind]
  if(length(files)>1){
    perblock <- switch(series,
                       "A" = 5,
                       "B" = 2)
    blocks <- length(files) %/% perblock 
    extra  <- length(files) %% perblock 
    grp <- c(rep(1:blocks, rep(perblock,blocks)),rep(blocks+1, extra))
    files <- split(files,grp)
  }else{
    files <- list(files)
  }
  cat("Processing series: ",series," \n")
  for(nblock in 1:length(files)){
    cat("Processing block: ",nblock,"/",length(files),"\n")
    block_data <- lapply(files[[nblock]],function(z){readRDS(z)})
    block_data <- rbindlist(block_data, fill = TRUE)
    block_data <- unique(block_data)
    saveRDS(block_data, file = paste0(folder_path,choice,"_", LETTERS[which(LETTERS == series)+1],"_",nblock,".rds"))
    rm(block_data)
    gc()
  }
}

structureHeader <- function(folder_root, index){
  folder_path <- paste0(folder_root,"Data/")
  if(any(grepl("header_sample.csv",list.dirs(folder_path)))){
    type <- "unlinked"
    folder_path <- paste0(folder_path,"header_sample.csv")
  }else{
    folder_path <- paste0(folder_root,"Data")
    type <- "linked"
  }
  save_path <- paste0(folder_root,"RData/processedData/header/")
  dir.create(save_path, recursive = TRUE)
  blockRead(folder_path, index, type, save_path)
  current_series <- "A"
  files <- list.files(save_path,full.names = TRUE)
  ind <- sapply(strsplit(files,"//"),function(z){z[2]})
  ind <- sapply(strsplit(ind,"_"),function(z){z[2]})
  nfiles <- sum(ind==current_series)
  while(nfiles > 2){
    combineDF(save_path, series = current_series, "header")
    current_series <- LETTERS[which(LETTERS == current_series) + 1]
    files <- list.files(save_path,full.names = TRUE)
    ind <- sapply(strsplit(files,"//"),function(z){z[2]})
    ind <- sapply(strsplit(ind,"_"),function(z){z[2]})
    nfiles <- sum(ind==current_series)
  }
  files <- list.files(save_path,full.names = TRUE)
  ind <- sapply(strsplit(files,"//"),function(z){z[2]})
  ind <- ind[ind != "unique_values.rds"]
  ind <- sapply(strsplit(ind,"_"),function(z){z[2]})
  ind <- which(ind != rev(sort(ind))[1])
  files <- files[ind]
  unlink(files)
  files <- list.files(save_path)
  files <- files[grep("_[A-Z]_",files)]
  new_files <- sapply(strsplit(files,"_"),function(z){paste(z[-2],collapse="_")})
  files <- paste0(save_path,files)
  new_files <- paste0(save_path,new_files)
  file.rename(files,new_files)
}


structureService <- function(folder_root, index){
  folder_path <- paste0(folder_root,"Data/")
  if(any(grepl("service_line_sample.csv",list.dirs(folder_path)))){
    type <- "unlinked"
    folder_path <- paste0(folder_path,"service_line_sample.csv")
  }else{
    type <- "linked"
    folder_path <- paste0(folder_root,"Data")
  }
  save_path <- paste0(folder_root,"RData/processedData/service/")
  dir.create(save_path, recursive = TRUE)
  blockRead(folder_path, index, type, save_path)
  current_series <- "A"
  files <- list.files(save_path,full.names = TRUE)
  ind <- sapply(strsplit(files,"//"),function(z){z[2]})
  ind <- ind[ind != "unique_values.rds"]
  ind <- sapply(strsplit(ind,"_"),function(z){z[2]})
  nfiles <- sum(ind==current_series)
  while(nfiles > 2){
    combineDF(save_path, series = current_series, "service")
    current_series <- LETTERS[which(LETTERS == current_series) + 1]
    files <- list.files(save_path,full.names = TRUE)
    ind <- sapply(strsplit(files,"//"),function(z){z[2]})
    ind <- sapply(strsplit(ind,"_"),function(z){z[2]})
    nfiles <- sum(ind==current_series)
  }
  files <- list.files(save_path,full.names = TRUE)
  ind <- sapply(strsplit(files,"//"),function(z){z[2]})
  ind <- ind[ind != "unique_values.rds"]
  ind <- sapply(strsplit(ind,"_"),function(z){z[2]})
  ind <- which(ind != rev(sort(ind))[1])
  files <- files[ind]
  unlink(files)
  files <- list.files(save_path)
  files <- files[grep("_[A-Z]_",files)]
  new_files <- sapply(strsplit(files,"_"),function(z){paste(z[-2],collapse="_")})
  files <- paste0(save_path,files)
  new_files <- paste0(save_path,new_files)
  file.rename(files,new_files)
}

structurePharmacy <- function(folder_root, index){
  folder_path <- paste0(folder_root,"Data/")
  if(any(grepl("rx_sample.csv",list.dirs(folder_path)))){
    type <- "unlinked"
    folder_path <- paste0(folder_path,"rx_sample.csv")
  }else{
    type <- "linked"
    folder_path <- paste0(folder_root,"Data")
  }
  save_path <- paste0(folder_root,"RData/processedData/pharmacy/")
  dir.create(save_path, recursive = TRUE)
  blockRead(folder_path, index, type, save_path)
  current_series <- "A"
  files <- list.files(save_path,full.names = TRUE)
  ind <- sapply(strsplit(files,"//"),function(z){z[2]})
  ind <- ind[ind != "unique_values.rds"]
  ind <- sapply(strsplit(ind,"_"),function(z){z[2]})
  nfiles <- sum(ind==current_series)
  while(nfiles > 2){
    combineDF(save_path, series = current_series, "pharmacy")
    current_series <- LETTERS[which(LETTERS == current_series) + 1]
    files <- list.files(save_path,full.names = TRUE)
    ind <- sapply(strsplit(files,"//"),function(z){z[2]})
    ind <- sapply(strsplit(ind,"_"),function(z){z[2]})
    nfiles <- sum(ind==current_series)
  }
  files <- list.files(save_path,full.names = TRUE)
  ind <- sapply(strsplit(files,"//"),function(z){z[2]})
  ind <- ind[ind != "unique_values.rds"]
  ind <- sapply(strsplit(ind,"_"),function(z){z[2]})
  ind <- which(ind != rev(sort(ind))[1])
  files <- files[ind]
  unlink(files)
  files <- list.files(save_path)
  files <- files[grep("_[A-Z]_",files)]
  new_files <- sapply(strsplit(files,"_"),function(z){paste(z[-2],collapse="_")})
  files <- paste0(save_path,files)
  new_files <- paste0(save_path,new_files)
  file.rename(files,new_files)
}

readUnique <- function(folder_root, type){
  file <- paste0(folder_root,"RData/processedData/",type,"/unique_values.rds")
  values <- readRDS(file)
  return(values)
}


