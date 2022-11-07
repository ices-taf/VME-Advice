################################################
#### code to source Table 1 - assessment sheet##
################################################

tab1 <- as.data.frame(matrix(data=NA,nrow = 17, ncol= 12))
tab1[1:4,1]   <- c("VME habitat","200-400m","400-800m",">800m") 
tab1[5:8,1]   <- c("VME index Medium and High","200-400m","400-800m",">800m")
tab1[9:12,1] <- c("VME index Low","200-400m","400-800m",">800m")
tab1[13:17,1] <- c("VME physical elements","Seamounts","Banks","Coral mounds","Mud volcanoes")

# get existing VMEs
tab1[2:4,2]   <- c(length(which(reg$Exist_VMEs==3 & reg$cat =="de2_4")),length(which(reg$Exist_VMEs==3 & reg$cat =="de4_8")),
                   length(which(reg$Exist_VMEs==3 & reg$cat =="de8")))
tab1[6:8,2]   <- c(length(which(reg$Exist_VMEs %in% c(2,1) & reg$cat =="de2_4")),length(which(reg$Exist_VMEs %in% c(2,1) & reg$cat =="de4_8")),
                   length(which(reg$Exist_VMEs %in% c(2,1) & reg$cat =="de8")))
tab1[10:12,2] <- c(length(which(reg$Exist_VMEs == 0 & reg$cat =="de2_4")),length(which(reg$Exist_VMEs == 0 & reg$cat =="de4_8")),
                   length(which(reg$Exist_VMEs == 0 & reg$cat =="de8")))
tab1[14,2]    <- reg %>% filter(!is.na(Exist_VMEs)) %>% select(seaMt) %>% sum(na.rm=T) 
tab1[15,2]    <- reg %>% filter(!is.na(Exist_VMEs)) %>% select(banks) %>% sum(na.rm=T) 
tab1[16,2]    <- reg %>% filter(!is.na(Exist_VMEs)) %>% select(corMd) %>% sum(na.rm=T)
tab1[17,2]    <- reg %>% filter(!is.na(Exist_VMEs)) %>% select(mudVolc) %>% sum(na.rm=T)

# now get overlap with fished area
tab1[c(2:4,6:8,10:12),c(3,4,5)] <- 0

if (1 %in% reg$FareaM){
  tt <- table(factor(reg$cat)[reg$FareaM==1],factor(reg$Exist_VMEs)[reg$FareaM==1]==3) 
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[2:4,3] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaM==1],reg$Exist_VMEs[reg$FareaM==1] %in% c(2,1))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[6:8,3] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaM==1],reg$Exist_VMEs[reg$FareaM==1] %in% c(0))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[10:12,3] <- tt[,which(colnames(tt) =="TRUE")]}
  
  tab1[14:17,3]  <- reg %>% filter(!is.na(Exist_VMEs), FareaM==1) %>% select(seaMt, banks, corMd, mudVolc) %>% purrr::map_dfr(sum, na.rm = T) %>% t()
  
}

if (1 %in% reg$FareaS){
  tt <- table(factor(reg$cat)[reg$FareaS==1],factor(reg$Exist_VMEs)[reg$FareaS==1]==3)
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[2:4,4] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(factor(reg$cat)[reg$FareaS==1],factor(reg$Exist_VMEs)[reg$FareaS==1] %in% c(2,1))
  if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[6:8,4] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(factor(reg$cat)[reg$FareaS==1],factor(reg$Exist_VMEs)[reg$FareaS==1] %in% c(0))
  if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[10:12,4] <- tt[,which(colnames(tt) =="TRUE")]}
  tab1[14:17,4]  <- reg %>% filter(!is.na(Exist_VMEs), FareaS==1) %>% select(seaMt, banks, corMd, mudVolc) %>% purrr::map_dfr(sum, na.rm = T) %>% t()
}

if (1 %in% reg$FareaC){
  tt <- table(factor(reg$cat)[reg$FareaC==1],factor(reg$Exist_VMEs)[reg$FareaC==1]==3)
  if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[2:4,5] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(factor(reg$cat)[reg$FareaC==1],factor(reg$Exist_VMEs)[reg$FareaC==1] %in% c(2,1))
  if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[6:8,5] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(factor(reg$cat)[reg$FareaC==1],factor(reg$Exist_VMEs)[reg$FareaC==1] %in% c(0))
  if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[10:12,5] <- tt[,which(colnames(tt) =="TRUE")]}
  tab1[14:17,5]  <- reg %>% filter(!is.na(Exist_VMEs), FareaC==1) %>% select(seaMt, banks, corMd, mudVolc) %>% purrr::map_dfr(sum, na.rm = T) %>% t()
  
}

# overlap for new and updated VME evidence

# get updated VMEs
tab1[2:4,7]   <- c(length(which(reg$New_VMEs==3 & reg$cat =="de2_4")),length(which(reg$New_VMEs==3 & reg$cat =="de4_8")),
                   length(which(reg$New_VMEs==3 & reg$cat =="de8")))
tab1[6:8,7]   <- c(length(which(reg$New_VMEs %in% c(2,1) & reg$cat =="de2_4")),length(which(reg$New_VMEs %in% c(2,1) & reg$cat =="de4_8")),
                   length(which(reg$New_VMEs %in% c(2,1) & reg$cat =="de8")))
tab1[10:12,7] <- c(length(which(reg$New_VMEs == 0 & reg$cat =="de2_4")),length(which(reg$New_VMEs == 0 & reg$cat =="de4_8")),
                   length(which(reg$New_VMEs == 0 & reg$cat =="de8")))
tab1[14,7]    <- reg %>% filter(!is.na(New_VMEs)) %>% select(seaMt) %>% sum(na.rm=T)
tab1[15,7]    <- reg %>% filter(!is.na(New_VMEs)) %>% select(banks) %>% sum(na.rm=T)
tab1[16,7]    <- reg %>% filter(!is.na(New_VMEs)) %>% select(corMd) %>% sum(na.rm=T)
tab1[17,7]    <- reg %>% filter(!is.na(New_VMEs)) %>% select(mudVolc) %>% sum(na.rm=T)

if (1 %in% reg$FareaM){
  tt <- table(factor(reg$cat)[reg$FareaM==1],factor(reg$New_VMEs)[reg$FareaM==1]==3) 
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[2:4,8] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaM==1],reg$New_VMEs[reg$FareaM==1] %in% c(2,1))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[6:8,8] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaM==1],reg$New_VMEs[reg$FareaM==1] %in% c(0))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[10:12,8] <- tt[,which(colnames(tt) =="TRUE")]}
  tab1[14:17,8]  <- reg %>% filter(!is.na(New_VMEs), FareaM==1) %>% select(seaMt, banks, corMd, mudVolc) %>% purrr::map_dfr(sum, na.rm = T) %>% t()
  
}
if (1 %in% reg$FareaS){
  tt <- table(factor(reg$cat)[reg$FareaS==1],factor(reg$New_VMEs)[reg$FareaS==1]==3) 
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[2:4,9] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaS==1],reg$New_VMEs[reg$FareaS==1] %in% c(2,1))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[6:8,9] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaS==1],reg$New_VMEs[reg$FareaS==1] %in% c(0))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[10:12,9] <- tt[,which(colnames(tt) =="TRUE")]}
  tab1[14:17,9]  <- reg %>% filter(!is.na(New_VMEs), FareaS==1) %>% select(seaMt, banks, corMd, mudVolc) %>% purrr::map_dfr(sum, na.rm = T) %>% t()
}
if (1 %in% reg$FareaC){
  tt <- table(factor(reg$cat)[reg$FareaC==1],factor(reg$New_VMEs)[reg$FareaC==1]==3) 
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[2:4,10] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaC==1],reg$New_VMEs[reg$FareaC==1] %in% c(2,1))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[6:8,10] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaC==1],reg$New_VMEs[reg$FareaC==1] %in% c(0))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[10:12,10] <- tt[,which(colnames(tt) =="TRUE")]}
  tab1[14:17,10]  <- reg %>% filter(!is.na(New_VMEs), FareaC==1) %>% select(seaMt, banks, corMd, mudVolc) %>% purrr::map_dfr(sum, na.rm = T) %>% t()
}

tab1[2:4,12]   <- c(length(which(reg$Tot_VMEs==3 & reg$cat =="de2_4")),length(which(reg$Tot_VMEs==3 & reg$cat =="de4_8")),
                   length(which(reg$Tot_VMEs==3 & reg$cat =="de8")))
tab1[6:8,12]   <- c(length(which(reg$Tot_VMEs %in% c(2,1) & reg$cat =="de2_4")),length(which(reg$Tot_VMEs %in% c(2,1) & reg$cat =="de4_8")),
                   length(which(reg$Tot_VMEs %in% c(2,1) & reg$cat =="de8")))
tab1[10:12,12] <- c(length(which(reg$Tot_VMEs == 0 & reg$cat =="de2_4")),length(which(reg$Tot_VMEs == 0 & reg$cat =="de4_8")),
                   length(which(reg$Tot_VMEs == 0 & reg$cat =="de8")))

tab1[14,12]    <- reg %>% filter(!is.na(Tot_VMEs)) %>% select(seaMt) %>% sum(na.rm=T)
tab1[15,12]    <- reg %>% filter(!is.na(Tot_VMEs)) %>% select(banks) %>% sum(na.rm=T)
tab1[16,12]    <- reg %>% filter(!is.na(Tot_VMEs)) %>% select(corMd) %>% sum(na.rm=T)
tab1[17,12]    <- reg %>% filter(!is.na(Tot_VMEs)) %>% select(mudVolc) %>% sum(na.rm=T)




# now get per VME polygon option the new VME c-squares that are inside
tab1b <- as.data.frame(matrix(data=NA,nrow = 17, ncol= 7))
tab1b[,1] <- tab1[,1]
tab1b[,2] <- tab1[,7]

if (1 %in% reg$scen11){
  tt <- matrix(table(reg$cat,reg$New_VMEs,reg$scen11))
  tt <- cbind(tt,matrix(table(reg$cat,reg$New_VMEs,reg$scen12)))
  tt <- cbind(tt,matrix(table(reg$cat,reg$New_VMEs,reg$scen21)))
  tt <- cbind(tt,matrix(table(reg$cat,reg$New_VMEs,reg$scen22)))
  tt <- cbind(tt,rep(0,nrow(tt))) #cbind(tt,matrix(table(reg$cat,reg$New_VMEs,reg$scen23)))
  tt <- rbind(tt[1:3,1:5],tt[4:6,1:5] + tt[7:9,1:5],tt[10:12,1:5])
  
  tab1b[2:4,3:7]   <- tt[1:3,]
  tab1b[6:8,3:7]   <- tt[4:6,]
  tab1b[10:12,3:7] <- tt[7:9,]
  tab1b[14:17,3:7] <- 0
}
