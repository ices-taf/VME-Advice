################################################
#### code to source Table 1 - assessment sheet##
################################################

tab1 <- as.data.frame(matrix(data=NA,nrow = 18, ncol= 7))
tab1[2:5,1]   <- c("VME habitat","200-400m","400-800m",">800m") 
tab1[6:9,1]   <- c("VME index Medium and High","200-400m","400-800m",">800m")
tab1[10:13,1] <- c("VME index Low","200-400m","400-800m",">800m")
tab1[14:18,1] <- c("VME physical elements","Seamounts","Banks","Coral mounds","Mud volcanoes")

# get existing VMEs
tab1[3:5,2]   <- c(length(which(reg$Exist_VMEs==3 & reg$cat =="de2_4")),length(which(reg$Exist_VMEs==3 & reg$cat =="de4_8")),
                   length(which(reg$Exist_VMEs==3 & reg$cat =="de8")))
tab1[7:9,2]   <- c(length(which(reg$Exist_VMEs %in% c(2,1) & reg$cat =="de2_4")),length(which(reg$Exist_VMEs %in% c(2,1) & reg$cat =="de4_8")),
                   length(which(reg$Exist_VMEs %in% c(2,1) & reg$cat =="de8")))
tab1[11:13,2] <- c(length(which(reg$Exist_VMEs == 0 & reg$cat =="de2_4")),length(which(reg$Exist_VMEs == 0 & reg$cat =="de4_8")),
                   length(which(reg$Exist_VMEs == 0 & reg$cat =="de8")))
tab1[15,2]    <- sum(reg$seaMt,na.rm=T)
tab1[16,2]    <- sum(reg$banks,na.rm=T)
tab1[17,2]    <- sum(reg$corMd,na.rm=T)
tab1[18,2]    <- sum(reg$mudVolc,na.rm=T)

# get updated VMEs
tab1[3:5,3]   <- c(length(which(reg$New_VMEs==3 & reg$cat =="de2_4")),length(which(reg$New_VMEs==3 & reg$cat =="de4_8")),
                   length(which(reg$New_VMEs==3 & reg$cat =="de8")))
tab1[7:9,3]   <- c(length(which(reg$New_VMEs %in% c(2,1) & reg$cat =="de2_4")),length(which(reg$New_VMEs %in% c(2,1) & reg$cat =="de4_8")),
                   length(which(reg$New_VMEs %in% c(2,1) & reg$cat =="de8")))
tab1[11:13,3] <- c(length(which(reg$New_VMEs == 0 & reg$cat =="de2_4")),length(which(reg$New_VMEs == 0 & reg$cat =="de4_8")),
                   length(which(reg$New_VMEs == 0 & reg$cat =="de8")))
tab1[15,3]    <- 0
tab1[16,3]    <- 0
tab1[17,3]    <- 0
tab1[18,3]    <- 0

# now get per VME polygon option the new VME c-squares that are inside
if (1 %in% reg$scen11){
  idx <- 3 #VME habitat
  tt <- table(reg$cat[reg$scen11==1],reg$New_VMEs[reg$scen11==1] %in% idx)[,2]
  tt <- rbind(tt,table(reg$cat[reg$scen12==1],reg$New_VMEs[reg$scen12==1] %in% idx)[,2])
  tt <- rbind(tt,table(reg$cat[reg$scen21==1],reg$New_VMEs[reg$scen21==1] %in% idx)[,2])
  tt <- rbind(tt,table(reg$cat[reg$scen22==1],reg$New_VMEs[reg$scen22==1] %in% idx)[,2])
  tt <- rbind(tt,c("NA","NA","NA")) # S2O3 table(reg$cat[reg$scen23==1],reg$New_VMEs[reg$scen23==1] %in% idx)[,2]
  tab1[3,4] <- toString(tt[,1]) ;tab1[4,4] <- toString(tt[,2]);tab1[5,4] <- toString(tt[,3])
  
  idx <- c(2,1) #VME index medium + high 
  tt <- table(reg$cat[reg$scen11==1],reg$New_VMEs[reg$scen11==1] %in% idx)[,2]
  tt <- rbind(tt,table(reg$cat[reg$scen12==1],reg$New_VMEs[reg$scen12==1] %in% idx)[,2])
  tt <- rbind(tt,table(reg$cat[reg$scen21==1],reg$New_VMEs[reg$scen21==1] %in% idx)[,2])
  tt <- rbind(tt,table(reg$cat[reg$scen22==1],reg$New_VMEs[reg$scen22==1] %in% idx)[,2])
  tt <- rbind(tt,c("NA","NA","NA")) # S2O3 table(reg$cat[reg$scen23==1],reg$New_VMEs[reg$scen23==1] %in% idx)[,2]
  tab1[7,4] <- toString(tt[,1]) ;tab1[8,4] <- toString(tt[,2]);tab1[9,4] <- toString(tt[,3])
  
  idx <- 0 #VME index low
  tt <- table(reg$cat[reg$scen11==1],reg$New_VMEs[reg$scen11==1] %in% idx)[,2]
  tt <- rbind(tt,table(reg$cat[reg$scen12==1],reg$New_VMEs[reg$scen12==1] %in% idx)[,2])
  tt <- rbind(tt,table(reg$cat[reg$scen21==1],reg$New_VMEs[reg$scen21==1] %in% idx)[,2])
  tt <- rbind(tt,table(reg$cat[reg$scen22==1],reg$New_VMEs[reg$scen22==1] %in% idx)[,2])
  tt <- rbind(tt,c("NA","NA","NA")) # S2O3 table(reg$cat[reg$scen23==1],reg$New_VMEs[reg$scen23==1] %in% idx)[,2]
  tab1[11,4] <- toString(tt[,1]) ;tab1[12,4] <- toString(tt[,2]);tab1[13,4] <- toString(tt[,3])
}

# now get overlap with fished area
if (1 %in% reg$FareaM){
tab1[3:5,5] <- table(reg$cat[reg$FareaM==1],reg$Tot_VMEs[reg$FareaM==1]==3)[,2]
tab1[7:9,5] <- table(reg$cat[reg$FareaM==1],reg$Tot_VMEs[reg$FareaM==1] %in% c(2,1))[,2]
tab1[11:13,5] <- table(reg$cat[reg$FareaM==1],reg$Tot_VMEs[reg$FareaM==1] ==0)[,2]
tab1[15:18,5]  <- c(sum(reg$seaMt[reg$FareaM==1],na.rm=T),sum(reg$banks[reg$FareaM==1],na.rm=T),
                    sum(reg$corMd[reg$FareaM==1],na.rm=T),sum(reg$mudVolc[reg$FareaM==1],na.rm=T))
}

if (1 %in% reg$FareaS){
tab1[3:5,6] <- table(reg$cat[reg$FareaS==1],reg$Tot_VMEs[reg$FareaS==1]==3)[,2]
tab1[7:9,6] <- table(reg$cat[reg$FareaS==1],reg$Tot_VMEs[reg$FareaS==1] %in% c(2,1))[,2]
tab1[11:13,6] <- table(reg$cat[reg$FareaS==1],reg$Tot_VMEs[reg$FareaS==1] ==0)[,2]
tab1[15:18,6]  <- c(sum(reg$seaMt[reg$FareaS==1],na.rm=T),sum(reg$banks[reg$FareaS==1],na.rm=T),
                    sum(reg$corMd[reg$FareaS==1],na.rm=T),sum(reg$mudVolc[reg$FareaS==1],na.rm=T))

}

if (1 %in% reg$FareaC){
tab1[3:5,7] <- table(reg$cat[reg$FareaC==1],reg$Tot_VMEs[reg$FareaC==1]==3)[,2]
tab1[7:9,7] <- table(reg$cat[reg$FareaC==1],reg$Tot_VMEs[reg$FareaC==1] %in% c(2,1))[,2]
tab1[11:13,7] <- table(reg$cat[reg$FareaC==1],reg$Tot_VMEs[reg$FareaC==1] ==0)[,2]
tab1[15:18,7]  <- c(sum(reg$seaMt[reg$FareaC==1],na.rm=T),sum(reg$banks[reg$FareaC==1],na.rm=T),
                    sum(reg$corMd[reg$FareaC==1],na.rm=T),sum(reg$mudVolc[reg$FareaC==1],na.rm=T))
}


colnames(tab1) <- c("","Total number of existing VME/VME element c-sqrs. within the assessed area",
                    paste("Number of new and updated VME/VME element c-sqrs. within the assessed area (",datacallyear,")",sep=""),
                    paste("Number of new and updated VME  c-sqrs. in defined VME polygons (S1O1, S1O2, S2O1, S2O2, S1O2 + S2O1) (",datacallyear,")",sep=""),
                    paste("Total number of VME/VME element c-sqrs. within mobile gear fished areas between ",newyear_fished[length(newyear_fished)],"-",newyear_fished[1],sep=""),                    
                    paste("Total number of VME/VME element c-sqrs. within static gear fished areas between ",newyear_fished[length(newyear_fished)],"-",newyear_fished[1],sep=""),
                    paste("Total number of VME/VME element c-sqrs. within static + mobile gear fished areas between ",newyear_fished[length(newyear_fished)],"-",newyear_fished[1],sep=""))
