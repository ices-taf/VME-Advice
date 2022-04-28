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
tab1[14,2]    <- sum(reg$seaMt,na.rm=T)
tab1[15,2]    <- sum(reg$banks,na.rm=T)
tab1[16,2]    <- sum(reg$corMd,na.rm=T)
tab1[17,2]    <- sum(reg$mudVolc,na.rm=T)

# now get overlap with fished area
tab1[c(2:4,6:8,10:12),c(3,4,5)] <- 0

if (1 %in% reg$FareaM){
  tt <- table(factor(reg$cat)[reg$FareaM==1],factor(reg$Tot_VMEs)[reg$FareaM==1]==3) 
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[2:4,3] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaM==1],reg$Tot_VMEs[reg$FareaM==1] %in% c(2,1))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[6:8,3] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaM==1],reg$Tot_VMEs[reg$FareaM==1] %in% c(0))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[10:12,3] <- tt[,which(colnames(tt) =="TRUE")]}
  tab1[14:17,3]  <- c(sum(reg$seaMt[reg$FareaM==1],na.rm=T),sum(reg$banks[reg$FareaM==1],na.rm=T),
                      sum(reg$corMd[reg$FareaM==1],na.rm=T),sum(reg$mudVolc[reg$FareaM==1],na.rm=T))
}

if (1 %in% reg$FareaS){
  tt <- table(factor(reg$cat)[reg$FareaS==1],factor(reg$Tot_VMEs)[reg$FareaS==1]==3)
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[2:4,4] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(factor(reg$cat)[reg$FareaS==1],factor(reg$Tot_VMEs)[reg$FareaS==1] %in% c(2,1))
  if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[6:8,4] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(factor(reg$cat)[reg$FareaS==1],factor(reg$Tot_VMEs)[reg$FareaS==1] %in% c(0))
  if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[10:12,4] <- tt[,which(colnames(tt) =="TRUE")]}
  tab1[14:17,4]  <- c(sum(reg$seaMt[reg$FareaS==1],na.rm=T),sum(reg$banks[reg$FareaS==1],na.rm=T),
                      sum(reg$corMd[reg$FareaS==1],na.rm=T),sum(reg$mudVolc[reg$FareaS==1],na.rm=T))
}

if (1 %in% reg$FareaC){
  tt <- table(factor(reg$cat)[reg$FareaC==1],factor(reg$Tot_VMEs)[reg$FareaC==1]==3)
  if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[2:4,5] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(factor(reg$cat)[reg$FareaC==1],factor(reg$Tot_VMEs)[reg$FareaC==1] %in% c(2,1))
  if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[6:8,5] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(factor(reg$cat)[reg$FareaC==1],factor(reg$Tot_VMEs)[reg$FareaC==1] %in% c(0))
  if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[10:12,5] <- tt[,which(colnames(tt) =="TRUE")]}
  tab1[14:17,5]  <- c(sum(reg$seaMt[reg$FareaC==1],na.rm=T),sum(reg$banks[reg$FareaC==1],na.rm=T),
                      sum(reg$corMd[reg$FareaC==1],na.rm=T),sum(reg$mudVolc[reg$FareaC==1],na.rm=T))
}

# get updated VMEs
tab1[2:4,7]   <- c(length(which(reg$New_VMEs==3 & reg$cat =="de2_4")),length(which(reg$New_VMEs==3 & reg$cat =="de4_8")),
                   length(which(reg$New_VMEs==3 & reg$cat =="de8")))
tab1[6:8,7]   <- c(length(which(reg$New_VMEs %in% c(2,1) & reg$cat =="de2_4")),length(which(reg$New_VMEs %in% c(2,1) & reg$cat =="de4_8")),
                   length(which(reg$New_VMEs %in% c(2,1) & reg$cat =="de8")))
tab1[10:12,7] <- c(length(which(reg$New_VMEs == 0 & reg$cat =="de2_4")),length(which(reg$New_VMEs == 0 & reg$cat =="de4_8")),
                   length(which(reg$New_VMEs == 0 & reg$cat =="de8")))
tab1[14,7]    <- 0
tab1[15,7]    <- 0
tab1[16,7]    <- 0
tab1[17,7]    <- 0

# now get per VME polygon option the new VME c-squares that are inside
if (1 %in% reg$scen11){
  tt <- matrix(table(reg$cat,reg$New_VMEs,reg$scen11))
  tt <- cbind(tt,matrix(table(reg$cat,reg$New_VMEs,reg$scen12)))
  tt <- cbind(tt,matrix(table(reg$cat,reg$New_VMEs,reg$scen21)))
  tt <- cbind(tt,matrix(table(reg$cat,reg$New_VMEs,reg$scen22)))
  tt <- cbind(tt,rep(0,nrow(tt))) #cbind(tt,matrix(table(reg$cat,reg$New_VMEs,reg$scen23)))
  tt <- rbind(tt[1:3,1:5],tt[4:6,1:5] + tt[7:9,1:5],tt[10:12,1:5])
  
  tab1[2:4,8:12]   <- tt[1:3,]
  tab1[6:8,8:12]   <- tt[4:6,]
  tab1[10:12,8:12] <- tt[7:9,]
  tab1[14:17,8:12] <- 0
}

# create one empty column
tab1$V6 <- " "
