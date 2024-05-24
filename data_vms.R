sar_layer <- readRDS("../../VME-advice_noGIT/EU/VMS data repository/All_VMS_datacall2022.rds") %>% 
  replace(is.na(.), 0) %>% 
  rename(c_square = c.square)
nam <- c(paste("SAR_total",refyear,sep="_"))
indexcol <- which(names(sar_layer) %in% nam) 
sar_layer$SAR <- rowMeans(sar_layer[indexcol],na.rm=T)
saveRDS(sar_layer, file = "data/sar_layer.rds")
