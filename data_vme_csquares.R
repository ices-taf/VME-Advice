## read in the processed VME file which includes the habitat modelled stuff
vme_data     <- read.csv(taf.data.path(vme_csquares_file), header=T)
saveRDS(vme_data, file = "data/vme_data.rds")
