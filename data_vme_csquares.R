## read in the processed VME file which includes the habitat modelled stuff
vme_data     <- read.csv(taf.data.path("VME_csquares_datacall_2022_eu.csv"), header=T)
saveRDS(vme_data, file = "data/vme_data")
