## read, process and export the VME Observations data file

vmedb <- read.csv(taf.data.path("VME_observations_datacall_2024_eu_29082024.csv"), 
            header=T,sep=";",row.names = NULL,stringsAsFactors = FALSE)

## Change the insert date of sample "SeaRover18_540"
vmedb$InsDateTime <- ifelse(vmedb$Sample == "SeaRover18_540", "2024-03-29 17:57:10.000", vmedb$InsDateTime)

## Save VME database 
saveRDS(vmedb, file = "data/vme_db.rds")