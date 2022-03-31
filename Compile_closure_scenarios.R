
##########################################################
 ############# script to run VME scenarios  #############
##########################################################

## before running the scenarios, please download all  ##
## data from ICES Sharepoint -->                      ##
## Get_data_VMEadvice_sharepoint.R

# set path to folder VME advice
  pathdir <- "C:/Users/danie/Documents/Online for git/VME-advice"

# set path to folder with VMS and VME data from sharepoint
  pathdir_nogit <- "C:/Users/danie/Documents/Online for git/VME-advice_noGIT" 
  
# install libraries
  source(paste(pathdir,"Utilities/Libraries_VMEadvice.R",sep="/"))  
  
# VME data call year
  datacallyear <- 2021

# run scenario 1 - option 1 & 2 (warnings are okay)
  source(paste(pathdir,"Utilities/Scenario_1_option_1.R",sep="/"))  
  source(paste(pathdir,"Utilities/Scenario_1_option_2.R",sep="/")) 
  
# run scenario 2 - option 1& 2 (warnings are okay)
  refyear       <- 2009:datacallyear-1   # specify years to estimate sar threshold
  SAR_threshold <- 0.43                  # SAR threshold value
  source(paste(pathdir,"Utilities/Scenario_2_option_1.R",sep="/")) 
  source(paste(pathdir,"Utilities/Scenario_2_option_2.R",sep="/"))
  
# done below creates a workspace that can be used for plotting
  
  # save closure workspace
  scen11 <- st_read(paste("2-Data processing/Scenario1_option1.shp",sep="/"))
  scen12 <- st_read(paste("2-Data processing/Scenario1_option2.shp",sep="/"))
  scen21 <- st_read(paste("2-Data processing/Scenario2_option1.shp",sep="/"))
  scen22 <- st_read(paste("2-Data processing/Scenario2_option2.shp",sep="/"))
  
  # load VME elements
  Bank       <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Bank.shp",sep="/"))
  Bank       <- st_make_valid(Bank)
  Coralmound <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_CoralMounds.shp",sep="/"))
  Coralmound <- st_make_valid(Coralmound)
  Mudvolcano <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Mud_Volcano.shp",sep="/"))
  Mudvolcano <- st_make_valid(Mudvolcano)
  Seamount   <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Seamount.shp",sep="/"))
  Seamount   <- st_make_valid(Seamount)
  Elements   <- rbind(Bank,Coralmound,Mudvolcano,Seamount)
  
  # clean and save workspace
  rm(list=setdiff(ls(), c("scen11" , "scen12","scen21","scen22","Elements")))
  save.image(file = "2-Data processing/Closures_workspace.RData")
  
  