
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
  

  
  