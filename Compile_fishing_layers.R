
##########################################################
 ############# script to obtain fishing layers #######
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
  
# load current fishing footprints of EU and NEAFC
  source(paste(pathdir,"Utilities/Footprint_current.R",sep="/"))  
  rm(list=c("EUFootp_mob","EUFootp_stat"))   # remove mobile/static ICES defined footprint -- not used
  
# obtain updated footprint based on available VMS data 
  # this can be run each year - but not used 
  # only used when requested to update the footprint following EUVME advice
  datacallyear      <- 2021
  refyear_footprint <- 2009:2011
  source(paste(pathdir,"Utilities/Footprint_update_newVMS.R",sep="/")) 

# obtain fished area 
  datacallyear      <- 2021
  refyear_fished <- 2009:2011
  newyear_fished <- (datacallyear-1):(datacallyear-3)
  source(paste(pathdir,"Utilities/Fishedarea_newVMS.R",sep="/")) 
  
# save footprint workspace
  save.image(file = paste(pathdir,"2-Data processing/Fishing_workspace.RData",sep="/"))
  