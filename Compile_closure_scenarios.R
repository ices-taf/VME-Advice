
##########################################################
###### script to run ICES EUVME advice scenarios  #######
##########################################################

## set path to folder and specify username 
  pathdir <- "C:/Users/danie/Documents/Online for git/VME-advice"
  pathdir_nogit <- "C:/Users/danie/Documents/Online for git/VME-advice_noGIT" # stores the VMS and VME data from sharepoint
  ices_username <- "vandenderen"

  
## install libraries
  source(paste(pathdir,"Utilities/Libraries_ICESVME.R",sep="/"))  
  