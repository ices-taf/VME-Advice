
##########################################################
###### script to run VME advice scenarios  #######
##########################################################

## set path to folder and specify username 
  pathdir <- "C:/Users/danie/Documents/Online for git/VME-advice"
  pathdir_nogit <- "C:/Users/danie/Documents/Online for git/VME-advice_noGIT" # stores the VMS and VME data from sharepoint
  ices_username <- "vandenderen"

  
## install libraries
  source(paste(pathdir,"Utilities/Libraries_VMEadvice.R",sep="/"))  
  
## download data sharepoint -- run only once
  options(icesSharePoint.username = ices_username)   # set ices username
  setspsite("/ExpertGroups/benchmarks/2022/WKVMEBM")
  spdir("2021_Meeting_Documents") # put password
  source("...")
  fnames <- spfiles("06. Data/Fisheries and benthic state", full = TRUE)
  for (fname in fnames) {
    spgetfile(fname, destdir = pathdir_nogit)
  }
  
  
  # double check that password is not stored on your computer, "Element not found" is okay 
  keyring::key_delete("icesSharePoint", ices_username)
  
## run the scenarios / options
  source(paste(pathdir,"Utilities/Scenario_1_option_1.R",sep="/"))  
  
  
  
  