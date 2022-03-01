
##########################################################
###### script to run VME advice scenarios  #######
##########################################################

## set path to folder and specify username 
  pathdir <- "C:/Users/danie/Documents/Online for git/VME-advice"
  pathdir_nogit <- "C:/Users/danie/Documents/Online for git/VME-advice_noGIT" # stores the VMS and VME data from sharepoint
  ices_username <- "vandenderen"

  
## install libraries
  source(paste(pathdir,"Utilities/Libraries_VMEadvice.R",sep="/"))  
  
## download data sharepoint 
  options(icesSharePoint.username = ices_username)   # set ices username
  options(icesSharePoint.site = "/xxxx/")  # set the site 
  spdir() # put password
  
  # double check that password is not stored on your computer, "Element not found" is okay 
  keyring::key_delete("icesSharePoint", ices_username)
  
## run the scenarios / options
  source(paste(pathdir,"Utilities/Scenario_1_option_1.R",sep="/"))  
  
  
  
  