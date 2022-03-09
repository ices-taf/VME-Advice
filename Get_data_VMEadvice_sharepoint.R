
################################################################################
# The below script allows you to link to the ICES VME benchmark  
# sharepoint data folder. Access is restricted to 
# WK participants that have signed the terms of use document
#
# The below script will automatically create a folder structure and 
# download the fisheries and VME data. 
#
#################################################################################

## set path to your github folder 
  pathdir <- "C:/Users/danie/Documents/Online for git/VME-advice"

## set path to new data folder (outside github) and create the folder
  pathdir_nogit <- "C:/Users/danie/Documents/Online for git/VME-advice_noGIT"
  dir.create(pathdir_nogit)   # create a folder 

## set ices username
  ices_username <- "vandenderen"

## install and load ices sharepoint library
#remotes::install_github("ices-tools-prod/icesSharePoint")
  library(icesSharePoint)

## download data sharepoint -- run only once
  options(icesSharePoint.username = ices_username)
  setspsite("/ExpertGroups/benchmarks/2022/WKVMEBM")
  fnames <- spdir("2021_Meeting_Documents/06. Data/Data (restricted access)/VME data repository/VME observations and csquares") # put password
  
  dir.create(paste(pathdir_nogit,"VME data repository",sep="/"))   # create VME folder
  dir.create(paste(pathdir_nogit,"VME data repository/VME observations and csquares",sep="/")) # create VME data folder
  dir.create(paste(pathdir_nogit,"VME data repository/VME elements",sep="/")) # create VME element folder
  dir.create(paste(pathdir_nogit,"VMS data repository",sep="/"))   # create VMS fishing folder

## load VME data
  pathVME <- paste(pathdir_nogit,"VME data repository/VME observations and csquares",sep="/") 
  fnames  <- spfiles("2021_Meeting_Documents/06. Data/Data (restricted access)/VME data repository/VME observations and csquares", full = TRUE)
  for (fname in fnames) {
     spgetfile(fname, destdir = pathVME)
  }

## load VME element data
  pathVME_elements <- paste(pathdir_nogit,"VME data repository/VME elements",sep="/")
  fnames <- spfiles("2021_Meeting_Documents/06. Data/Data (restricted access)/VME data repository/VME elements", full = TRUE)
  for (fname in fnames) {
    spgetfile(fname, destdir = pathVME_elements)
  }

## load VMS data
pathVMS <- paste(pathdir_nogit,"VMS data repository",sep="/")
fnames <- spfiles("2021_Meeting_Documents/06. Data/Data (restricted access)/VMS data repository", full = TRUE)
for (fname in fnames) {
  spgetfile(fname, destdir = pathVMS)
}

# delete username storage 
keyring::key_delete("icesSharePoint", ices_username)

# double check that password is not stored on your computer, "Element not found" is okay 
keyring::key_delete("icesSharePoint", ices_username)

# done
