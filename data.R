## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)

mkdir("data")

# set parameters and filenames, load required libraries, load utility functions
source("data_parameters.R")
source("data_libraries.R")
source("data_utilities.R")
source("data_VMEelements.R")


source(paste(pathdir,"Utilities/Scenario_1_option_1.R",sep="/"))  

source(paste(pathdir,"Utilities/ADGVME2022_Scenario_1_option_2.R",sep="/")) 
