## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)

mkdir("data")

# set parameters and filenames, load required libraries, load utility functions
source("data_parameters.R")
source("data_libraries.R")
source("data_utilities.R")

# load and process required data layers
source("data_VMEelements.R")
source("data_vmes.R")
source("data_vme_csquares.R") # this is the combined VME observation and modelled habitat layer
source("data_other_layers.R") # contact neil.campbell@ices.dk for access

