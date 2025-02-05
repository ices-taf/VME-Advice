## Preprocess data, write TAF data tables

## Before:
## After:

rm(list=ls())

library(icesTAF)

mkdir("data")

# set parameters and filenames, load required libraries, load utility functions

source("utilities.R")
source("utilities_libraries.R")

# load and process required data layers


sourceTAF("data_parameters.R")
sourceTAF("data_vms.R")
sourceTAF("data_vme_elements.R")
sourceTAF("data_vme.R")
sourceTAF("data_vme_csquares.R") # this is the combined VME observation and modelled habitat layer


