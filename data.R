## Preprocess data, write TAF data tables

## Before:
## After:

rm(list=ls())

library(icesTAF)

mkdir("data")

# load required libraries, load utility functions

source("utilities.R")
source("utilities_libraries.R")

# set filenames, 
        elements_file <- "physical_VME_elements(fixed)"
vme_observations_file <- "VME_observations_datacall_2024_eu_29082024.csv"
    vme_csquares_file <- "VME_csquares_datacall_2022_eu.csv"

    
# load and process required data layers

sourceTAF("data_VME_observations.R")
sourceTAF("data_VMEelements.R")
sourceTAF("data_vmes.R")
sourceTAF("data_vme_csquares.R") # this is the combined VME observation and modelled habitat layer


