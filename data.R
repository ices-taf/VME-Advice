## Preprocess data, write TAF data tables

## Before:
## After:

rm(list=ls())

library(icesTAF)

mkdir("data")

# set parameters and filenames, load required libraries, load utility functions

source("data_libraries.R")
source("utilities.R")

# load and process required data layers

source("data_VMEelements.R")
source("data_vmes.R")
source("data_vme_csquares.R") # this is the combined VME observation and modelled habitat layer
source("../../VME-advice_noGIT/data_other_layers.R") # contact neil.campbell@ices.dk for access

