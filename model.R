## Run analysis, write model results

## Before:
## After:

library(icesTAF)
source("utilities.R")
source("utilities_libraries.R")

mkdir("model")
mkdir(paste0("model/", Sys.Date()))

sourceTAF("model_scenarios.R")
sourceTAF("model_output.R")
