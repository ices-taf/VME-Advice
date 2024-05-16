## Run analysis, write model results

## Before:
## After:

library(icesTAF)
source("utilities.R")
source("utilities_libraries.R")

mkdir("model")
mkdir("model/alt")

sourceTAF("model_scenarios.R")
sourceTAF("model_output.R")
