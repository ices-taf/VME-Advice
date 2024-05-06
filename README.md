# VME-Advice
ICES Advice on Vulnerable Marine Areas 

The code within this repository generates a range of scenarios for the protection of vulnerable marine ecosystems.

# Commercially sensitive data
Some of the data used in these scripts is commercially sensitive and restricted. Users should contact neil.campbell@ices.dk for access to restriced files.  
**Commercially sensitive data must never be uploaded to git**, and should be stored outside of the repository.  
In order to run the assessment, users should update the path to this file in `boot/eu_vme.R`, pointing to a secure directory on their local machine or network.

# Methodology

The VME assessment methodology is described in detail [here](http://doi.org/10.17895/ices.pub.20101637).

## data. R and data_*.R
These scripts prepare the input data for use in model.R and model_*.R

## model.R and model_*.R
Generate the csquares and polygons considered relevant for the protection of Vulnerable Marine Ecosystems according to scenarios A-E.

## output.R

## report.R


