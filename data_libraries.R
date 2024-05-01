# Install and load required packages using pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, raster, dplyr, purrr, tidyr, nngeo, icesTAF, icesVMS, vmstools)