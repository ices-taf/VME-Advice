# Install and load required packages using pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, rgdal, rgeos, maptools, leaflet, htmlwidgets, smoothr, 
raster, tmap, dplyr, tidyr, knitr, kableExtra, htmlTable, glue, icesTAF,
data.table, nngeo, vmstools, icesVMS)
