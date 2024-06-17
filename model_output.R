## Take the output of the scenario process and export

load("model/alt/vme_scenario_cquares.RData")
vme_records <- readRDS("data/vme_records.rds")
sar_layer <- readRDS("data/sar_layer.rds")
assessment_area <- readRDS("boot/data/eu_vme/assessment_area.rds")
fishing_footprint <- readRDS("boot/data/eu_vme/fishing_footprint.rds")
bathymetry <- readRDS("boot/data/eu_vme/bathymetry.rds")
source("utilities_libraries.R")
source("utilities.R")

#load csquare areas

scen.a <- alt2_scenario_outputs(vme.scenario.a.csquares, "Scenario_A", vme_records = vme_records, assessment_area = assessment_area, fishing_footprint = fishing_footprint, bathymetry = bathymetry)

scen.b <- alt2_scenario_outputs(vme.scenario.b.csquares, "Scenario_B", vme_records = vme_records, assessment_area = assessment_area, fishing_footprint = fishing_footprint, bathymetry = bathymetry)

scen.c <- alt2_scenario_outputs(vme.scenario.c.csquares, "Scenario_C", vme_records = vme_records, assessment_area = assessment_area, fishing_footprint = fishing_footprint, bathymetry = bathymetry)

scen.d <- alt2_scenario_outputs(vme.scenario.d.csquares, "Scenario_D", vme_records = vme_records, assessment_area = assessment_area, fishing_footprint = fishing_footprint, bathymetry = bathymetry)

scen.e <- alt2_scenario_outputs(vme.scenario.e.csquares, "Scenario_E", vme_records = vme_records, assessment_area = assessment_area, fishing_footprint = fishing_footprint, bathymetry = bathymetry)

save(list = c("vme.scenario.a.csquares", "vme.scenario.b.csquares", "vme.scenario.c.csquares",
              "vme.scenario.d.csquares", "vme.scenario.e.csquares"), file= "model/alt/vme_scenarios.RData")