## Take the output of the scenario process and export

load("model/vme_scenario_cquares.RData")
vme_records <- readRDS("data/vme_records.rds")
sar_layer <- readRDS("boot/data/eu_vme/sar_layer.rds")
assessment_area <- readRDS("boot/data/eu_vme/assessment_area.rds")
fishing_footprint <- readRDS("boot/data/eu_vme/fishing_footprint.rds")
bathymetry <- readRDS("boot/data/eu_vme/bathymetry.rds")

scen.a <- scenario_outputs(vme.scenario.a.csquares, "Scenario_A", vme_records, assessment_area, fishing_footprint, bathymetry)

scen.b <- scenario_outputs(vme.scenario.b.csquares, "Scenario_B", vme_records, assessment_area, fishing_footprint, bathymetry)

scen.c <- scenario_outputs(vme.scenario.c.csquares, "Scenario_C", vme_records, assessment_area, fishing_footprint, bathymetry)

scen.d <- scenario_outputs(vme.scenario.d.csquares, "Scenario_D", vme_records, assessment_area, fishing_footprint, bathymetry)

scen.e <- scenario_outputs(vme.scenario.e.csquares, "Scenario_E", vme_records, assessment_area, fishing_footprint, bathymetry)

save(list = c("vme.scenario.a.csquares", "vme.scenario.b.csquares", "vme.scenario.c.csquares",
              "vme.scenario.d.csquares", "vme.scenario.e.csquares"), file= "model/vme_scenarios.RData")