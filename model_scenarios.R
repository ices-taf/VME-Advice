## Run the various VME scenarios, based on the loaded data, using the scenario functions

vme_data     <- readRDS("data/vme_data.rds")
vme_elements_csquares <- readRDS("data/vme_elements.rds")
vme_elements_raw <- readRDS("boot/data/eu_vme/vme_elements_raw.rds")
vme_records <- readRDS("data/vme_records.rds")
sar_layer <- readRDS("boot/data/eu_vme/sar_layer.rds")


## Scenario A
vme.scenario.a.csquares <- vme_scenario_A(vme_data)

## Scenario B
vme.scenario.b.csquares <- vme_scenario_B(vme_data, vme_records, vme_elements_raw = vme_elements_raw, vme_elements_csquares = vme_elements_csquares, scenario_A_csquares = vme.scenario.a.csquares)

## Scenario C
vme.scenario.c.csquares <- vme_scenario_C(vme_data, sar_layer, SAR_threshold = 0.43)

## Scenario D
vme.scenario.d.csquares <- vme_scenario_D(vme_data, sar_layer, 0.43)

## Scenario E
vme.scenario.e.csquares <- vme_scenario_E(scenario_B_csquares = vme.scenario.b.csquares, scenario_C_csquares = vme.scenario.c.csquares)

save(list = c("vme.scenario.a.csquares", "vme.scenario.b.csquares", "vme.scenario.c.csquares",
             "vme.scenario.d.csquares", "vme.scenario.e.csquares"), file= "model/alt/vme_scenario_cquares.RData")
