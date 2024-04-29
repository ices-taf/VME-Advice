## Run the various VME scenarios, based on the loaded data, using the scenario functions

names(vme_elements)[3] <- "csquares"

## Scenario A
vme.scenario.a.csquares <- vme_scenario_A(vme_data)

## Scenario B
vme.scenario.b.csquares <- vme_scenario_B(vme_data, vme_elements)

## Scenario C
vme.scenario.c.csquares <- vme_scenario_C(vme_data, sar_layer, 0.43)

## Scenario D
vme.scenario.d.csquares <- vme_scenario_D(vme_data, sar_layer, 0.43)

## Scenario E
vme.scenario.e.csquares <- vme_scenario_E(vme_data, vme_elements, sar_layer, 0.43)

save(list = c("vme.scenario.a.csquares", "vme.scenario.b.csquares", "vme.scenario.c.csquares",
             "vme.scenario.d.csquares", "vme.scenario.e.csquares"), file= "vme_scenarios.RData")
