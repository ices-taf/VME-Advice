## Take the output of the scenario process and export

load("model/vme_scenario_cquares.RData")

scen.a <- scenario_outputs(vme.scenario.a.csquares, "Scenario_A", vme_records)

scen.b <- scenario_outputs(vme.scenario.b.csquares, "Scenario_B", vme_records)

scen.c <- scenario_outputs(vme.scenario.c.csquares, "Scenario_C", vme_records)

scen.d <- scenario_outputs(vme.scenario.d.csquares, "Scenario_D", vme_records)

scen.e <- scenario_outputs(vme.scenario.e.csquares, "Scenario_E", vme_records)

save(list = c("vme.scenario.a.csquares", "vme.scenario.b.csquares", "vme.scenario.c.csquares",
              "vme.scenario.d.csquares", "vme.scenario.e.csquares"), file= "model/vme_scenarios.RData")