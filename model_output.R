## Take the output of the scenario process and export

load("vme_scenarios.RData")

scen.a <- scenario_outputs(vme.scenario.a.csquares, "Scenario_A", vme_records)

scen.b <- scenario_outputs(vme.scenario.b.csquares, "Scenario_B", vme_records)

scen.c <- scenario_outputs(vme.scenario.c.csquares, "Scenario_C", vme_records)

scen.d <- scenario_outputs(vme.scenario.d.csquares, "Scenario_D", vme_records)

scen.e <- scenario_outputs(vme.scenario.e.csquares, "Scenario_E", vme_records)

