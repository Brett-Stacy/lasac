# analyzing output from sim scenarios
# All scenarios
# 14/10/19



scenario_names = c("con0.1", "con0.5", "con1.0", "con1.5",
                   "dec0.1", "dec0.5", "dec1.0", "dec1.5",
                   "inc0.1", "inc0.5", "inc1.0", "inc1.5")

sim_paths = paste0("C:/Users/bstacy/Documents/GitHub/lasac/projects/simulations/sim/",  scenario_names,  "/Output_Niter_", nsim,  "_Scenario_",  scenario_names, ".RDS")


out = list()
for (i in 1:length(scenario_names)) {
  out[scenario_names[i]] = readRDS(sim_paths[i])
}

