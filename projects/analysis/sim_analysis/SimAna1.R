# analyzing output from sim scenarios
# All scenarios
# 14/10/19

library(gridExtra)

nsim = 1000

scenario_names = c("con0.1", "con0.5", "con1.0", "con1.5",
                   "dec0.1", "dec0.5", "dec1.0", "dec1.5",
                   "inc0.1", "inc0.5", "inc1.0", "inc1.5")

sim_paths = paste0("C:/Users/bstacy/Documents/GitHub/lasac/projects/simulations/sim/",  scenario_names,  "/Output_Niter_", nsim,  "_Scenario_",  scenario_names, ".RDS")


out = list()
for (i in 1:length(scenario_names)) {
  out[[scenario_names[i]]] = readRDS(sim_paths[i])
}




## Plots----
SSB_plot = function(out){

  probs=c("lwr"=0.025, "mid"=0.5, "upr"=0.975)
  TRmid <- apply(out$True$SSB, 2, quantile, probs[["mid"]])
  TRlwr <- apply(out$True$SSB, 2, quantile, probs[["lwr"]])
  TRupr <- apply(out$True$SSB, 2, quantile, probs[["upr"]])
  EMmid <- apply(out$Est$SSB, 2, quantile, probs[["mid"]])
  EMlwr <- apply(out$Est$SSB, 2, quantile, probs[["lwr"]])
  EMupr <- apply(out$Est$SSB, 2, quantile, probs[["upr"]])

  df = data.frame(year = years, TRmid, EMmid)



  ggplot(data = df) +
    geom_line(aes(x = year, y = TRmid), color = "blue") +
    geom_ribbon(aes(x = year, ymin = TRlwr, ymax = TRupr), fill = "blue", alpha = .2) +
    geom_line(aes(x = year, y = EMmid), color = "red") +
    geom_ribbon(aes(x = year, ymin = EMlwr, ymax = EMupr), fill = "red", alpha = .2) +
    geom_hline(aes(yintercept = .5*TRmid[1])) +
    geom_hline(aes(yintercept = .2*TRmid[1])) +
    theme_bw() +
    scale_x_continuous(limits=c(1990,2011),expand=c(0,0),name='Year') +
    scale_y_continuous(limits=c(0, 270000),expand=c(0,0),name='SSB (tonnes)')
    # scale_y_continuous(limits=c(0,1.1*max(TRupr)),expand=c(0,0),name='SSB (tonnes)')

}
# SSB_plot(out$con0.1)
# SSB_plot(out$con0.5)
# SSB_plot(out$con1.0)
# SSB_plot(out$con1.5)
# SSB_plot(out$dec0.1)
# SSB_plot(out$dec0.5)
# SSB_plot(out$dec1.0)
# SSB_plot(out$dec1.5)
# SSB_plot(out$inc0.1)
# SSB_plot(out$inc0.5)
# SSB_plot(out$inc1.0)
# SSB_plot(out$inc1.5)



allplots = lapply(out, SSB_plot)



grid.arrange(grobs = allplots, ncol = 4)

