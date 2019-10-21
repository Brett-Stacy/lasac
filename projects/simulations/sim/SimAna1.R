# analyzing output from sim scenarios
# All scenarios
# 14/10/19

library(gridExtra)
library(ggplot2)
library(egg)
library(dplyr)

nsim = 1000

scenario_names1 = c("con0.1", "con0.5", "con1.0", "con1.5",
                   "dec0.1", "dec0.5", "dec1.0", "dec1.5",
                   "inc0.1", "inc0.5", "inc1.0", "inc1.5")
scenario_names = c("Constant Under-report 10%", "Constant Under-report 50%", "Constant Under-report 100%", "Constant Under-report 150%",
                   "Decreasing Under-report 10%", "Decreasing Under-report 50%", "Decreasing Under-report 100%", "Decreasing Under-report 150%",
                   "Increasing Under-report 10%", "Increasing Under-report 50%", "Increasing Under-report 100%", "Increasing Under-report 150%")


out_paths = paste0("C:/Users/", Sys.info()["login"], "/Documents/GitHub/lasac/projects/simulations/sim/",  scenario_names1,  "/Output_Niter_", nsim,  "_Scenario_",  scenario_names1, ".RDS")


out = list()
for (i in 1:length(scenario_names)) {
  out[[scenario_names1[i]]] = readRDS(out_paths[i])
  out[[scenario_names1[i]]][["Scenario"]] = scenario_names[i]
}

## Inspect AM output for similarity
plot(out$con0.1$Est$SSB %>% colMeans, pch = "")
for (i in 1:length(scenario_names)){
  lines(out[[scenario_names1[i]]]$Est$SSB %>% colMeans)
}


years = 1990:2010

## Plots ----
SSB_plot = function(out){

  probs=c("lwr"=0.025, "mid"=0.5, "upr"=0.975)
  TRmid <- apply(out$True$SSB, 2, quantile, probs[["mid"]])/1000
  TRlwr <- apply(out$True$SSB, 2, quantile, probs[["lwr"]])/1000
  TRupr <- apply(out$True$SSB, 2, quantile, probs[["upr"]])/1000
  EMmid <- apply(out$Est$SSB, 2, quantile, probs[["mid"]])/1000
  EMlwr <- apply(out$Est$SSB, 2, quantile, probs[["lwr"]])/1000
  EMupr <- apply(out$Est$SSB, 2, quantile, probs[["upr"]])/1000

  df = data.frame(year = years, TRmid, EMmid)



  ggplot(data = df) +
    geom_line(aes(x = year, y = TRmid), color = "blue") +
    geom_ribbon(aes(x = year, ymin = TRlwr, ymax = TRupr), fill = "blue", alpha = .2) +
    geom_line(aes(x = year, y = EMmid), color = "red") +
    geom_ribbon(aes(x = year, ymin = EMlwr, ymax = EMupr), fill = "red", alpha = .2) +
    geom_hline(aes(yintercept = .5*TRmid[1]), linetype = "dashed") +
    geom_hline(aes(yintercept = .2*TRmid[1])) +
    theme_article() +
    ggtitle(out$Scenario) +
    {if(out$Scenario %in% scenario_names[c(5)]) scale_y_continuous(limits=c(0, 280),expand=c(0,0),name="SSB ('000 tonnes)")} +
    {if(out$Scenario %in% scenario_names[-c(5)]) scale_y_continuous(limits=c(0, 280),expand=c(0,0),name="")} +
    {if(out$Scenario %in% scenario_names[c(9:12)]) scale_x_continuous(limits=c(1990,2011), expand=c(0,0), name="Year")} +
    {if(out$Scenario %in% scenario_names[-c(9:12)]) scale_x_continuous(limits=c(1990,2011), expand=c(0,0), name="", labels = NULL)} +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(size = 10))
    #       axis.text.x = element_blank())+

    # scale_x_continuous(limits=c(1990,2011),expand=c(0,0),name='Year')
    # scale_y_continuous(limits=c(0, 280000),expand=c(0,0),name='SSB (tonnes)')
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


# dev.new()
# grid.arrange(grobs = allplots, ncol = 4)
grid.arrange(grobs = allplots, ncol = 4, bottom = "Year")






















