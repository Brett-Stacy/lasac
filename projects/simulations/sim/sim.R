# simulating OM data with CASAL
# All scenarios
# 14/10/19



## Packages ----
library(casal)
library(lasac)
library(ggplot2)

## House ----
house = paste0("C:/Users/", Sys.info()["login"], "/Documents/GitHub/lasac")
casal_path = paste0(house, "/projects/simulations/sim/")
setwd(casal_path)
copy_path = paste0(house, "/projects/simulations/copy_house/")


## Scenarios ----
years = 1990:2010
LL1_catch     = 6000
LL1_catch_all = c(0, rep(6000, length(years)-1))

scenario = matrix(0, nrow = 12, ncol = length(years))
scenario[1,-1] = 0.1*LL1_catch
scenario[2,-1] = 0.5*LL1_catch
scenario[3,-1] = 1.0*LL1_catch
scenario[4,-1] = 1.5*LL1_catch
scenario[5,-1] = seq(from = 2*0.1*LL1_catch, to = 0, length.out = (years[length(years)]-years[1])) # decreasing...
scenario[6,-1] = seq(from = 2*0.5*LL1_catch, to = 0, length.out = (years[length(years)]-years[1]))
scenario[7,-1] = seq(from = 2*1.0*LL1_catch, to = 0, length.out = (years[length(years)]-years[1]))
scenario[8,-1] = seq(from = 2*1.5*LL1_catch, to = 0, length.out = (years[length(years)]-years[1]))
scenario[9,-1] = seq(from = 0, to = 2*0.1*LL1_catch, length.out = (years[length(years)]-years[1])) # increasing...
scenario[10,-1] = seq(from = 0, to = 2*0.5*LL1_catch, length.out = (years[length(years)]-years[1]))
scenario[11,-1] = seq(from = 0, to = 2*1.0*LL1_catch, length.out = (years[length(years)]-years[1]))
scenario[12,-1] = seq(from = 0, to = 2*1.5*LL1_catch, length.out = (years[length(years)]-years[1]))

scenario_names = c("con0.1", "con0.5", "con1.0", "con1.5",
                   "dec0.1", "dec0.5", "dec1.0", "dec1.5",
                   "inc0.1", "inc0.5", "inc1.0", "inc1.5")




### Grand Loop
for(j in 1:length(scenario_names)){
  ## Create directories with copy of necessary files (casal.exe, skel, etc.) ----
  dir.create(paste0(casal_path, scenario_names[j]))
  list.of.files = list.files(copy_path)
  list.of.paths = file.path(copy_path, list.of.files)
  file.copy(from = list.of.paths,
            to = paste0(casal_path, scenario_names[j]))
  sim_path = paste0(casal_path, scenario_names[j], "/")

  ## Load and modify pop.csl for LL2 catch scenarios ----
  OGcasalpop_skel = casal::extract.csl.file(paste0(sim_path,"casal_population_skel.csl"))

  OGcasalpop_skel$`fishery[LL2]`$command                   = "fishery"
  OGcasalpop_skel$`fishery[LL2]`$value                     = "LL2"
  OGcasalpop_skel$`fishery[LL2]`$years                     = OGcasalpop_skel$`fishery[LL1]`$years
  OGcasalpop_skel$`fishery[LL2]`$catches                   = scenario[j,]
  OGcasalpop_skel$`fishery[LL2]`$U_max                     = OGcasalpop_skel$`fishery[LL1]`$U_max
  OGcasalpop_skel$`fishery[LL2]`$selectivity               = OGcasalpop_skel$`fishery[LL1]`$selectivity
  OGcasalpop_skel$`fishery[LL2]`$future_constant_catches   = OGcasalpop_skel$`fishery[LL1]`$future_constant_catches

  casal::write.csl.file(OGcasalpop_skel, paste0(sim_path,"casal_population.csl"))
  casal::write.csl.file(OGcasalpop_skel, paste0(sim_path,"TRpopulation.csl")) # want TRue pop.csl to match casal_pop.csl so we are estimating using the same file as used to simulate observations


  ## OK now for real ----
  # Iterate, over-write, extract

  ## must set wd because casal automatically writes things like true.dat to the WD, not where casal is ran from
  setwd(sim_path)

  ## Generate true.dat to use in simulations
  shell(paste0(sim_path, "casal -e -q -O true.dat -f casal_"), intern = T)


  # Extract stuff from true output

  nsim = 1000


  out = list()
  out$True = list(SSB = matrix(NA, ncol = length(years), nrow = nsim), year = years)
  out$Est  = list(SSB = matrix(NA, ncol = length(years), nrow = nsim), year = out$True$year)
  TRcasalest_skel = casal::extract.csl.file(paste0(sim_path,"TRestimation_skel.csl"))
  EMcasalest_skel = casal::extract.csl.file(paste0(sim_path,"EMestimation_skel.csl"))



  for (i in 1:nsim) {
    ## run one simulation using true.dat
    shell(paste0(sim_path, "casal -s 1 tempSIM -i true.dat -f casal_"), intern = T)

    ## read in the simulated observations into R
    casalest_sim = casal::extract.csl.file(paste0(sim_path,"tempSIM.par1"))

    ## append them to the True estimation skeleton, these are the same data files as used to generate true.dat, but replacing the observations
    TRcasalest_full = append(TRcasalest_skel, casalest_sim)

    ## overwrite the TRestimation file
    casal::write.csl.file(TRcasalest_full, paste0(sim_path,"TRestimation.csl"))


    ## run casal point estimate on using the updated estimation file and save in the temporary TRoutput.log
    shell(paste0(sim_path, "casal -e -q -f TR > TRoutput.log"), intern = T)

    ## extract SSB from the log and append it to the list
    out$True$SSB[i,] = casal::extract.quantities("TRoutput.log", path=sim_path)$SSBs$SSB




    ## append them to the skeleton
    casalest_full = append(EMcasalest_skel, casalest_sim)

    ## overwrite the EMestimation file
    casal::write.csl.file(casalest_full, paste0(sim_path,"EMestimation.csl"))


    ## run casal point estimate on using the updated estimation file and save in the temporary EMoutput.log
    shell(paste0(sim_path, "casal -e -q -f EM > EMoutput.log"), intern = T)


    ## extract SSB from the log and append it to the list
    out$Est$SSB[i,] = casal::extract.quantities("EMoutput.log", path=sim_path)$SSBs$SSB

  } # end of iteration loop
  # save output
  saveRDS(out, file=paste0(sim_path, "Output_Niter_", nsim,  "_Scenario_",  scenario_names[j], ".RDS"))
} # end of scenario loop




## Plots ----

plot(out$True$year, colMeans(out$True$SSB), col = "blue")
for (i in 1:nsim) {
  lines(out$Est$year, out$Est$SSB[i,])
}
points(out$True$year, colMeans(out$True$SSB), col = "blue", pch = 19)


plot(out$True$year, colMeans(out$True$SSB), type = "l", col = "blue")
for (i in 1:nsim) {
  lines(out$True$year, out$True$SSB[i,], col = "blue")
  lines(out$Est$year, out$Est$SSB[i,])
}





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
  scale_y_continuous(limits=c(0,1.1*max(TRupr)),expand=c(0,0),name='Spawning Stock Biomass')


p4 = ggplot() +
  geom_line(data = under_SSB, aes(x = year, y = SSB_Virgin*100)) +
  geom_ribbon(data = under_SSB, aes(x = year, ymin=SSB_Virgin_lower*100,ymax=SSB_Virgin_upper*100),
              fill='gray', alpha = 1, color = "black", linetype = 1) +
  geom_ribbon(data = no_under_SSB, aes(x = year, ymin = SSB_Virgin_lower*100, ymax = SSB_Virgin_upper*100),
              fill = "red", alpha = .25, color = "red", linetype = 2) +
  geom_point(data = under_SSB, aes(x = year, y = SSB_Virgin*100)) +
  geom_point(data = no_under_SSB, aes(x = year, y = SSB_Virgin*100), color = "red", shape = 2) +
  geom_hline(aes(yintercept=30), color = "black") +
  geom_vline(aes(xintercept=2024), color='red') +
  geom_point(aes(x = 5, y = 5, color = "black")) +
  geom_point(aes(x = 6, y = 5, color = "red")) +
  theme_bw() +
  theme(legend.position = c(.75, .75),legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) +
  scale_colour_manual(name = "% TAC Undercatch", values = c("black"="black", "red"="red"), labels = c("5%", "0%")) +
  guides(color = guide_legend(override.aes = list(shape = c(16, 2), fill = c("black", "red"))))  +
  scale_x_continuous(limits=c(1990,2027),expand=c(0,0),name='Year') +
  scale_y_continuous(limits=c(0,100),expand=c(0,0),name='Spawning Stock Biomass (% Virgin)') +
  ggtitle(label = "")


