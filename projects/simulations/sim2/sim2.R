# Try simulating OM data with CASAL
# 10/10/19


# Generate a free parameter file (e.g. the MPD)
# casal -e -q -O true.dat -f casal_

# Simulate 5 (or in a real experiment, many more) observations
# casal -s 5 tempSIM -i true.dat

# run casal on simulated
# casal -e -q -f EM > EMoutput.log


# The point is to get a baseline with correct simulated observations from correct catch. Then estimate it for a baseline. Check this baseline SSB against the original csl file -e. Just do that one time, but simulate 1000 times
# Then simulate data when there is IUU catch, then estimate it using the observations, but still with the original assumed catch. Extract SSB and check it against the baseline



## Packages ----
library(casal)
library(lasac)
library(ggplot2)

## House ----
house = paste0("C:/Users/", Sys.info()["login"], "/Documents/GitHub/lasac")
casal_path = paste0(house, "/projects/simulations/sim2/")
setwd(casal_path)


# ## Raw ----
# # Simulate stuff
# shell(paste0(casal_path, "casal -s 1 tempSIM -i true.dat -f casal_"))
#
#
# # re-read power points
# # figure out how to run simulations from R, then read them in, then paste them into est.csl, then run casal, then extract SSB
# ## read in the simulated observations into R
# casalest = casal::extract.csl.file(paste0("C:/Users/", Sys.info()["login"], "/Documents/GitHub/lasac/projects/simulations/sim1/","tempSIM.par1.sim1"))
#
# casalest2 = casal::extract.csl.file(paste0("C:/Users/", Sys.info()["login"], "/Documents/GitHub/lasac/projects/simulations/sim1/","casal_estimation_skel.csl"))
#
# casalest3 = append(casalest2, casalest)
#
# casal::write.csl.file(casalest3, paste0("C:/Users/", Sys.info()["login"], "/Documents/GitHub/lasac/projects/simulations/sim1/","EMestimation.csl"))
#
# # save/plot ssb
# # paste the observations into the estimating model EMestimation.csl and EMpopulation.csl?
# # run casal -e
# # compare ssb true to ssb est. should be the same or similar.
# # do it again but change catch in EMpopulation.csl to say half.
# # compare ssb again.
#
#
#
#
#
# # Extract stuff from true output
# out = list()
# out$True = casal::extract.quantities("TRoutput.log", path=casal_path)
# out$Est  = casal::extract.quantities("EMoutput.log", path=casal_path)
#
# plot(out$True$SSBs$year, out$True$SSBs$SSB)
# lines(out$Est$SSBs$year, out$Est$SSBs$SSB)
#
#
#
#

## OK now for real ----
# Iterate, over-write, extract
years = 1990:2010

# shell(paste0(casal_path, "casal -e -q -O true.dat -f casal_"))


# Extract stuff from true output

nsim = 50


out = list()
out$True = list(SSB = matrix(NA, ncol = length(years), nrow = nsim), year = years)
out$Est  = list(SSB = matrix(NA, ncol = length(years), nrow = nsim), year = out$True$year)
TRcasalest_skel = casal::extract.csl.file(paste0(casal_path,"TRestimation_skel.csl"))
EMcasalest_skel = casal::extract.csl.file(paste0(casal_path,"EMestimation_skel.csl"))



for (i in 1:nsim) {
  ## run one simulation using true.dat
  shell(paste0(casal_path, "casal -s 1 tempSIM -i true.dat -f casal_"))

  ## read in the simulated observations into R
  casalest_sim = casal::extract.csl.file(paste0(casal_path,"tempSIM.par1"))

  ## append them to the skeleton1, these are the same data files as used to generate true.dat, but replacing the observations
  TRcasalest_full = append(TRcasalest_skel, casalest_sim)

  ## overwrite the TRestimation file
  casal::write.csl.file(TRcasalest_full, paste0(casal_path,"TRestimation.csl"))


  ## run casal point estimate on using the updated estimation file and save in the temporary TRoutput.log
  shell(paste0(casal_path, "casal -e -q -f TR > TRoutput.log"))

  ## extract SSB from the log and append it to the list
  out$True$SSB[i,] = casal::extract.quantities("TRoutput.log", path=casal_path)$SSBs$SSB




  ## append them to the skeleton
  casalest_full = append(EMcasalest_skel, casalest_sim)

  ## overwrite the EMestimation file
  casal::write.csl.file(casalest_full, paste0(casal_path,"EMestimation.csl"))


  ## run casal point estimate on using the updated estimation file and save in the temporary EMoutput.log
  shell(paste0(casal_path, "casal -e -q -f EM > EMoutput.log"))


  ## extract SSB from the log and append it to the list
  out$Est$SSB[i,] = casal::extract.quantities("EMoutput.log", path=casal_path)$SSBs$SSB

}


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


