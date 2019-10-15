# Try some parallelizing
# 15/10/19





## Packages ----
library(casal)
library(lasac)
library(ggplot2)
library(doParallel)

## House ----
house = paste0("C:/Users/", Sys.info()["login"], "/Documents/GitHub/lasac")
casal_path = paste0(house, "/projects/simulations/sim3/")
setwd(casal_path)


## OK now for real ----
# Iterate, over-write, extract
years = 1990:2010

# shell(paste0(casal_path, "casal -e -q -O true.dat -f casal_"))


# Extract stuff from true output

nsim = 10


out = list()
out$True = list(SSB = matrix(NA, ncol = length(years), nrow = nsim), year = years)
out$Est  = list(SSB = matrix(NA, ncol = length(years), nrow = nsim), year = out$True$year)
TRcasalest_skel = casal::extract.csl.file(paste0(casal_path,"TRestimation_skel.csl"))
EMcasalest_skel = casal::extract.csl.file(paste0(casal_path,"EMestimation_skel.csl"))


# without parallel ----
time.start = Sys.time()
for (i in 1:nsim) {
  ## run one simulation using true.dat
  shell(paste0(casal_path, "casal -s 1 tempSIM -i true.dat -f casal_"), intern = T)

  ## read in the simulated observations into R
  casalest_sim = casal::extract.csl.file(paste0(casal_path,"tempSIM.par1"))

  ## append them to the skeleton1, these are the same data files as used to generate true.dat, but replacing the observations
  TRcasalest_full = append(TRcasalest_skel, casalest_sim)

  ## overwrite the TRestimation file
  casal::write.csl.file(TRcasalest_full, paste0(casal_path,"TRestimation.csl"))


  ## run casal point estimate on using the updated estimation file and save in the temporary TRoutput.log
  shell(paste0(casal_path, "casal -e -q -f TR > TRoutput.log"), intern = T)

  ## extract SSB from the log and append it to the list
  out$True$SSB[i,] = casal::extract.quantities("TRoutput.log", path=casal_path)$SSBs$SSB




  ## append them to the skeleton
  casalest_full = append(EMcasalest_skel, casalest_sim)

  ## overwrite the EMestimation file
  casal::write.csl.file(casalest_full, paste0(casal_path,"EMestimation.csl"))


  ## run casal point estimate on using the updated estimation file and save in the temporary EMoutput.log
  shell(paste0(casal_path, "casal -e -q -f EM > EMoutput.log"), intern = T)


  ## extract SSB from the log and append it to the list
  out$Est$SSB[i,] = casal::extract.quantities("EMoutput.log", path=casal_path)$SSBs$SSB

}
Sys.time() - time.start

# with parallel ----
time.start = Sys.time()
registerDoParallel(cores=6)
# foreach(i=1:30000) %dopar% sqrt(i)
foreach(i=1:nsim) %dopar% {
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
Sys.time() - time.start


## Plots ----

plot(out$True$year, colMeans(out$True$SSB), type = "l", col = "blue")
for (i in 1:nsim) {
  lines(out$True$year, out$True$SSB[i,], col = "blue")
  lines(out$Est$year, out$Est$SSB[i,])
}



