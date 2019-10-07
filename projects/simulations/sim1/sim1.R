# Try simulating OM data with CASAL
# 4/10/19


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

## House ----
house = paste0("C:/Users/", Sys.info()["login"], "/Documents/GitHub/lasac")
setwd(house)



# re-read power points
# figure out how to run simulations from R, then read them in, then paste them into est.csl, then run casal, then extract SSB
## read in the simulated observations into R
casalest = casal::extract.csl.file(paste0("C:/Users/", Sys.info()["login"], "/Documents/GitHub/lasac/projects/simulations/sim1/","tempSIM.par1.sim1"))

casalest2 = casal::extract.csl.file(paste0("C:/Users/", Sys.info()["login"], "/Documents/GitHub/lasac/projects/simulations/sim1/","casal_estimation_skel.csl"))

casalest3 = append(casalest2, casalest)

casal::write.csl.file(casalest3, paste0("C:/Users/", Sys.info()["login"], "/Documents/GitHub/lasac/projects/simulations/sim1/","EMestimation.csl"))

# save/plot ssb
# paste the observations into the estimating model EMestimation.csl and EMpopulation.csl?
# run casal -e
# compare ssb true to ssb est. should be the same or similar.
# do it again but change catch in EMpopulation.csl to say half.
# compare ssb again.
