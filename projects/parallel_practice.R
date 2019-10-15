# practice parallel computing on lasac



library(doParallel)

cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)


time.start = Sys.time()
registerDoParallel(cores=2)
foreach(i=1:30000) %dopar% sqrt(i)
Sys.time() - time.start


time.start = Sys.time()
for(i in 1:30000) sqrt(i)
Sys.time() - time.start