#Example 1. Starting from known stand-level data
BAest<-BAmodule(AD0=41, HD0=33.3, N0=1920, model=2, projection=FALSE)
BAest$BA0
sims <- stand_simulator(dom_sp=3, zone=2, AD0=41, ADF=51, HD0=33.3, BA0=54.65, N0=1920, Nmodel=1, BAmodel=2, PropNN=NA)
sims
plot_results(sims)


BAest<-BAmodule(AD0=41, HD0=39, N0=1020, model=1, projection=FALSE)
BAest$BA0
sims <- stand_simulator(dom_sp=1, zone=2, AD0=41, ADF=81, HD0=39, BA0=43.45, N0=1020, Nmodel=1, BAmodel=1, PropNN=NA)
sims
plot_results(sims)

sims <- stand_simulator(dom_sp=1, zone=2, AD0=41, ADF=51, HD0=30.3, BA0=38.3, N0=1860, Nmodel=1, BAmodel=1, PropNN=NA)
sims
plot_results(sims)


