# tests

rm(list=ls())

# Example other plot - Plot 57

# Example Paulo 1


# Plot 60 - it works
plot.t0<-read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P60_M1.csv')
plot.t1<-read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P60_M2.csv')

p.t0<-input_module(type='tree',zone=4,AD=38.1,HD=23.13,SI=NA,area=500,AF=44.1,tree.list=plot.t0,V_model=1,ddiam=FALSE)
p.t1<-input_module(type='tree',zone=4,AD=38.1,HD=23.13,SI=NA,area=500,AF=44.1,tree.list=plot.t1,V_model=1,ddiam=FALSE)

# Plot 57 - it works
plot.t0<-read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P57_M1.csv')
plot.t1<-read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P57_M2.csv')

p.t0<-input_module(type='tree',zone=2,AD=24.5,HD=18.46,SI=NA,area=500,AF=30.5,tree.list=plot.t0,V_model=1,ddiam=FALSE)
p.t1<-input_module(type='tree',zone=2,AD=24.5,HD=18.46,SI=NA,area=500,AF=30.5,tree.list=plot.t1,V_model=1,ddiam=FALSE)

# Plot 125
plot.t0<-read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P125_M1.csv')
plot.t1<-read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P125_M2.csv')

p.t0<-input_module(type='tree',zone=4,AD=31.6,HD=27.7,SI=NA,area=250,AF=37.6,tree.list=plot.t0,V_model=1,ddiam=FALSE)
p.t1<-input_module(type='tree',zone=4,AD=31.6,HD=27.7,SI=NA,area=250,AF=37.6,tree.list=plot.t1,V_model=1,ddiam=FALSE)

# Plot 124
plot.t0<-read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P124_M1.csv')
plot.t1<-read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P124_M2.csv')

p.t0<-input_module(type='tree',zone=2,AD=20.7,HD=17.65,SI=NA,area=250,AF=26.7,tree.list=plot.t0,V_model=1,ddiam=FALSE)
p.t1<-input_module(type='tree',zone=2,AD=20.7,HD=17.65,SI=NA,area=250,AF=26.7,tree.list=plot.t1,V_model=1,ddiam=FALSE)

# Plot 41 - it works
plot.t0<-read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P41_M1.csv')
plot.t1<-read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P41_M2.csv')

p.t0<-input_module(type='tree',zone=2,AD=51.85,HD=30,SI=NA,area=500,AF=57.85,tree.list=plot.t0,V_model=1,ddiam=FALSE)
p.t1<-input_module(type='tree',zone=2,AD=51.85,HD=30,SI=NA,area=500,AF=57.85,tree.list=plot.t1,V_model=1,ddiam=FALSE)

# Plot 43 - it works
plot.t0<-read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P43_M1.csv')
plot.t1<-read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P43_M2.csv')

p.t0<-input_module(type='tree',zone=2,AD=39.3,HD=24,SI=NA,area=500,AF=45.3,tree.list=plot.t0,V_model=2,ddiam=FALSE)
p.t1<-input_module(type='tree',zone=2,AD=39.3,HD=24,SI=NA,area=500,AF=45.3,tree.list=plot.t1,V_model=2,ddiam=FALSE)


###########################

obs.t0<-core_module(input=p.t0$input)
obs.t1<-core_module(input=p.t1$input)
obs.t0$sp.table
obs.t1$sp.table
#obs.t0$stand.table[2,,]
#obs.t1$stand.table[2,,]

# tree simulation to t1
obs.t0$type<-'tree'
sim.tree.t1<-tree_simulator(core.tree=obs.t0$input)
head(sim.tree.t1$input$tree.list)
est.tree.t1<-core_module(input=sim.tree.t1$input)
est.tree.t1$sp.table
est.tree.t1$stand.table[2,,]

# stand simulation to t1
obs.t0$type<-'stand'
obs.t0$ddiam<-TRUE
sim.stand.t1<-stand_simulator(core.stand=obs.t0$input)
head(sim.stand.t1$input$tree.list)
sim.stand.t1$input$sp.table
sim.stand.t1$input$sim.stand

#sim.stand.t1$input$sp.table
sim.stand.t1$input$type<-'stand'  
sim.stand.t1$input$ddiam<-TRUE    
sim.stand.t1$input$V_module<-2     
est.stand.t1<-core_module(input=sim.stand.t1$input)
est.stand.t1$sp.table
est.stand.t1$stand.table[2,,]


obs.t1$sp.table
est.tree.t1$sp.table
est.stand.t1$sp.table


# Plotting a nice barplot
blue <- rgb(0, 0, 1, alpha=0.2)
red <- rgb(1, 0, 0, alpha=0.2)
green <- rgb(0, 1, 0, alpha=0.2)
barplot(est.stand.t1$stand.table[2,,7], space=0, col=red)
barplot(est.tree.t1$stand.table[2,,7], space=0, col=green, add=TRUE)
barplot(obs.t1$stand.table[2,,7], space=0, col=blue, add=TRUE)

