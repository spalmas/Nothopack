# tests

rm(list=ls())

# Example other plot - Plot 57

# Example Paulo 1
plot.t0<-read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P60_M1.csv')
plot.t1<-read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P60_M2.csv')

p.t0<-input_module(type='tree',zone=4,AD=38.1,HD=23.13,SI=NA,area=500,AF=44.1,tree.list=plot.t0,V_model=1,ddiam=FALSE)
p.t1<-input_module(type='tree',zone=4,AD=38.1,HD=23.13,SI=NA,area=500,AF=44.1,tree.list=plot.t1,V_model=1,ddiam=FALSE)

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

