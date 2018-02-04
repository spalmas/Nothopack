# tests

plot<- read.csv(file= 'C:/MASTER UFL/research/Gezan/Modelling/INFO Nothofagus 2/Research Palmas/UACH_P41_M1.csv')
head(plot)
tree<-inputmodule(type='tree',zone=2,AD=51.85,HD=30.0,SI=NA,area=500,AF=57.85,tree.list=plot,V_model=1)
head(tree$tree.list)
core.tree<-core_module(input=tree$input)
core.tree$sp.table
core.tree$type<-"tree"
stand<-stand_simulator(core.stand = core.tree)
results.stand<-core_module(input = stand$input)
results.stand$sp.table
report(core.stand=results.stand)
