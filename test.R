# tests

<<<<<<< HEAD
plot<- read.csv(file= 'C:/MASTER UFL/research/Gezan/Modelling/INFO Nothofagus 2/Research Palmas/UACH_P41_M1.csv')
head(plot)
tree<-inputmodule(type='tree',zone=2,AD=51.85,HD=30.0,SI=NA,area=500,AF=57.85,tree.list=plot,V_model=1)
=======
rm(list=ls())

setwd('D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation')
plot<- read.csv(file='UACH_P41_M1.csv')
plot<- read.csv(file='UACH_P41_M2.csv')

#plot<- read.csv(file= 'C:/MASTER UFL/research/Gezan/Modelling/INFO Nothofagus 2/Research Palmas/UACH_P41_M1.csv')

head(plot)
tree<-input_module(type='tree',zone=3,AD=52.31,HD=42.40,SI=NA,area=500,AF=58.31,tree.list=plot,V_model=1)

tree<-input_module(type='tree',zone=2,AD=51.85,HD=30,SI=NA,area=500,AF=57.85,tree.list=plot,NHA_model=1)
tree<-input_module(type='tree',zone=2,AD=51.85,HD=30,SI=NA,area=500,AF=57.85,tree.list=plot,NHA_model=2)
>>>>>>> 01f3010dda72f90bb49ce632fcb80434ea7bec2a
head(tree$tree.list)
tree$sp.table

core.tree<-core_module(input=tree$input)
core.tree$sp.table
core.tree$type<-"tree"
stand<-stand_simulator(core.stand = core.tree)
results.stand<-core_module(input = stand$input)
results.stand$sp.table
report(core.stand=results.stand)


# Example Paulo 1
plot<- read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P41_M1.csv')
tree<-input_module(type='tree',zone=3,AD=52.31,HD=42.40,SI=NA,area=500,AF=58.31,tree.list=plot,V_model=1)
head(tree$tree.list)
tree$sp.table

plot<- read.csv(file='D:/WORK_PARTIAL/SFRC/Students/PauloMoreno/Y2017/PlotsValidation/UACH_P41_M2.csv')
tree<-input_module(type='tree',zone=3,AD=58.31,SI=22.91,area=500,AF=NA,tree.list=plot,V_model=1)
head(tree$tree.list)
tree$sp.table

View(tree$tree.list)

# No simulation - tree data
tree$input$type<-"tree"
tree$input$ddiam<-FALSE
coret<-core_module(input=tree$input)
coret$sp.table
coret$stand.table[5,,]

# No simulation - ddiam
tree$input$ddiam<-TRUE
tree$input$type<-"stand"
coret<-core_module(input=tree$input)
coret$sp.table
coret$stand.table[5,,]

# Simulation - from tree data - stand simulation
tree$input$ddiam<-FALSE
tree$input$type<-"tree"
coret<-core_module(input=tree$input)
coret$sp.table
coret$stand.table[5,,]

coret$input$type<-'stand'
stand<-stand_simulator(core.stand=coret)
stand$input$type<-'stand'
stand$input$ddiam<-TRUE
stand$input$sp.table
stand$results

results.stand<-core_module(input=stand$input)
results.stand$sp.table

# Simulation - from tree data - tree simulation
tree$input$ddiam<-FALSE
tree$input$type<-"tree"
coret<-core_module(input=tree$input)
coret$sp.table
coret$stand.table[5,,]

coret$input$type<-'tree'
treesim<-tree_simulator(core.tree=coret$input)
head(treesim$input$tree.list)

results.tree<-core_module(input=treesim$input)
results.tree$sp.table
results.tree$stand.table[5,,]

# Compatibility (concept!)
results.stand$input
results.tree$input
results.comp$input<-compatibilty_module(stand=results.stand$input,tree=results.tree$input)  
results.comp.final<-core_module(input=reulsts.comp$input)


##############################
# input_module

# Example 1: Input from stand-level data
BA<-c(36.5,2.8,0.0,2.4)
N<-c(464,23,0,48)
plot1<-input_module(type='stand',zone=1,AD=28,AF=40,HD=23.5,N=N,BA=BA,V_model=1,ddiam=FALSE)
plot1$sp.table

# Going to core

#Option 1 - No ddiam - Model 1
plot1$input$ddiam<-FALSE
plot1$input$V_model<-1
core1<-core_module(input=plot1$input)
core1$sp.table
core1$stand.table[5,,]

#Option 1 - No ddiam - Model 2
plot1$input$ddiam<-FALSE
plot1$input$V_model<-2
core1<-core_module(input=plot1$input)
core1$sp.table
core1$stand.table[5,,]

#Option 1 - With ddiam - Model 99
plot1$input$ddiam<-TRUE
plot1$input$V_model<-99  # Irrelevant
core1<-core_module(input=plot1$input)
core1$sp.table
core1$stand.table[5,,]


# Example 2: Input from tree data
plot2<- read.csv(file= 'data/Plot_example.csv')
plot2<-input_module(type='tree',zone=2,AD=28,HD=15.5,area=500,tree.list=plot2)
plot2$sp.table
head(plot2$tree.list)


#Option 1 - Stand - No ddiam - Model 2
plot2$input$type<-'stand'
plot2$input$ddiam<-FALSE
plot2$input$V_model<-2
core2<-core_module(input=plot2$input)
core2$sp.table
core2$stand.table[5,,]

#Option 2 - Stand - Yes ddiam - Model 2
plot2$input$type<-'stand'
plot2$input$ddiam<-TRUE
plot2$input$V_model<-2
core2<-core_module(input=plot2$input)
core2$sp.table
core2$stand.table[4,,]

#Option 3 - tree (ddiam from data) - Model 2
plot2$input$type<-'tree'
plot2$input$ddiam<-TRUE
plot2$input$V_model<-2
core2<-core_module(input=plot2$input)
core2$sp.table
core2$stand.table[2,,]

#Option 4 - Stand - No ddiam - Model 1
plot2$input$ddiam<-TRUE # or FALSE
plot2$input$V_model<-1
core1<-core_module(input=plot2$input)
core1$sp.table
core1$stand.table[2,,]


Vmodule(BA=40.31,HD=18.5,PNHAN=0.9130435)
