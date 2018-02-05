# Example: Generation of distribution from stand-level input data
BA<-c(36.5,2.8,0,2.4)
N<-c(464,23,0,48)
plot<-input_module(type='stand',zone=2,AD=28,HD=23.5,N=N,BA=BA,ddiam=TRUE)
plot$sp.table

Dd<-diam_dist(sp.table=plot$sp.table, HD=plot$HD,  
               DOM.SP=plot$DOM.SP, zone=plot$zone)
Dd[5,,]  # Total diameter distribution
Dd[4,,]
Dd[3,,]
Dd[2,,]
Dd[1,,]

# Ploting distribution for each specie
barplot(as.matrix(Dd[4,,5]), main='Diameter Distribution all species', xlab='DBH Class', beside=TRUE, col=4)
