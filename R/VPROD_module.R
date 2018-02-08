#' Calculates tree volume inside bark based on taper equations for different products according to specifications.
#'
#' \code{Vmodule_product} For the characteristics of a given tree (DBH, HT, SPECIES) calculates the volume for different
#' products according to specifications. The products are obtained in order of preference.
#'
#' @param dom_sp Dominant specie (1:Rauli, 2:Roble, 3:Coigue, 4:Others or Mixed) 
#' @param zone Growth zone of the corresponding stand
#' @param DBH diameter at breast height (cm)
#' @param HT total tree height (m)
#' @param stump length of stump to discount (default 0.3 m) 
#' @param d.min Vector of minimum log diameters (cm) for products
#' @param l.min Vector of log lengths (cm) for products
#' 
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#' 
#' Gezan, S.A. and Moreno, P. (2000b). ????????????. 
#' Reporte Interno. Projecto FONDEF D97I1065. Chile
#'  
#' @return Tree volume without bark (m3) based on the specifications of restrictions provided
#'
#' @examples
#' # Example: Calculates volume for different products (start with stump = 0) 
#' Vmodule_product(SPECIES=1, zone=1, DBH=22.1, HT=18.2)

Vmodule_product <- function(SPECIES=NA, zone=NA, DBH=NA, HT=NA){

  products <- products_setup
  p<-length(products)
  
  hp <- matrix(data=0,nrow=p,ncol=1) # height for a product
  vp <- matrix(data=0,nrow=p,ncol=1) # volume for a product
  for (k in 1:p) {
    hp[k]<-get_taper(SPECIES=SPECIES, zone=zone, DBH=DBH, HT=HT, di=products$Dmin[k])$hi
  }

  # Defining sections and their type for 30 sections.
  h.init <- matrix(data=0,nrow=30,ncol=1)
  h.fin  <- matrix(data=0,nrow=30,ncol=1)
  h.prod <- matrix(data=0,nrow=30,ncol=1)
  I <- 1
  for (k in 1:30) {   
    if ((hp[I]-h.init[k])>=products$Length[I]) {
      h.fin[k] <- h.init[k]+products$Length[I]
      h.prod[k] <- I
      h.init[k+1] <- h.fin[k]
      h.init[k+2] <- h.fin[k]
      h.init[k+3] <- h.fin[k]
      h.init[k+4] <- h.fin[k]
      h.init[k+5] <- h.fin[k]
    }
    else {
      I <- I + 1
    }
    if (I>p) { break }
  }
  sections <- data.frame(h.init=h.init,h.fin=h.fin,h.prod=h.prod) 
  sections <- sections[ which(sections$h.prod!=0),]
  for (k in 1:nrow(sections)) {   
    sections$vcum[k] <- Vmodule_individual(SPECIES=SPECIES, zone=zone, DBH=DBH, HT=HT, 
                                               blength=sections$h.fin[k], stump=0)
    if (k==1) {
      sections$vsection[k] <- sections$vcum[k] 
    } else {
    sections$vsection[k] <- sections$vcum[k]-sections$vcum[k-1] 
    }
  }
  sections
  vtot <- Vmodule_individual(SPECIES=SPECIES, zone=zone, DBH=DBH, HT=HT, blength=HT, stump=0)
  vsums <- aggregate(sections$vsection,by=list(sections$h.prod), FUN=sum)
  colnames(vsums)<-c('P','vsum')
  
  vprod <- data.frame(matrix(data=0,nrow=p,ncol=2))
  vprod[,1] <- seq(1:5)
  colnames(vprod)<-c('P','vsums')
  
  vprod2<-merge(vprod,vsums,by=c('P'),all=TRUE)
  
  for (k in 1:nrow(vprod2)) {
    if (is.na(vprod2$vsum[k])) {
      vprod2$vsum[k] <- 0
    }
  }
  vprod2$vsum[5]<-vtot-sum(vprod2$vsum[1:4])
  vfin <- data.frame(P=vprod2$P,vol=vprod2$vsum)
  
  return(vfin)
}
