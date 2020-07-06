#lidnesday

library(rgdal)
library(rgeos)
require(ggplot2)
library(nlme)
library(MuMIn)
path<-'C:/Users/drago/Documents/R Projects/Disso'

eles<-read.csv(paste('../elemydatuse.csv', sep=''))
#roan<-read.csv(paste('../roanmydatuse.csv', sep=''))

grid_shape<-readOGR(paste(path, '/Khaudum_shapefiles', sep=''), layer='Khaudum_predgrid500m')
predgrid<-grid_shape@data

#SALSA

library(MRSea)

#df <- eles

#colnames(df)
#factorList <- c("yearmonth")
#varList <- c("mindistToWaters", "x.pos", "y.pos", "mindistToFence")
#salsa1DList <- list(fitnessMeasure="QAIC", minKnots_1d=rep(1, 4),
 #                   maxKnots_1d=rep(4, 4), startKnots_1d=rep(1, 4),
  #                  degree=rep(1, 4), maxIterations=10, 
   #                 gaps=rep(0, 4))

#initialModel <- glm(response ~ as.factor(yearmonth), 
    #                offset=log(areakmsq), family=quasipoisson, 
     #               data=df)

#salsa_eles <- MRSea::runSALSA1D(initialModel, salsa1DList, varList, 
 #                               factorList, varlist_cyclicSplines=NULL, 
  #                              splineParams=NULL, datain=df, 
   #                             suppress.printout=TRUE, removal=FALSE,
    #                            panelid=NULL)

# Pick best model based on fitnessMeasure
#bestModel <- salsa_eles$bestModel

# Sandwich covariance matrix estimators (to be introduced later)
#library(sandwich)

# Summary output
#anova(bestModel, test="F")




#from2018 lecture notes

initialModel_eles <- glm(response ~ as.factor(year), 
                    offset=log(areakmsq), family=quasipoisson, 
                    data=eles)

factorList <- c("year")

varList <- c("mindistToWaters")

#we are going to add x.pos and y.pos in the 2D model
#salsa1dlist<-list(fitnessMeasure="QAIC", minKnots_1d = c(1),
#                  maxKnots_1d=c(4), startKnots_1d = c(1), degree=c(2),
#                  maxIterations=100, gaps=c(0))

salsa1dlist<-list(fitnessMeasure="QAIC", minKnots_1d = c(1),
                  maxKnots_1d=c(3), startKnots_1d = c(1), degree=c(2),
                  maxIterations=100, gaps=c(0))

salsa1dout<-runSALSA1D(initialModel_eles, salsa1dlist, varList,
                       factorList, varlist_cyclicSplines = NULL,
                       splineParams = NULL, datain=eles,
                       suppress.printout = FALSE)

bestModel1D<-salsa1dout$bestModel

anova(bestModel1D)

knotgrid<- getKnotgrid(coordData = cbind(eles$x.pos,
                                         eles$y.pos))

distMats <- makeDists(cbind(eles$x.pos, eles$y.pos),
                      na.omit(knotgrid))

#min knots = the number ofr water holes
#salsa2dlist<-list(fitnessMeasure = "QAIC", knotgrid = knotgrid,
#                  startKnots=12, minKnots=12, maxKnots=20, gap=0,
#                  interactionTerm="yearmonth")

#simplest model
salsa2dlist<-list(fitnessMeasure = "QAIC", knotgrid = knotgrid,
                  startKnots=4, minKnots=2, maxKnots=4, gap=0)


#simplest model with interaction term
salsa2dlist<-list(fitnessMeasure = "QAIC", knotgrid = knotgrid,
                  startKnots=4, minKnots=2, maxKnots=4, gap=0,
                  interactionTerm = "year")


salsa2dOutput<-runSALSA2D(bestModel1D, salsa2dlist,
                          d2k=distMats$dataDist, k2k=distMats$knotDist,
                          splineParams=NULL, tol=0, chooserad=F,
                          panels=NULL, suppress.printout=FALSE)


bestModel2D_int<-salsa2dOutput$bestModel

vignette(topic='UsingMRSea_v1.0', package='MRSea')bestModel2D_noint<-salsa2dOutput$bestModel







