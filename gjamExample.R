library(gjam)
data(forestTraits)
xdata       <- forestTraits$xdata
traitTypes  <- forestTraits$traitTypes
plotByTree  <- gjamReZero(forestTraits$treesDeZero)  # re-zero data
tmp         <- gjamSpec2Trait(pbys = plotByTree, 
                              sbyt = forestTraits$specByTrait, 
                              tTypes = forestTraits$traitTypes)
tTypes    <- tmp$traitTypes
u         <- tmp$plotByCWM
censor    <- tmp$censor
specByCWM <- tmp$specByCWM
M           <- ncol(u)
n           <- nrow(u)
colnames(u)
traitTypes



tmp    <- gjamCensorY(values = c(0,1), intervals = cbind( c(-Inf,0),c(1,Inf) ),
                      y = u, whichcol = c(13:14))
censor <- append(censor,list('CA' = tmp$censor))


specByTrait <- forestTraits$specByTrait


plotByTrees <- gjamReZero(forestTraits$treesDeZero)
dim(plotByTrees)


traitList <- list(plotByTrait = u, traitTypes = tTypes, 
                  specByTrait = specByCWM)
reductList <- list(r = 3, N = 20)
modelList <- list(ng=2000,burnin=500, typeNames = 'CC', 
                  holdoutN = 20,
                  traitList = traitList, reductList = reductList)
output  <- gjamGibbs(~ temp + stdage + deficit*soil, 
                     xdata = xdata, 
                     ydata = plotByTrees, modelList = modelList)
plotPars  <- list(width=4, height=4, corLines=F,
                  SMALLPLOTS=F,CLUSTERPLOTS=T)                  
fit       <- gjamPlot(output = output, plotPars)


