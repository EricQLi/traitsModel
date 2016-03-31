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


###TRM
tmp    <- gjamCensorY(values = c(0,1), intervals = cbind( c(-Inf,0),c(1,Inf) ),
                      y = u, whichcol = c(13:14))
censor <- append(censor, list('CA' = tmp$censor))

modelList <- list(ng = 2000, burnin = 500, typeNames = tTypes,
                  holdoutN = 20, xfactors='soil', censor=censor)

output    <- gjamGibbs(~temp + stdage + deficit*soil, #~ temp + u1 + u2 + u3 + moisture*(deficit+soil),
                       xdata = xdata, ydata = u, 
                       modelList = modelList)

plotPars  <- list(width = 3, height = 3, CLUSTERPLOTS = TRUE)           
fit       <- gjamPlot(output, plotPars)




##PRM

specByTrait <- forestTraits$specByTrait


plotByTrees <- gjamReZero(forestTraits$treesDeZero)
dim(plotByTrees)

traitList <- list(plotByTrait = u, traitTypes = tTypes, 
                  specByTrait = specByCWM)
reductList <- list(r = 3, N = 20)
modelList <- list(ng=1000,burnin=500, typeNames = 'CC', 
                  holdoutN = 20,
                  traitList = traitList, reductList = reductList)

output  <- gjamGibbs(~ temp + stdage + deficit*soil, 
                     xdata = xdata, 
                     ydata = plotByTrees, modelList = modelList)

plotPars  <- list(width=4, height=4, corLines=F,
                  SMALLPLOTS=F,CLUSTERPLOTS=T)                  
fit       <- gjamPlot(output = output, plotPars)




output$modelSummary$betaTraitMu   # Q by M coefficient matrix alpha
output$modelSummary$betaTraitSe   # Q by M coefficient std errors
output$modelSummary$sigmaTraitMu  # M by M covariance matrix omega
output$modelSummary$sigmaTraitSe  # M by M covariance std errors




output$modelSummary$tMu[1:5,]     # n by M predictive means
output$modelSummary$tSd[1:5,]   # n by M predictive std errors


