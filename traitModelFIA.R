library(gjam)

plotByX <- as.data.frame(read.csv('data/post/plotByX.csv'))
plotByW <- as.matrix(read.csv('data/post/plotByW.csv'))
speciesByTraits <- as.data.frame(read.csv('data/post/speciesByTraits.csv'))

traitTypes <- c(rep('CON',6), 'CAT')

tmp         <- gjamSpec2Trait(pbys = plotByW, 
                              sbyt = speciesByTraits, 
                              tTypes = traitTypes)

traitList <- list(plotByTrait = tmp$plotByCWM, 
                  traitTypes = traitTypes, 
                  specByTrait = tmp$specByTrait)

reductList <- list(r = 3, N = 20)

modelList <- list(ng=10000, burnin=5000, typeNames = 'CC', 
                  holdoutN = 20,
                  traitList = traitList, 
                  reductList = reductList)

output  <- gjamGibbs(~ temp + therm + deficit + moisture + 
                       deficit*moisture + therm*moisture +
                       #deficit*temp + therm*temp +
                       soil +
                       #+ temp*soil
                       moisture*soil + deficit*soil + therm*soil,
                     xdata = plotByX, 
                     ydata = plotByW, 
                     modelList = modelList)

#save(output, file = 'output1.RData')

plotPars  <- list(width=4, height=4, corLines=F,
                  SMALLPLOTS=F, CLUSTERPLOTS=T)                  

fit       <- gjamPlot(output = output, plotPars)

output$modelSummary$betaTraitMu   # Q by M coefficient matrix alpha
output$modelSummary$betaTraitSe   # Q by M coefficient std errors
output$modelSummary$sigmaTraitMu  # M by M covariance matrix omega
output$modelSummary$sigmaTraitSe  # M by M covariance std errors

output$modelSummary$tMu[1:5,]     # n by M predictive means
output$modelSummary$tSd[1:5,]   # n by M predictive std errors

