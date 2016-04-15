library(gjam)

plotByX <- as.data.frame(read.csv('data/post/plotByX.csv'))
plotByY <- as.matrix(read.csv('data/post/plotByY.csv'))
plotByW <- as.matrix(read.csv('data/post/plotByW.csv'))
speciesByTraits <- as.data.frame(read.csv('data/post/speciesByTraits.csv'))

traitTypes <- c(rep('CON',6), rep('CAT', 1))

tmp         <- gjamSpec2Trait(pbys = plotByY, 
                              sbyt = speciesByTraits, 
                              tTypes = traitTypes)

traitList <- list(plotByTrait = tmp$plotByCWM, 
                  traitTypes = tmp$traitTypes, 
                  specByTrait = tmp$specByTrait)


reductList <- list(r = 3, N = 30)

modelList <- list(ng=15000, 
                  burnin=8000,
                  typeNames = 'CA', 
                  holdoutN = 20,
                  #xfactors='soil',
                  reductList = reductList,
                  traitList = traitList)

xdata <- plotByX
# xdata <- xdata[,c(3:8)]
# str(xdata)
# xdata [,-6] <- scale(xdata[,-6])

output  <- gjamGibbs(~ temp  + deficit + moisture + soil +
                       deficit*moisture + therm*moisture ,
                       #deficit*temp + therm*temp + temp*soil+
                      # soil +
                       #moisture*soil + deficit*soil + therm*soil,
                     xdata = plotByX, 
                     ydata = plotByY, 
                     modelList = modelList)


save(output, file = 'output-15-8-dr-l.RData')

#load('output-6-3.RData')


plotPars  <- list(width=4, height=4, corLines=F,
                  SMALLPLOTS=F, CLUSTERPLOTS=T)                  

fit       <- gjamPlot(output = output, plotPars)

output$modelSummary$betaTraitMu   # Q by M coefficient matrix alpha
output$modelSummary$betaTraitSe   # Q by M coefficient std errors
output$modelSummary$sigmaTraitMu  # M by M covariance matrix omega
output$modelSummary$sigmaTraitSe  # M by M covariance std errors

output$modelSummary$tMu[1:5,]     # n by M predictive means
output$modelSummary$tSd[1:5,]   # n by M predictive std errors

