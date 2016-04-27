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

modelList <- list(ng=100, 
                  burnin=50,
                  typeNames = 'CA', 
                  holdoutN = 20,
                  reductList = reductList,
                  traitList = traitList)


attr(plotByX$soil,'reference') <- 'Others'   # reference class
attr(plotByX$soil,'intType')   <- 'ref'

str(plotByX)

output  <- gjamGibbs(~ temp  + deficit + moisture + soil + u1 + u2 + u3 + 
                       deficit*moisture + temp*moisture +
                       moisture*soil + deficit*soil + temp*soil,
                     xdata = plotByX, 
                     ydata = plotByY, 
                     modelList = modelList)


save(output, file = 'output-15-10-dr3.30.RData')
load('~/Google Drive/Shared/output-15-10-dr5.40.RData')

plotPars  <- list(width=4, height=4, corLines=F,
                  SMALLPLOTS=F, CLUSTERPLOTS=T)                  

fit       <- gjamPlot(output = output, plotPars)

output$modelSummary$betaTraitMu   # Q by M coefficient matrix alpha
output$modelSummary$betaTraitSe   # Q by M coefficient std errors
output$modelSummary$sigmaTraitMu  # M by M covariance matrix omega
output$modelSummary$sigmaTraitSe  # M by M covariance std errors

output$modelSummary$tMu[1:5,]     # n by M predictive means
output$modelSummary$tSd[1:5,]   # n by M predictive std errors

