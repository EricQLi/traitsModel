source('traitPreModel.R')

str(plotByX)
str(plotByY)
str(plotByW)

traitList <- list(plotByTrait = traitData$plotByCWM, 
                  traitTypes = traitData$traitTypes, 
                  specByTrait = traitData$specByTrait)


reductList <- list(r = 3, N = 30)

modelList <- list(ng=20000, 
                  burnin=10000,
                  typeNames = 'CA', 
                  holdoutN = 20,
                  reductList = reductList,
                  traitList = traitList)


attr(plotByX$soil,'reference') <- 'Others'   # reference class
attr(plotByX$soil,'intType')   <- 'ref'


output  <- gjamGibbs(~ temp  + deficit + moisture + soil + u1 + u2 + u3 + 
                       deficit*moisture + temp*moisture +
                       moisture*soil + deficit*soil + temp*soil,
                     xdata = plotByX, 
                     ydata = plotByY, 
                     modelList = modelList)


save(output, file = 'output-20-10-dr3.30.RData')
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

