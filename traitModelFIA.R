library(gjam)

browseVignettes('gjam')
source('traitPreModel.R')

traitData  <- gjamSpec2Trait(pbys = plotByW, 
                             sbyt = speciesByTraits, 
                             tTypes = traitTypes)

traitList <- list(plotByTrait = traitData$plotByCWM, 
                  traitTypes = traitData$traitTypes, 
                  specByTrait = traitData$specByTrait)


reductList <- list(r = 3, N = 30)

modelList <- list(ng=10000, 
                  burnin=6000,
                  typeNames = 'FC', 
                  holdoutN = 20,
                  reductList = reductList,
                  traitList = traitList)


attr(plotByX$soil,'reference') <- 'Others'   # reference class
attr(plotByX$soil,'intType')   <- 'ref'

output  <- gjamGibbs(~ temp  +  deficit + moisture + soil ,
                     xdata = plotByX, 
                     ydata = plotByW, 
                     modelList = modelList)

# save(output, file =paste('output', modelList$ng, modelList$burnin, 'dr', reductList$r, reductList$N,data(),'.RData', sep = '-'))

head(output$modelSummary$sigmaTraitMu)
head(output$modelSummary$betaTraitMu)

plotPars  <- list(width=5, height=4, corLines=T, 
                  SMALLPLOTS=T, CLUSTERPLOTS=T)                  

fit       <- gjamPlot(output = output, plotPars)


