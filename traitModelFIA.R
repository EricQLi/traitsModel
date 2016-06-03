library(gjam)
library(data.table)

set.seed(2016)
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

set.seed(2016)

# output  <- gjamGibbs(~ temp + moisture + deficit + surplus + soil +u1 + u2 + u3 +
#                        moisture*temp + moisture*soil + temp*soil + deficit*soil + deficit*temp,   
#                      xdata = plotByX, 
#                      ydata = plotByW, 
#                      modelList = modelList)


# save.image(paste('output-mainModel', 
#                  modelList$ng/1000, modelList$burnin/1000,
#                  substring(make.names(Sys.time()),2), 
#                  '.RData', sep = '-'))

load('output-mainModel-10-6-2016.06.02.20.27.03-.RData')

head(output$modelSummary$sigmaTraitMu)
head(output$modelSummary$betaTraitMu)

plotPars  <- list(width=5, height=4, corLines=T, 
                  SMALLPLOTS=T, CLUSTERPLOTS=T)                  

fit       <- gjamPlot(output = output, plotPars)


