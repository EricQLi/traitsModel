# this script file is meant to use for analyzing model output
source('figures/traitPostAux.R')
source('~/Projects/procVisData/dataViz.R')

post <- postGibbsChains(betachains = output$chains$agibbs, 
                        burnin = output$burnin,
                        traitsToPlot = c('N','P','SLA') ,
                        predictorsToPlot = c('temp:soilAlfInc','temp:soilEntVert','temp:soilMol','temp:soilSpodHist','temp:soilUltKan'), 
                        onlySignificant = F, 
                        normalized = T, 
                        standardizedT = T,
                        sdTraits = sdTraits,
                        excludeIntercept  =F,
                        includeInteractions = T,
                        includeMainEffects = F, exactPredictors = T)

posteriorPlots(post, 'figures/traitFig.PostProc.Interactions.Temperature', statsParam=statsParam)

