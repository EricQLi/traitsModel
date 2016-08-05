# this script file is meant to use for analyzing model output
source('figures/traitPostAux.R')
source('~/Projects/procVisData/dataViz.R')

post <- postGibbsChains(betachains = output$chains$agibbs, 
                        burnin = output$burnin,
                        traitsToPlot = c('N','P','SLA') ,
                        predictorsToPlot = c('moisture'), 
                        onlySignificant = F, 
                        normalized = T, 
                        excludeIntercept  =F,
                        includeInteractions = T,
                        includeMainEffects = F)

posteriorPlots('figures/traitFig.PostProc.Interactions.Moisture')


