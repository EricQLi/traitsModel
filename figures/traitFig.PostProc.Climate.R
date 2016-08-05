# this script file is meant to use for analyzing model output
source('figures/traitPostAux.R')
source('~/Projects/procVisData/dataViz.R')

post <- postGibbsChains(betachains = output$chains$agibbs, 
                        burnin = output$burnin,
                        traitsToPlot = c('N','P','SLA') ,
                        predictorsToPlot = c('temp', 'moisture','deficit','I(temp^2)', 'I(deficit^2)', 'I(moisture^2)'), 
                        onlySignificant = F, 
                        normalized = T, 
                        excludeIntercept  =F,
                        includeInteractions = F,
                        includeMainEffects = T)

posteriorPlots('figures/traitFig.PostProc.Climate')
