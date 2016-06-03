source('figures/traitPostAux.R')
postMoist <- postGibbsChains(betachains = output$chains$agibbs, 
                        burnin = output$burnin,
                        traitsToPlot = c('N') ,
                        predictorsToPlot = c('moisture'), 
                        onlySignificant = F, 
                        normalized = F, 
                        includeInteractions = T, 
                        includeMainEffects = T)

colnames(postMoist$chains)





