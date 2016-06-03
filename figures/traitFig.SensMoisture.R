dim(output$chains$agibbs)
dim(output$x)

post <- postGibbsChains(betachains = output$chains$agibbs, 
                        burnin = output$burnin,
                        traitsToPlot = c('N') ,
                        predictorsToPlot = c('moisture'), 
                        onlySignificant = F, 
                        normalized = F, 
                        includeInteractions = T, 
                        includeMainEffects = T)

dim(post$chains)

colnames(output$chains$agibbs)[1:30]
colnames(post$chains)





