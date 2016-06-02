# this script file is meant to use for analyzing model output
source('figures/traitPostAux.R')
source('~/Projects/procVisData/dataViz.R')

post <- postGibbsChains(betachains = output$chains$agibbs, 
                        burnin = output$burnin,
                        traitsToPlot = c('N','P','SLA') ,
                        predictorsToPlot = c('deficit'), 
                        onlySignificant = F, 
                        normalized = T, 
                        excludeIntercept = T,
                        withInteractions = T)

png('figures/traitFig.PostProc.Interactions.Deficit.png', width = 8, height = 6, res =150, units='in')
par(mfrow=c(3,2), bty='n', xaxt='s', yaxt='n', mar=c(1,1,1,1), oma=c(1,1,1,1))
for(t in c('N','P','SLA')){
  chains <- post$chains[,which(post$nameMatrix[,trait]==t)]
  plotGibbsBoxplots(chains, textAdj = 0, labels = post$nameMatrix$predictor, sigPlot = F, sort = F)
  # plotGibbsChains(chains, labels = post$nameMatrix$predictor)
  mtext(text = t, side = 2, line = 0, cex=2, font=2)
  plotGibbsDensity(chains, labels = post$nameMatrix$predictor, txtAdj = 0, title.text = '', xlab = '', ylab = '')
}
dev.off()
