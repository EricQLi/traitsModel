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
post$nameMatrix
png('figures/traitFig.PostProc.Interactions.Moisture.png', width = 8, height = 6, res =150, units='in')
par(mfrow=c(3,2), bty='n', xaxt='s', yaxt='n', mar=c(1,1,1,1), oma=c(1,1,1,1))
for(t in c('N','P','SLA')){
  chains <- as.matrix(post$chains[,which(post$nameMatrix[,trait]==t)])
  if(is.null(dim(chains))) chains <- as.matrix(chains, ncol = 1)
  plotGibbsBoxplots(chains, textAdj = 0, labels = post$nameMatrix$predictor, sigPlot = F, sort = F)
  # plotGibbsChains(chains, labels = post$nameMatrix$predictor)
  mtext(text = t, side = 2, line = 0, cex=2, font=2)
  plotGibbsDensity(chains, labels = post$nameMatrix$predictor, txtAdj = 0, title.text = '', xlab = '', ylab = '')
}
dev.off()
