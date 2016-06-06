source('slopeAspectFunctions.r')

png('figures/traitFig.AspectEffect.png', width = 3, height = 6, res =150, units='in')
par(mfrow=c(3,1), bty='n', xaxt='s', yaxt='s', mar=c(4,4,1,0), oma=c(.5,.5,.5,.5))

for(trait in c('N','P','SLA')){
  post <- postGibbsChains(betachains = output$chains$agibbs, 
                          burnin = output$burnin,
                          traitsToPlot = trait ,
                          predictorsToPlot = c('u1','u2','u3'), 
                          onlySignificant = F, 
                          normalized = T, 
                          excludeIntercept  =F,
                          includeInteractions = F,
                          includeMainEffects = T)
  betaSlope <- t(post$chains)
  
  tmp <- plotAspectEffect(betaSlope = betaSlope, textSize = .0001,
                          slopeRange = c(.15,.16), maxNumber = 10)
  mtext(text = trait, side = 3, cex = 1.2, line = -2, at = -3, adj = 0)
}

dev.off()