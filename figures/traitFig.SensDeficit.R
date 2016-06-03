library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitPreModel.R')
source('traitFunctions.R')
source('figures/traitColorSet.R')

source('figures/traitPostAux.R')

traitNames <- colnames(traitList$plotByTrait)
traitSd <- apply(traitData$plotByCWM, 2, sd)

deficitSensList <- list()

for(j in 4:6){
  postMoist <- postGibbsChains(betachains = output$chains$agibbs, 
                               burnin = output$burnin,
                               traitsToPlot = traitNames[j] ,
                               predictorsToPlot = c('deficit'), 
                               onlySignificant = F, 
                               normalized = F, 
                               includeInteractions = T, 
                               includeMainEffects = T)
  
  colnames(postMoist$chains)
  
  sensVectors <- cbind(1, output$x[,c("moisture","soilAlfInc","soilEntVert","soilMol","soilSpodHist","soilUltKan","temp")])
  
  deficitSens <- sensVectors%*%t(postMoist$chains)
  deficitSensList[[length(deficitSensList)+1]] <- rowMeans(deficitSens)/traitSd[j]
}
names(deficitSensList) <- traitNames[4:6]


valRange <- c()

png('figures/traitFig.SensDeficit.png', units='in',res=300, height  = 5, width=15)
par(mfrow=c(1,3), oma=c(0.0,0,2,0))
for(j in 4:6){
  
  ssj <- deficitSensList[[j-3]]
  
  par(xaxt='n', yaxt='n')
  mapColorData(x = plotByX$plotLon, y = plotByX$plotLat, data = ssj,
               valRange = quantile(ssj, probs = seq(0.05,.95, by = .1), na.rm = T), 
               cex.all = 2, colList = rev(colList.SurfAndTurf), symSize=1 )
  
  # mtext(text =  switch(j-3, 
  #                      bquote(.(tNames[j-3])~ (mg/g)),
  #                      bquote(.(tNames[j-3])~ (mg/g)),
  #                      bquote(.(tNames[j-3])~ (cm^2/g))),
  #       side = 3, line = .7, cex=2)
  mtext(text = traitNames[j], side = 3, line = .7, cex=2)
  mtext(text =  switch(j-3, '(a)','(b)','(c)'), side = 3, line = 1.5,at = -100, cex=2)
  
  lines(glacialLine[-nrow(glacialLine),1],glacialLine[-nrow(glacialLine),2],lwd=6,col='white')
  lines(glacialLine[-nrow(glacialLine),1],glacialLine[-nrow(glacialLine),2],lwd=2,col='darkgrey')
  plot(ecoRegion, add=T)
  par(xaxt='s', yaxt='s')
  
  axis(1, cex.axis=1.7)
  axis(2, cex.axis=1.7)
}
mtext(outer = T, side = 3, text = 'Sensitivity to deficit (dimensionless)', cex = 1.6)
dev.off()
