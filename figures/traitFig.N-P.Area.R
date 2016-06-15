library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitFunctions.R')
source('figures/traitColorSet.R')

CWT <- getCWT.Mass.Area(output, speciesByTraits, plotByW)

png('figures/traitFig.N-P.Area.png', units='in',res=300, height  = 5, width=10)
par(mfrow=c(1,2))
for(j in 4:5){
  ssj <-  CWT$perArea[,j-3]
  par(xaxt='n', yaxt='n')
  par(xaxt='n', yaxt='n')
  mapColorData(x = plotByX$plotLon, y = plotByX$plotLat, data = ssj,
               valRange = quantile(ssj, probs = seq(0.05,.95, by = .1), na.rm = T), 
               xlim = range(plotByX$plotLon), 
               ylim = range(plotByX$plotLat), 
               cex.all = 1, colList = rev(colList.SurfAndTurf), symSize=.45 )
  
  # mapSiteData(scaleSym = scaleSym, lonLatAll[,1], lonLatAll[,2], ssj,cex.all = 1,
  #           colList = rev(colList.SurfAndTurf), 
  #           valRange = quantile(ssj, probs = seq(0.1,.9, by = .1), na.rm = T), fill = T, lineCol = '#50505030')   
  
  mtext(text =  bquote(.(tNames[j-3])~ (mg/cm^2)),side = 3, line = 1, cex=1.5)
  mtext(text =  switch(j-3, '(a)','(b)','(c)'), side = 3, line = 1,at = -103, cex=1.5)
  
  mapOutlines(glacialLine, ecoRegion, lwd2 = 8)
  par(xaxt='s', yaxt='s')
  axis(1, cex.axis=1.2)
  axis(2, cex.axis=1.2)
}
dev.off()

