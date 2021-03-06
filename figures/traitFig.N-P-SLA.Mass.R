library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitFunctions.R')
source('figures/traitColorSet.R')

png('figures/traitFig.N-P-SLA.Mass.png', units='in',res=300, height  = 5, width=13)
par(mfrow=c(1,3), oma=c(0.0,0,1,0), mar=c(2,3,1,1))
for(j in 4:6){
  
  ssj <- traitData$plotByCWM[,j]
  
  par(xaxt='n', yaxt='n')
  mapColorData(x = plotByX$plotLon, y = plotByX$plotLat, data = ssj,
               valRange = quantile(ssj, probs = seq(0.05,.95, by = .1), na.rm = T), 
               xlim = range(plotByX$plotLon), 
               ylim = range(plotByX$plotLat), statesborder = F,
               cex.all = 2, colList = rev(colList.SurfAndTurf), symSize=1 )
  plot(mapRegion, add = T)
  
  mtext(text =  switch(j-3, 
                       bquote(.(tNames[j-3])~ (mg/g)),
                       bquote(.(tNames[j-3])~ (mg/g)),
                       bquote(.(tNames[j-3])~ (cm^2/g))),
        side = 3, line = .7, cex=2)
  mtext(text =  switch(j-3, '(a)','(b)','(c)'), side = 3, line = 1.5,at = -100, cex=2)
  
  mapOutlines(glacialLine, mapRegion, lwd2 = 8)
  
  par(xaxt='s', yaxt='s')
  
  axis(1, cex.axis=1.7)
  axis(2, cex.axis=1.7)
}
dev.off()

