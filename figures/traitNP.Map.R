source('~/Projects/procVisData/geoSpatial.R')
source('traitFunctions.R')

png('figures/N-P-SLA-Mass.png', units='in',res=300, height  = 5, width=15)
par(mfrow=c(1,3), oma=c(0.0,0,1,0))
for(j in 4:6){
  
  ssj <- traitData$plotByCWM[,j]
  
  par(xaxt='n', yaxt='n')
  mapColorData(x = plotByX$plotLon, y = plotByX$plotLat, data = ssj,
               valRange = quantile(ssj, probs = seq(0.05,.95, by = .1), na.rm = T), 
               cex.all = 2, colList = rev(colList.SurfAndTurf), symSize=1 )

  mtext(text =  switch(j-3, 
                       bquote(.(tNames[j-3])~ (mg/g)),
                       bquote(.(tNames[j-3])~ (mg/g)),
                       bquote(.(tNames[j-3])~ (cm^2/g))),
        side = 3, line = .7, cex=2)
  mtext(text =  switch(j-3, '(a)','(b)','(c)'), side = 3, line = 1.5,at = -100, cex=2)
  
  lines(glacialLine[-nrow(glacialLine),1],glacialLine[-nrow(glacialLine),2],lwd=6,col='white')
  lines(glacialLine[-nrow(glacialLine),1],glacialLine[-nrow(glacialLine),2],lwd=2,col='darkgrey')
  plot(ecoRegion, add=T)
  par(xaxt='s', yaxt='s')
  
  axis(1, cex.axis=1.7)
  axis(2, cex.axis=1.7)
}
dev.off()

#####################################
CWT <- getCWT.Mass.Area(speciesByTraits, plotByW)

png('figures/N-P-Area.png', units='in',res=300, height  = 5, width=10)
par(mfrow=c(1,2))
for(j in 4:5){
  ssj <-  CWT$perArea[,j-3]
  par(xaxt='n', yaxt='n')
  par(xaxt='n', yaxt='n')
  mapColorData(x = plotByX$plotLon, y = plotByX$plotLat, data = ssj,
               valRange = quantile(ssj, probs = seq(0.05,.95, by = .1), na.rm = T), 
               cex.all = 1, colList = rev(colList.SurfAndTurf), symSize=1 )

    # mapSiteData(scaleSym = scaleSym, lonLatAll[,1], lonLatAll[,2], ssj,cex.all = 1,
    #           colList = rev(colList.SurfAndTurf), 
    #           valRange = quantile(ssj, probs = seq(0.1,.9, by = .1), na.rm = T), fill = T, lineCol = '#50505030')   
  
  mtext(text =  bquote(.(tNames[j-3])~ (mg/cm^2)),side = 3, line = 1, cex=1.5)
  mtext(text =  switch(j-3, '(a)','(b)','(c)'), side = 3, line = 1,at = -103, cex=1.5)
  
  mapOutlines(lwd2 = 8)
  par(xaxt='s', yaxt='s')
  axis(1, cex.axis=1.2)
  axis(2, cex.axis=1.2)
}
dev.off()
#####################################

png(paste0(resultDir,paste0('mean/NP-ratio.png')), units='in',res=300, height  = 8, width=8)
layout(matrix(c(3,1,1,3,1,1,4,2,2),3,3))
par(mar=c(0,0,0,0), oma=c(5,5,5,5), cex.axis=1.7)
#plot(NA, xlim=range(lonLatAll[,1]), ylim=range(lonLatAll[,2]), xlab='', ylab='')
#mapColorData(valRange = quantile(plotNP, probs=c(.1,.9)), x = lonLatAll[,1], y = lonLatAll[,2], data = plotNP, colList = colList.purpleOrange, ADD=T, cex.all = 2)
mapColorData(valRange = quantile(plotNP, probs=c(.1,.9)), x = lonLatAll[,1], y = lonLatAll[,2], data = plotNP, colList = colList.purpleOrange, cex.all = 2)
mapOutlines(lwd2 = 10)

mtext(text = 'Leaf N:P', side = 3, line = -3, cex=1.5)
mtext(text = 'Longitude (°)', side = 1, line = 3, cex=1.5)
mtext(text = 'Latitude (°)', side = 2, line = 3, cex=1.5)
mtext('(a)', 3, at = -68, line = -2.5, cex=2)

g <- gridData1
par(xaxt='s', yaxt='n',bty='n')
tmp = myBinPlot(g$lat, g$NP, xbreaks = seq(25,50, 3), noPlot = T)
boxplot(tmp$bins, horizontal = T, varwidth = T, outline = F)
lines(tmp$q[,3], 1:nrow(tmp$q), lwd=10, col='#80808080')
mtext(text = expression(bar('N:P')), side = 1, line = 3, cex=1.5)
mtext('(b)', 3, at = 17.3, line = -2.5, cex=2)

par(xaxt='n',yaxt='s')
tmp = myBinPlot(g$lon, g$NP, xbreaks = seq(-100,-65, 5), noPlot = T)
boxplot(tmp$bins, varwidth = T, outline = F)
lines(tmp$q[,3], lwd=10, col='#80808080')
mtext(text = expression(bar('N:P')), side = 2, line = 3, cex=1.5)
mtext('(c)', 3, cex=2 , at=7.3, line=2)

par(yaxt='n')
tmp = myBinPlot(g$elev, g$NP, xbreaks = seq(0,800, 100), noPlot = T)
boxplot(tmp$bins, varwidth = T, outline = F)
par(xaxt='s',yaxt='s')
axis(3, at = 1:8, labels = c(1:8)*100)
lines(tmp$q[,3], lwd=10, col='#80808080')
axis(4)
mtext(text = 'Elevation (m)', side = 3, line = 3, cex=1.5)
mtext(text = expression(bar('N:P')), side = 4, line = 3, cex=1.5)
mtext('(d)', 3, cex=2 , at= 10, line=2)

dev.off()
