source('~/Projects/procVisData/dataViz.R')

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitPreModel.R')
source('traitFunctions.R')
source('figures/traitColorSet.R')

#####################################
plotNP <- CWT$perMass[,'N']/CWT$perMass[,'P']
g <- data.frame(lat=plotByX$plotLat, lon =plotByX$plotLon, NP=plotNP, elev =plotByX$elev)

png('figures/traitFig.N-P.Ratio.png', units='in',res=300, height  = 8, width=8)
layout(matrix(c(3,1,1,3,1,1,4,2,2),3,3))
par(mar=c(0,0,0,0), oma=c(6,6,5,5), cex.axis=1.7)
#plot(NA, xlim=range(lonLatAll[,1]), ylim=range(lonLatAll[,2]), xlab='', ylab='')
#mapColorData(valRange = quantile(plotNP, probs=c(.1,.9)), x = lonLatAll[,1], y = lonLatAll[,2], data = plotNP, colList = colList.purpleOrange, ADD=T, cex.all = 2)

mapColorData(valRange = quantile(plotNP, probs=c(.1,.9)), 
             x = plotByX$plotLon, y = plotByX$plotLat, data = plotNP,
             colList = colList.purpleOrange, cex.all = 2)
mapOutlines(glacialLine, ecoRegion, lwd2 = 10)

mtext(text = 'Leaf N:P', side = 3, line = -2.5, cex=1.5)
mtext(text = expression(Longitude~(degree)), side = 1, line = 4, cex=1.5)
mtext(text = expression(Latitude~(degree)), side = 2, line = 3.5, cex=1.5)
mtext('(a)', 3, at = -68, line = -2.5, cex=2)

par(xaxt='s', yaxt='n',bty='n')
tmp = myBinPlot(g$lat, g$NP, xbreaks = seq(25,50, 3), noPlot = T)
boxplot(tmp$bins, horizontal = T, varwidth = T, outline = F)
lines(tmp$q[,3], 1:nrow(tmp$q), lwd=10, col='#80808080')
mtext(text = expression(bar('N:P')), side = 1, line = 3.5, cex=1.5)
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
mtext(text = 'Altitude (m)', side = 3, line = 3, cex=1.5)
mtext(text = expression(bar('N:P')), side = 4, line = 3, cex=1.5)
mtext('(d)', 3, cex=2 , at= 10, line=2)

dev.off()
