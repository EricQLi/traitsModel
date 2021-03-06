library(raster)

source('traitFunctions.R')
source('figures/traitColorSet.R')
source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

png('figures/traitFig.Env-Soil-Eco.png', units='in',res=300, width = 10, height=15)
par(mfrow=c(3,2), yaxt='n',ann=F, xaxt='n', mar=c(0,0,4,0), oma=c(2,0.5,0,0.5))

for(fig in 1:6){
  symSize  <- 1
  legend.txt <- switch(fig, c('Cold','Hot'), c('Dry','Moist'), c('Low','High'), c('Low','High'), soilType)
  # if (fig<5) legend.txt <- NULL
  title <- switch(fig, 'Winter Temperature', 'Soil Moisture', 'Hydrothermal Surplus', 'Hydrothermal Deficit', 'Soil Taxonomy Order', 'Ecoregions')
  zz <- switch(fig, plotByX[,'temp'],plotByX[,'moisture'] ,plotByX[,'surplus'] ,plotByX[,'deficit'], as.character(plotByX$soil))
  colList <- switch(fig, colList.purpleOrange, colList.orangePurple, colList.purpleOrange, colList.purpleOrange, NULL)
  wSoil <- match(plotByX$soil, soilColTable[,1])
  wSoil[is.na(wSoil)] <- 3
  colS <- switch(fig, NULL, NULL, NULL, NULL, colListSoil[wSoil])
  cex.all <- switch(fig, 5,5,5,5 ,2)
  legend.col <- switch(fig, 'black', 'black', 'black', 'black', colListSoil)
  if(fig!=6){
    valRange <- range(zz)
    if(fig<5)valRange <- quantile(zz, probs=seq(.1,.9, length.out = 100))
    if(fig<5)legend.txt <- signif(range(valRange),1)
    mapColorData( symSize = symSize, x = plotByX$plotLon, y = plotByX$plotLat,
                  data = zz,
                  legend.txt = legend.txt, legend.col = legend.col, 
                  valRange = valRange,statesborder = F, 
                  xlim = range(plotByX$plotLon), 
                  ylim = range(plotByX$plotLat), 
                  colList = colList ,cex.all = cex.all, col=colS)
    mapOutlines(glacialLine, mapRegion)
  }
  if(fig==6){
    par(pty="s")
    plot(NA, xlim=range(plotByX$plotLon), ylim=range(plotByX$plotLat))
    plot(mapRegion, col=colRegion[as.numeric(as.factor(mapRegion$PROVINCE))],add=T)
    legend('bottomright', legend = mapRegionPROVINCE, col=colRegion[wc], lwd=10, bty='n',cex=1.2)
  }
  mtext(title, side = 3, cex=1.5, line = .5, font=2)
  mtext(switch(fig, '(a)', '(b)', '(c)', '(d)', '(e)','(f)'), side = 3, at=-102, line=1, cex=2)
}

dev.off()

