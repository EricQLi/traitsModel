library(raster)

zinkeSoil <- shapefile('data/CDIAC_NDP18_221/data/zinke_soil.shp')


sgnLat <- rep(1, length(zinkeSoil$LAT))
sgnLat[grep('S', zinkeSoil$LAT)] <- -1
zinkeSoil$lat <- as.numeric(strsplit(zinkeSoil$LAT, split = 'N|S'))*sgnLat


sgnLon <- rep(1, length(zinkeSoil$LON))
sgnLon[grep('W', zinkeSoil$LON)] <- -1
zinkeSoil$lon <- as.numeric(strsplit(zinkeSoil$LON, split = 'W|E'))*sgnLon

mapColorData(zinkeSoil$lon, zinkeSoil$lat, zinkeSoil$CARBON)
mapColorData(zinkeSoil$lon, zinkeSoil$lat, zinkeSoil$NITROGEN)



wbox <- zinkeSoil$lat>=min(plotByX$plotLat) &  
  zinkeSoil$lat<=max(plotByX$plotLat) &
  zinkeSoil$lon>=min(plotByX$plotLon) &  
  zinkeSoil$lon<=max(plotByX$plotLon) 

png('figures/traitFig.Soil.CN.Zinke.png', units='in',res=300, height  = 10, width=15)
par(mfrow=c(1,2))#,oma = c(3,7,5,5),mar = c(0,0,0,0))

for(j in 1:2){
  ssj <- switch (j,   
                 zinkeSoil$CARBON, 
                 zinkeSoil$NITROGEN)
  par(xaxt='n', yaxt='n')
  valRange <- quantile(ssj[wbox], probs=seq(.025,.975, length.out = 10), na.rm=T)
  
  mapColorData(x=zinkeSoil$lon[wbox],y =  zinkeSoil$lat[wbox],  data = ssj[wbox], 
               symSize = 1.5,
               xlim = range(plotByX$plotLon), equiLatLon = T, 
               ylim = range(plotByX$plotLat),
               valRange = valRange,
               legend.txt = paste0(signif(range(valRange),2), ' (g/m^2)'),
               colList =rev(colList.SurfAndTurf),
               ADD=F, cex.all = 1.5, #legendShow = F 
  )
  
  # lines(glacialLine[-nrow(glacialLine),1],glacialLine[-nrow(glacialLine),2],lwd=6,col='white')
  # lines(glacialLine[-nrow(glacialLine),1],glacialLine[-nrow(glacialLine),2],lwd=2,col='darkgrey')
  par(xaxt='s', yaxt='s')
  
  mtext(text = switch (j,  'Carbon', 
                       'Nitrogen'  ),
  side = 3, cex = 2
  )
  
  axis(1, cex.axis=1.7)
  axis(2, cex.axis=1.7)
  
}
dev.off()

