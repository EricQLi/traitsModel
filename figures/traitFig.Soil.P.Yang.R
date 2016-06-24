library(RNetCDF)
library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitFunctions.R')
source('figures/traitColorSet.R')

fid <- open.nc('data/GLOBAL_PHOSPHORUS_DIST_MAP_1223/data/pforms_den.nc')

dataPhos <-read.nc(fid)

dataPhos$Lon <- rep(dataPhos$lon, times=360)
dataPhos$Lat <- rep(dataPhos$lat, each=720)

usShape <- shapefile('data/maps/states/states.shp')
wPl <- !is.na(whichPolygon(cbind(dataPhos$Lon, dataPhos$Lat), usShape))

wbox <- dataPhos$Lat>=min(plotByX$plotLat) &  
  dataPhos$Lat<=max(plotByX$plotLat) &
  dataPhos$Lon>=min(plotByX$plotLon) &  
  dataPhos$Lon<=max(plotByX$plotLon) 

png('figures/traitFig.Soil.P.Yang.png', units='in',res=300, height  = 10, width=15)
par(mfrow=c(2,3))#,oma = c(3,7,5,5),mar = c(0,0,0,0))

for(j in 1:6){
  ssj <- switch (j,   
                 dataPhos$tot, 
                 dataPhos$lab, 
                 dataPhos$org, 
                 dataPhos$occ, 
                 dataPhos$sec, 
                 dataPhos$apa
  )
  
  par(xaxt='n', yaxt='n')
  valRange <- quantile(ssj[wbox&wPl], probs=seq(.025,.975, length.out = 10), na.rm=T)
  
  mapColorData(x=dataPhos$Lon[wbox&wPl],y =  dataPhos$Lat[wbox&wPl],  data = ssj[wbox&wPl], 
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
  
  mtext(text = switch (j,  'Total P', 
                       'Labile Inorganic P',
                       'Organic P',
                       'Occluded P',
                       'Seconday Mineral  P',
                       'Apatite P'
                       ),
        side = 3, cex = 2
        )
  
  axis(1, cex.axis=1.7)
  axis(2, cex.axis=1.7)
  
}
dev.off()

