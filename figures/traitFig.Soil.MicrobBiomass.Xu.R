library(RNetCDF)
library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitFunctions.R')
source('figures/traitColorSet.R')

fid <- open.nc('data/GLOBAL_MICROBIAL_BIOMASS_C_N_P_1264/data/Global_Soil_Microbial_BiomassCN.nc')

dataMicrobBio <-read.nc(fid)

dataMicrobBio$Lon <- rep(dataMicrobBio$lon, times=360)
dataMicrobBio$Lat <- rep(dataMicrobBio$lat, each=720)

usShape <- shapefile('data/maps/states/states.shp')
wPl <- !is.na(whichPolygon(cbind(dataMicrobBio$Lon, dataMicrobBio$Lat), usShape))

wbox <- dataMicrobBio$Lat>=min(plotByX$plotLat) &  
  dataMicrobBio$Lat<=max(plotByX$plotLat) &
  dataMicrobBio$Lon>=min(plotByX$plotLon) &  
  dataMicrobBio$Lon<=max(plotByX$plotLon) 

png('figures/traitFig.Soil.MicrobBiomass.Xu.png', units='in',res=300, height  = 10, width=15)
par(mfrow=c(2,3),oma = c(3,7,5,5),mar = c(0,0,0,0))

for(j in 1:6){
  ssj <- switch (j,   
                 dataMicrobBio$SMC30cm, 
                 dataMicrobBio$SMN30cm, 
                 dataMicrobBio$CN30cm, 
                 dataMicrobBio$SMC100cm, 
                 dataMicrobBio$SMN100cm, 
                 dataMicrobBio$CN100cm)
  
  par(xaxt='n', yaxt='n')
  valRange <- quantile(ssj[wbox&wPl], probs=seq(.025,.975, length.out = 100), na.rm=T)
  
  mapColorData(x=dataMicrobBio$Lon[wbox&wPl],y =  dataMicrobBio$Lat[wbox&wPl],  data = ssj[wbox&wPl], 
               symSize = 1.5,
               xlim = range(plotByX$plotLon), equiLatLon = T, 
               ylim = range(plotByX$plotLat),
               valRange = valRange,
               #legend.txt = paste0(signif(range(valRange),2), ' (g/m^2)'),
               colList =colList.GiantGoldFish,
               ADD=F, cex.all = 2, #legendShow = F 
  )
  
  # lines(glacialLine[-nrow(glacialLine),1],glacialLine[-nrow(glacialLine),2],lwd=6,col='white')
  # lines(glacialLine[-nrow(glacialLine),1],glacialLine[-nrow(glacialLine),2],lwd=2,col='darkgrey')
  #par(xaxt='s', yaxt='s')
  
  mtext(text = switch (as.character(j),  
                       '1'='Carbon',
                       '2'='Nitrogen',
                       '3'='C:N',''), side = 3, cex = 2, line = 2  )
  mtext(text = switch (as.character(j),  
                       '1'='30 cm',
                       '4'='100 cm'), side = 2, cex = 2, line = 2  )
  axis(1, cex.axis=1.7)
  axis(2, cex.axis=1.7)
  
}
dev.off()



