library(raster)
library(data.table)
library(maps)

usSoil <- raster('~/Google Drive/forOutside/soil3Extent/soilRaster3x.tif')
soilCols <- usSoil@data@attributes[[1]]
splt <- strsplit(as.character(soilCols$ORDER3), split = '_')
splt <- lapply(splt, function(x){if(length(x)==2) x[[3]]<-''; x})
splt <- unlist(splt)
splt[splt==''] <- NA
splt <- matrix(splt, ncol=3, byrow = T)
colnames(splt) <- c('order','suborder','greatgroup')
splt <- as.data.frame(splt)

soilColsTable <- cbind(soilCols, splt)
soilColsTable <- as.data.table(soilColsTable)


soilColsTable$SoilColor <- '#CCCCCC'
soilColsTable[order=='Alfisols', SoilColor:='#9ed824']
soilColsTable[order=='Andisols', SoilColor:='#fc00ab']
soilColsTable[order=='Aridisols', SoilColor:='#ffd2ab']
soilColsTable[order=='Entisols', SoilColor:='#47d4d7']
soilColsTable[order=='Gelisols', SoilColor:='#289bb4']
soilColsTable[order=='Histosols', SoilColor:='#911a20']
soilColsTable[order=='Inceptisols', SoilColor:='#cd533d']
soilColsTable[order=='Mollisols', SoilColor:='#118207']
soilColsTable[order=='Oxisols', SoilColor:='#fd0006']
soilColsTable[order=='Spodosols', SoilColor:='#ceb0ce']
soilColsTable[order=='Ultisols', SoilColor:='#fe9507']
soilColsTable[order=='Vertisols', SoilColor:='#ffff0b']

# plot(usSoil, xlim=c(-132,-66), ylim=c(22,52), col=soilColsTable$SoilColor[])
# map('state', add=T)


soilColsTable$SoilColor <- '#EEEEEE'
soilColsTable[order=='Alfisols'|order=='Inceptisols', c('SoilColor','Name'):=list('#6c6c6c', 'AlfInc')]
soilColsTable[order=='Ultisols', c('SoilColor','Name'):=list('#a59ec2', 'Ult')]
soilColsTable[order=='Ultisols'&greatgroup=='Kanhapludults', c('SoilColor','Name'):=list('#7ccbbb', 'UltKan')]
soilColsTable[order=='Spodosols'|order=='Histosols',  c('SoilColor','Name'):=list('#f8695f', 'SpodHist')]
soilColsTable[order=='Mollisols', c('SoilColor','Name'):=list('#6da0c8', 'Mol')]
soilColsTable[order=='Entisols'|order=='Vertisols', c('SoilColor','Name'):=list('#faa550', 'EntVert')]


# plot(usSoil, col=soilColsTable$SoilColor[])
# map('state', add=T)
# map('us', add=T)
# 
# plot(usSoil, xlim=c(-102,-66), ylim=c(22,52), col=soilColsTable$SoilColor)
# map('us', add=T)

usSoilP <- rasterToPoints(usSoil)
usSoilDF <- as.data.frame(usSoilP)

colnames(usSoilDF) <- c('Longitude','Latitude','Soil')
usSoilDF <- cbind(usSoilDF, soilColsTable[usSoilDF$Soil, .(ORDER3, order, suborder,greatgroup, SoilColor, Name)])
head(usSoilDF)

library(ggplot2)

ggSoil <-ggplot(usSoilDF, aes(Longitude, Latitude)) +
  geom_raster(aes(fill= Name)) +
  guides(fill=guide_legend(title='Soil Type')) +
  xlim(-125,-68) +
  ylim(25,50)

ggSoil +  theme_linedraw()



# usSoilDT <- as.data.table(usSoilDF)
# 
# usSoilDTCropped <- usSoilDT[Longitude>- 100&Longitude< -67 &Latitude>25&Latitude<49,
#                             .(Longitude, Latitude, Name, SoilColor)]
# 
# library(akima)
# usSoilDTCropped.Apprx <- interp(usSoilDTCropped$Longitude, usSoilDTCropped$Latitude,usSoilDTCropped$Name, 
#                                   )
#   
  
  
  