cl <- read.csv('data/pre/climate.csv')
so <- read.csv('data/pre/soilAll.csv', header = F)
ll <- read.csv('data/pre/lonLatAll.csv')

bm <- read.csv('data/pre/traitWt.csv')
tw <- bm/rowSums(bm)
  
ww <- read.csv('data/pre/wwAll.csv')$w


plotByX <- cbind(ll, cl)[ww,]
plotByX$soil <- so[ww,]

plotByW <- tw[ww,]
plotByY <- bm[ww,]

dem <- read.csv('data/pre/demAll.csv')

elev <- dem$elevation
plotByX <- cbind(plotByX, elev)

tt <- read.csv('data/pre/traitSp.csv')
traitBySpecies <- tt
rownames(traitBySpecies) <- c('SM','WD','MH','N','P','SLA','Deciduous','BLEver','NLEver')

speciesByTraits <- t(traitBySpecies)

leaf <- colnames(speciesByTraits)[7:9][rowSums(speciesByTraits[,7:9]*matrix(1:3, nrow = 65, ncol = 3, byrow = T))]
speciesByTraits <- cbind(speciesByTraits[,1:6],leaf )
# cwt.m <- read.csv('data/pre/traitMuAll.csv')
# cwt.v <- read.csv('data/pre/traitSdAll.csv')



source('~/Projects/procVisData/geoSpatial.R')
library(raster)
library(data.table)
library(tools)

### readin slope aspect
allPlotDEM <- read.csv('data/pre/allPlotDEMdata.csv')

allPlotDEM[which(is.na(allPlotDEM$slope)), c("slope","aspect")] <- 0

plotNames <- matrix(unlist(strsplit(rownames(plotByX), split = '_ereg_')), ncol = 2, byrow = T)[,1]
allPlotDEM$X <- as.character(allPlotDEM$X)
w <-match(plotNames, allPlotDEM$X)
plotByX <- cbind(plotByX, allPlotDEM[w,4:6])
plotByX <- as.data.table(plotByX)

physioShape <- shapefile('data/maps/physioProvinceLatLon/physioProvinceLatLon.shp')
wPoly <- whichPolygon(plotByX[,.(plotLon, plotLat)], physioShape)
physioRegion <- as.factor(toTitleCase(tolower(physioShape@data$PROVINCE[wPoly])))
table(physioRegion)

ecoShape <- shapefile('data/maps/ecoregions/eco_us_latlon.shp')
wEco <- whichPolygon(plotByX[,.(plotLon, plotLat)], ecoShape)
ecoRegion <- as.factor(toTitleCase(tolower(ecoShape@data$PROVINCE[wEco])))
table(ecoRegion)

plotByX$ecoRegion <- ecoRegion
plotByX$physioRegion <- physioRegion
exposure <- slopeAspectToExposure(plotByX$slope, plotByX$aspect, degree = T)

plotByX <- cbind(plotByX, exposure)

write.table(plotByX, 'data/post/plotByX.csv', sep = ',')
write.table(plotByW, 'data/post/plotByW.csv', sep = ',')
write.table(plotByY, 'data/post/plotByY.csv', sep = ',')
write.table(speciesByTraits, 'data/post/speciesByTraits.csv', sep = ',')


