glacialLine <- as.matrix( read.table('data/maps/glacialLimit.txt',header=T) )

glacial <- point.in.polygon(plotByX$plotLon, 
                            plotByX$plotLat, 
                            glacialLine[,1], 
                            glacialLine[,2])
wglacial <- which(glacial == 1)

glacialLine <- glacialLine[1:51,]
mapRegion <- shapefile('data/maps/ecoregions/eco_us_latlon_provMerged.shp')
ecoRegion <- shapefile('data/maps/ecoregions/eco_us_latlon.shp')

tNames <- c('Leaf N','Leaf P','SLA')

colList.orangePurple <- c('#e66101', '#EEAC7C', '#f7f7f7', '#AA99C8', '#5e3c99')
colList.brownGreen   <- c('#a6611a', '#dfc27d', '#f5f5f5', '#80cdc1', '#018571')
colList.redGreen <- c("#D73027", "#FDB671", "#FFFFBF", "#B4DF75", "#1A9850")
colList.brownGreyGreen <- c('#8c510a', '#d8b365', '#d5d5d5', '#5ab4ac', '#01665e')

colList.purpleOrange <- rev(colList.orangePurple)
colList.greenBrown <- rev(colList.brownGreen)
colList.greenRed <- rev(colList.redGreen)
colList.greenGreyBrown <- rev(colList.brownGreyGreen)

colList <- colList.purpleOrange

colList.orangeGreyPurple <- colorRampPalette(colList.orangePurple)(13)
colList.orangeGreyPurple[7] <- 'grey'

colList.SurfAndTurf <- c('#20948B','#6AB187','#f7f7f7','#F4CC70','#DE7A22')
colList.SereneAndSpaLike <- c('#ED5752','#E2DFA2','#92AAC7')
colList.FunAndTropical <- c('#FA6E59','#F8A055','#FFDB5C','#92AAC7','#4897D8')
colList.WarmAndRustic <- c('#805A3B','#C60000','#FD974F','#FEF2E4')
colList.DistinctiveAndUnexpected <- c('#52908B','#E5E2CA','#E7472E')
colList.GiantGoldFish <- c('#6ed3e7','#a9dcdb','#e1e5ce','#e1e5ce','#f86a20')


# colListSoil <- c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f')
colListSoil<- c('#8dd3c7','#7f7f7f','#bebada','#fb8072','#80b1d3','#fdb462')
soilType <- c("UltKan","AlfInc","Ult","SpodHist","Mol","EntVert")
#  scaleSym <- sm
scaleSym <- .3
soilColTable <- cbind(soilType, colListSoil)




colRegion <- rep(NA, 52)
# wc <- c(1,17, 21,22,23,25,26,30,31,35,36,37,41,42,46)
wc <- c(1,17, 21,22,31,36,46)
wc <- c(1,17, 21,22,23,30, 31,35, 36,37,41,42, 46)
mapRegion$PROVINCE[wc]

# colRegion[wc] <- strsplit('#1b9e77
# #d95f02
# #7570b3
# #e7298a
# #66a61e
# #e6ab02
# #a6761d', '\n')[[1]]
mapRegionPROVINCE <- c('Adirondack','Appalachian','E Broadleaf C.','E Broadleaf O.',
                       'Everglades','Laurentian','Mississippi','Ouachita','Coastal','Ozark',
                       'Prairie S.','Prairie T.','Southeastern')
colRegion[wc] <- colorRampPalette(colList.greenRed[-3])(length(wc))

