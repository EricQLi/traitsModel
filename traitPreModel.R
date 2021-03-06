
plotByX <- as.data.frame(read.csv('data/post/plotByX.csv'))
soilGrids <- as.data.frame(read.csv('data/post/soilGrids.ext.csv'))
plotByX <- cbind(plotByX, soilGrids)

attr(plotByX$soil,'reference') <- 'Others'   # reference class
attr(plotByX$soil,'intType')   <- 'ref'

plotByX$soil5 <- plotByX$soil

plotByX$soil4 <- plotByX$soil
plotByX$soil4[plotByX$soil=='AlfInc'] <- 'Others' 
plotByX$soil4 <- as.factor(as.character(plotByX$soil4))

attr(plotByX$soil4,'reference') <- 'Others'   # reference class
attr(plotByX$soil4,'intType')   <- 'ref'

# plotByX <- plotByX[,-c('ecoRegion')]
# plotByX <- plotByX[,-"physioRegion"]

plotByY <- as.matrix(read.csv('data/post/plotByY.csv'))
plotByW <- as.matrix(read.csv('data/post/plotByW.csv'))
speciesByTraits <- as.data.frame(read.csv('data/post/speciesByTraits.csv'))


traitMuAll <- read.csv('data/post/traitMuAll.csv')
traitSdAll <- read.csv('data/post/traitSdAll.csv')



traitTypes <- c(rep('CON',6), rep('CAT', 1))



rareSpecies <- which(colSums(plotByY>0)/nrow(plotByY)<.02)
rareSpecies


sdTraits <- apply(plotByW%*%as.matrix(speciesByTraits[,1:6]), 2, sd)
sdTraits <- c(sdTraits, rep(1,3))
names(sdTraits)[7:9] <- c('leafother', 'leafDeciduous', 'leafNLEver')
sdTraits