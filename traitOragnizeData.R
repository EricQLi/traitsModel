cl <- read.csv('data/pre/climate.csv')
so <- read.csv('data/pre/soilAll.csv', header = F)
ll <- read.csv('data/pre/lonLatAll.csv')

bm <- read.csv('data/pre/traitWt.csv')
tw <- bm/rowSums(bm)
  
ww <- read.csv('data/pre/wwAll.csv')$w


plotByX <- cbind(ll, cl, so)[ww,]
plotByW <- tw[ww,]

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

write.table(plotByX, 'data/post/plotByX.csv', sep = ',')
write.table(plotByW, 'data/post/plotByW.csv', sep = ',')
write.table(speciesByTraits, 'data/post/speciesByTraits.csv', sep = ',')

