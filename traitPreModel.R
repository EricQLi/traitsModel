library(gjam)

plotByX <- as.data.frame(read.csv('data/post/plotByX.csv'))
plotByY <- as.matrix(read.csv('data/post/plotByY.csv'))
plotByW <- as.matrix(read.csv('data/post/plotByW.csv'))
speciesByTraits <- as.data.frame(read.csv('data/post/speciesByTraits.csv'))

traitTypes <- c(rep('CON',6), rep('CAT', 1))

traitData         <- gjamSpec2Trait(pbys = plotByY, 
                                    sbyt = speciesByTraits, 
                                    tTypes = traitTypes)



