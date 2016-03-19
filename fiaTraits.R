library(gjam)

plotByX <- as.data.frame(read.csv('data/post/plotByX.csv'))
plotByW <- as.matrix(read.csv('data/post/plotByW.csv'))
speciesByTraits <- as.data.frame(read.csv('data/post/speciesByTraits.csv'))

traitTypes <- c(rep('CON',6), 'CAT')

tmp         <- gjamSpec2Trait(pbys = plotByW, 
                              sbyt = speciesByTraits, 
                              tTypes = traitTypes)

plotByW <- gjamReZero(forestTraits$treesDeZero)
speciesByTraits <- as.data.frame(forestTraits$specByTrait)[,c(1:10)]
traitTypes <- forestTraits$traitTypes[c(1:10)]

tmp         <- gjamSpec2Trait(pbys = plotByW, 
                              sbyt = speciesByTraits, 
                              tTypes = traitTypes)

