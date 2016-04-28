library(gjam)

plotByX <- as.data.frame(read.csv('data/post/plotByX.csv'))
plotByY <- as.matrix(read.csv('data/post/plotByY.csv'))
plotByW <- as.matrix(read.csv('data/post/plotByW.csv'))
speciesByTraits <- as.data.frame(read.csv('data/post/speciesByTraits.csv'))

traitTypes <- c(rep('CON',6), rep('CAT', 1))

traitData         <- gjamSpec2Trait(pbys = plotByY, 
                                    sbyt = speciesByTraits, 
                                    tTypes = traitTypes)

traitList <- list(plotByTrait = traitData$plotByCWM, 
                  traitTypes = traitData$traitTypes, 
                  specByTrait = traitData$specByTrait)


reductList <- list(r = 3, N = 30)

modelList <- list(ng=100, 
                  burnin=50,
                  typeNames = 'CA', 
                  holdoutN = 20,
                  reductList = reductList,
                  traitList = traitList)


attr(plotByX$soil,'reference') <- 'Others'   # reference class
attr(plotByX$soil,'intType')   <- 'ref'

