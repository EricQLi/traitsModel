library(gjam)

set.seed(2016)
source('traitPreModel.R')

traitData  <- gjamSpec2Trait(pbys = plotByW, 
                             sbyt = speciesByTraits, 
                             tTypes = traitTypes)

traitList <- list(plotByTrait = traitData$plotByCWM, 
                  traitTypes = traitData$traitTypes, 
                  specByTrait = traitData$specByTrait)

reductList <- list(r = 3, N = 30)

modelList <- list(ng=2000, 
                  burnin=1000,
                  typeNames = 'FC', 
                  holdoutN = 20,
                  reductList = reductList,
                  traitList = traitList)

attr(plotByX$soil,'reference') <- 'Others'   # reference class
attr(plotByX$soil,'intType')   <- 'ref'

output  <- gjamGibbs(~ 
                     #temp  +  deficit + moisture + therm + soil 
                     #u1 + u2 + u3 + 
                     #u1*temp + u2*temp + u3*temp
                     #deficit*therm +
                     #deficit*temp + therm*temp +		      
                     #deficit*moisture + temp*moisture + therm*moisture +		      
                     #moisture*soil + deficit*soil + temp*soil + therm*soil
                     ,   
                     xdata = plotByX, 
                     ydata = plotByW, 
                     modelList = modelList)

output$modelSummary$DIC
output$modelSummary$score


