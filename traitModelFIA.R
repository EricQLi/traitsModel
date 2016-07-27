library(gjam)
library(data.table)

set.seed(2016)
source('traitPreModel.R')

traitData  <- gjamSpec2Trait(pbys = plotByW, 
                             sbyt = speciesByTraits, 
                             tTypes = traitTypes)

traitList <- list(plotByTrait = traitData$plotByCWM, 
                  traitTypes = traitData$traitTypes, 
                  specByTrait = traitData$specByTrait)

reductList <- list(r = 3, N = 20)

modelList <- list(ng=8000, 
                  burnin=4000,
                  typeNames = 'FC', 
                  holdoutN = 20,
                  reductList = reductList,
                  traitList = traitList)

set.seed(2016)


output  <- gjamGibbs(~ temp + moisture + deficit + soil +u1 + u2 + u3 + 
                      #I(moisture^2) + 
                       moisture*deficit + 
                       moisture*soil + 
                       temp*soil + 
                       deficit*soil,
                     
                     xdata = plotByX,
                     ydata = plotByW,
                     modelList = modelList)

save.image(paste('output-mainModel',
                 modelList$ng/1000, modelList$burnin/1000,
                 substring(make.names(Sys.time()),2),
                 '.RData', sep = '-'))


View(output$modelSummary$sigmaTraitMu)
View(output$modelSummary$betaTraitMu)

# wf <- grep("leaf",tnames)                              # leaf habit
# wc <- which(tnames %in% c("WD","MH","SM") ) # wood anatomy
# 
# boxBorder[wc] <- 'brown';     boxCol[wc] <- 'tan'
# boxBorder[wf] <- 'darkblue';  boxCol[wf] <- 'lightblue'
# boxBorder[wo] <- 'darkgreen'; boxCol[wo] <- 'lightgreen'
# 
# pl  <- list(width = 3, height = 3, GRIDPLOTS = TRUE, plotAllY = T,
#             boxBorder = boxBorder, boxCol = boxCol, 
#             SMALLPLOTS = F, SAVEPLOTS=T, sigOnly=F, 
#             sdScaleX = T, sdScaleY = T)

# fit <- gjamPlot(output = output, plotPars = pl)

