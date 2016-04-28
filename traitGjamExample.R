library(gjam)
data(forestTraits)
xdata       <- forestTraits$xdata
traitTypes  <- forestTraits$traitTypes
plotByTrees <- gjamReZero(forestTraits$treesDeZero)  # re-zero data
tmp         <- gjamSpec2Trait(pbys = plotByTrees, 
                              sbyt = forestTraits$specByTrait, 
                              tTypes = forestTraits$traitTypes)
tTypes      <- tmp$traitTypes
u           <- tmp$plotByCWM
censor      <- tmp$censor
specByTrait <- tmp$specByTrait
M           <- ncol(u)
n           <- nrow(u)
colnames(u)
traitTypes
tTypes


censorList    <- gjamCensorY(values = c(0,1), 
                             intervals = cbind( c(-Inf,0),c(1,Inf) ),
                             y = u, whichcol = c(13:14))$censor


traitList <- list(plotByTrait = u, traitTypes = tTypes, 
                  specByTrait = specByTrait)

reductList <- list(r = 3, N = 20)

modelList <- list(ng=100, burnin=50, typeNames = 'CC', holdoutN = 20,
                  traitList = traitList, reductList = reductList)

output  <- gjamGibbs(~ temp + stdage + deficit*soil, xdata = xdata, 
                     ydata = plotByTrees, modelList = modelList)

S <- ncol(plotByTrees)
boxBorder <- rep('black',S)
boxCol    <- rep('white',S)

wr <- which(specByTrait[,'xylem_ring'] == 1)            # ring porous
wb <- which(specByTrait[,'leaf_needleevergreen'] == 1)  # evergreen
ws <- which(specByTrait[,'shade'] >= 4)                 # shade tolerant
boxBorder[wr] <- 'brown'; boxCol[wr] <- 'tan'
boxBorder[ws] <- 'black'; boxCol[ws] <- 'grey'
boxBorder[wb] <- 'darkgreen'; boxCol[wb] <- 'lightgreen'

plotPars  <- list(width=4, height=4, corLines=F, SMALLPLOTS=F,GRIDPLOTS=T,
                  boxBorder = boxBorder, boxCol = boxCol)                  
fit       <- gjamPlot(output = output, plotPars)