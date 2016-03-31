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



censorList    <- gjamCensorY(values = c(0,1), intervals = cbind( c(-Inf,0),c(1,Inf) ),
                             y = u, whichcol = c(13:14))$censor


###TRM
modelList <- list(ng = 2000, burnin = 500, typeNames = tTypes,
                  holdoutN = 20, censor=censor, notStandard = c('u1','u2','u3'))
output    <- gjamGibbs(~ temp + u1 + u2 + u3 + moisture*(deficit+soil),
                       xdata = xdata, ydata = u, 
                       modelList = modelList)
tnames    <- colnames(u)
boxBorder <- rep('black', M)                          # highlight types
boxCol    <- rep('grey',M)
wo <- which(tnames %in% c("leafN","leafP","SLA") )    # foliar traits
wf <- grep("leaf_",tnames)                            # leaf habit
wc <- which(tnames %in% c("woodSG","xylem_ring") )    # wood anatomy

boxBorder[wc] <- 'brown'; boxCol[wc] <- 'tan'
boxBorder[wf] <- 'darkblue'; boxCol[wf] <- 'lightblue'
boxBorder[wo] <- 'darkgreen'; boxCol[wo] <- 'lightgreen'

plotPars  <- list(width = 3, height = 3, GRIDPLOTS = TRUE,
                  boxBorder = boxBorder, boxCol = boxCol)           
fit       <- gjamPlot(output, plotPars)


output$modelSummary$betaMu      # Q by M coefficient matrix alpha
output$modelSummary$betaSe      # Q by M coefficient std errors
output$modelSummary$sigMu       # M by M covariance matrix omega
output$modelSummary$sigSe       # M by M covariance std errors


fit$betaEstimates[1:5,]      # Q by M coefficient matrix alpha


##PTM

traitList <- list(plotByTrait = u, traitTypes = tTypes, 
                  specByTrait = specByTrait)
reductList <- list(r = 3, N = 20)

modelList <- list(ng=2000, burnin=500, typeNames = 'CC', 
                  holdoutN = 20,
                  traitList = traitList, reductList = reductList)

output  <- gjamGibbs(~ temp + stdage + deficit*soil, 
                     xdata = xdata, 
                     ydata = plotByTrees, modelList = modelList)

S <- ncol(plotByTrees)
boxBorder <- rep('black',S)
boxCol <- rep('white',S)

wr <- which(specByTrait[,'xylem_ring'] == 1)  # ring porous
wb <- which(specByTrait[,'leaf_other'] == 1)  # broadleaf evergreen
boxBorder[wr] <- 'brown'; boxCol[wr] <- 'tan'
boxBorder[wb] <- 'darkgreen'; boxCol[wb] <- 'lightgreen'

plotPars  <- list(width=4, height=4, corLines=F,
                  SMALLPLOTS=F,GRIDPLOTS=T,
                  boxBorder = boxBorder, boxCol = boxCol)                  
fit       <- gjamPlot(output = output, plotPars)


output$modelSummary$betaTraitMu   # Q by M coefficient matrix alpha
output$modelSummary$betaTraitSe   # Q by M coefficient std errors
output$modelSummary$sigmaTraitMu  # M by M covariance matrix omega
output$modelSummary$sigmaTraitSe  # M by M covariance std errors


output$modelSummary$tMu[1:5,]     # n by M predictive means
output$modelSummary$tSd[1:5,]   # n by M predictive std errors