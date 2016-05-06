library(gjam)

plotByX <- as.data.frame(read.csv('data/post/plotByX.csv'))
plotByY <- as.matrix(read.csv('data/post/plotByY.csv'))
plotByW <- as.matrix(read.csv('data/post/plotByW.csv'))
speciesByTraits <- as.data.frame(read.csv('data/post/speciesByTraits.csv'))

#load(file = 'bijan.RData')



traitTypes <- c(rep('CON',6), rep('CAT', 1))

tmp         <- gjamSpec2Trait(pbys = plotByY, 
                              sbyt = speciesByTraits, 
                              tTypes = traitTypes)
tTypes      <- tmp$traitTypes
u           <- tmp$plotByCWM
censor      <- tmp$censor
specByTrait <- tmp$specByTrait
M           <- ncol(u)
n           <- nrow(u)
colnames(u)
traitTypes
tTypes

traitList <- list(plotByTrait = tmp$plotByCWM, 
                  traitTypes = tmp$traitTypes, 
                  specByTrait = tmp$specByTrait)


reductList <- list(r = 3, N = 20)

modelList <- list(ng=10, 
                  burnin=1,
                  typeNames = 'CA', 
                  holdoutN = 20,
                  reductList = reductList,
                  traitList = traitList)
xdata <- plotByX
ydata <- plotByY

attr(xdata$soil,'reference') <- 'Others'   # reference class
attr(xdata$soil,'intType')   <- 'ref'

# wRare <- which(colnames(ydata)%in%c('acerBarb', 'betuNigr', 'caryAqua', 'magnGran', 'persBorb', 'pinuPalu', 'querLaur', 'querMari', 'querMich', 'magnAcum', 'magnFras'))
# ydata <- ydata[,-wRare]

output  <- gjamGibbs(~ temp  + moisture,
                     xdata = xdata, 
                     ydata = ydata, 
                     modelList = modelList)

summary(output$modelSummary$tMu)



plotPars  <- list(width=4, height=4, corLines=F,
                  SMALLPLOTS=F, CLUSTERPLOTS=T)                  

fit       <- gjamPlot(output = output, plotPars)

output$modelSummary$betaTraitMu   # Q by M coefficient matrix alpha
output$modelSummary$betaTraitSe   # Q by M coefficient std errors
output$modelSummary$sigmaTraitMu  # M by M covariance matrix omega
output$modelSummary$sigmaTraitSe  # M by M covariance std errors

output$modelSummary$tMu[1:5,]     # n by M predictive means
output$modelSummary$tSd[1:5,]   # n by M predictive std errors

