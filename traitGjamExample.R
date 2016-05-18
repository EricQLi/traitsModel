library(gjam)

data(forestTraits)
xdata <- forestTraits$xdata                    # n X Q
types <- forestTraits$traitTypes               # 12 trait types 
sbyt  <- forestTraits$specByTrait              # S X 12
pbys  <- gjamReZero(forestTraits$treesDeZero)  # n X S
head(sbyt)


table(sbyt$leaf)      # four levels

table(sbyt$xylem)     # diffuse/tracheid vs ring-porous

tmp         <- gjamSpec2Trait(pbys, sbyt, types)
tTypes      <- tmp$traitTypes                  # M = 15 values
u           <- tmp$plotByCWM                   # n X M
censor      <- tmp$censor                      # (0, 1) censoring, two-level CAT's
specByTrait <- tmp$specByTrait                 # S X M
M           <- ncol(u)
n           <- nrow(u)

types                     # 12 individual trait types


cbind(colnames(u),tTypes) # M trait names and types


censorList    <- gjamCensorY(values = c(0,1), intervals = cbind( c(-Inf,0),c(1,Inf) ), 
                             y = u, whichcol = c(13:14))$censor

attr(xdata$soil,'reference') <- 'other'   # reference class
attr(xdata$soil,'intType')   <- 'ref'     # intercept is reference class




tl  <- list(plotByTrait = u, traitTypes = tTypes, specByTrait = specByTrait)
rl  <- list(r = 3, N = 20)
ml  <- list(ng = 2000, burnin = 500, typeNames = 'CC', holdoutN = 20,
            traitList = tl, reductList = rl)
out <- gjamGibbs(~ temp + stdage + deficit*soil, xdata = xdata, 
                 ydata = pbys, modelList = ml)
S <- nrow(specByTrait)
boxBorder <- rep('black',S)
boxCol    <- rep('white',S)

wr <- which(specByTrait[,'ring'] == 1)                  # ring porous
wb <- which(specByTrait[,'leafneedleevergreen'] == 1)   # evergreen
ws <- which(specByTrait[,'shade'] >= 4)                 # shade tolerant
boxBorder[wr] <- 'brown'; boxCol[wr] <- 'tan'
boxBorder[ws] <- 'black'; boxCol[ws] <- 'grey'
boxBorder[wb] <- 'darkgreen'; boxCol[wb] <- 'lightgreen'

par(family = '')
pl  <- list(width=4, height=4, corLines=F, SMALLPLOTS=F,GRIDPLOTS=T,
            boxBorder = boxBorder, boxCol = boxCol)                  
gjamPlot(output = out, pl)


out$modelSummary$betaTraitMu   # Q by M coefficient matrix alpha
out$modelSummary$betaTraitSe   # Q by M coefficient std errors
out$modelSummary$sigmaTraitMu  # M by M covariance matrix omega
out$modelSummary$sigmaTraitSe  # M by M covariance std errors


out$modelSummary$tMu[1:5,]     # n by M predictive means
out$modelSummary$tSd[1:5,]     # n by M predictive std errors



#indirect and direct effect

xdrydry <- xwetdry  <- out$x[1,]
xdrydry['moisture'] <- xdrydry['deficit'] <- -1
xwetdry['moisture'] <- 1
xwetdry['deficit']  <- -1

par(mfrow=c(2,2), bty='n', mar=c(1,3,1,1), oma = c(0,0,0,0), 
    mar = c(3,2,2,1), tcl = -0.5, mgp = c(3,1,0), family='')

fit1 <- gjamIIE(output = out, xvector = xdrydry)
fit2 <- gjamIIE(output = out, xvector = xwetdry)

gjamIIEplot(fit1, response = 'leafbroaddeciduous', 
            effectMu = c('main','int'), 
            effectSd = c('main','int'), legLoc = 'bottomleft',
            ylim=c(-.1,.1))
title('deciduous')
gjamIIEplot(fit1, response = 'leafneedleevergreen', 
            effectMu = c('main','int'), 
            effectSd = c('main','int'), legLoc = 'bottomleft',
            ylim=c(-.1,.1))
title('evergreen')

gjamIIEplot(fit2, response = 'leafbroaddeciduous', 
            effectMu = c('main','int'), 
            effectSd = c('main','int'), legLoc = 'bottomleft',
            ylim=c(-.1,.1))
gjamIIEplot(fit2, response = 'leafneedleevergreen', 
            effectMu = c('main','int'), 
            effectSd = c('main','int'), legLoc = 'bottomleft',
            ylim=c(-.1,.1))

xvector <- out$x[1,]
par(mfrow=c(2,1), bty='n', mar=c(1,1,1,1), oma = c(0,0,0,0), 
    mar = c(3,2,2,1), tcl = -0.5, mgp = c(3,1,0), family='')

omitY <- colnames(u)[colnames(u) != 'leafbroaddeciduous'] # omit all but deciduous

fit <- gjamIIE(out, xvector)
gjamIIEplot(fit, response = 'leafP', effectMu = c('main','ind'), 
            effectSd = c('main','ind'), legLoc = 'topright',
            ylim=c(-.6,.6))
title('foliar P')
gjamIIEplot(fit, response = 'leafN', effectMu = c('main','ind'), 
            effectSd = c('main','ind'), legLoc = 'bottomright',
            ylim=c(-.6,.6))
title('foliar N')
