library(gjam)
data(forestTraits)
xdata <- forestTraits$xdata                    # n X Q
types <- forestTraits$traitTypes               # 12 trait types 
sbyt  <- forestTraits$specByTrait              # S X 12
pbys  <- gjamReZero(forestTraits$treesDeZero)  # n X S
head(sbyt)

table(sbyt$leaf)      # four levels
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

attr(xdata$soil,'reference') <- 'reference'   # reference class
attr(xdata$soil,'intType')   <- 'ref'         # intercept is reference class





tl  <- list(plotByTrait = u, traitTypes = tTypes, specByTrait = specByTrait)
rl  <- list(r = 3, N = 20)
ml  <- list(ng = 2000, burnin = 500, typeNames = 'CC', holdoutN = 20,
            traitList = tl, reductList = rl)
out <- gjamGibbs(~ temp + moisture + deficit  + soil +u1 + u2 + u3 +
                   moisture*deficit + moisture*soil + 
                   temp*soil + deficit*soil + deficit*temp,
                 xdata = xdata, 
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

save(out, file = 'outExample.RData')





