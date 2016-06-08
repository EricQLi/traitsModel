library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitFunctions.R')
source('figures/traitColorSet.R')

CWT <- getCWT.Mass.Area(output, speciesByTraits, plotByW)

png('figures/traitFig.CondOnLeaf.png', units='in',res=300, height  = 15, width=15)
par(mfrow=c(3,3),oma = c(5,6,5,2), mar=c(2,2,1,1))
for(i in 1:3){
  for(j in 4:6){
    ssj.pred <- CWT$pred[,j]
    ssj.dec <- CWT$cond$condDecid[,j]
    ssj.ever <- CWT$cond$condEver[,j]
    
    ssj <- switch(i, ssj.pred, ssj.dec, ssj.ever)
    
    par(xaxt='n', yaxt='n')
    
    mapColorData(plotByX$plotLon, plotByX$plotLat, ssj, 
                 valRange = quantile(switch(i, c(ssj.pred,ssj.dec) ,c(ssj.pred,ssj.dec) , ssj.ever), probs=seq(0.1,.9, length.out = 100 )),
                 # valRange = quantile( c(ssj.pred,ssj.dec, ssj.ever), probs=seq(0.05,.95, length.out = 100 )),
                 # valRange = quantile( ssj, probs=seq(0.05,.95, length.out = 100 )),
                 xlim = range(plotByX$plotLon), ylim = range(plotByX$plotLat),
                 colList = paste0(colList.purpleOrange, '10'), symSize =1 , symPch = 16,
                 cex.all = 3)
    
    #mapOutlines(glacialLine, mapRegion)
    title.txt <-  switch(j-3, 
                         bquote(.(tNames[j-3])~ (mg/g)),
                         bquote(.(tNames[j-3])~ (mg/g)),
                         bquote(.(tNames[j-3])~ (cm^2/g)))
    
    if(i==1) mtext(text = title.txt, side = 3, line = 1, cex=2.5, font = 2)
    
    if(i==1) mtext(text =  switch(j-3, '(a)','(b)','(c)'), side = 3, line = -2.5, at = -68, cex=2, font = 2)
    if(i==2) mtext(text =  switch(j-3, '(d)','(e)','(f)'), side = 3, line = -2.5, at = -68, cex=2, font = 2)
    if(i==3) mtext(text =  switch(j-3, '(g)','(h)','(i)'), side = 3, line = -2.5, at = -68, cex=2, font = 2)
    
    if(j==4) mtext(text =  switch(i, 'Community','Deciduous','Evergreen'), side = 2, line = 4, cex=2, font = 2)
    
    
    par(xaxt='s', yaxt='s')
    
    if(j==4)axis(2, cex.axis=2)
    if(i==3)axis(1, cex.axis=2)
  }
}
dev.off()
############