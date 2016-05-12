library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitPreModel.R')
source('traitFunctions.R')
source('figures/traitColorSet.R')


png('figures/traitFig.Decid-Ever.png', units='in',res=300, height  = 10, width=15)
par(mfrow=c(2,3),oma = c(3,7,5,5),mar = c(0,0,0,0))

for(i in 1:2){
  for(j in 1:3){
    
    ssj.obs <- traitMuAll[,c(7,8,9)][,j]     #observed traits
    ssj.pred <- output$modelSummary$tMu[,c(8,7,9)][,j] #predicted traits
    
    ssj <- switch(i, ssj.obs, ssj.pred)
    ssj <-  (ssj>=switch(j, 0.9, .01, .1))*1
    # summary(ssj.pred)
    ssj[ssj==0] <- NA
    if(all(is.na(ssj))) ssj=0
    par(xaxt='n', yaxt='n')
    mapColorData(plotByX$plotLon, plotByX$plotLat, ssj, 
                 xlim = range(plotByX$plotLon), ylim = range(plotByX$plotLat),
                 valRange = c(0,1),colList = c('white','#1A9850A0'), ADD=F, cex.all = 2, legendShow = F )
    mapOutlines(glacialLine, ecoRegion, lwd2 = 8)
    
    if(i==1)mtext(text =  switch(j-6, 'Deciduous','BL Evergreen','NL Evergreen'), side = 3, line = 2, cex=2, font=2)
    if(i==1)mtext(text =  switch(j-6, '(a)','(b)','(c)'), side = 1, line = -2,at = -68, cex=2, font=2)
    if(i==2)mtext(text =  switch(j-6, '(d)','(e)','(f)'), side = 1, line = -2,at = -68, cex=2, font=2)
    
    if(i==2)
    {
      par(xaxt='s', yaxt='s')
      # axis(1, cex.axis=1.7)
      par(xaxt='n', yaxt='n')
    }
    if(j==1)
    {
      par(xaxt='s', yaxt='s')
      # axis(2, cex.axis=1.7)
      par(xaxt='n', yaxt='n')
      mtext(text = switch(i, 'Observed', 'Predicted'), side = 2, cex=2, line = 3, font=2)
    }
    
  }
}
dev.off()
