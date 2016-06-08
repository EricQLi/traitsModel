require(data.table)

# betachains <- output$chains$agibbs
# burnin <- floor(nrow(betachains)/2)
# includeInteractions <- F
# traitsToPlot <- c('N','P','SLA') 
# predictorsToPlot <- NULL
# onlySignificant <- T

plotSensitivity <- function(pngName, txtTitle, paramSensList, traitNames, colList=rev(colList.SurfAndTurf)){
  
  png(pngName, units='in',res=300, height  = 5, width=15)
  par(mfrow=c(1,3), oma=c(0.0,0,2,0))
  for(j in 4:6){
    
    ssj <- paramSensList[[j-3]]$mean
    
    ww <- paramSensList[[j-3]]$sign!=0
    
    par(xaxt='n', yaxt='n')
    mapColorData(x = plotByX$plotLon[ww], y = plotByX$plotLat[ww], data = ssj[ww],
                 valRange = quantile(ssj, probs = seq(0.05,.95, by = .1), na.rm = T), 
                 cex.all = 2, colList = colList, symSize=.7 )
    
    mtext(text = traitNames[j], side = 3, line = .7, cex=2)
    mtext(text =  switch(j-3, '(a)','(b)','(c)'), side = 3, line = 1.5,at = -100, cex=2)
    
    par(xaxt='s', yaxt='s')
    axis(1, cex.axis=1.7)
    axis(2, cex.axis=1.7)
  }
  mtext(outer = T, side = 3, text = txtTitle, cex = 1.6)
  dev.off()
  
}
plotSensitivitySign <- function(pngName, txtTitle, paramSensList, traitNames, colList=c( "#5e3c99", "#f7f7f7", "#e66101")){
  
  png(pngName, units='in',res=300, height  = 5, width=15)
  par(mfrow=c(1,3), oma=c(0.0,0,2,0))
  for(j in 4:6){
    
    ssj <- paramSensList[[j-3]]$sign
    ww <- ssj!=0
    par(xaxt='n', yaxt='n')
    mapColorData(x = plotByX$plotLon[ww], y = plotByX$plotLat[ww], data = ssj[ww],
                 #valRange = quantile(ssj, probs = seq(0.05,.95, by = .1), na.rm = T), 
                 valRange = c(-1,0,1),
                 cex.all = 2, colList = colList, symSize=.7)
    
    mtext(text = traitNames[j], side = 3, line = .7, cex=2)
    mtext(text =  switch(j-3, '(a)','(b)','(c)'), side = 3, line = 1.5,at = -100, cex=2)
    
    par(xaxt='s', yaxt='s')
    axis(1, cex.axis=1.7)
    axis(2, cex.axis=1.7)
  }
  mtext(outer = T, side = 3, text = txtTitle, cex = 1.6)
  dev.off()
  
}
postGibbsChains <- function(betachains,
                            burnin=1, 
                            traitsToPlot=NULL,
                            predictorsToPlot=NULL, 
                            normalized = T,
                            includeInteractions=T,
                            includeMainEffects =T,
                            excludeIntercept = T,
                            onlySignificant=T){
  
  wFactors <- which(apply(output$x, 2, function(x)all(x%in%c(0,1))))
  sdCols <- apply(output$x, 2, sd)
  sdCols[wFactors] <- 1
  
  
  chains <- betachains[-(1:burnin),]
  
  ng <- nrow(chains)
  nbeta <- ncol(chains)
  fullMatrix <- matrix(unlist(
    strsplit(colnames(chains), split = '_')), 
    ncol=2, byrow = T)
  colnames(fullMatrix) <- c('trait', 'predictor')
  fullMatrix <- as.data.table(fullMatrix)
  id <- 1:nbeta
  fullMatrix <- cbind(id, fullMatrix)
  fullMatrix$interaction <- grepl(':',fullMatrix$predictor)
  fullMatrix
  fullMatrix[, pred:=strsplit(predictor,':')]
  tmp <- function(x){
    if(length(x)!=1) {
      return(unlist(x))
    }else{
      return(c(unlist(x),NA))
    }
  }
  fullMatrix$pred1 <- t(sapply(fullMatrix$pred, tmp))[,1]
  fullMatrix$pred2 <- t(sapply(fullMatrix$pred, tmp))[,2]
  
  
  sdCols <- sdCols[match(fullMatrix$predictor, names(sdCols))]
  if(normalized) chains <- t(t(chains)*sdCols)
  
  summChains <- t(apply(chains, 2, quantile, probs=c(.5,.025,.975)))
  colnames(summChains) <- c('median','low','high')
  fullMatrix <- cbind(fullMatrix, summChains)
  fullMatrix[, signifcant:=sign(high*low)==1]
  fullMatrix
  if(is.null(traitsToPlot)) traitsToPlot <- unique(fullMatrix$trait)
  if(is.null(predictorsToPlot)) predictorsToPlot <- unique(fullMatrix$predictor)
  
   predictorFilter = 1:nrow(fullMatrix)%in%unique(unlist(apply(as.matrix(predictorsToPlot), 1, grep, fullMatrix$predictor)))
   interactionFilter<- c(includeInteractions, !includeMainEffects)
   if(!(includeInteractions|includeMainEffects)) interactionFilter <- c()
   
  nameMatrix <- fullMatrix[
    trait%in%traitsToPlot&
      (predictor%in%predictorsToPlot|
         pred1%in%predictorsToPlot|
         pred2%in%predictorsToPlot)&
      #predictorFilter&
      (signifcant|!onlySignificant)&
      interaction%in%interactionFilter&
      ((predictor!='intercept')|!excludeIntercept)
    , ]
  
  list(    chains = chains[, nameMatrix$id],
           nameMatrix = nameMatrix,
           fullchain =chains,
           fullMatrix = fullMatrix
  )
}