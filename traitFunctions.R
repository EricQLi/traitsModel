source('~/Projects/procVisData/bayesianFunctions.R')

getCWT.CondOnLeaf <- function(){
  tMu <- output$modelSummary$tMu
  deciTrait <- everTrait <- tMu
  
  deciTrait[,7:9] <- everTrait[,7:9] <- 0
  everTrait[,'leafNLEver'] <- 1
  everTrait[,'leafother'] <- 1
  deciTrait[,'leafDeciduous'] <- 1
  
  oMu <- output$modelSummary$sigmaTraitMu
  
  condDecid <- conditionalMVNVec(deciTrait[,-7],tMu[,-7],oMu[-7,-7],cdex=c(1:6))
  condEver <- conditionalMVNVec(everTrait[,-7],tMu[,-7],oMu[-7,-7],cdex=c(1:6))
  
  # condDecid <- conditionalMVNVec(deciTrait,tMu,oMu,cdex=c(1:6))
  # condEver <- conditionalMVNVec(everTrait,tMu,oMu,cdex=c(1:6))
  
  # sgibbs <- out1$chains$sgibbs
  # sMu <- matrix( colMeans(sgibbs),S,S )
  # colnames(condDecid$mu) <- colnames(sMu)[1:6]
  # colnames(condEver$mu) <- colnames(sMu)[1:6]
  
  list(condDecid=condDecid$mu, condEver=condEver$mu)
}

getCWT.Mass.Area <- function(speciesByTraits, plotByW){
  CWT.Mass <- (plotByW)%*%as.matrix(speciesByTraits[,c("N","P","SLA")])
  speciesByTraits.Area <- speciesByTraits[,c("N","P")]/speciesByTraits[,"SLA"]
  speciesByTraits.Area['other',] <- 0
  CWT.Area <- (plotByW)%*%as.matrix(speciesByTraits.Area[,c("N","P")])
  
  list(perMass = CWT.Mass, perArea = CWT.Area)
}

mapOutlines <- function(glacialLine, mapRegion, lwd1 =2, lwd2=6, col1='grey',col2='white'){
  lines(glacialLine[-nrow(glacialLine),1], glacialLine[-nrow(glacialLine),2], lwd=lwd2, col=col2)
  lines(glacialLine[-nrow(glacialLine),1], glacialLine[-nrow(glacialLine),2], lwd=lwd1, col=col1)
  plot(mapRegion, add=T)
}