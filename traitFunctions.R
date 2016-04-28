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