library(data.table)
source('~/Projects/procVisData/bayesianFunctions.R')

getSensitivity <- function(param, output, interactionsList, traitNames, traitData) {
  
  traitSd <- apply(traitData$plotByCWM, 2, sd)
  
  paramSensList <- list()
  
  for(j in 4:6){
    postParam <- postGibbsChains(betachains = output$chains$agibbs, 
                                 burnin = output$burnin,
                                 traitsToPlot = traitNames[j] ,
                                 predictorsToPlot = param, 
                                 onlySignificant = F, 
                                 normalized = F, 
                                 includeInteractions = T, 
                                 includeMainEffects = T)
    
    
    sensVectors <- cbind(1, output$x[,interactionsList])
    
    paramSens <- sensVectors%*%t(postParam$chains)/traitSd[j]
    paramSens.Summ <- as.data.frame(t(apply(paramSens, 1, quantile, probs=c(.5,.025,.975))))
    paramSens.Summ$mean <- rowMeans(paramSens)
    paramSens.Summ$signif <- (sign(paramSens.Summ[,"2.5%"]*paramSens.Summ[,"97.5%"])>0)
    paramSens.Summ$sign <- paramSens.Summ$signif*sign(paramSens.Summ[,"2.5%"])
    
    paramSensList[[length(paramSensList)+1]] <- paramSens.Summ
    
  }
  names(paramSensList) <- traitNames[4:6]
  
  paramSensList
}

getCWT.FilterOnLeaf <- function(output, speciesByTraits, plotByW){
  species <- rownames(speciesByTraits)
  sp <- 1:length(species)
  speciesByTraitsDT <- as.data.table(cbind(sp,species, speciesByTraits))
  
  evergreenLeaf <- speciesByTraitsDT[leaf=='NLEver', .(sp, species)]
  deciduousLeaf <- speciesByTraitsDT[leaf=='Deciduous', .(sp, species)]
  
  CWT.evergreen <- (plotByW[,evergreenLeaf$species])%*%as.matrix(
    speciesByTraits[evergreenLeaf$species,c("N","P","SLA")])
  
  CWT.deciduous <- (plotByW[,deciduousLeaf$species])%*%as.matrix(
    speciesByTraits[deciduousLeaf$species,c("N","P","SLA")])
  
  betaTraitMu.Ever <- output$modelSummary$betaMu[,evergreenLeaf$sp]%*%
    as.matrix(traitData$specByTrait[evergreenLeaf$sp,])
  
  betaTraitMu.Decid <- output$modelSummary$betaMu[,deciduousLeaf$sp]%*%
    as.matrix(traitData$specByTrait[deciduousLeaf$sp,])
  
  
  
  spCols <- matrix(unlist(strsplit(colnames(output$chains$bgibbs),split = '_')), ncol=2, byrow=T)[,1]
  
  decidCols <- spCols%in%deciduousLeaf$species
  everCols <- spCols%in%evergreenLeaf$species
  
  dim(output$chains$bgibbs[,decidCols])
  colnames(output$chains$bgibbs[,everCols])
  
  list(evergreen = CWT.evergreen, 
       deciduous = CWT.deciduous,
       betaTraitMu.Ever=betaTraitMu.Ever[,c("N","P","SLA")],
       betaTraitMu.Decid=betaTraitMu.Decid[,c("N","P","SLA")]
  )
}

getCWT.CondOnLeaf <- function(output){
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