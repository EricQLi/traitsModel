require(data.table)

# betachains <- output$chains$agibbs
# burnin <- floor(nrow(betachains)/2)
# withInteractions <- F
# traitsToPlot <- c('N','P','SLA') 
# predictorsToPlot <- NULL
# onlySignificant <- T

postGibbsChains <- function(betachains,
                            burnin=1, 
                            withInteractions=T, 
                            traitsToPlot=NULL,
                            predictorsToPlot=NULL, 
                            onlySignificant=T,
                            normalized = T,
                            excludeIntercept = T){
  wFactors <- which(apply(output$x, 2, function(x)all(x%in%c(0,1))))
  sdCols <- apply(output$x, 2, sd)
  sdCols[wFactors] <- 1
  
  
  chains <- betachains[-(1:burnin),]
  
  ng <- nrow(chains)
  nbeta <- ncol(chains)
  nameMatrix <- matrix(unlist(
    strsplit(colnames(chains), split = '_')), 
    ncol=2, byrow = T)
  colnames(nameMatrix) <- c('trait', 'predictor')
  nameMatrix <- as.data.table(nameMatrix)
  id <- 1:nbeta
  nameMatrix <- cbind(id, nameMatrix)
  nameMatrix$interaction <- grepl(':',nameMatrix$predictor)
  nameMatrix
  nameMatrix[, pred:=strsplit(predictor,':')]
  tmp <- function(x){
    if(length(x)!=1) {
      return(unlist(x))
    }else{
      return(c(unlist(x),NA))
    }
  }
  nameMatrix$pred1 <- t(sapply(nameMatrix$pred, tmp))[,1]
  nameMatrix$pred2 <- t(sapply(nameMatrix$pred, tmp))[,2]
  
  
  sdCols <- sdCols[match(nameMatrix$predictor, names(sdCols))]
  if(normalized) chains <- t(t(chains)*sdCols)
  
  summChains <- t(apply(chains, 2, quantile, probs=c(.5,.025,.975)))
  colnames(summChains) <- c('median','low','high')
  nameMatrix <- cbind(nameMatrix, summChains)
  nameMatrix[, signifcant:=sign(high*low)==1]
  nameMatrix
  if(is.null(traitsToPlot)) traitsToPlot <- unique(nameMatrix$trait)
  if(is.null(predictorsToPlot)) predictorsToPlot <- unique(nameMatrix$predictor)
  
  predictorFilter = 1:nrow(nameMatrix)%in%unique(unlist(apply(as.matrix(predictorsToPlot), 1, grep, nameMatrix$predictor)))
  
  nameMatrix <- nameMatrix[
    interaction==withInteractions&
      trait%in%traitsToPlot&
      #predictor%in%predictorsToPlot&
      predictorFilter&
      (signifcant|!onlySignificant)&
      ((predictor!='intercept')|!excludeIntercept)
      , ]
  
  list(    chains = chains[, nameMatrix$id],
           nameMatrix= nameMatrix)
}