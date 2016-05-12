require(data.table)

# betachains <- output$chains$agibbs
# burnin <- floor(nrow(betachains)/2)
# withInteractions <- F
# traitsToPlot <- c('N','P','SLA') 
# predictorsToPlot <- NULL
# onlySignificant <- T

postGibbsChains <- function(betachains, burnin=1, withInteractions=F, traitsToPlot=NULL, predictorsToPlot=NULL, onlySignificant=T){
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
  nameMatrix
  summChains <- t(apply(chains, 2, quantile, probs=c(.5,.025,.975)))
  colnames(summChains) <- c('median','low','high')
  nameMatrix <- cbind(nameMatrix, summChains)
  nameMatrix[, signifcant:=sign(high*low)==1]
  nameMatrix
  if(is.null(traitsToPlot)) traitsToPlot <- unique(nameMatrix$trait)
  if(is.null(predictorsToPlot)) predictorsToPlot <- unique(nameMatrix$predictor)

    if(onlySignificant) nameMatrix <- nameMatrix[interaction==withInteractions&trait%in%traitsToPlot&predictor%in%predictorsToPlot&signifcant, ]
  if(!onlySignificant) nameMatrix <- nameMatrix[interaction==withInteractions&trait%in%traitsToPlot&predictor%in%predictorsToPlot, ]
  
  list(    chains = chains[, nameMatrix$id],
           nameMatrix= nameMatrix)
}