library(gjam)

set.seed(2016)
source('traitPreModel.R')

traitData  <- gjamSpec2Trait(pbys = plotByW, 
                             sbyt = speciesByTraits, 
                             tTypes = traitTypes)

traitList <- list(plotByTrait = traitData$plotByCWM, 
                  traitTypes = traitData$traitTypes, 
                  specByTrait = traitData$specByTrait)

reductList <- list(r = 3, N = 30)

modelList <- list(ng=2000, 
                  burnin=1000,
                  typeNames = 'FC', 
                  holdoutN = 20,
                  reductList = reductList,
                  traitList = traitList)

attr(plotByX$soil,'reference') <- 'Others'   # reference class
attr(plotByX$soil,'intType')   <- 'ref'

models <- readLines('traitModel.Models.txt')

for(modelNo in 11:length(models)){
  set.seed(2016)
  
  output  <- gjamGibbs(as.formula(models[modelNo]),
                       xdata = plotByX, ydata = plotByW, 
                       modelList = modelList)  
  
  strTime <- substring(make.names(Sys.time()),2)
  
  save.image(paste('modelSelection/output', modelNo,
                   modelList$ng/1000, modelList$burnin/1000,
                   strTime, '.RData', sep = '-'))
  
  modelSelectSumm.Cur <- data.frame(modelNo=modelNo,
                                    time =strTime,
                                    DIC=output$modelSummary$DIC,
                                    yScore=mean(output$modelSummary$yscore),
                                    xScore=mean(output$modelSummary$xscore, na.rm = T))
  
  modelSelectSumm <- read.csv('modelSelection/modelSelectSumm.csv')
  modelSelectSumm <- rbind(modelSelectSumm, modelSelectSumm.Cur)
  write.csv(modelSelectSumm, file = 'modelSelection/modelSelectSumm.csv', row.names = F)
  
}

