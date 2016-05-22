library(gjam)

set.seed(2016)
source('traitPreModel.R')

traitData  <- gjamSpec2Trait(pbys = plotByW, 
                             sbyt = speciesByTraits, 
                             tTypes = traitTypes)

traitList <- list(plotByTrait = traitData$plotByCWM, 
                  traitTypes = traitData$traitTypes, 
                  specByTrait = traitData$specByTrait)

reductList <- list(r = 3, N = 20)

modelList <- list(ng=300, 
                  burnin=100,
                  typeNames = 'FC', 
                  holdoutN = 20,
                  reductList = reductList,
                  traitList = traitList)

attr(plotByX$soil,'reference') <- 'Others'   # reference class
attr(plotByX$soil,'intType')   <- 'ref'

models <- readLines('modelSelection/traitModel.Models.txt')

for(modelNo in 1:length(models)){
  set.seed(2016)
  
  output  <- gjamGibbs(as.formula(models[modelNo]),
                       xdata = plotByX, ydata = plotByW, 
                       modelList = modelList)  
  sysTime <- Sys.time()
  strTime <- substring(make.names(sysTime),2)
  
  save.image(paste('modelSelection/output', modelNo,
                   modelList$ng/1000, modelList$burnin/1000,
                   strTime, '.RData', sep = '-'))
  
  modelSelectSumm.Cur <- data.frame(Model.No=modelNo,
                                    Predictors = models[modelNo],
                                    Time =as.character(sysTime),
                                    DIC=output$modelSummary$DIC,
                                    xScore=mean(output$modelSummary$xscore, na.rm = T),
                                    yScore=mean(output$modelSummary$yscore, na.rm = T)
                                    )
  modelSelectSumm <- read.csv('modelSelection/modelSelectSumm.csv')
  modelSelectSumm <- rbind(modelSelectSumm, modelSelectSumm.Cur)
  write.csv(modelSelectSumm, file = 'modelSelection/modelSelectSumm.csv', row.names = F)
  rmarkdown::render('modelSelection/traitModelSelection.Rmd',c("html_document", "pdf_document"))
}

