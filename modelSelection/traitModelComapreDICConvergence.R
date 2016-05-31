mod55 <- read.csv('modelSelection/modelSelectSumm-55.csv')
mod300 <- read.csv('modelSelection/modelSelectSumm-300.csv')
mod15 <- read.csv('modelSelection/modelSelectSumm.csv')

colnames(mod55)[4] <- 'DIC.55'
colnames(mod300)[4] <- 'DIC.300'
colnames(mod15)[4] <- 'DIC.15'

mod <- merge(mod55, mod300, by='Model.No')

mod <- merge(mod55, mod15, by='Model.No')

plot(mod[,c('DIC.15', 'DIC.55','DIC.300')])

plot(mod$xScore.x, mod$xScore.y)
plot(mod$yScore.x, mod$yScore.y)
