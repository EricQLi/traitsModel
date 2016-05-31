mod55 <- read.csv('modelSelection/modelSelectSumm-55.csv')
mod300 <- read.csv('modelSelection/modelSelectSumm-300.csv')
mod15 <- read.csv('modelSelection/modelSelectSumm.csv')

colnames(mod55)[4] <- 'DIC.55'
colnames(mod300)[4] <- 'DIC.300'
colnames(mod15)[4] <- 'DIC.15'

mod <- merge(mod300, mod55, by='Model.No')
plot(mod[,c( 'DIC.55','DIC.300')])


mod <- merge(mod, mod15, by='Model.No')
plot(mod[,c('DIC.15', 'DIC.55','DIC.300')], col=mod$Model.No)
plot(mod[,c('xScore.x', 'xScore.y','xScore')], col=mod$Model.No)
plot(mod[,c('yScore.x', 'yScore.y','yScore')], col=mod$Model.No)

abline(0,1)
par(mfrow=c(1,3))
plot(mod[,c( 'DIC.55','DIC.15')], col=mod$Model.No);abline(0,1)
plot(mod[,c( 'xScore.y','xScore')], col=mod$Model.No);abline(0,1)
plot(mod[,c( 'yScore.y','yScore')], col=mod$Model.No);abline(0,1)
#plot(mod[,c('DIC.15','xScore','yScore')])
