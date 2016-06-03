mod55 <- read.csv('modelSelection/modelSelectSumm-55.csv')
mod300 <- read.csv('modelSelection/modelSelectSumm-300.csv')
mod15 <- read.csv('modelSelection/modelSelectSumm-15.csv')

colnames(mod55)[4] <- 'DIC.2000'
colnames(mod300)[4] <- 'DIC.500'
colnames(mod15)[4] <- 'DIC.4000'

mod <- merge(mod300, mod55, by='Model.No')
plot(mod[,c( 'DIC.2000','DIC.500')])
abline(0,1)

plot(mod[,c('DIC.4000', 'DIC.2000','DIC.500')])

mod <- merge(mod, mod15, by='Model.No')
plot(mod[,c('DIC.4000', 'DIC.2000','DIC.500')], col=mod$Model.No)
plot(mod[,c('xScore.x', 'xScore.y','xScore')], col=mod$Model.No)
plot(mod[,c('yScore.x', 'yScore.y','yScore')], col=mod$Model.No)

abline(0,1)
par(mfrow=c(1,3))
plot(mod[,c( 'DIC.2000','DIC.4000')], col=mod$Model.No);abline(0,1)
plot(mod[,c( 'xScore.y','xScore')], col=mod$Model.No);abline(0,1)
plot(mod[,c( 'yScore.y','yScore')], col=mod$Model.No);abline(0,1)
plot(mod[,c('DIC.4000','xScore','yScore')])
