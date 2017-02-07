library(data.table)
library(ggplot2)

source('traitPreModel.R')
source('traitFunctions.R')
source('figures/traitColorSet.R')
source('figures/traitPostAux.R')

paramSensList <- getSensitivity(param = c('moisture', 'I(moisture^2)'),
                                output, traitNames, traitData)

moisture <- output$x[,"moisture"]
pred <- CWT$pred[,c("N","P","SLA")]

DT <- as.data.table(cbind(moisture, pred))

ggplot(data = DT[!moisture%in%c(-1,0,1)], aes(moisture, N))  +
  geom_smooth(method="lm", formula= y ~ x + I(x^2)) + 
  ylab('N (mg/g)') +
  ggsave('figures/traitFig.MoistureQuadratic.N.png', height = 3, width = 4, units = 'in', dpi = 150)

ggplot(data = DT[!moisture%in%c(-1,0,1)], aes(moisture, P)) +
  geom_smooth(method="lm", formula= y ~ x + I(x^2), col='red') + 
  ylab('P (mg/g)') +
  ggsave('figures/traitFig.MoistureQuadratic.P.png', height = 3, width = 4, units = 'in', dpi = 150)

ggplot(data = DT[!moisture%in%c(-1,0,1)], aes(moisture, SLA))  +
  geom_smooth(method="lm", formula= y ~ x + I(x^2)) + 
  ylab('SLA (cm²/g)') +
  ggsave('figures/traitFig.MoistureQuadratic.SLA.png', height = 3, width = 4, units = 'in', dpi = 150)
