library(data.table)
library(ggplot2)

matEnvTrait <- data.table(cbind(traitData$plotByCWM,
                                plotByX))
matEnvTrait[, Climate:=as.factor('normal')]
matEnvTrait[, Site:=as.factor('normal')]

matEnvTrait[deficit > mean(deficit) + 2*sd(deficit), Climate:= as.factor('dry')]
matEnvTrait[deficit < mean(deficit) - 2*sd(deficit), Climate:= as.factor('wet')]

matEnvTrait[moisture > mean(moisture) + 1*sd(moisture), Site:= as.factor('wet')]
matEnvTrait[moisture < mean(moisture) - 1*sd(moisture), Site:= as.factor('dry')]


ggplot(matEnvTrait[(!moisture%in%c(-1,0,1))&Climate!='normal'], aes(moisture, N)) + xlab('Soil moisture index') + ylab('N (mg/g)') + geom_smooth(method='lm', aes(col=Climate)) + theme(legend.position = 'none')  + ggsave('figures/traitFig.traitClimateSiteInteractions1.png', height = 3, width = 3, units = 'in', dpi = 300)
ggplot(matEnvTrait[(!moisture%in%c(-1,0,1))&Climate!='normal'], aes(moisture, P)) + xlab('Soil moisture index') + ylab('P (mg/g)') + geom_smooth(method='lm', aes(col=Climate))  + theme(legend.position = 'none')  + ggsave('figures/traitFig.traitClimateSiteInteractions2.png', height = 3, width = 3, units = 'in', dpi = 300)
ggplot(matEnvTrait[(!moisture%in%c(-1,0,1))&Climate!='normal'], aes(moisture, SLA)) + xlab('Soil moisture index') + ylab('SLA (cm�/g)') + geom_smooth(method='lm', aes(col=Climate)) + theme(legend.position = c(.8, .2))  + ggsave('figures/traitFig.traitClimateSiteInteractions3.png', height = 3, width = 3, units = 'in', dpi = 300) 

ggplot(matEnvTrait[(!deficit%in%c(0))&Site!='normal'], aes(deficit, N, col=Site)) + xlab('Deficit') + ylab('N (mg/g)') + geom_smooth(method='lm') + theme(legend.position = 'none')  + ggsave('figures/traitFig.traitClimateSiteInteractions4.png', height = 3, width = 3, units = 'in', dpi = 300)
ggplot(matEnvTrait[(!deficit%in%c(0))&Site!='normal'], aes(deficit, P, col=Site)) + xlab('Deficit') + ylab('P (mg/g)') + geom_smooth(method='lm')  + theme(legend.position = 'none')  + ggsave('figures/traitFig.traitClimateSiteInteractions5.png', height = 3, width = 3, units = 'in', dpi = 300)
ggplot(matEnvTrait[(!deficit%in%c(0))&Site!='normal'], aes(deficit, SLA, col=Site)) + xlab('Deficit') + ylab('SLA (cm�/g)') + geom_smooth(method='lm')   + theme(legend.position = c(.8,.8))  + ggsave('figures/traitFig.traitClimateSiteInteractions6.png', height = 3, width = 3, units = 'in', dpi = 300)



