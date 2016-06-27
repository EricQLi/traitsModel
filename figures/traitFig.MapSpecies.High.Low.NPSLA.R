library(data.table) 
source('figures/traitPostAux.R')

speciesByTraitsDT <- as.data.table(speciesByTraits)
speciesByTraitsDT$species <- rownames(speciesByTraits)

highP <- speciesByTraitsDT[order(P, decreasing = T),.(P,species)][1:5, species]
lowP <- speciesByTraitsDT[order(P, decreasing = F),.(P,species)][1:5, species]

highN <- speciesByTraitsDT[order(N, decreasing = T),.(P,species)][1:5, species]
lowN <- speciesByTraitsDT[order(N, decreasing = F),.(P,species)][1:5, species]

highSLA <- speciesByTraitsDT[order(SLA, decreasing = T),.(P,species)][1:5, species]
lowSLA <- speciesByTraitsDT[order(SLA, decreasing = F),.(P,species)][1:5, species]



mapMultiSpecies(group = highP, 
                nameFile = 'figures/traitFig.MapSpecies.HighP.png',
                txtTitle = 'Relative abundance of high P species',
                plotByW, plotByX)

mapMultiSpecies(group = highN, 
                nameFile = 'figures/traitFig.MapSpecies.HighN.png',
                txtTitle = 'Relative abundance of high N species',
                plotByW, plotByX)

mapMultiSpecies(group = highSLA, 
                nameFile = 'figures/traitFig.MapSpecies.HighSLA.png',
                txtTitle = 'Relative abundance of high SLA species',
                plotByW, plotByX)

mapMultiSpecies(group = lowP, 
                nameFile = 'figures/traitFig.MapSpecies.LowP.png',
                txtTitle = 'Relative abundance of low P species',
                plotByW, plotByX)

mapMultiSpecies(group = lowN, 
                nameFile = 'figures/traitFig.MapSpecies.LowN.png',
                txtTitle = 'Relative abundance of low N species',
                plotByW, plotByX)

mapMultiSpecies(group = lowSLA, 
                nameFile = 'figures/traitFig.MapSpecies.Low.SLA.png',
                txtTitle = 'Relative abundance of low SLA species',
                plotByW, plotByX)
