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
                txtTitle = 'Total fraction of biomass for high phosphorous species',
                plotByW, plotByX)

mapMultiSpecies(group = highN, 
                nameFile = 'figures/traitFig.MapSpecies.HighN.png',
                txtTitle = 'Total fraction of biomass for high nitrogen species',
                plotByW, plotByX)

mapMultiSpecies(group = highSLA, 
                nameFile = 'figures/traitFig.MapSpecies.HighSLA.png',
                txtTitle = 'Total fraction of biomass for high SLA species',
                plotByW, plotByX)

mapMultiSpecies(group = lowP, 
                nameFile = 'figures/traitFig.MapSpecies.LowP.png',
                txtTitle = 'Total fraction of biomass for low phosphorous species',
                plotByW, plotByX)

mapMultiSpecies(group = lowN, 
                nameFile = 'figures/traitFig.MapSpecies.LowN.png',
                txtTitle = 'Total fraction of biomass for low nitrogen species',
                plotByW, plotByX)

mapMultiSpecies(group = lowSLA, 
                nameFile = 'figures/traitFig.MapSpecies.Low.SLA.png',
                txtTitle = 'Total fraction of biomass for low SLA species',
                plotByW, plotByX)
