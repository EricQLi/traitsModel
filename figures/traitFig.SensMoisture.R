library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitPreModel.R')
source('traitFunctions.R')
source('figures/traitColorSet.R')

source('figures/traitPostAux.R')

param <- c('moisture', 'I(moisture^2)')
txtTitle <- 'Sensitivity to moisture (dimensionless)'
pngName <- 'figures/traitFig.SensMoisture.png'
# interactionsList = c("deficit","soilAlfInc","soilEntVert","soilMol","soilSpodHist","soilUltKan")
# pngNameSign <- 'figures/traitFig.SensMoisture.Signif.png'

traitNames <- colnames(traitList$plotByTrait)
paramSensList <- getSensitivity(param, output, traitNames, traitData)
plotSensitivity(pngName, txtTitle, paramSensList, traitNames, subFigIndex = c('(g)', '(h)', '(i)') )
# plotSensitivitySign(pngNameSign, txtTitle, paramSensList, traitNames)

