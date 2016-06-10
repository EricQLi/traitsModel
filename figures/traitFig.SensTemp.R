library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitPreModel.R')
source('traitFunctions.R')
source('figures/traitColorSet.R')

source('figures/traitPostAux.R')


param <- 'temp'
txtTitle <- 'Sensitivity to temperature (dimensionless)'
pngName <- 'figures/traitFig.SensTemp.png'
# interactionsList = c("soilAlfInc","soilEntVert","soilMol","soilSpodHist","soilUltKan","deficit")
# pngNameSign <- 'figures/traitFig.SensTemp.Signif.png'

traitNames <- colnames(traitList$plotByTrait)
paramSensList <- getSensitivity(param, output, traitNames, traitData)
plotSensitivity(pngName, txtTitle, paramSensList, traitNames)
# plotSensitivitySign(pngNameSign, txtTitle, paramSensList, traitNames)

