library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitPreModel.R')
source('traitFunctions.R')
source('figures/traitColorSet.R')

source('figures/traitPostAux.R')

txtTitle <- 'Sensitivity to temperature (dimensionless)'
interactionsList = c("soilAlfInc","soilEntVert","soilMol","soilSpodHist","soilUltKan","deficit")
pngName <- 'figures/traitFig.SensTemp.png'
pngNameSign <- 'figures/traitFig.SensTemp.Signif.png'

param <- 'temp'

traitNames <- colnames(traitList$plotByTrait)
paramSensList <- getSensitivity(param, output, interactionsList, traitNames, traitData)
plotSensitivity(pngName, txtTitle, paramSensList, traitNames)
plotSensitivitySign(pngNameSign, txtTitle, paramSensList, traitNames)

