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
param <- 'temp'

paramSensList <- getSensitivity(param, output, interactionsList, traitList, traitData)
plotSensitivity(pngName, txtTitle, paramSensList)
plotSensitivitySign(pngName, txtTitle, paramSensList)
