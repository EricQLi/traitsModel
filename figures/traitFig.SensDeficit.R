library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitPreModel.R')
source('traitFunctions.R')
source('figures/traitColorSet.R')

source('figures/traitPostAux.R')

txtTitle <- 'Sensitivity to deficit (dimensionless)'
interactionsList = c("moisture","soilAlfInc","soilEntVert","soilMol","soilSpodHist","soilUltKan","temp")
pngName <- 'figures/traitFig.SensDeficit.png'
param <- 'deficit'

paramSensList <- getSensitivity(param, output, interactionsList, traitList, traitData)
plotSensitivity(pngName, txtTitle, paramSensList)
plotSensitivitySign(pngName, txtTitle, paramSensList)

