library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitPreModel.R')
source('traitFunctions.R')
source('figures/traitColorSet.R')

source('figures/traitPostAux.R')

txtTitle <- 'Sensitivity to moisture (dimensionless)'
interactionsList = c("deficit","soilAlfInc","soilEntVert","soilMol","soilSpodHist","soilUltKan")
pngName <- 'figures/traitFig.SensMoisture.Signif.png'
param <- 'moisture'

paramSensList <- getSensitivity(param, output, interactionsList, traitList, traitData)
plotSensitivity(pngName, txtTitle, paramSensList)
plotSensitivitySign(pngName, txtTitle, paramSensList)

