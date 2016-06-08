library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitPreModel.R')
source('traitFunctions.R')
source('figures/traitColorSet.R')

source('figures/traitPostAux.R')

txtTitle <- 'Sensitivity to moisture (dimensionless)'
interactionsList = c("deficit","soilAlfInc","soilEntVert","soilMol","soilSpodHist","soilUltKan")
pngName <- 'figures/traitFig.SensMoisture.png'
# pngNameSign <- 'figures/traitFig.SensMoisture.Signif.png'
param <- 'moisture'

traitNames <- colnames(traitList$plotByTrait)
paramSensList <- getSensitivity(param, output, interactionsList, traitNames, traitData)
plotSensitivity(pngName, txtTitle, paramSensList, traitNames)
# plotSensitivitySign(pngNameSign, txtTitle, paramSensList, traitNames)

