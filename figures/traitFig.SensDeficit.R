library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitPreModel.R')
source('traitFunctions.R')
source('figures/traitColorSet.R')

source('figures/traitPostAux.R')

param <- 'deficit'
txtTitle <- 'Sensitivity to deficit (dimensionless)'
pngName <- 'figures/traitFig.SensDeficit.png'
#interactionsList = c("moisture","soilAlfInc","soilEntVert","soilMol","soilSpodHist","soilUltKan","temp")
#pngNameSign <- 'figures/traitFig.SensDeficit.Signif.png'

traitNames <- colnames(traitList$plotByTrait)
paramSensList <- getSensitivity(param, output, traitNames, traitData)
plotSensitivity(pngName, txtTitle, paramSensList, traitNames)
#plotSensitivitySign(pngNameSign, txtTitle, paramSensList, traitNames)

