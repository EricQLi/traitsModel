library(raster)

source('~/Projects/procVisData/geoSpatial.R')
source('~/Projects/procVisData/colorProc.R')

source('traitFunctions.R')
source('figures/traitColorSet.R')

xuCNP <- read.csv('data/GLOBAL_MICROBIAL_BIOMASS_C_N_P_1264/data/Soil_Microbial_Biomass_C_N_P_spatial.csv')
xuCNP.Head <- names(xuCNP[1,])
xuCNP.Unit <- xuCNP[1,]

xuCNP <- read.csv('data/GLOBAL_MICROBIAL_BIOMASS_C_N_P_1264/data/Soil_Microbial_Biomass_C_N_P_spatial.csv', skip = 2, header = F)
colnames(xuCNP) <- as.character(xuCNP.Head)
head(xuCNP)

for(i in 1:ncol(xuCNP)) xuCNP[,i] <- as.character(xuCNP[,i])
xuCNP[xuCNP==''] <- NA

xuCNP$Latitude <- as.numeric(xuCNP$Latitude)
xuCNP$Longitude <- as.numeric(xuCNP$Longitude)

xuCNP$MAT <- as.numeric(xuCNP$MAT)
xuCNP$MAP <- as.numeric(xuCNP$MAP)

xuCNP$Soil_microbial_biomass_carbon <- as.numeric(xuCNP$Soil_microbial_biomass_carbon)
xuCNP$Soil_microbial_biomass_nitrogen <- as.numeric(xuCNP$Soil_microbial_biomass_nitrogen)
xuCNP$Soil_microbial_biomass_phosphorus <- as.numeric(xuCNP$Soil_microbial_biomass_phosphorus)

xuCNP$Soil_organic_carbon <- as.numeric(xuCNP$Soil_organic_carbon)
xuCNP$Total_nitrogen <- as.numeric(xuCNP$Total_nitrogen)
xuCNP$Total_organic_phosphorus <- as.numeric(xuCNP$Total_organic_phosphorus)

xuCNP$pH <- as.numeric(xuCNP$pH)
xuCNP$Upper_depth <- as.numeric(xuCNP$Upper_depth)
xuCNP$Lower_depth <- as.numeric(xuCNP$Lower_depth)
xuCNP$Depth <- as.numeric(xuCNP$Depth)




xuCNP$Lat <- xuCNP$Latitude
xuCNP$Longitude <- xuCNP$Longitude

wPl <- xuCNP$Country=='United States of America'

wbox <- xuCNP$Lat>=min(plotByX$plotLat) &  
  xuCNP$Lat<=max(plotByX$plotLat) &
  xuCNP$Lon>=min(plotByX$plotLon) &  
  xuCNP$Lon<=max(plotByX$plotLon) 

png('figures/traitFig.SoilCNP.Xu.png', units='in',res=300, height  = 10, width=15)
par(mfrow=c(2,3),oma = c(3,7,5,5),mar = c(0,0,0,0))

for(j in 1:6){
  ssj <- switch (j,   
                 xuCNP$Soil_microbial_biomass_carbon, 
                 xuCNP$Soil_microbial_biomass_nitrogen, 
                 xuCNP$Soil_microbial_biomass_phosphorus, 
                 xuCNP$Soil_organic_carbon, 
                 xuCNP$Total_nitrogen, 
                 xuCNP$Total_organic_phosphorus)
  
  par(xaxt='n', yaxt='n')
  valRange <- quantile(ssj[wbox&wPl], probs=seq(.025,.975, length.out = 10), na.rm=T)
  
  mapColorData(x=xuCNP$Lon[wbox&wPl],y =  xuCNP$Lat[wbox&wPl],  data = ssj[wbox&wPl], 
               symSize = 5, alpha = .5,
               xlim = range(plotByX$plotLon), equiLatLon = T, 
               ylim = range(plotByX$plotLat),
               valRange = valRange, statesborder = F,
               #legend.txt = paste0(signif(range(valRange),2), ' (g/m^2)'),
               colList =rev(colList.SurfAndTurf),
               ADD=F, cex.all = 3, #legendShow = F 
  )
  plot(mapRegion, add = T)
  
  # lines(glacialLine[-nrow(glacialLine),1],glacialLine[-nrow(glacialLine),2],lwd=6,col='white')
  # lines(glacialLine[-nrow(glacialLine),1],glacialLine[-nrow(glacialLine),2],lwd=2,col='darkgrey')
  #par(xaxt='s', yaxt='s')

  mtext(text = switch (as.character(j),
                       '1'='Carbon',
                       '2'='Nitrogen',
                       '3'='Phosphorus',''), side = 3, cex = 2, line = 2  )
  mtext(text = switch (as.character(j),
                       '1'='Microbial',
                       '4'='Total', ''), side = 2, cex = 2, line = 2  )
  axis(1, cex.axis=1.7)
  axis(2, cex.axis=1.7)
  
}
dev.off()



# 
# library(gjam)
# 
# xdata <- xuCNP
# ydata <- xuCNP[,c("Soil_microbial_biomass_carbon", "Soil_microbial_biomass_nitrogen", "Soil_microbial_biomass_phosphorus", "Soil_organic_carbon", "Total_nitrogen", "Total_organic_phosphorus", "pH")][,c(1:3, 5:6)]
# 
# ml  <- list(ng = 2000, burnin = 1000, typeNames = rep('CON', ncol(ydata)))
# 
# out <- gjamGibbs(formula = ~ MAT + MAP + MAT*MAP + pH + pH*MAT + pH *MAP, xdata = xdata, ydata = ydata, modelList = ml)
# 
# summary(out$modelSummary)
# pl  <- list(width = 3,height = 2, 
#             GRIDPLOTS = T, SMALLPLOTS = F)
# gjamPlot(output = out, plotPars = pl)

