library(gjam)

xdata <- xuCNP
ydata <- xuCNP[,c("Soil_microbial_biomass_carbon", "Soil_microbial_biomass_nitrogen", "Soil_microbial_biomass_phosphorus", "Soil_organic_carbon", "Total_nitrogen", "Total_organic_phosphorus", "pH")][,c(1:3, 5:6)]

ml  <- list(ng = 2000, burnin = 1000, typeNames = rep('CON', ncol(ydata)))

out <- gjamGibbs(formula = ~ MAT + MAP + MAT*MAP + pH + pH*MAT + pH *MAP, xdata = xdata, ydata = ydata, modelList = ml)

summary(out$modelSummary)
pl  <- list(width = 3,height = 2, 
            GRIDPLOTS = T, SMALLPLOTS = F)
gjamPlot(output = out, plotPars = pl)
