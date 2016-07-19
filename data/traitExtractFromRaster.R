library(raster)

r <- raster('~/Google Drive/MODIS/TAXNWRB_250m_ll.tif')
spCoords <- read.csv('data/pre/lonLatAll.csv')
ww <- read.csv('data/pre/wwAll.csv')$w

so <- read.csv('data/pre/soilAll.csv', header = F)

rEx <- extract(r, spCoords)
 
library(data.table)
tmp <- as.data.table(cbind(so, rEx))

tmp[,.N, .(rEx, V1)]
