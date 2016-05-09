library(Rcpp)
library(RcppArmadillo)

source('gjam/R/gjamCensorY.r')
source('gjam/R/gjamDeZero.r')
source('gjam/R/gjamGibbs.r')
source('gjam/R/gjamHfunctions.r')
source('gjam/R/gjamIIE.r')
source('gjam/R/gjamIIEplot.r')
source('gjam/R/gjamPlot.r')
source('gjam/R/gjamPredict.r')
source('gjam/R/gjamReZero.r')
source('gjam/R/gjamSimData.r')
source('gjam/R/gjamSpec2Trait.r')
source('gjam/R/gjamTrimY.r')
source('gjam/R/RcppExports.R')
sourceCpp('gjam/src/cppFns.cpp')
sourceCpp('gjam/src/RcppExports.cpp')

load('gjam/data/forestTraits.RData')

source('gjamLoop.R')

