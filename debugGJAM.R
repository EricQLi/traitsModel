library(Rcpp)
library(RcppArmadillo)

source('~/Projects/traitsModel/gjam/R/gjamCensorY.r')
source('~/Projects/traitsModel/gjam/R/gjamDeZero.r')
source('~/Projects/traitsModel/gjam/R/gjamGibbs.r')
source('~/Projects/traitsModel/gjam/R/gjamHfunctions.r')
source('~/Projects/traitsModel/gjam/R/gjamIIE.r')
source('~/Projects/traitsModel/gjam/R/gjamIIEplot.r')
source('~/Projects/traitsModel/gjam/R/gjamPlot.r')
source('~/Projects/traitsModel/gjam/R/gjamPredict.r')
source('~/Projects/traitsModel/gjam/R/gjamReZero.r')
source('~/Projects/traitsModel/gjam/R/gjamSimData.r')
source('~/Projects/traitsModel/gjam/R/gjamSpec2Trait.r')
source('~/Projects/traitsModel/gjam/R/gjamTrimY.r')
source('~/Projects/traitsModel/gjam/R/RcppExports.R')
sourceCpp('~/Projects/traitsModel/gjam/src/cppFns.cpp')
sourceCpp('~/Projects/traitsModel/gjam/src/RcppExports.cpp')

load('~/Projects/traitsModel/gjam/data/forestTraits.RData')

source('gjamLoop.R')

