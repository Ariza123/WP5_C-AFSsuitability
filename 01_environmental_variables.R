#PACKAGES####

library(rgbif)
library(maptools)
library(rgeos)
library(raster)
library(dismo)
library(geosphere)
library(rworldmap)
library(ggplot2)
library(rgdal)
library(dplyr)


## Data from WorldClim

common_path<-setwd("C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/WorldClim") 
files <- list.files(
  path <- common_path,
  pattern <- "\\.tif$",
  recursive = TRUE,          
  full.names = TRUE          
)

StackWC<-stack(files)

## Introduce BD with coordinates

BD<-read.csv("C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/BD.csv")

## Extract climate data

extracted<-extract(StackWC,BD[,c("decimalLongitude","decimalLatitude")])

extracted<-as.data.frame(extracted)

## Calibration subset

Historical <- extracted %>%
  select(contains("BCC.CSM2.MR"))%>%
  select(contains("ssp126"))%>%
  select(ends_with(".1"))

## VIF

vif<-vifstep(Historical,th=8)
ind = rep(0, length(vif@excluded))
for (i in 1:length(vif@excluded)) {
  ind[i] = which(colnames(Historical) == vif@excluded[i])
}
myExpl = Historical[ ,c(-ind)]
myExpl_hist = myExpl


## Data from SoilGrids

common_path<-setwd("C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/WorldClim") 
files <- list.files(
  path <- common_path,
  pattern <- "\\.tif$",
  recursive = TRUE,          
  full.names = TRUE          
)

StackSD<-stack(files)

## Extract soil data

extractedsoil<-extract(StackSD,BD[,c("decimalLongitude","decimalLatitude")])

extractedsoil<-as.data.frame(extractedsoil)

## Merge all data

Merged<-merge(myExpl_hist,extracted)

BD_hist<-merge(BD, Merged)


## Prediction subset

Future <- extractedsoil %>%
  select(contains("2041.2060"))

BD_fut<-merge(BD,Future)

## Save the results

write.csv(BD_hist, "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/BD_hist.csv")
write.csv(BD_fut, "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/BD_fut.csv")
