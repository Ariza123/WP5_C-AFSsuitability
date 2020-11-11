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
library(usdm)
library(gdalUtils)

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

BD<-read.csv("C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/BD_ocurrence.csv")

## Extract climate data

extracted<-extract(StackWC,BD[,c("decimalLongitude","decimalLatitude")])

extracted<-as.data.frame(extracted)

write.csv(extracted, "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/WorldClim/extracted.csv")

## Calibration subset

Historical <- extracted %>%
  select(contains("bio_"))

## VIF

vif<-vifstep(Historical,th=8)
ind = rep(0, length(vif@excluded))
for (i in 1:length(vif@excluded)) {
  ind[i] = which(colnames(Historical) == vif@excluded[i])
}
myExpl = Historical[ ,c(-ind)]
myExpl_hist = myExpl


## Data from SoilGrids

#Bulk density

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/bdod/bdod_0-5cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/bdod_0-5cm_SoilGrids2.tif")

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/bdod/bdod_5-15cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/bdod_5-15cm_SoilGrids2.tif")



gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/bdod/bdod_15-30cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/bdod_15-30cm_SoilGrids2.tif")

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/bdod/bdod_30-60cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/bdod_30-60cm_SoilGrids2.tif")


gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/bdod/bdod_60-100cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/bdod_60-100cm_SoilGrids2.tif")


#CEC

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/cec/cec_0-5cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/cec_0-5cm_SoilGrids2.tif")

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/cec/cec_5-15cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/cec_5-15cm_SoilGrids2.tif")



gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/cec/cec_15-30cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/cec_15-30cm_SoilGrids2.tif")

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/cec/cec_30-60cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/cec_30-60cm_SoilGrids2.tif")


gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/cec/cec_60-100cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/cec_60-100cm_SoilGrids2.tif")


common_path<-setwd("C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids") 
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
