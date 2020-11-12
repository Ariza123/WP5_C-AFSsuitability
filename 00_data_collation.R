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
library(car)


## Data from GBIF

common_path<-setwd("C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/GBIF") 
files <- list.files(
  path <- common_path,
  pattern <- ".*(occurrence).*txt$",
  recursive = TRUE,          
  full.names = TRUE          
)
data = lapply(files, read.csv, sep="\t") 

BD<-do.call(rbind.data.frame, data)

## Remove NA values in coordinates

BD <- BD[!is.na(BD$decimalLatitude)&BD$decimalLongitude!="NA",]

#Remove duplicate coordinates

unduplicated<- BD %>% 
  group_by(species) %>%  
  distinct(decimalLatitude, .keep_all = TRUE) %>%
  distinct(decimalLongitude, .keep_all = TRUE)

BD<-unduplicated


#Remove sea points and others

setwd("C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/GIS/Others/Paises_Mundo")
ctries <- readOGR("Paises_Mundo.shp")
proj4string(ctries) <- "+proj=longlat +datum=WGS84"

coord <- SpatialPoints(BD[,c("decimalLongitude","decimalLatitude")])
proj4string(coord) <- "+proj=longlat +datum=WGS84"

BD$countries <- extract(ctries,coord)

BD <- BD[!is.na(BD$countries[3]),]

# Select points with ocurrence status=present

BD<-BD[which(BD$occurrenceStatus=="PRESENT"),]

# Select points with basis of record = human observation

basisofrecord <- as.character(BD[,"basisOfRecord"])
basisofrecord <- unique(basisofrecord)

A<-BD[which(BD$basisOfRecord=="HUMAN_OBSERVATION"),]
B<-BD[which(BD$basisOfRecord=="LIVING_SPECIMEN"),]
C<-BD[which(BD$basisOfRecord=="OBSERVATION"),]
D<-BD[which(BD$basisOfRecord=="MATERIAL_SAMPLE"),]
E<-BD[which(BD$basisOfRecord=="PRESERVED_SPECIMEN"),]
BD<-rbind(A,B,C,D,E)

## Check decimal places https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r/5173906

decimalplaces <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}


#Remove coordinates with ZERO decimals in both LAT and LONG 
basic1 <- data.frame()
for (i in 1:length(BD[,1])){
  print(i)
  ifelse(decimalplaces(BD[i,"decimalLatitude"]) + 
           decimalplaces(BD[i,"decimalLongitude"]) >0,
         basic1 <- rbind(basic1,BD[i,]),
         print("NA"))
}

BD <- basic1 

# Remove points points from 1970 or before

BD <- BD[!is.na(BD$year),]

BD<-BD[which(BD$year>1970),]

# Reduce the possible effects of sampling bias and spatial autocorrelation (This step has been done externally with QGIS)

# Introduce the results extracted from QGIS


BD<- BD %>% 
  group_by(species) %>%  
  distinct(id, .keep_all = TRUE)

BD<-select(BD,gbifID,basisOfRecord,occurrenceStatus,year,month,day,decimalLatitude,decimalLongitude,scientificName,level0Name,species,locality,family)


# Remove outliers by average temperature (WorldClim)

ListTemperature<-list.files("C:/Users/USUARIO/Desktop/WorldClim/wc2.1_2.5m_tavg","\\.tif$",
                     all.files = T, recursive = T,full.names = T)

StackTemperature<-stack(ListTemperature)

coord <- SpatialPoints(BD[,c("decimalLongitude","decimalLatitude")])
proj4string(coord) <- "+proj=longlat +datum=WGS84"

Temp_values <- extract(StackTemperature,coord)

Avg_temperature<-rowMeans(Temp_values)

BD$Temperature<-Avg_temperature

ggplot(BD,x=1) + geom_boxplot(aes(y=Temperature))

OutVals = boxplot(BD$Temperature)$out
Out_values<-which(BD$Temperature %in% OutVals)

BD<-BD[-c(Out_values),]

ggplot(BD,x=1) + geom_boxplot(aes(y=Temperature))

# Remove by number of observations

sp <- BD %>% 
  group_by(species) %>%  
  count(species) %>%
  filter(n > 60)

listsp<-as.list(sp$species)

selected<-which(BD$species %in% listsp)

BD<-BD[c(selected),]


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


#clay


gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/clay/clay_0-5cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/clay_0-5cm_SoilGrids2.tif")

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/clay/clay_5-15cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/clay_5-15cm_SoilGrids2.tif")



gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/clay/clay_15-30cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/clay_15-30cm_SoilGrids2.tif")

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/clay/clay_30-60cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/clay_30-60cm_SoilGrids2.tif")


gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/clay/clay_60-100cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/clay_60-100cm_SoilGrids2.tif")


#Nitrogen


gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/nitrogen/nitrogen_0-5cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/nitrogen_0-5cm_SoilGrids2.tif")

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/nitrogen/nitrogen_5-15cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/nitrogen_5-15cm_SoilGrids2.tif")



gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/nitrogen/nitrogen_15-30cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/nitrogen_15-30cm_SoilGrids2.tif")

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/nitrogen/nitrogen_30-60cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/nitrogen_30-60cm_SoilGrids2.tif")


gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/nitrogen/nitrogen_60-100cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/nitrogen_60-100cm_SoilGrids2.tif")


#phh20

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/phh2o/phh2o_0-5cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/phh2o_0-5cm_SoilGrids2.tif")

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/phh2o/phh2o_5-15cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/phh2o_5-15cm_SoilGrids2.tif")



gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/phh2o/phh2o_15-30cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/phh2o_15-30cm_SoilGrids2.tif")

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/phh2o/phh2o_30-60cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/phh2o_30-60cm_SoilGrids2.tif")


gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/phh2o/phh2o_60-100cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/phh2o_60-100cm_SoilGrids2.tif")

#sand

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/sand/sand_0-5cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/sand_0-5cm_SoilGrids2.tif")

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/sand/sand_5-15cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/sand_5-15cm_SoilGrids2.tif")



gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/sand/sand_15-30cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/sand_15-30cm_SoilGrids2.tif")

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/sand/sand_30-60cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/sand_30-60cm_SoilGrids2.tif")


gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/sand/sand_60-100cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/sand_60-100cm_SoilGrids2.tif")


#silt

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/silt/silt_0-5cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/silt_0-5cm_SoilGrids2.tif")

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/silt/silt_5-15cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/silt_5-15cm_SoilGrids2.tif")



gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/silt/silt_15-30cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/silt_15-30cm_SoilGrids2.tif")

gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/silt/silt_30-60cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/silt_30-60cm_SoilGrids2.tif")


gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/silt/silt_60-100cm_mean.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/silt_60-100cm_SoilGrids2.tif")


#Soil classes


gdalwarp(t_srs="EPSG:4326", multi=TRUE, wm=200, 
         co=c("BIGTIFF=YES", "COMPRESS=DEFLATE", "TILED=TRUE"),
         tr=c(0.25,0.25), # Desired output resolution
         verbose=T,
         "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/wrb/MostProbable.vrt", # Input VRT
         "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/SoilGrids/wrb_SoilGrids2.tif")






