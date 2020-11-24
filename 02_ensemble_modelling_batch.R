
# Packages ####

if(!require(raster)) install.packages("raster") else library(raster)

setwd("C:/Users/Pablo/OneDrive - Universidad de Córdoba/1_proyectos/2019_CocoAgroForecast/WP5 - suitability/")
parentwd <- getwd()

# Final seletion of variables for modelling

###########################3
# Occurrence data filtered
BD_calib <- read.csv("./BD/BD_calibrate.csv")
# BD_fut <- read.csv("./BD/BD_fut.csv")


### COMENTARIOS:
# - Al extraer las variables de la BD calib hay puntos que se quedan sin datos (40 de suelo y 1 de clima)
# esto habría que considerarlo para el número de puntos por especie para mostar el final de datos que partimos
# Del análisis de correlación para suelo nos quedamos con sand,ph,cec y nitrogen. bulk density tiene mas NA
# sand es un buen predictor de silt y clay así que simplificamos
# de clima bio3 y bio4 estan muy correlacionados así que nos quedamos con la bio4 que es más relevante
# bio14 y bio15 también lo están así que nos quedamos con la 15 que es mas relevante.
# seleccion final: sand,ph,cec y nitrogen, bio2, 4, 8, 9,13,15,18 y 19
# Para esta primera versión probamos con 4 modelos principales (GLM,GBM,RF y GAM) haciendo pseudoausencias a 500km

#### Load all data 
common_path<-"./BD/Current/"
files <- list.files(
  path <- common_path,
  pattern <- "\\.tif$",
  recursive = F,          
  full.names = TRUE          
)
bio_current <- raster::stack(files[c(1:17)])
# plot(bio_current)

####################
# Test collinearity
BD_calib_variables <- extract(bio_current,BD_calib[,c("decimalLongitude","decimalLatitude")],df=T)
# summary(BD_calib_variables)
# 
# library(corrplot)
# cor_mat <- cor(na.omit(BD_calib_variables))
# 
# png(filename = "processing/figures/corrplot_all.png" , width=600,height = 600,pointsize = 9 )
# corrplot(cor_mat,method = "number",order="hclust",addrect = 4,is.corr = T, )
# dev.off()

#####################
# Prepare data

# Occurrence
BD_calib_clean <- BD_calib[!is.na(BD_calib_variables$cec_mean30100cm_SoilGrids2),]
summary(BD_calib_clean)

# Predictors
bio_current <- stack(files[c(2,4,5,6,13,15,16,17,8,10,11,12)])
# plot(bio_current)
bio_current_pred <- stack(raster::crop(bio_current,extent(-17,15,2,13),))
# plot(bio_current_pred)

# Model parameters
# see Liu et al (2013) doi:10.1111/jbi.12058
thres <- "MaxSens+Spec"
ncross <- 4 # number of groups for cross validation
npseudo <- 1000 ## number of pseudobscences per species
distpseud <- 500000 # metres


# df <- subset(BD_calib_clean[,2:4],species=="Acacia mangium" | species=="Cola acuminata")

# Species
df <- BD_calib_clean[,2:4]
species_all <- sort(unique(df$species))

## Send scripts
ini <- 3;fin<-4
jobRunScript(path = "C:/Users/Pablo/Documents/R_projects/WP5_C-AFSsuitability/02_ensemble_modelling.R",importEnv = T,name =paste("species",ini,"-",fin)) 


library(rstudioapi)
  ini <- 1;fin<-1
while(fin<length(species_all)){
  fin<- ini+5
  if(fin>=length(species_all)){fin = length(species_all)}
  print(paste("Mandados: especie ",ini, " - ",fin))
  jobRunScript(path = "/WP5_C-AFSsuitability/02_ensemble_modelling.R",importEnv = TRUE,name =paste("species",ini,"-",fin)) 
  ini <- fin+1
}