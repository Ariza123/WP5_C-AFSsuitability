####################################################
# SCRIPT TO MODEL CURRENT AND FUTURE NICHES OF 
# TREE SPECIES USING ENSEMBLE MODELLING 
# Updated November 2020
# based on Sousa et al 2019 https://doi.org/10.7910/DVN/0O1GW1

#...................................................
#...................................................
# Packages ####

if(!require(gdalUtils)) install.packages("tidyverse") else library(tidyverse)
if(!require(tidyverse)) install.packages("tidyverse") else library(tidyverse)
if(!require(magrittr)) install.packages("magrittr") else library(magrittr)
if(!require(sp)) install.packages("sp") else library(sp)
if(!require(dismo)) install.packages("dismo") else library(dismo)
if(!require(raster)) install.packages("raster") else library(raster)
if(!require(maptools)) install.packages("maptools") else library(maptools)
if(!require(rgeos)) install.packages("rgeos") else library(rgeos)
if(!require(rJava)) install.packages("rJava") else library(rJava)
if(!require(rgdal)) install.packages("rgdal") else library(rgdal)
if(!require(geosphere)) install.packages("geosphere") else library(geosphere)
if(!require(maps)) install.packages("maps") else  library(maps)
if(!require(alphahull)) install.packages("alphahull") else library(alphahull)
if(!require(tm)) install.packages("tm") else  library(tm)
if(!require(maxent)) install.packages("maxent") else library(maxent)
if(!require(gbm)) install.packages("gbm") else library(gbm)
if(!require(gam)) install.packages("gam") else library(gam)
if(!require(earth)) install.packages("earth") else library(earth)
if(!require(vegan)) install.packages("vegan") else library(vegan)
if(!require(MASS)) install.packages("MASS") else library(MASS)
if(!require(mgcv)) install.packages("mgcv") else library(mgcv)
if(!require(cluster)) install.packages("cluster") else library(cluster)
if(!require(rpart)) install.packages("rpart") else  library(rpart)
if(!require(effects)) install.packages("effects") else library(effects)
if(!require(multcomp)) install.packages("multcomp") else library(multcomp)
if(!require(ellipse)) install.packages("ellipse") else library(ellipse)
if(!require(maptree)) install.packages("maptree") else library(maptree)
if(!require(splancs)) install.packages("splancs") else library(splancs)
if(!require(spatial)) install.packages("spatial") else library(spatial)
if(!require(akima)) install.packages("akima") else library(akima)
if(!require(nnet)) install.packages("nnet") else library(nnet)
if(!require(randomForest)) install.packages("randomForest") else library(randomForest)
if(!require(mda)) install.packages("mda") else library(mda)
if(!require(kernlab)) install.packages("kernlab") else library(kernlab)
if(!require(e1071)) install.packages("e1071") else library(e1071)
if(!require(sem)) install.packages("sem") else library(sem)
if(!require(car)) install.packages("car") else library(car)
if(!require(maxlike)) install.packages("maxlike") else library(maxlike)
if(!require(glmnet)) install.packages("glmnet") else library(glmnet)
if(!require(PresenceAbsence)) install.packages("PresenceAbsence") else library(PresenceAbsence)
if(!require(tcltk2)) install.packages("tcltk2") else library(tcltk2)
if(!require(BiodiversityR)) install.packages("BiodiversityR") else  library(BiodiversityR)

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
bio_current <- stack(files[c(1:17)])
# plot(bio_current)

####################
# Test collinearity
BD_calib_variables <- extract(bio_current,BD_calib[,c("decimalLongitude","decimalLatitude")],df=T)
summary(BD_calib_variables)

library(corrplot)
cor_mat <- cor(na.omit(BD_calib_variables))

png(filename = "processing/figures/corrplot_all.png" , width=600,height = 600,pointsize = 9 )
corrplot(cor_mat,method = "number",order="hclust",addrect = 4,is.corr = T, )
dev.off()

#####################
# Prepare data

# Occurrence
BD_calib_clean <- BD_calib[!is.na(BD_calib_variables$cec_mean30100cm_SoilGrids2),]
summary(BD_calib_clean)

# Predictors
bio_current <- stack(files[c(2,4,5,6,13,15,16,17,8,10,11,12)])
# plot(bio_current)
bio_current_pred <- stack(raster::crop(bio_current,extent(-9,1,4,12),))
# plot(bio_current_pred)

# Model parameters
# see Liu et al (2013) doi:10.1111/jbi.12058
thres <- "MaxSens+Spec"
ncross <-2 # number of groups for cross validation
npseudo <- 100 ## number of pseudobscences per species
distpseud <- 500000 # metres

#...................................................
#...................................................
# Run ensemble modelling ####

# Muestra para calibrar
df <- subset(BD_calib_clean[,2:4],species=="Acacia mangium" | species=="Cola acuminata")


species <- sort(unique(df$species))

for (i in seq_along(species) ) {
  
  cat("\n######## \n Ensemble modelling for", species[i], "\n Time:", date())
  
  output <- paste0("processing/enm/", species[i], "/")
  dir.create(output, showWarnings = FALSE, recursive = TRUE)
  
  # BiodiversityR saves outputs on the current working directory
  # we set this to the intended output directory
  
  setwd(output)
  
  # sampling data
  coord <- df[df$species == species[i], ]
  
  coord <- coord[c("decimalLongitude","decimalLatitude")]
  
  n <- nrow(coord)
  

  
  # Run ensemble modelling
  # step 1: 4-fold cross-validation. 
  #splits the presence and background locations in a user-defined (k) number of subsets (i.e. k-fold cross-validation), then sequentially calibrates individual suitability models with (k-1) combined subsets and evaluates those with the remaining one subset, whereby each subset is used once for evaluation in the user-defined number (k) of runs.
  cat("\n Step 1: calibrating ENM algorithms \n")
  # Make sure the java maxent file is in R library directory dismo
  enm_step1 <- ensemble.calibrate.weights(x = bio_current, p = coord, an = npseudo,
                                          k=ncross,layer.drops=NULL, excludep = TRUE,
                                          SSB.reduce = T, CIRCLES.d = distpseud,
                                          SINK=TRUE, species.name= species[i],
                                          MAXENT=0, MAXNET=0, MAXLIKE=0,GBM=1, GBMSTEP=0, RF=1, 
                                          CF=0,GLM=1, GLMNET=0,GLMSTEP=0, GAM=1, 
                                          GAMSTEP=0, MGCV=0, MGCVFIX=0, 
                                          EARTH=0, RPART=0, NNET=0, FDA=0, 
                                          SVM=0, SVME=0, BIOCLIM=0, 
                                          DOMAIN=0, MAHAL=0,MAHAL01=0,
                                          ENSEMBLE.tune=TRUE, PROBIT=TRUE,
                                          threshold.method =  thres,
                                          threshold.sensitivity = 0.9,
                                          threshold.PresenceAbsence = TRUE, 
                                          ENSEMBLE.best=0, 
                                          ENSEMBLE.exponent=c(1, 2, 4, 6, 8),
                                          ENSEMBLE.min=0.7,
                                          Yweights="BIOMOD", factors=NULL,
                                          PLOTS=FALSE, formulae.defaults=TRUE,
                                          GBMSTEP.learning.rate=0.002)
  
  
  
  # step 2: create models that will be used for the raster predictions
  # models with input.weights <0.05 are excluded
  output_weights <- enm_step1$output.weights
  output_weights[output_weights < 0.05] <- 0

  cat("Step 2: model species distribution with selected ENM algorithms \n")
  
  enm_step2 <- ensemble.calibrate.models(x=bio_current, p=coord, 
                                         a=enm_step1$a,
                                         k=ncross, layer.drops=NULL,
                                         SINK=TRUE, species.name = species[i],
                                         models.keep=TRUE,evaluations.keep=TRUE,
                                         input.weights=output_weights,
                                         threshold.method =  thres,
                                         threshold.sensitivity = 0.9,
                                         threshold.PresenceAbsence = TRUE, 
                                         ENSEMBLE.tune=FALSE, PROBIT=TRUE,
                                         Yweights="BIOMOD", factors=NULL,
                                         PLOTS=FALSE, formulae.defaults=TRUE,
                                         GBMSTEP.learning.rate=0.002,
                                         models.save = TRUE)
  
  save(enm_step1,file = paste0("./models/", species[i],"_enmstep1.RData") )
  save(enm_step2,file = paste0("./models/", species[i],"_enmstep2.RData") )

  cat("Step 3.1: Generate map of current distribution \n")
  #step3: use previously calibrated models to construct consensus layers
  ensemble_current <- ensemble.raster(xn = bio_current_pred ,
                                      models.list = enm_step2$models,
                                      input.weights=output_weights,
                                      thresholds = enm_step2$models$thresholds,
                                      SINK=TRUE,
                                      RASTER.species.name = species[i], 
                                      RASTER.stack.name="_bio_current",
                                      RASTER.models.overwrite=T	
                                                                            )
  
  ### write raster for each gcm model in RCP45 and RCP85 
  # for (k in seq_along(gcm)){
  #   cat("Step 3.2: Predict future distribution, GCM", toupper(gcm[k]), "\n")
  #   
  #   #load GCM layers
  #   gcmfiles <- list.files(paste0(parentwd, "/data/worldclim/gcm/", gcm[k], "bi50/"), 
  #                          pattern = ".tif$",
  #                          full.names = TRUE)
  #   
  #   gcm_model <- raster::stack(gcmfiles)
  #   
  #   gcm_model <- subset(gcm_model, subset = bio_vif)
  #   
  #   crs(gcm_model) <- myproj
  #   
  #   gcm_model <- crop(gcm_model, mesoam)
  #   
  #   gcm_model <- stack(gcm_model)
  # 
  #   ensemble_gcm_model <- ensemble.raster(xn = gcm_model,
  #                                         models.list = enm_step2$models,
  #                                         input.weights = output_weights,
  #                                         thresholds = enm_step2$models$thresholds,
  #                                         SINK = TRUE,
  #                                         RASTER.species.name = species[i], 
  #                                         RASTER.stack.name = gcm[k])
  #   
  # }
  
  #return to parent directory
  setwd(parentwd)
  
}







