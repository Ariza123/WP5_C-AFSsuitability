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
# if(!require(rJava)) install.packages("rJava") else library(rJava)
if(!require(rgdal)) install.packages("rgdal") else library(rgdal)
if(!require(geosphere)) install.packages("geosphere") else library(geosphere)
if(!require(maps)) install.packages("maps") else  library(maps)
if(!require(alphahull)) install.packages("alphahull") else library(alphahull)
if(!require(tm)) install.packages("tm") else  library(tm)
# if(!require(maxent)) install.packages("maxent") else library(maxent)
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



#...................................................
#...................................................
# Run ensemble modelling ####

gcm <- c("BCC-CSM2-MR","CanESM5","CNRM-CM6-1","CNRM_ESM2-1","IPSL-CM6A-LR","MIROC6","MIROC-ES2L")
scenario <- c("2021-2040","2041-2060")
rcp <- c("ssp126","ssp585")
species <- species_all[ini:fin]

## Working directory
parentwd <- getwd() # model outputs
wcwd <- "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability" # raster

for (i in seq_along(species) ) {
  
  cat("\n######## \n Ensemble modelling for", species[i], "\n Time:", date())
  
  output <- paste0("processing/enm/", species[i], "/")
  
  # BiodiversityR saves outputs on the current working directory
  # we set this to the intended output directory
  
  setwd(output)
  
  load(file = paste0("./models/", species[i],"_enmstep1.RData") )
  load(file = paste0("./models/", species[i],"_enmstep2.RData") )

  output_weights <- enm_step1$output.weights
  output_weights[output_weights < 0.05] <- 0

  ### write raster for each gcm model in RCP45 and RCP85 
  for (k in seq_along(gcm)){
    
    for (s in seq_along(scenario)){
      
      for (r in seq_along(rcp)){
        
        cat("Step 3.2: Predict future distribution, GCM", toupper(gcm[k]),"_", rcp[r],"_",scenario[s], "\n")
    
        #load GCM layers
        gcmfiles <- paste0(wcwd, "/BD/Future/wc2.1_2.5m_bioc_", gcm[k], "_", rcp[r],"_",scenario[s],"_corr.tif")
    
        gcm_model <- raster::stack(gcmfiles)
        gcm_model <- gcm_model[[c(2,4,8,9,13,15,18,19)]] # select our climate variables
        
        bio_future <- bio_current_pred
        bio_future[[5]] <- gcm_model[[1]] # overwrite climate projectios
        bio_future[[6]] <- gcm_model[[2]] # overwrite climate projectios
        bio_future[[7]] <- gcm_model[[3]] # overwrite climate projectios
        bio_future[[8]] <- gcm_model[[4]] # overwrite climate projectios
        bio_future[[9]] <- gcm_model[[5]] # overwrite climate projectios
        bio_future[[10]] <- gcm_model[[6]] # overwrite climate projectios
        bio_future[[11]] <- gcm_model[[7]] # overwrite climate projectios
        bio_future[[12]] <- gcm_model[[8]] # overwrite climate projectios
        names(bio_future) <- names(bio_current_pred)
        
        ensemble_gcm_model <- ensemble.raster(xn = bio_future,
                                              models.list = enm_step2$models,
                                              input.weights = output_weights,
                                              thresholds = enm_step2$models$thresholds,
                                              SINK = TRUE,
                                              RASTER.species.name = species[i],
                                              RASTER.stack.name = paste(gcm[k],rcp[r],scenario[s],sep = "_"))
        save(ensemble_gcm_model,file = paste0("./models/", species[i],"_enmstep3.RData") )
      }
    }
  }
  #return to parent directory
  setwd(parentwd)
  
}

  






