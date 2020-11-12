####################################################
# SCRIPT TO MODEL CURRENT AND FUTURE NICHES OF 
# TREE SPECIES USING ENSEMBLE MODELLING 
# Updated November 2020
# based on Sousa et al 2019 https://doi.org/10.7910/DVN/0O1GW1

#...................................................
#...................................................
# Packages ####



setwd("C:/Users/Pablo/OneDrive - Universidad de Córdoba/1_proyectos/2019_CocoAgroForecast/WP5 - suitability/")
# Final seletion of variables for modelling

BD_hist <- read.csv("./BD/BD_hist.csv")
BD_fut <- read.csv("./BD/BD_fut.csv")



bio_current <- subset(bio_current, subset = bio_vif)

#...................................................
#...................................................
# Run ensemble modelling ####

species <- sort(unique(df$acronym))


for (i in seq_along(species) ) {
  
  cat("\n######## \n Ensemble modelling for", species[i], "\n Time:", date())
  
  output <- paste0("processing/enm/", species[i], "/")
  dir.create(output, showWarnings = FALSE, recursive = TRUE)
  
  # BiodiversityR saves outputs on the current working directory
  # we set this to the intended output directory
  patentwd <- getwd()
  
  setwd(output)
  
  # sampling data
  coord <- df[df$acronym == species[i], ]
  
  coord <- coord[c("x","y")]
  
  if(nrow(coord) > 5000){
    # calculate largest distance
    largedist <- coord[ sample(row.names(coord), 3000) ,] %>%
      raster::pointDistance(., longlat = FALSE) %>%
      max(., na.rm = TRUE)

  } else{
    # calculate largest distance
    largedist <- coord %>%
      raster::pointDistance(., longlat = FALSE) %>%
      max(., na.rm = TRUE)
    
  }
  
  # make a convex hull and remove duplicated coordinates in the same grid-cell
  hull <- convHull(coord, lonlat = TRUE)
  # extent convex hull
  ext_hull <- gBuffer(hull@polygons, width = 0.1 * largedist)
  crs(ext_hull) <- myproj
  # define raster
  r <- raster(ext_hull)
  # set the resolution of the cells to 2-5 minutes see Ranjitkar et al (2016)
  res(r) <- res(bio_current)
  
  coord %<>%
    as.matrix() %>% 
    as.data.frame() %>%
    dismo::gridSample(., r, n=1)
  
  n <- nrow(coord)
  
  # define threshold 
  # see Liu et al (2013) doi:10.1111/jbi.12058
  if(n >  150) thres <- "MaxSens+Spec"
  
  # Run ensemble modelling
  # step 1: 4-fold cross-validation
  cat("\n Step 1: calibrating ENM algorithms \n")
  
  enm_step1 <- ensemble.calibrate.weights(x = bio_current, p = coord, an = 1000,
                                          k=4,layer.drops=NULL, excludep = TRUE,
                                          SINK=TRUE, species.name= species[i],
                                          MAXENT=1, GBM=1, GBMSTEP=1, RF=1, 
                                          GLM=1, GLMSTEP=1, GAM=1, 
                                          GAMSTEP=1, MGCV=1, MGCVFIX=1, 
                                          EARTH=1, RPART=1, NNET=1, FDA=1, 
                                          SVM=1, SVME=1, BIOCLIM=1, 
                                          DOMAIN=1, MAHAL=1,
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
                                         k=4, layer.drops=NULL,
                                         SINK=TRUE, species.name = species[i],
                                         models.keep=TRUE,
                                         input.weights=output_weights,
                                         threshold.method =  thres,
                                         threshold.sensitivity = 0.9,
                                         threshold.PresenceAbsence = TRUE, 
                                         ENSEMBLE.tune=FALSE, PROBIT=TRUE,
                                         Yweights="BIOMOD", factors=NULL,
                                         PLOTS=FALSE, formulae.defaults=TRUE,
                                         GBMSTEP.learning.rate=0.002,
                                         models.save = TRUE)

  
  cat("Step 3.1: Generate map of current distribution \n")
  #step3: use previously calibrated models to construct consensus layers
  ensemble_current <- ensemble.raster(xn = bio_current ,
                                      models.list = enm_step2$models,
                                      input.weights=output_weights,
                                      thresholds = enm_step2$models$thresholds,
                                      SINK=TRUE,
                                      RASTER.species.name = species[i], 
                                      RASTER.stack.name="bio_current")
  
  ### write raster for each gcm model in RCP45 and RCP85 
  for (k in seq_along(gcm)){
    cat("Step 3.2: Predict future distribution, GCM", toupper(gcm[k]), "\n")
    
    #load GCM layers
    gcmfiles <- list.files(paste0(parentwd, "/data/worldclim/gcm/", gcm[k], "bi50/"), 
                           pattern = ".tif$",
                           full.names = TRUE)
    
    gcm_model <- raster::stack(gcmfiles)
    
    gcm_model <- subset(gcm_model, subset = bio_vif)
    
    crs(gcm_model) <- myproj
    
    gcm_model <- crop(gcm_model, mesoam)
    
    gcm_model <- stack(gcm_model)

    ensemble_gcm_model <- ensemble.raster(xn = gcm_model,
                                          models.list = enm_step2$models,
                                          input.weights = output_weights,
                                          thresholds = enm_step2$models$thresholds,
                                          SINK = TRUE,
                                          RASTER.species.name = species[i], 
                                          RASTER.stack.name = gcm[k])
    
  }
  
  #return to parent directory
  setwd(parentwd)
  
}







