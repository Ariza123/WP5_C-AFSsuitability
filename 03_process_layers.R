####################################################
###### SCRIPT TO PROCESS LAYERS FROM ENSEMBLE MODELLING
# Updated 13Aug2018
####################################################

library("tidyverse")
library("magrittr")
library("sp")
library("dismo")
library("raster")
library("maptools")
library("rgdal")
library("rgeos")


# Define extention of study region
ext <- raster::extent(-17,15,2,13)
proj <- "+proj=longlat +datum=WGS84" 

# Read species names and acronyms
setwd("C:/Users/Pablo/OneDrive - Universidad de Córdoba/1_proyectos/2019_CocoAgroForecast/WP5 - suitability")
BD_calib <- read.csv("./BD/BD_calibrate.csv")
sp <- unique(BD_calib$species)
  # faltarían acrónimos


RCP <- c("ssp126","ssp585")



# Read rasters of tree species
# run over the raster of current presence
# add to a list then stack 
tree <- list()

pb <- txtProgressBar(min = 1, max = length(sp), style = 3)

spnames <- sort(sp) # hacer acrónimos

for (i in seq_along(spnames)) {
  
  r <- list.files(paste0("processing/enm/", spnames[i] , "/ensembles/presence"),
                  pattern = "_bio_current.gri$", 
                  full.names = TRUE)
  
  r %<>% 
    raster::stack( . ) %>% 
    raster::crop(. , ext) 
    # raster::mask(. , border, inverse = FALSE) %>% 
    # raster::mask(. , lakes, inverse = TRUE)
  
  tree[[i]] <- r
     
  
  setTxtProgressBar(pb, i)
  
}

close(pb)

# convert this list into a raster stack 
tree %<>%
  raster::stack(.)

names(tree) <- gsub("_bio_current_presence", "", names(tree))

output <- "processing/layers_comb/"

dir.create(output,
           showWarnings = FALSE,
           recursive = TRUE)


# Export all layers combined as a single raster
writeRaster(tree, 
            filename = "./processing/layers_comb/trees_baseline.grd", 
            format = "raster", 
            overwrite = TRUE, 
            bylayer = FALSE)

# Export sum of all presences
tree %<>% 
  raster::calc(. , fun = sum)

writeRaster(tree, 
            filename = "./processing/layers_comb/sum_trees_baseline.tif", 
            format = "GTiff", 
            overwrite = TRUE)

# Read layers of future presence for all species
tree_rcp <- list()

pb <- txtProgressBar(min = 1, max = length(sp), style = 3)

for (i in seq_along(spnames)) {

  for (j in seq_along(RCP)){

    #read rasters of k rcp scenarios
    r <- list.files(paste0("processing/enm/", spnames[i] , "/ensembles/presence"),
                    pattern = paste0(RCP[j],"_2021-2040.gri$"),
                    full.names = TRUE)
    r %<>%
      raster::stack(. ) %>%
      raster::crop(. , ext) %>% #crop rasters within defined extention
      raster::calc(. , fun = mean) #calculate the mean of all k rcp scenarios
      # raster::mask(. , border, inverse = FALSE) %>% 
      # raster::mask(. , lakes, inverse = TRUE)
    
    # if less than 66% of models agree with the presence then 0
    r[r[] < 0.659 ] <- 0
    # if more than 66% of models agree with the presence then 1 
    r[r[] > 0.659 ] <- 1 
    
    rname <- paste0(sp[i],RCP[j])
    
    tree_rcp[[rname]] <- r
  
  }
  
  setTxtProgressBar(pb, i)
}

close(pb)

# Export layers 
for (i in seq_along(RCP)){
  
  r <- subset(tree_rcp, grepl(RCP[i] , names(tree_rcp)))
  
  r <- raster::stack(r)
  
  names(r) <- gsub("[0-9]+", "", names(r))
  
  writeRaster(r, 
              filename = paste0("processing/layers_comb/trees_rcp", RCP[i],".grd"), 
              format="raster",
              overwrite = TRUE, 
              bylayer = FALSE)
  
  # sum the future presence of all focal species in i RCP scenario
  r <- raster::calc(r, fun=sum)
  # add to list
  writeRaster(r, 
              filename = paste0("processing/layers_comb/sum_trees_rcp", RCP[i],".tif"), 
              format = "GTiff", 
              overwrite = TRUE)

}

# Generate rasters dividing species per main use
dic_species <- read.csv("BD/dic_species.csv",sep=";")

sp <- merge(data.frame(sp),dic_species,by.x="sp",by.y="Scientific.names",all.x=T)
uses <- sort(unique(sp$Uses))

for(i in seq_along(uses)){


  for(j in seq_along(RCP)){
    tree_rcp <- stack(paste("processing/layers_comb/trees_rcp", RCP[j],".gri",sep="" ))
    r <- subset(tree_rcp, names(tree_rcp) %in% paste0(sp_use[i] , RCP[j]) )

    r <- raster::stack(r)

    names(r) <- gsub("[0-9]+", "", names(r))

    writeRaster(r,
                filename = paste0("processing/layers_comb/", uses[i] ,"_trees_rcp", RCP[j],".grd"),
                format = "raster",
                overwrite = TRUE,
                bylayer = FALSE)

    # sum the future presence of all focal species in i RCP scenario
    r <- raster::calc(r, fun = sum)
    # add to list
    writeRaster(r,
                filename = paste0("processing/layers_comb/sum_", uses[i] ,"_trees_rcp",RCP[j],".tif"),
                format = "GTiff",
                overwrite = TRUE)

  }

}

 
