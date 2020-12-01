####################################################
###### Calculate change in suitability

# .........................................
# .........................................
# Packages ####
library("tidyverse")
library("magrittr")
library("svglite")
library("gridExtra")
library("sp")
library("dismo")
library("raster")
library("rgdal")
library("rgeos")

# .........................................
# .........................................
# Read species names
setwd("C:/Users/Pablo/OneDrive - Universidad de Córdoba/1_proyectos/2019_CocoAgroForecast/WP5 - suitability")
BD_calib <- read.csv("./BD/BD_calibrate.csv")
sp <- unique(BD_calib$species)
spnames <- sort(sp) # hacer acrónimos
# .........................................
# .........................................
# Prepare data and files to process maps ####
# names of RCP scenarios
RCP <- c("ssp126","ssp585")

#define extention of study area
ext <- raster::extent(-17,15,2,13)
# define projection
proj <- "+proj=longlat +datum=WGS84"

output <- "processing/species_sets/"
dir.create(output, showWarnings = FALSE, recursive = TRUE)

### read shapefile of inland water
# lakes <- "data/shapefiles/water_areas/mesoamerica_water_areas_dcw.shp"
# lakes %<>%  
#   readOGR(.) %>%
#   subset(.$HYC_DESCRI == "Perennial/Permanent") %>%
#   raster::crop(. , ext)
# 
# 
# # Read country borders 
# border <- "data/shapefiles/country_borders/Mesoamerica.shp"
# border %<>% 
#   readOGR(.) %>%
#   raster::crop(. , ext)

# .........................................
# .........................................
# Run over spnames ####

# create NULL dataframe to keep data on area changes for all species 
changes_all <- NULL

for (i in seq_along(spnames)) {
  cat(i, "out of", length(spnames), "\n")
  
  presence <- paste0("processing/enm/", spnames[i], "/ensembles/presence/")

  #create directory to save rasters
  output2 <- paste0(output, spnames[i])
  
  dir.create(output2, showWarnings = FALSE, recursive = TRUE)
  
  # load rasters of current niches
  # raster of current presence-absence
  presence_current <- raster::stack(paste0(presence,spnames[i], "__bio_current.grd"))
  # crop raster using Mesoamerica extention
  presence_current <- raster::crop(presence_current, ext)
  # # remove Caribbean islands
  # presence_current <- raster::mask(presence_current, border, inverse=FALSE)
  # # remove presence in lakes
  # presence_current <- raster::mask(presence_current, lakes, inverse=TRUE)
  presence_current <- raster::stack(presence_current)

  # run over RCP models
  for(j in seq_along(RCP)) {
    # read presence-absence rasters under modelled RCP scenario##
   presence_rcp <- raster::stack(list.files(paste0("processing/enm/", spnames[i] , "/ensembles/presence"),
               pattern = paste0(RCP[j],"_2021-2040.gri$"),
               full.names = TRUE))

    # mean values of 1-0 raster (presence and absence)
    # this is also the measure agreement raster
    presence_rcp_mean <- raster::calc(presence_rcp, fun = mean)
    

    # Define likelihood RCP mask
    # more than 66% of the raster must agree on the presence
    # of the species in each grid cell
    # further information about likelihood at Mastrandrea el al (2010)
    # raster for future presence-absence
    presence_rcp <- presence_rcp_mean
    presence_rcp[presence_rcp[] < 0.659] <- 0 
    presence_rcp[presence_rcp[] >= 0.659] <- 1 
    #raster of threshold for future presence (defined by likelihood)
    thresh_presence_rcp <- presence_rcp
    thresh_presence_rcp[thresh_presence_rcp[] == 0] <- NA
    
    #identify change in suitability in RCP scenario
    #future minus current raster
    presence_rcp[presence_rcp[]==1] <- 2
    #change in suitability codes
    #  1 = always suitable
    #  0 = never suitable
    # -1 = no longer suitable
    #  2 = new habitat
    change_suit_rcp <- raster::overlay(presence_rcp, 
                                       presence_current, 
                                       fun = function(x,y) {(x-y)})
    
    # write tif files
    # current presence-absence layer 
    writeRaster(change_suit_rcp, 
                filename = paste0(output2,"/",spnames[i],"_",RCP[j],"_change.tif"), 
                format = "GTiff", 
                overwrite = TRUE)
    writeRaster(presence_rcp, 
                filename = paste0(output2,"/",spnames[i],"_",RCP[j],"_presence.tif"), 
                format = "GTiff", 
                overwrite = TRUE)
    writeRaster(presence_rcp_mean, 
                filename = paste0(output2,"/",spnames[i],"_",RCP[j],"_presence_agreement.tif"), 
                format = "GTiff", 
                overwrite = TRUE)
  }
  
  # current presence-absence
  writeRaster(presence_current, 
              filename = paste0(output2,"/",spnames[i],"_presence.tif"), 
              format="GTiff", 
              overwrite=TRUE)
}





