####################################################
# #### CALCULATE THE CHANGES IN SUITABILITY OF FOCAL SPECIES 
# IN FUTURE SUITABLE AREAS FOR COFFEE AND COCOA
# This script generates the dataframe for Fig. 4

#..............................................
#..............................................
# Packages ####
library("tidyverse")
library("svglite")
library("magrittr")
library("raster")
library("gridExtra")
library("scales")

#..............................................
#..............................................
# Read tree data ####
# Read species names
setwd("C:/Users/Pablo/OneDrive - Universidad de Córdoba/1_proyectos/2019_CocoAgroForecast/WP5 - suitability")
BD_calib <- read.csv("./BD/BD_calibrate.csv")
sp <- unique(BD_calib$species)
spnames <- sort(sp) # hacer acrónimos
# .........................................
# .........................................
# Prepare data and files to process maps ####
# names of RCP scenarios
RCP <- c("current","ssp126","ssp585")
scenario <- c("2021-2040","2041-2060")
#define extention of study area
ext <- raster::extent(-17,15,2,13)
# define projection
proj <- "+proj=longlat +datum=WGS84"

#..............................................
#..............................................
# Read changes of focal species ####
# this loop calculate the frequencies of suitability of each 
# focal species using the mask generated above.
changes <- NULL

for (i in spnames) {
  for (s in seq_along(scenario)){
    for (j in RCP) {
      
      if (j=="current") {
        r <- raster(paste("processing/species_sets/", i, "/", i, "_presence.tif",sep="" )) 
        }else{
          r <- raster(paste("processing/species_sets/", i, "/", i, "_", j ,"_",scenario[s],"_change.tif",sep="" ))
          }
      
        land <- r
        frequencies <- data.frame(array(dim = c(1, 11)))
        names(frequencies) <- c("acronym","system","never_suitable(0)", "remains_suitable(1)", 
                                "no_longer_suitable(-1)", "new_habitat(2)","area_km2","remains_suitable_(1)_perc",
                                "no_longer_suitable(-1)_perc", "new_habitat_(2)_per","net_change_perc")
        freq1 <- as.data.frame(raster::freq(land, useNA="no"))
        freq1 <- freq1[is.na(freq1[, 1]) == F, ]
        
        frequencies[1,1] <- i
        frequencies[1,2] <- paste(scenario[s],j,sep="_")
        
        if (length(freq1[freq1[, 1] == 0, 2]) > 0) {
          frequencies[1, 3] <- freq1[freq1[, 1] == 0, 2]
        }
        if (length(freq1[freq1[, 1] == 1, 2]) > 0) {
          frequencies[1, 4] <- freq1[freq1[, 1] == 1, 2]
        }
        if (length(freq1[freq1[, 1] == -1, 2]) > 0) {
          frequencies[1, 5] <- freq1[freq1[, 1] == -1, 2]
        }
        if (length(freq1[freq1[, 1] == 2, 2]) > 0) {
          frequencies[1, 6] <- freq1[freq1[, 1] == 2, 2]
        }
        
        frequencies[is.na(frequencies)] = 0
        
        #percent of always suitable areas
        frequencies[8] <- frequencies[,4] / sum(frequencies[,c(4,5)], na.rm = TRUE) * 100
        #percent of no longer suitable areas
        frequencies[9] <- frequencies[,5] / sum(frequencies[,c(4,5)], na.rm = TRUE) * 100
        #percent of new suitable areas
        frequencies[10] <- frequencies[,6] / sum(frequencies[,c(4,5)], na.rm = TRUE) * 100
        #percent of net change 
        frequencies[11] <- frequencies[,10] - frequencies[,9]
        #add to changes dataframe
        changes <- rbind(changes, frequencies)
        
      
    }
   }
  }
  
  # changes %<>% 
  #   inner_join(., sp, by="acronym") %>% 
  #   filter(. , !grepl("current", system)) 
  
  # export dataframe
  write_csv(changes, paste0("processing/figures/wining_losing_sp.csv"))
  
  #..............................................
  #..............................................
  # Calculate changes ####
  changes <- changes[,-c(7:11)]
  names(changes) <- c("acronym","System","never","remain","no_longer","new")
  
  changes %<>% 
    mutate(area = rep("suitable", nrow(changes)),
           area_raster = never + remain + no_longer + new, #number of cells (areas) the raster cover
           cover = ((remain + no_longer) * 100) / area_raster, #proportion of current area
           future_cover = ((remain + new) * 100) / area_raster, #proportion of future areas
           change = future_cover  - cover) %>%  #change between current and future suitable areas
    as_tibble()
  
  # make labels of systems (land use)
  
  changes$System_label <- as.factor(changes$System)
  # levels(changes$System_label) <- RCP
  
  
  # add names and labels of focal species 
  # uses <- sort(unique(changes$main_use))
  
  #..............................................
  #..............................................
  # Make charts ####
  # Loop por emission scenario
  plots <- list()
  it <- 1
  for (s in seq_along(scenario)){
  
  for(j in c(2:3)){

      df <- subset(changes, grepl(scenario[s], changes$System))
      df <- subset(df, grepl(RCP[j], df$System))
      df$area <- " "
      head(df)
      
      df %<>% arrange(. , change)
      
      df$id <- as.integer(factor(df$acronym, levels = unique(df$acronym)))
      
      lab <- ""
      
      p <- ggplot(data=df) +
        geom_point(aes(x=cover,y=id), pch  = 21, size=2, fill="grey",colour="black") +
        geom_segment(data=subset(df, df$change <= 0), 
                     mapping = aes(x=cover, xend=future_cover,
                                   y=id, yend=id),
                     arrow=arrow(length = unit(0.1, "cm")),
                     size=1, 
                     color= "#ca0020") +
        geom_segment(data=subset(df, df$change > 0), 
                     mapping = aes(x=cover, xend=future_cover,
                                   y=id, yend=id),
                     arrow=arrow(length = unit(0.1, "cm")),
                     size=1, 
                     color= "#0571b0") +
        scale_y_continuous(breaks = as.integer(df$id), labels = df$acronym) +
        scale_x_continuous(breaks = seq(0,100, by=25), limits = c(0,100)) +
        labs(x = paste0("Suitable area (%)"),
             y = NULL) +
        facet_grid(area ~ System_label) +
        theme(axis.text.y = element_text(size = 10, angle = 0, hjust = 1, 
                                         vjust = 0.5, face="italic", colour = "black"),
              axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, 
                                         vjust = 1, face="plain", colour = "black"),
              axis.title = element_text(size = 12, face="bold"),
              panel.border = element_rect(colour = "black", fill=NA, size=0.5),
              plot.background = element_blank(),
              panel.background = element_blank(),
              strip.text.x = element_text(size = 12, colour = "black"),
              strip.text.y = element_text(size=12, colour = "black"),
              strip.background = element_rect(colour="black", fill="#FFFFFF")) 
      
      
      plots[[it]] <- p
      it <- it+1
  
      }
    }
  
  p20 <- grid.arrange(
    plots[[1]],
    plots[[2]],
     nrow = 1)
  
  p40 <- grid.arrange(
    plots[[3]],
    plots[[4]],
    nrow = 1)

  ggsave(paste0("processing/figures/changessuitability_ord_change21-40.png"),
         plot = p20,
         width = 30,
         height = 15,
         units = "cm")

  ggsave(paste0("processing/figures/changessuitability_ord_change41-60.png"),
         plot = p40,
         width = 30,
         height = 15,
         units = "cm")


