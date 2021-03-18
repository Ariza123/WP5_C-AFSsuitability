####################################################
# #### CALCULATE model and variable importance

#..............................................
#..............................................
# Packages ####
library("raster")
library("gridExtra")
library(ggplot2)
library(ggExtra)
library(ggpubr)
library(gbm)
library(tidyr)
library(viridis)

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


#..............................................
#..............................................


## Working directory
parentwd <- getwd() # model outputs
wcwd <- "C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability" # raster

# parent table for evaluations
evaluations_full <- NULL
importance_full <- NULL

pb <- txtProgressBar(min = 1, max = length(species), style = 3) # progress bar

for (i in seq_along(species) ) {
  
  output <- paste0("processing/enm/", species[i], "/")
  setwd(output)
  
  # load(file = paste0("./models/", species[i],"_enmstep1.RData") )
  load(file = paste0("./models/", species[i],"_enmstep2.RData") )
  ################
  # Save evaluations
  eval.sp <- enm_step2$eval.table
  eval.sp$specie <- species[i]
  eval.sp$model <- row.names(eval.sp)
  
  #save in parent table
  evaluations_full <- rbind(evaluations_full,eval.sp)
  
  #####################
  # Save importance
  imp.sp <- (enm_step2$models$GBM)
  importance <- as.data.frame(t(summary(imp.sp)[2]))
  importance$specie <- species[i]
  
  #save in parent table
  importance_full <- rbind(importance_full,importance)
  
  #return to parent directory
  setwd(parentwd)
  
  setTxtProgressBar(pb, i)
  
}

close(pb) # close progress bar

write.csv(evaluations_full,file="processing/figures/sdm_evaluations.csv")
write.csv(importance_full,file="processing/figures/sdm_gbm_importance.csv")
###################################
## Boxplot evaluations per species

evaluations_plot <- subset(evaluations_full,model!="ENSEMBLE")

etiquetas <- evaluations_plot[evaluations_plot$AUC<0.8,"specie"]

png(filename = "processing/figures/SDMevaluations.png",
    width = 500, height = 500, units = "px", pointsize = 18,
    bg = "white")

pevalu <- ggscatterhist(data=evaluations_plot,x="AUC",y="TSS", color="model", 
              size = 3, alpha = 0.6,
              palette = c("#00AFBB", "#E7B800", "#FC4E07","light green"),
              margin.plot = "boxplot", 
              margin.params = list(fill = "model", size = 0.2),
              label="specie",font.label = c(10, "plain"),label.select=etiquetas,
              ggtheme=theme_bw(base_size = 14))

dev.off()


###################################
## Key variables per species ####

names(importance_full) <- c("BIO2","BIO13","pH","BIO4","BIO18","BIO9","BIO8","BIO19","CEC","N","SAND","BIO15","SPECIES")

importance_plot <- importance_full %>% 
  pivot_longer(cols=-SPECIES,names_to="variables") 

# transform into long version

png(filename = "processing/figures/SDMimportance.png",
    width = 800, height = 400, units = "px", pointsize = 18,
    bg = "white")

# pimp <- 
  ggplot(importance_plot,aes(x=reorder(variables,-value,FUN=median,), y=value,fill=variables))+
    geom_violin(width=1.4) +
    geom_boxplot(width=0.1, color="grey", alpha=0.2) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    theme_bw(base_size = 16) + theme(legend.position="none") +
    xlab("") +
    ylab("Variable importance (%)")

dev.off()

######################
## PLOT IDONEIDAD VS BIO4 ####

# Prepare data and files to process maps ####
# names of RCP scenarios
RCP <- c("ssp126","ssp585")
scenario <- c("current","2021-2040","2041-2060")
#define extention of study area
ext <- raster::extent(-17,15,2,13)
# define projection
proj <- "+proj=longlat +datum=WGS84"

# Load BIO4
common_path<-"./BD/Current/"
files <- list.files(
  path <- common_path,
  pattern <- "\\.tif$",
  recursive = F,          
  full.names = TRUE          
)
bio_4 <- crop(raster(files[15]),ext)

stats_especie_all <- NULL

for (i in spnames) {
  for (s in seq_along(scenario)){
    for (j in RCP) {
      
      if (scenario[s]=="current") {
      cocoa <- raster("processing/enm/Theobroma cacao/ensembles/suitability/Theobroma cacao__bio_current.grd")
      r <- raster(paste("processing/species_sets/", i, "/", i, "_presence.tif",sep="" )) 
      stats_especie <- data.frame(array(dim = c(1, 4)))
      names(stats_especie) <- c("specie","scenario","cocoa","bio4")
      stats_especie[1,1] <- i
      stats_especie[1,2] <- paste(scenario[s],sep="_")
      stats_especie[1,3] <- mean(values(mask(cocoa,r,maskvalue=0)),na.rm=T)
      stats_especie[1,4] <-  mean(values(mask(bio_4,r,maskvalue=0)),na.rm=T)
      
      stats_especie_all <- rbind(stats_especie_all,stats_especie)
      
      }else{
      cocoa <- raster(paste("processing/species_sets/Theobroma cacao/Theobroma cacao_", j ,"_",scenario[s], "_presence_agreement.tif",sep=""))
      r <- raster(paste("processing/species_sets/", i, "/", i, "_", j ,"_",scenario[s],"_presence.tif",sep="" ))
      
      stats_especie <- data.frame(array(dim = c(1, 4)))
      names(stats_especie) <- c("specie","scenario","cocoa","bio4")
      stats_especie[1,1] <- i
      stats_especie[1,2] <- paste(scenario[s],j,sep="_")
      stats_especie[1,3] <- mean(values(mask(cocoa,r,maskvalue=0)),na.rm=T)
      stats_especie[1,4] <-  mean(values(mask(bio_4,r,maskvalue=0)),na.rm=T)
      
      stats_especie_all <- rbind(stats_especie_all,stats_especie)
      
      }
      
    }
  }
}

stats_especie_all[stats_especie_all$scenario=="current","cocoa"] <- stats_especie_all[stats_especie_all$scenario=="current","cocoa"]/1000
stats_especie_all <- unique.data.frame(stats_especie_all)
# export dataframe
write_csv(stats_especie_all, paste0("processing/figures/suitability_vs_bio4.csv"))

# Hacemos gráficas
stats_especie_all <- read.csv("processing//figures/suitability_vs_bio4.csv")
dic_species <- read.csv("BD/dic_species.csv",sep=";")

stats_especie_all <- merge(stats_especie_all,dic_species,by.x="specie",by.y="Scientific.names",all.x=T)

library(ggrepel)
png(filename = "processing/figures/suitability_vs_bio4.png",
    width = 600, height = 600, units = "px", pointsize = 18,
    bg = "white")

ggplot(subset(stats_especie_all,scenario=="current"),aes(x=cocoa, y=bio4,label=specie,colour = factor(Uses)))+
  geom_point()+geom_text_repel()+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_bw(base_size = 16) + theme(legend.position="none") +
  xlab("Cocoa suitability") +
  ylab("BIO4-Temperature seasonality (ºC)")

#  annotation_custom(,xmin=0.6, xmax=1, ymin=1.4, ymax=1.6) #para añadir algo dentro


dev.off()

### Adding trajectories
# change format to wide
stats_especie_all %<>%  
  mutate(bio4=bio4/100) %>%
  tidyr::pivot_wider(names_from=c(scenario),values_from=c(cocoa,bio4))
  
names(stats_especie_all)<-c("specie","Local.names","X","uses","X.1","Tree.propagation","cocoa_current","cocoa_20212040_ssp126" ,"cocoa_20212040_ssp585", "cocoa_20412060_ssp126",
 "cocoa_20412060_ssp585" ,"bio4_current","bio4_20212040_ssp126"  ,"bio4_20212040_ssp585"  ,"bio4_20412060_ssp126","bio4_20412060_ssp585" )

png(filename = "processing/figures/suitability_vs_bio4_traj.png",
    width = 600, height = 600, units = "px", pointsize = 18,
    bg = "white")

ggplot(stats_especie_all,aes(x=cocoa_current,  y=bio4_current,color=factor(uses),label=specie))+
  geom_point()+geom_text_repel()+
  geom_segment( mapping = aes(x=cocoa_current, xend=cocoa_20412060_ssp585,
                    y=bio4_current, yend=bio4_20412060_ssp585,color=factor(uses)),
                    arrow=arrow(length = unit(0.2, "cm")),
                    size=0.5)+
  
  scale_color_viridis(discrete=TRUE) +
  theme_bw(base_size = 16) + theme(legend.position="none") +
  xlab("Cocoa suitability") +
  ylab("BIO4-Temperature seasonality (ºC)")

#  annotation_custom(,xmin=0.6, xmax=1, ymin=1.4, ymax=1.6) #para añadir algo dentro
  
  
dev.off()

