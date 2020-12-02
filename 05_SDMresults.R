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
species <- sort(sp) # hacer acrónimos
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
## Key variables per species

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
