

setwd("C:/Users/Pablo/OneDrive - Universidad de Córdoba/1_proyectos/2019_CocoAgroForecast/WP5 - suitability/") 
# setwd("C:/Users/USUARIO/Universidad de Córdoba/Pablo Gonzalez Moreno - 2019_CocoAgroForecast/WP5 - suitability/BD/dataverse_files") 

library(vegan)
library(venneuler)  #graficas de Venn
library("VennDiagram")

source(file = "Scripts/Funcion_varpartGLMFull.R")

datos_analisis_raw <- read.csv("processing/figures/yield_variables.csv")

datos_analisis <- datos_analisis_raw[!is.na(datos_analisis_raw$cocoa_prod_total_kgs),]
summary(datos_analisis)

datos_analisis$cocoa_prod_total_kgs <- datos_analisis$cocoa_prod_total_kgs + 0.001
model_area <- glm(cocoa_prod_total_kgs ~ cocoa_land_used_morethan5_ha, data=datos_analisis,family = gaussian(link=log))
datos_analisis$prod_corrected <- residuals(model_area)

# Second identify other variability from key factors
full.model <- glm(prod_corrected ~ clay_mean30100cm_SoilGrids2+cocoa_pest_applied_unitha+cocoa_fung_applied_unitha+cocoa_pruning_lab_n_ha+suitability+cocoa_losses_pests_type_6, data=datos_analisis)
summary(full.model)

# Work only with the final variables
datos_part <- subset(datos_analisis,select=c(prod_corrected,clay_mean30100cm_SoilGrids2,cocoa_pest_applied_unitha,cocoa_fung_applied_unitha,cocoa_pruning_lab_n_ha,suitability,cocoa_losses_pests_type_6))
datos_part <- na.omit(datos_part)
attach(datos_part)
environment <- cbind(clay_mean30100cm_SoilGrids2, suitability)
pests <- cbind(cocoa_pest_applied_unitha,cocoa_fung_applied_unitha,cocoa_losses_pests_type_6)
management <- cocoa_pruning_lab_n_ha
detach(datos_part)

#install.packages("vegan")
library(vegan)  
PART <- varpart(datos_part$prod_corrected, environment, pests, management) 
plot(PART,Xnames=c("environment","pests","management")) 



# Diagrama de venn proporcional
fract <- PART$part$indfract[,3]
#lo de abajo tendrías que cambiarlo según el número de grupos
v <- venneuler(c(A=fract[1], B=fract[2], C=fract[3],"A&B"=fract[4],"B&C"=fract[5],"A&C"=fract[6],"A&B&C"=fract[7]))
v$labels <- c("","",""
plot(v)

v <- venn.diagram(c(A=fract[1], B=fract[2], C=fract[3],"A&B"=fract[4],"B&C"=fract[5],"A&C"=fract[6],"A&B&C"=fract[7]),filename = "processing/figures/prueba.tif")
plot(v)
labels(v)
