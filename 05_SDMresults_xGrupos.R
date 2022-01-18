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



#..............................................
#..............................................

# Read species dictionary

species_dic <- read.csv("BD/dic_species.csv",sep=";")
species_dic <- dplyr::select(species_dic,c("Scientific.names","Habitat","Type"))

# Read importance per species it comes from 05_SDMresults.R
importance_full <- read.csv("processing/figures/sdm_gbm_importance.csv",sep=";")
importance_full <- importance_full[,2:14]
names(importance_full) <- c("BIO2","BIO13","pH","BIO4","BIO18","BIO9","BIO8","BIO19","CEC","N","SAND","BIO15","SPECIES")



#add species info
importance_full <- merge(importance_full,species_dic,by.x="SPECIES",by.y="Scientific.names")



###################################
## Key variables per species ####


# transform into long version
importance_plot <- importance_full %>% 
  pivot_longer(cols=-c("SPECIES","Habitat","Type"),names_to="variables") 

var_type <- data.frame(variables=c("BIO2","BIO13","pH","BIO4","BIO18","BIO9","BIO8","BIO19","CEC","N","SAND","BIO15"),
                       var_type=c("Bioclimatic","Bioclimatic","Edaphic","Bioclimatic","Bioclimatic","Bioclimatic","Bioclimatic","Bioclimatic","Edaphic","Edaphic","Edaphic","Bioclimatic")
)

importance_plot <- merge(importance_plot,var_type,by="variables")

png(filename = "processing/figures/SDMimportance.png",
    width = 800, height = 400, units = "px", pointsize = 18,
    bg = "white")

# Plot for all species
  ggplot(importance_plot,aes(x=reorder(variables,-value,FUN=median,), y=value,fill=variables))+
    geom_violin(width=1.4) +
    geom_boxplot(width=0.1, color="grey", alpha=0.2) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    theme_bw(base_size = 16) + theme(legend.position="none") +
    xlab("") +
    ylab("Variable importance (%)")

dev.off()

# Plot per group types
png(filename = "processing/figures/SDMimportance_groupType.png",
    width = 800, height = 400, units = "px", pointsize = 18,
    bg = "white")
ggplot(importance_plot, aes(x=reorder(variables,-value,FUN=median,),y=value, fill=Type)) + 
  geom_boxplot() + xlab("") + ylab("Variable importance (%)") + theme(axis.text.x = element_text(angle = 30, hjust = 1))
dev.off()

png(filename = "processing/figures/SDMimportance_groupHabitat.png",
    width = 800, height = 400, units = "px", pointsize = 18,
    bg = "white")
ggplot(importance_plot, aes(x=reorder(variables,-value,FUN=median,),y=value, fill=Habitat)) + 
  geom_boxplot() + xlab("") + ylab("Variable importance (%)") + theme(axis.text.x = element_text(angle = 30, hjust = 1))
dev.off()

png(filename = "processing/figures/SDMimportance_FuncVartype.png",
    width = 800, height = 400, units = "px", pointsize = 18,
    bg = "white")
  ggplot(importance_plot, aes(x=reorder(var_type,-value,FUN=median,),y=value, fill=Type)) + 
    geom_boxplot() + xlab("") + ylab("Variable importance (%)") + theme(axis.text.x = element_text(angle = 30, hjust = 1))
dev.off()



## Compare contrasts
library(multcomp)
library(lsmeans)

# Functional vs variable type
mod1 <- lm(value~var_type*Type,data=importance_plot)
summary(mod1)
#creating a new variable of interaction
# https://stats.oarc.ucla.edu/r/faq/how-can-i-test-contrasts-in-r/

importance_plot$int <- with(importance_plot, interaction(var_type, Type, sep = " x "))
mod2_type <- lm(value~int,data=importance_plot)
summary(mod2_type)

#using multcomp
test <- glht(mod2_type, linfct = mcp(int = "Tukey"))
summary(test)
groups <- cld(test)
#using lsmeans
type_lsm <- lsmeans(mod2_type,"int")
summary(type_lsm)
cld(type_lsm)

# create table to add labels
labs <- tibble(var_type=rep(c("Bioclimatic","Edaphic"),4),
               Type=c(rep("Fruit",2),rep("Leguminous",2),rep("Other",2),rep("Timber",2)),
               labels=toupper(groups$mcletters$Letters),
               variables =attr(groups$mcletters$Letters,"names"),
               ypos=rep(80,8))

png(filename = "processing/figures/SDMimportance_FuncVartype.png",
    width = 800, height = 400, units = "px", pointsize = 20,
    bg = "white")
ggplot(importance_plot, aes(x=reorder(var_type,-value,FUN=median,),y=value, fill=Type)) + 
  geom_boxplot() + xlab("") + ylab("Variable importance (%)") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_text(data=labs, aes(x=var_type,label=labels,y=ypos),
            position = position_dodge(width = .75))
dev.off()

#####
# Habitat vs variable type
mod1 <- lm(value~var_type*Habitat,data=importance_plot)
summary(mod1)
#creating a new variable of interaction
# https://stats.oarc.ucla.edu/r/faq/how-can-i-test-contrasts-in-r/

importance_plot$inthab <- with(importance_plot, interaction(var_type, Habitat, sep = " x "))
mod2 <- lm(value~inthab,data=importance_plot)
summary(mod2)

#using multcomp
test <- glht(mod2, linfct = mcp(inthab = "Tukey"))
summary(test)
groups <- cld(test)
groups
#using lsmeans
hab_lsm <- lsmeans(mod2,"inthab")
summary(hab_lsm)
cld(hab_lsm)

labs <- tibble(var_type=rep(c("Bioclimatic","Edaphic"),2),
                   Habitat=c(rep("Exotic",2),rep("Native",2)),
                    labels=toupper(groups$mcletters$Letters),
               variables =attr(groups$mcletters$Letters,"names"),
                   ypos=rep(80,4))


png(filename = "processing/figures/SDMimportance_HabitatVartype.png",
    width = 800, height = 400, units = "px", pointsize = 20,
    bg = "white")
ggplot(importance_plot, aes(x=reorder(var_type,-value,FUN=median,),y=value, fill=Habitat)) + 
  geom_boxplot() + xlab("") + ylab("Variable importance (%)") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_text(data=labs, aes(x=var_type,label=labels,y=ypos),
            position = position_dodge(width = .75))
dev.off()


# Accumulated  importance ####

importance_plot_group <- importance_plot %>% 
  group_by(SPECIES,Habitat,Type,var_type) %>%
  summarise(value=sum(value))

# Functional vs variable type
importance_plot_group$int <- with(importance_plot_group, interaction(var_type, Type, sep = " x "))
mod2_type <- lm(value~int,data=importance_plot_group)
summary(mod2_type)

#using multcomp
test <- glht(mod2_type, linfct = mcp(int = "Tukey"))
summary(test)
groups <- cld(test)

# create table to add labels
labs <- tibble(var_type=rep(c("Bioclimatic","Edaphic"),4),
               Type=c(rep("Fruit",2),rep("Leguminous",2),rep("Other",2),rep("Timber",2)),
               labels=toupper(groups$mcletters$Letters),
               variables =attr(groups$mcletters$Letters,"names"),
               ypos=rep(100,8))

png(filename = "processing/figures/SDMimportance_FuncVartype_acc.png",
    width = 800, height = 400, units = "px", pointsize = 20,
    bg = "white")
ggplot(importance_plot_group, aes(x=reorder(var_type,-value,FUN=median,),y=value, fill=Type)) + 
  geom_boxplot() + xlab("") + ylab("Variable importance (%)") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_text(data=labs, aes(x=var_type,label=labels,y=ypos),
            position = position_dodge(width = .75))
dev.off()

#####
# Habitat vs variable type

importance_plot_group$inthab <- with(importance_plot_group, interaction(var_type, Habitat, sep = " x "))
mod2 <- lm(value~inthab,data=importance_plot_group)
summary(mod2)

#using multcomp
test <- glht(mod2, linfct = mcp(inthab = "Tukey"))
summary(test)
groups <- cld(test)
groups


labs <- tibble(var_type=rep(c("Bioclimatic","Edaphic"),2),
               Habitat=c(rep("Exotic",2),rep("Native",2)),
               labels=toupper(groups$mcletters$Letters),
               variables =attr(groups$mcletters$Letters,"names"),
               ypos=rep(100,4))


png(filename = "processing/figures/SDMimportance_HabitatVartype_acc.png",
    width = 800, height = 400, units = "px", pointsize = 20,
    bg = "white")
ggplot(importance_plot_group, aes(x=reorder(var_type,-value,FUN=median,),y=value, fill=Habitat)) + 
  geom_boxplot() + xlab("") + ylab("Variable importance (%)") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_text(data=labs, aes(x=var_type,label=labels,y=ypos),
            position = position_dodge(width = .75))
dev.off()
