## PACKAGES 

library(pacman)
pacman::p_load(ggplot2,dplyr,sp,raster,skimr,lubridate,tidyr,gtable,gridExtra,plotrix,maptools,maps,ggmap,haven,usdm,MASS,Hmisc,mapdata)

## Introduce data

setwd("C:/Users/Pablo/OneDrive - Universidad de Córdoba/1_proyectos/2019_CocoAgroForecast/WP5 - suitability/") 

BD <- read.csv("BD/dataverse_files/BD_var_sel.csv")

## Data analysis

options(scipen=999)

#Focus on dependent variables

ggplot(BD,x=1) + geom_boxplot(aes(y=cocoa_prod_total_kgs))
ggplot(BD,x=1) + geom_boxplot(aes(y=cocoa_prod_total_kgsha))

#Remove outliers

OutVals = boxplot(BD$cocoa_prod_total_kgs)$out
Out_values<-which(BD$cocoa_prod_total_kgs %in% OutVals)

BD<-BD[-c(Out_values),]

OutVals = boxplot(BD$cocoa_prod_total_kgsha)$out
Out_values<-which(BD$cocoa_prod_total_kgsha %in% OutVals)

BD<-BD[-c(Out_values),]


# Correct the types of variables

str(BD)

BD <- BD %>%
  mutate(crops_important1 = as.factor(crops_important1), 
         crops_important2 = as.factor(crops_important2),
         cocoa_hh = as.factor(cocoa_hh),
         crops_important1_5ago = as.factor(crops_important1_5ago),
         crops_important2_5ago = as.factor(crops_important2_5ago),
         cocoa_years = as.factor(cocoa_years),
         cocoa_land_incdecnc_5ago = as.factor(cocoa_land_incdecnc_5ago),
         cocoa_land_inc_how_5ago = as.factor(cocoa_land_inc_how_5ago),
         cocoa_intercrop_yn = as.factor(cocoa_intercrop_yn),
         cocoa_intercrop_5yo = as.factor(cocoa_intercrop_5yo),
         cocoa_intercrop_5_25yo = as.factor(cocoa_intercrop_5_25yo),
         cocoa_intercrop_25yo = as.factor(cocoa_intercrop_25yo),
         cocoa_varieties_1 = as.factor(cocoa_varieties_1),
         cocoa_varieties_2 = as.factor(cocoa_varieties_2),
         cocoa_varieties_3 = as.factor(cocoa_varieties_3),
         cocoa_varieties_4 = as.factor(cocoa_varieties_4),
         cocoa_varieties_5 = as.factor(cocoa_varieties_5),
         cocoa_varieties_6 = as.factor(cocoa_varieties_6),
         cocoa_varieties_7 = as.factor(cocoa_varieties_7),
         cocoa_varieties_999 = as.factor(cocoa_varieties_999),
         cocoa_trees_rows_yn = as.factor(cocoa_trees_rows_yn),
         cocoa_gfert_yn = as.factor(cocoa_gfert_yn),
         cocoa_lfert_yn = as.factor(cocoa_lfert_yn),
         cocoa_manurecomp_yn = as.factor(cocoa_manurecomp_yn),
         cocoa_herb_yn = as.factor(cocoa_herb_yn),
         cocoa_pest_yn = as.factor(cocoa_pest_yn),
         cocoa_fung_yn = as.factor(cocoa_fung_yn),
         cocoa_weed_yn = as.factor(cocoa_weed_yn),
         cocoa_pruning_yn = as.factor(cocoa_pruning_yn),
         cocoa_irrigation_yn = as.factor(cocoa_irrigation_yn),
         cocoa_losses_disease_yn = as.factor(cocoa_losses_disease_yn),
         cocoa_losses_pests_yn = as.factor(cocoa_losses_pests_yn),
         cocoa_losses_disease_type_1 = as.factor(cocoa_losses_disease_type_1),
         cocoa_losses_disease_type_2 = as.factor(cocoa_losses_disease_type_2),
         cocoa_losses_disease_type_3 = as.factor(cocoa_losses_disease_type_3),
         cocoa_losses_disease_type_4 = as.factor(cocoa_losses_disease_type_4),
         cocoa_losses_disease_type_5 = as.factor(cocoa_losses_disease_type_5),
         cocoa_losses_disease_affect = as.factor(cocoa_losses_disease_affect),
         cocoa_losses_pests_type_1 = as.factor(cocoa_losses_pests_type_1),
         cocoa_losses_pests_type_2 = as.factor(cocoa_losses_pests_type_2),
         cocoa_losses_pests_type_3 = as.factor(cocoa_losses_pests_type_3),
         cocoa_losses_pests_type_4 = as.factor(cocoa_losses_pests_type_4),
         cocoa_losses_pests_type_5 = as.factor(cocoa_losses_pests_type_5),
         cocoa_losses_pests_type_6 = as.factor(cocoa_losses_pests_type_6),
         cocoa_losses_pests_type_7 = as.factor(cocoa_losses_pests_type_7),
         cocoa_losses_pests_type_9 = as.factor(cocoa_losses_pests_type_9),
         cocoa_losses_pests_type_10 = as.factor(cocoa_losses_pests_type_10),
         cocoa_losses_pests_type_999 = as.factor(cocoa_losses_pests_type_999),
         cocoa_losses_pests_affect = as.factor(cocoa_losses_pests_affect),
         cocoa_training_5years_yn = as.factor(cocoa_training_5years_yn),
         cocoa_gfert_times = as.factor(cocoa_gfert_times),
         cocoa_lfert_times = as.factor(cocoa_lfert_times),
         cocoa_manurecomp_times = as.factor(cocoa_manurecomp_times),
         cocoa_herb_times = as.factor(cocoa_herb_times),
         cocoa_pest_times = as.factor(cocoa_pest_times),
         cocoa_fung_times = as.factor(cocoa_fung_times),
         cocoa_weed_times = as.factor(cocoa_weed_times),
         n_crops_produced = as.factor(n_crops_produced),
         cocoa_land_numlandparcels = as.numeric(cocoa_land_numlandparcels),
         cocoa_trees_most = as.numeric(cocoa_trees_most),
         cocoa_pruning_times = as.factor(cocoa_pruning_times))


# Synthetic variables

Total_lab <- BD %>%
  dplyr::select(contains("lab"))

BD$Total_lab_n_ha<-rowSums(Total_lab)

BD$cocoa_trees_ha<- 10000/(BD$cocoa_tree_spacing_trees * BD$cocoa_tree_spacing_rows)


## Extract environmental information

#WorldClim

common_path<- "./BD/WorldClim/wc2.1_2.5m_bio/" 
files <- list.files(
  path <- common_path,
  pattern <- "\\.tif$",
  recursive = TRUE,          
  full.names = TRUE          
)

StackWC<-stack(files)

extractedWC<-as.data.frame(raster::extract(StackWC,BD[,c("longitude","latitude")]))

#SoilData

common_path<-c("BD/SoilGrids_Res/") 
files <- list.files(
  path <- common_path,
  pattern <- "\\.tif$",
  recursive = TRUE,          
  full.names = TRUE          
)

Stacksoil<-stack(files)

extractedsoil<-as.data.frame(raster::extract(Stacksoil,BD[,c("longitude","latitude")]))

#Habitat suitability

Suitability<- raster("processing/enm/Theobroma cacao/ensembles/suitability/Theobroma cacao__bio_current.grd")

extractedsuit<-as.data.frame(raster::extract(Suitability,BD[,c("longitude","latitude")]))

Merge<-cbind(BD,extractedWC,extractedsoil,extractedsuit)

# Normality test (n>50)

ks.test(BD$cocoa_prod_total_kgs,"pnorm")
ks.test(BD$cocoa_prod_total_kgsha,"pnorm")
hist(BD$cocoa_prod_total_kgs)
hist(BD$cocoa_prod_total_kgsha)

# Groups of variables

# 1 Environmental data

Environmental_data<-Merge[,c(91:116)]

# 2 Inputs

Inputs1 <- dplyr::select(Merge,ends_with("times"))
Inputs1<-Inputs1[,-c(7,8)]
Inputs2 <- dplyr::select(Merge,ends_with("unitha"))
Inputs<-cbind(Inputs1,Inputs2)

# 3 Management

Management1<-dplyr::select(Merge,ends_with("lab_n_ha"))
Management1<-Management1[,-c(1)]
Management2 <- dplyr::select(Merge,ends_with("yn"))
Management2<-Management2[,c(3:11)]
Management<-cbind(Management1,Management2)

# 4 Crop characteristics

Characteristics1<-Merge[,c(5,6,7,8,9,10,11,12,13,14,15,16,17,20,21,22,23)]
Characteristics2 <- dplyr::select(Merge,contains("varieties"))
Characteristics3 <- Merge[,c(43,44,45,89)]
Characteristics<-cbind(Characteristics1,Characteristics2,Characteristics3)

# 5 Pests and disease

Losses <- dplyr::select(Merge,contains("losses"))

# VIF Environmental data

Environmental_data<- Environmental_data %>% 
  dplyr::distinct(bdod_mean30100cm_SoilGrids2, .keep_all = TRUE)

vif<-vifstep(Environmental_data,th=8)
ind = rep(0, length(vif@excluded))
for (i in 1:length(vif@excluded)) {
  ind[i] = which(colnames(Environmental_data) == vif@excluded[i])
}
myExpl = Environmental_data[ ,c(-ind)]
Environmental_data<-myExpl

Environmental_data<-Merge[,c(25,92,96,98,100,102,103,104,110,112,113,114,116)]


# GLM Environmental data

muestra <- sample(1:2910, 2037)
training <- Environmental_data[muestra,]
testing <- Environmental_data[-muestra,]

#BACKWARD

full.model <- glm(cocoa_prod_total_kgsha ~ ., data=training)
summary(full.model)

modback <- stepAIC(full.model, trace=TRUE, direction="backward")

modback$anova

summary(modback)

#FORWARD

empty.model <- glm(cocoa_prod_total_kgsha ~ 1, data=training)
horizonte <- formula(cocoa_prod_total_kgsha ~ wc2.1_2.5m_bio_10 + wc2.1_2.5m_bio_14 + 
                       wc2.1_2.5m_bio_16 + wc2.1_2.5m_bio_18 + wc2.1_2.5m_bio_2 + 
                       wc2.1_2.5m_bio_3 + wc2.1_2.5m_bio_4 + bdod_mean30100cm_SoilGrids2 + 
                       clay_mean30100cm_SoilGrids2 + nitrogen_mean30100cm_SoilGrids2 + 
                       phh2o_mean30100cm_SoilGrids2 + silt_mean30100cm_SoilGrids2)

modforw <- stepAIC(empty.model, trace=FALSE, direction="forward", scope=horizonte)
modforw$anova
summary(modforw)



#BOTH

modboth <- stepAIC(empty.model, trace=FALSE, direction="both", scope=horizonte)
modboth$anova
summary(modboth)

# Group variable selection

Environmental_data<-Merge[,c(92,96,98,100,103,112,113,114,116)]


# GLM Management

Management$cocoa_prod_total_kgsha<-Merge$cocoa_prod_total_kgsha #Include response variable

muestra <- sample(1:2910, 2037)
training <- Management[muestra,]
testing <- Management[-muestra,]

#BACKWARD

full.model <- glm(cocoa_prod_total_kgsha ~ ., data=training)
summary(full.model)

modback <- stepAIC(full.model, trace=TRUE, direction="backward")

modback$anova

summary(modback)

# Group variable selection

Management<-Merge[,c(27,28,29,30,33,34,89,58,47,68)]


# GLM Losses

Losses$cocoa_prod_total_kgsha<-Merge$cocoa_prod_total_kgsha #Include response variable
Losses<-Losses[,-c(1,2)]
muestra <- sample(1:2910, 2037)
training <- Losses[muestra,]
testing <- Losses[-muestra,]

#BACKWARD

full.model <- glm(cocoa_prod_total_kgsha ~ ., data=training)
summary(full.model)

modback <- stepAIC(full.model, trace=TRUE, direction="backward")

modback$anova

summary(modback)

# Group variable selection

Losses<-Merge[,c(72,76,79,82,83)]


# GLM Characteristics

Characteristics$cocoa_prod_total_kgsha<-Merge$cocoa_prod_total_kgsha

skim(Characteristics)
Characteristics<-Characteristics[,-c(4,15,16,17,22,27,28,29)] #Remove variables with NAN > 1500 observations

#Include response variable
muestra <- sample(1:2910, 2037)
training <- Characteristics[muestra,]
testing <- Characteristics[-muestra,]

#BACKWARD

training<-na.omit(training)

full.model <- glm(cocoa_prod_total_kgsha ~ ., data=training)
summary(full.model)

modback <- stepAIC(full.model, trace=TRUE, direction="backward")

modback$anova

summary(modback)

#FORWARD

empty.model <- glm(cocoa_prod_total_kgsha ~ 1, data=training)
horizonte <- formula(cocoa_prod_total_kgsha ~ n_crops_produced + crops_important1 + 
                       crops_important2 + crops_important1_5ago + crops_important2_5ago + 
                       cocoa_land_used_morethan5_ha + cocoa_land_numlandparcels + 
                       cocoa_years + cocoa_trees_most + cocoa_trees_age_5yo_ha + 
                       cocoa_trees_age_5_25yo_ha + cocoa_trees_age_25yo_ha + cocoa_intercrop_yn + 
                       cocoa_varieties_1 + cocoa_varieties_2 + cocoa_varieties_3 + 
                       cocoa_varieties_7 + cocoa_varieties_5 + cocoa_varieties_6 + 
                       cocoa_varieties_999 + cocoa_trees_rows_yn)

modforw <- stepAIC(empty.model, trace=FALSE, direction="forward", scope=horizonte)
modforw$anova
summary(modforw)



#BOTH

modboth <- stepAIC(empty.model, trace=FALSE, direction="both", scope=horizonte)
modboth$anova
summary(modboth)


# Group variable selection

Characteristics<-Merge[,c(6,11,13,15,16,17,35,37,40,42)]



# GLM Inputs

Inputs$cocoa_prod_total_kgsha<-Merge$cocoa_prod_total_kgsha

skim(Inputs)
Inputs<-Inputs[,-c(1,2,3,4,6)] #Remove variables with NAN > 1500 observations

#Include response variable
muestra <- sample(1:2910, 2037)
training <- Inputs[muestra,]
testing <- Inputs[-muestra,]

#BACKWARD

training<-na.omit(training)

full.model <- glm(cocoa_prod_total_kgsha ~ ., data=training)
summary(full.model)

modback <- stepAIC(full.model, trace=TRUE, direction="backward")

modback$anova

summary(modback)

#FORWARD

empty.model <- glm(cocoa_prod_total_kgsha ~ 1, data=training)
horizonte <- formula(cocoa_prod_total_kgsha ~ cocoa_pest_times + cocoa_lfert_applied_unitha + 
                       cocoa_herb_applied_unitha + cocoa_pest_applied_unitha + cocoa_fung_applied_unitha)

modforw <- stepAIC(empty.model, trace=FALSE, direction="forward", scope=horizonte)
modforw$anova
summary(modforw)


#BOTH

modboth <- stepAIC(empty.model, trace=FALSE, direction="both", scope=horizonte)
modboth$anova
summary(modboth)

# Group variable selection

Inputs<-Merge[,c(52,60,63)]


## Pre-selection of variables by group (~ 5 per group)

NewMerge<-cbind(Environmental_data,Inputs,Management,Characteristics,Losses)
NewMerge$cocoa_prod_total_kgsha<-Merge$cocoa_prod_total_kgsha

str(NewMerge)


NewMerge <- NewMerge %>%
  dplyr::mutate(cocoa_pest_yn = as.numeric(cocoa_pest_yn), 
                cocoa_gfert_yn = as.numeric(cocoa_gfert_yn),
                cocoa_irrigation_yn = as.numeric(cocoa_irrigation_yn),
                crops_important1 = as.numeric(crops_important1),
                cocoa_years = as.numeric(cocoa_years),
                cocoa_varieties_1 = as.numeric(cocoa_varieties_1),
                cocoa_varieties_3 = as.numeric(cocoa_varieties_3),
                cocoa_varieties_5 = as.numeric(cocoa_varieties_5),
                cocoa_varieties_999 = as.numeric(cocoa_varieties_999),
                cocoa_losses_disease_type_2 = as.numeric(cocoa_losses_disease_type_2),
                cocoa_losses_disease_affect = as.numeric(cocoa_losses_disease_affect),
                cocoa_losses_pests_type_3 = as.numeric(cocoa_losses_pests_type_3),
                cocoa_losses_pests_type_6 = as.numeric(cocoa_losses_pests_type_6),
                cocoa_losses_pests_type_7 = as.numeric(cocoa_losses_pests_type_7))

# Correlation analysis

pvalue<-rcorr(as.matrix(NewMerge), type='spearman')$P
pvalue<-as.data.frame(pvalue[,c(38)])

NewMerge<-NewMerge[,c(2,3,4,5,6,7,8,10,11,12,14,17,18,19,20,21,24,26,27,30,31,33,34,36)]# p-value<0.05

NewMerge$cocoa_prod_total_kgsha<-Merge$cocoa_prod_total_kgsha

Svalue<-rcorr(as.matrix(NewMerge), type='spearman')$r
Svalue<-as.data.frame(Svalue[,c(25)])


# Variable selection

skim(NewMerge)

NewMerge <- NewMerge %>%
  dplyr::mutate(cocoa_pest_yn = as.factor(cocoa_pest_yn), 
                cocoa_gfert_yn = as.factor(cocoa_gfert_yn),
                cocoa_varieties_3 = as.factor(cocoa_varieties_3),
                cocoa_varieties_5 = as.factor(cocoa_varieties_5),
                cocoa_losses_disease_type_2 = as.factor(cocoa_losses_disease_type_2),
                cocoa_losses_disease_affect = as.factor(cocoa_losses_disease_affect),
                cocoa_losses_pests_type_6 = as.factor(cocoa_losses_pests_type_6))

Varselection<-NewMerge[,c(1,4,5,13,15,16,8,9,10,20,17,22,23,24,25)]

Varselection %>%
  select_if(is.factor) %>%
  gather() %>%
  ggplot(aes(value)) + geom_bar() + facet_wrap(~key,scales='free') +
  theme(axis.text=element_text(size=6))


## MODEL 1

VarModel1<-NewMerge[,c(5,9,10,13,17,24,25)]

muestra <- sample(1:2910, 2037)
training <- VarModel1[muestra,]
testing <- VarModel1[-muestra,]

training<- training %>% 
  dplyr::distinct(clay_mean30100cm_SoilGrids2, .keep_all = TRUE)

training %>%
  select_if(is.factor) %>%
  gather() %>%
  ggplot(aes(value)) + geom_bar() + facet_wrap(~key,scales='free') +
  theme(axis.text=element_text(size=6))

full.model <- glm(cocoa_prod_total_kgsha ~ ., data=training)
summary(full.model)



testing$prod_predicted<-predict(full.model, testing)

RMSE<-sqrt( sum( (testing$cocoa_prod_total_kgsha - testing$prod_predicted)^2 , na.rm = TRUE ) / nrow(testing) )
AVE<-sum(-testing$cocoa_prod_total_kgsha+testing$prod_predicted)/nrow(testing)
R2<-cor(testing$cocoa_prod_total_kgsha,testing$prod_predicted, method ="pearson",use = "complete.obs")
S<-cor(testing$cocoa_prod_total_kgsha,testing$prod_predicted, method ="spearman",use = "complete.obs")
