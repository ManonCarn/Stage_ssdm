######## Analyses complémentaires à partir des SSDM ########
library(raster)
library(terra)
library(sf)
library(SSDM)
library(rgdal)
library(sfhotspot)
library(rgeos)
library(ggplot2)
library(stats)
library(tidyverse)
library(dplyr)

# scp -r  manon@niamoto.ird.nc:~/stage_ssdm/final/results/ESDM_copie  "D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/SSDM_713sp"   


#### SI besoin de re load data arranged #### 
# occurences dataset 
occ_path = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/"
file_occ = "data_final2_thinning_arranged.csv"
# file_occ = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final2_thinning_arranged.rds")
# get occurences table and apply spatial thinning (Env resolution) on species occurrence to reduce spatial sampling bias
Occ = load_occ(path = occ_path, Env, file = file_occ,
               Xcol = "LONGITUDE", Ycol = "LATITUDE", Spcol = "SPECIES",
               GeoRes = TRUE)

# modify species names (avoid blank spaces)
# Occ <- Occ[,-1]
# names(Occ) = c('SpeciesID', 'Longitude', 'Latitude')
# Occ$SpeciesID <- gsub("\\s+", " ", stringr::str_trim(Occ$SpeciesID ))
# Occ$SpeciesID <- unlist(lapply(trsplit(Occ$SpeciesID, " "), function(x) paste(x[1:2],collapse="_")))

# save 
saveRDS(Occ, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final2_thinning_arranged_load.rds")
# load 

Occ <- readRDS(file = paste0(occ_path, "/data_final2_thinning.rds"))




#### Nouveau tab pour ESDM dans ordre du max occ ####
data_final2_thinning = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final2_thinning.rds")
sp_tot = data_final2_thinning$SpeciesID
list_sp = unique(sp_tot)
nb = as.data.frame(table(data_final2_thinning[1]))
nb = arrange(nb, desc(Freq))
ordre_sp = paste(nb$SpeciesID)
ordre_tot = NULL
for (i in 1:length(ordre_sp)){
  n = nb[i,2]
  sp = paste(rep(nb[i,1],n))
  ordre_tot = c(ordre_tot,sp)
}


ordre_tot = data.frame(ordre_tot,1:136808)
names(ordre_tot) = c("SpeciesID","ordre")
ordre_tot = arrange(ordre_tot,SpeciesID)
data_final2_thinning = arrange(data_final2_thinning, SpeciesID)

tab_arrange = cbind(data_final2_thinning, ordre_tot$ordre) 
names(tab_arrange)[4] = "ordre"
tab_arrange = arrange(tab_arrange,ordre)
tab_arrange = tab_arrange[-4]
saveRDS(tab_arrange, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final2_thinning_arranged.rds")
write.csv2 (tab_arrange, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final2_thinning_arranged.csv") 

# scp -r  "D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2_thinning_arranged.rds"  manon@niamoto.ird.nc:~/stage_ssdm/final/data
data_final2_thinning_arranged = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final2_thinning_arranged.rds")



#### Analyses sur occ et var env ####
altitude = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/Raster_base2/altitude_ok.tif")
alizes = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/Raster_base2/alizes_ok.tif")
cti = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/Raster_base2/cti_ok.tif")
ensoleillement = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/Raster_base2/ensoleillement_ok.tif")
pente = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/Raster_base2/pente_ok.tif")
precipitation = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/Raster_base2/precipitation_res_ok.tif")
precipitation2 = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/Raster_base2/precipitation_res_ok2.tif")
substrat = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/Raster_base2/substrat_ok.tif")

alizes[is.na(precipitation)]<- NA
altitude[is.na(precipitation)]<- NA
cti[is.na(precipitation)]<- NA
ensoleillement[is.na(precipitation)]<- NA
pente[is.na(precipitation)]<- NA
substrat[is.na(precipitation)]<- NA

altitude_valeurs = na.omit(values(altitude)) # plus de valeurs que les autres : 1 707 316
pente_valeurs = na.omit(values(pente)) # all same : 1 705 375
cti_valeurs = na.omit(values(cti)) 
alizes_valeurs = na.omit(values(alizes))
ensoleillement_valeurs = na.omit(values(ensoleillement))
precipitation_valeurs = na.omit(values(precipitation2)) # moins de valeurs : 1 702 663
substrat_valeurs = na.omit(values(substrat))

altitude_tot = na.omit(values(altitude)) # plus de valeurs que les autres : 1 707 316
pente_tot = na.omit(values(pente)) # all same : 1 705 375
cti_tot = na.omit(values(cti))
alizes_tot = na.omit(values(alizes))
ensoleillement_tot = na.omit(values(ensoleillement))
precipitation_tot = na.omit(values(precipitation2)) # moins de valeurs : 1 702 663
substrat_tot = na.omit(values(substrat))


env_tot = list(altitude_tot,pente_tot,cti_tot,alizes_tot,ensoleillement_tot,precipitation_tot,substrat_tot)
# saveRDS(env_tot,file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/env_tot.rds")

tab_env = data.frame(altitude_valeurs,pente_valeurs,cti_valeurs,alizes_valeurs,ensoleillement_valeurs,precipitation_valeurs,substrat_valeurs)
# saveRDS(tab_env, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/tab_env.rds")
tab_env = cbind(tab_env, log(precipitation_valeurs), log(ensoleillement_valeurs))
names(tab_env) = c("altitude","pente","cti","alizes","ensoleillement","precipitation","substrat")
boxplot(log(tab_env$altitude),tab_env$pente,tab_env$cti,tab_env$alizes,tab_env$log_prec,tab_env$substrat,tab_env$log_enso)

## test corrélation Pearson entre var env 
library(Hmisc)
tab_env = readRDS ("D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/tab_env.rds")

cor.test(tab_env$altitude, tab_env$ensoleillement, method= "pearson") # 0.2358728
cor = rcorr(as.matrix(tab_env), type = "pearson")
cor$P

pairs(tab_env)


env_tot = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/env_tot.rds")

min = rep(0,7)
max = rep(0,7)
moy = rep(0,7)
med = rep(0,7)

options(scipen=50,digits = 7)
for (i in 1:7){
  min[i] = min(env_tot[[i]])
  max[i] = max(env_tot[[i]])
  moy[i] = mean(env_tot[[i]])
  med[i] = median(env_tot[[i]])
}

tab_valeurs = data.frame(min,max,moy,med)

## Tableau occurrences familles etc 
data_final2_thinning = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final2_thinning.rds")
data_final2 = read.csv2("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final2.csv") 
data_final2 = data_final2[-1]

data_tot = read.csv ("D:/Vanessa/Mes Documents/Stage Manon C/r/Data/data_final_2/occ_total_nc_202306091601.csv", sep = ';')
data_unique = read.csv ("D:/Vanessa/Mes Documents/Stage Manon C/r/Data/data_final_2/occ_total_nc_unique_202306091600.csv", sep = ';')

data_family = unique(data_tot[,4:5])
data_family = arrange(data_family,taxaname)
family = rep(0,length(data_final2$SPECIES))
data_final_family = cbind(data_final2,family)

for (i in 1:length(data_final_family$SPECIES)){
  data_final_family[i,4] <- data_family[data_family$taxaname == data_final_family[i,1],1]
}

# saveRDS(data_final_family, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final_family.rds")
data_final_family = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final_family.rds")

data_family2 = data_family
data_family2$taxaname <- gsub("\\s+", " ", stringr::str_trim(data_family2$taxaname))
data_family2$taxaname <- unlist(lapply(strsplit(data_family2$taxaname, " "), function(x) paste(x[1:2],collapse="_")))
data_family2= unique(data_family2)

family = rep(0,length(data_final2_thinning$SpeciesID))
data_final_family_thin = cbind(data_final2_thinning,family)
for (i in 1:length(data_final_family_thin$SpeciesID)){
  data_final_family_thin[i,4] <- data_family2[data_family2$taxaname == data_final_family_thin[i,1],1]
}
# saveRDS(data_final_family_thin, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final_family_thin.rds")
data_final_family_thin = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final_family_thin.rds")

## compter nb occ par sp et par famille 
# nb occ data 
list_sp = unique(data_final_family$SPECIES)
nb_occ = rep(0,length(list_sp))
for (i in 1:length(nb_occ)) {
  tab = subset(data_final_family, SPECIES == list_sp[i])
  nb_occ[i] = length(tab$SPECIES)
}
# saveRDS(nb_occ, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/nb_occ.rds")
nb_occ = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/nb_occ.rds")

# nb occ family data 
list_family = unique(data_final_family$family)
nb_occ_f = rep(0,length(list_family))
for (i in 1:length(nb_occ_f)) {
  tab = subset(data_final_family, family == list_family[i])
  nb_occ_f[i] = length(tab$family)
}

# nb sp family data 
nb_sp = rep(0,length(list_family))
for (i in 1:length(nb_sp)){
  tab = unique(subset(data_final_family[,c(1,4)], family == list_family[i]))
  nb_sp[i] = length(tab$SPECIES)
}

tab_occ = data.frame(list_family,nb_sp,nb_occ_f)
# saveRDS(tab_occ, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/tab_occ.rds")
tab_occ = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/tab_occ.rds")


# nb occ data thin
list_sp_thin = unique(data_final_family_thin$SpeciesID)
nb_occ_thin = rep(0,length(list_sp_thin))
for (i in 1:length(nb_occ_thin)) {
  tab = subset(data_final_family_thin, SpeciesID == list_sp_thin[i])
  nb_occ_thin[i] = length(tab$SpeciesID)
}
# saveRDS(nb_occ_thin, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/nb_occ_thin.rds")
nb_occ_thin = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/nb_occ_thin.rds")

sum(nb_occ_thin > 1000) #9
sum(nb_occ_thin < 250) # 1019 
1019/1144 

mean(nb_occ_thin)
median(nb_occ_thin)

ggplot(data.frame(unique(data_final_family_thin$SpeciesID, nb_occ_thin))) +
  geom_histogram(aes(x=nb_occ_thin)) +
  labs(  x = "nombre d'occurrences par espèce")

# nb occ family data thin
list_family_thin = unique(data_final_family_thin$family)
nb_occ_f_thin = rep(0,length(list_family_thin))
for (i in 1:length(nb_occ_f_thin)) {
  tab = subset(data_final_family_thin, family == list_family_thin[i])
  nb_occ_f_thin[i] = length(tab$family)
}

# nb sp family data thin
nb_sp_thin = rep(0,length(list_family_thin))
for (i in 1:length(nb_sp_thin)){
  tab = unique(subset(data_final_family_thin[,c(1,4)], family == list_family_thin[i]))
  nb_sp_thin[i] = length(tab$SpeciesID)
}

tab_occ_thin = data.frame(list_family_thin,nb_sp_thin,nb_occ_f_thin)
# saveRDS(tab_occ_thin, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/tab_occ_thin.rds")
tab_occ_thin = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/tab_occ_thin.rds")

## plot toutes occ
mask <- readOGR("D:/Vanessa/Mes Documents/Stage Manon C/r/mask_rasters/contour_grande_terre_clean.shp")
alizes <- raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/Raster_base/alizes.tif")
mask <- spTransform(mask, crs(alizes))

plot(mask, main = "Occurrences jeu de données total")
points(data_final_family[,2:3], pch = 20)

plot(mask, main = "Occurrences jeu de données après sélection")
points(data_final_family_thin[,2:3],pch=20)


## surface des occurrences + hist + corrélation surface sdm 
library(red)
# path = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_final/manon/ESDM/" 
# list_sdm = list.files(path)
data_final2_thinning = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final2_thinning_names.rds")
# data_final2_thinning = subset(data_final2_thinning,SpeciesID %in% list_sdm)

list_sp = unique(data_final2_thinning$SpeciesID)
surface_occ = rep(0,length(list_sp))
for (i in 1:length(list_sp)){
  tab = subset(data_final2_thinning, SpeciesID == list_sp[i])
  tab = tab[,2:3]
  surface_occ[i] = aoo(tab)
}

sum(surface_occ < 500) # 998
998/1144
median(surface_occ) # 156
mean(surface_occ) # 245.7867
sum(surface_occ > 1000) # 31

library(ggplot2)
ggplot(data.frame(list_sp, surface_occ)) +
  geom_histogram(aes(x=surface_occ)) +
  labs(  x = "Surface des occurrences par la méthode AOO (km2)", y = "fréquence")




#### Stacking à partir ESDM #### 
# scp -r  manon@niamoto.ird.nc:~/stage_ssdm/test2/results/ESDM_final   "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp"
# scp -r  manon@niamoto.ird.nc:~/stage_ssdm/R_scripts/load_model_modif.R   "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp"

source("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp/load_model_modif.R")
environment(load_esdm_modif) <- environment(load_esdm)
assignInNamespace("load_esdm", load_esdm_modif, ns = "SSDM")
path_load = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/ESDM_copie" ## changer dossier source
all_esdm_names <- list.files(path_load)
list_all_esdm <- sapply(all_esdm_names, function(x) load_esdm_modif(x, path_load))
names(list_all_esdm) <- NULL

# saveRDS(list_all_esdm, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/list_all_esdm_706.rds")
esdm = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/list_all_esdm_706.rds")

list_methods_stacking <- list(name = NULL, method = "pSSDM", rep.B = 1000,   ## changer method de stacking
                              Env = NULL, endemism = c("WEI", "Binary"),
                              eval = TRUE, verbose = TRUE, GUI = FALSE)

stack_bin <- do.call(stacking, c(list_all_esdm, list_methods_stacking))
stack_prob <- do.call(stacking, c(list_all_esdm_706, list_methods_stacking))

save.stack(stack_prob, name = "SSDM_706_prob", path = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/")  ## nom fichier et path
# SSDM_45sp = load_stack(name = "SSDM_final2", path = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp/") 
# debug(load_stack)

saveRDS(stack_bin, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/stack_713_bin.rds")
saveRDS(stack_prob, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/stack_prob_706.rds")

saveRDS(stack_bin@diversity.map, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp/stack_bin_div.rds")
saveRDS(stack_bin@endemism.map, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp/stack_bin_end.rds")
saveRDS(stack_bin@uncertainty, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp/stack_bin_unc.rds")

saveRDS(stack_prob@diversity.map, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp/stack_prob_div.rds")
saveRDS(stack_prob@endemism.map, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp/stack_prob_end.rds")
saveRDS(stack_prob@uncertainty, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp/stack_prob_unc.rds")

# Erreur apparait pendant stacking 713 sp
# Ensemble model Archidendropsis_fournieri.Ensemble.SDM : Error in compareRaster(x) : different extent 
# Error in compareRaster(x) : different extent

# Stack creation...
# naming... done.
# range restriction... done.
# diversity mapping...
# Local species richness computed by thresholding and then summing.
# Killed

## Regarder extend des 706 sp pour avoir les mêmes 
list_all_esdm_706 = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/list_all_esdm_706.rds")
extend = data.frame(rep(0,706),rep(0,706),rep(0,706),rep(0,706))
names(extend) = c("xmin","xmax","ymin","ymax")
for (i in 1:706){
  for (j in 1:4){
    extend[i,j] = list_all_esdm_706[[3]]@binary@extent[j]   
  }
}

# -> même extend 


#### Stacking avec filtre de dispersion sur les ESDM #### 
# comme Pouteau 2019 range de 18,2km (méthode boucle sur les espèces, distance moy entre les occurance de chaque sp puis moy de ces distances toutes sp) :
# matrice de distance, attention diagonale (fonction diag(matrice)=NA)

## Calcul du filtre de dispersion = distance buffer = distance moyenne entre les voisins les plus proches  
data_final2 = read.csv2("D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2.csv")
data_final2 = data_final2[,-1]
sum(table(data_final2$SPECIES)<2) # 14 sp avec 1 seule occ 

# cord.dec = SpatialPoints(cbind(data_final2$LONGITUDE,data_final2$LATITUDE),proj4string=CRS("+proj=longlat"))
# cord.dec <- spTransform(cord.dec, CRS("+init=epsg:32758"))
# long = cord.dec@coords[,1]
# lat = cord.dec@coords[,2]
# data_final2_UTM = data.frame(data_final2$SPECIES,long,lat)
# names(data_final2_UTM) <- c("SPECIES","LONGITUDE","LATITUDE")
# write.csv2(data_final2_UTM, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2_UTM.csv")
data_final2_UTM = read.csv2("D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2_UTM.csv")
data_final2_UTM = data_final2_UTM[,-1]
sum(table(data_final2_UTM$SPECIES)<5)
sp_inf5 = list_sp[table(data_final2_UTM$SPECIES)<5]
sp_sup5 = list_sp[table(data_final2_UTM$SPECIES)>= 5]
data_final2_UTM_sup5 = data_final2_UTM[data_final2_UTM$SPECIES %in% sp_sup5,]

## EPSG 3060
# cord.dec = SpatialPoints(cbind(data_final2$LONGITUDE,data_final2$LATITUDE),proj4string=CRS("+proj=longlat"))
# cord.dec <- spTransform(cord.dec, CRS("+init=epsg:3060"))
# cord.dec <- spTransform(cord.dec, CRS("+proj=utm +zone=58 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# long = cord.dec@coords[,1]
# lat = cord.dec@coords[,2]
# # data_final2_UTM2 = data.frame(data_final2$SPECIES,long,lat)
# data_final2_testUTM = data.frame(data_final2$SPECIES,long,lat)
# names(data_final2_testUTM) <- c("SPECIES","LONGITUDE","LATITUDE")
# write.csv2(data_final2_testUTM, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2_testUTM.csv")
data_final2_UTM2 = read.csv2("D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2_UTM2.csv")
data_final2_UTM2 = data_final2_UTM2[,-1]
data_final2_testUTM=read.csv2("D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2_testUTM.csv")
data_final2_testUTM = data_final2_testUTM[,-1]

## EPSG 2982
# cord.dec = SpatialPoints(cbind(data_final2$LONGITUDE,data_final2$LATITUDE),proj4string=CRS("+proj=longlat"))
# cord.dec <- spTransform(cord.dec, CRS("+init=epsg:2982"))
# long = cord.dec@coords[,1]
# lat = cord.dec@coords[,2]
# data_final2_UTM3 = data.frame(data_final2$SPECIES,long,lat)
# names(data_final2_UTM3) <- c("SPECIES","LONGITUDE","LATITUDE")
# write.csv(data_final2_UTM3, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2_UTM3.csv")
data_final2_UTM3 = read.csv("D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2_UTM3.csv", header=T)
data_final2_UTM3 = data_final2_UTM3[,-1]

## tab thinning 
# data_final2_thinning = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final2_thinning.rds")
# coord = SpatialPoints(cbind(data_final2_thinning$Longitude,data_final2_thinning$Latitude),proj4string=CRS("+proj=longlat"))
# coord_UTM = spTransform(coord, CRS("+proj=utm +zone=58 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# data_final2_thinning_UTM = data.frame(data_final2_thinning$SpeciesID,coord_UTM@coords[,1],coord_UTM@coords[,2])
# saveRDS(data_final2_thinning_UTM, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2_thinning_UTM.rds")
data_final2_thinning_UTM = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2_thinning_UTM.rds")


tab = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2_thinning_UTM.rds")
tab = read.csv2("D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2_UTM.csv")
tab = tab[,-1]
names(tab) <- c("SPECIES","LONGITUDE","LATITUDE")
list_sp = unique(tab$SPECIES)
moy_occ_ngb = rep(0,length(list_sp))
# moy_occ = rep(0,length(list_sp))
# med_occ = rep(0, length(list_sp))
for (i in 1:length(list_sp)){
  data_sp = tab[tab$SPECIES == list_sp[i],]
  mat_sp = matrix (c(data_sp$LONGITUDE,data_sp$LATITUDE),nrow=length(data_sp$LONGITUDE))
  mat_dist = dist(mat_sp)
  mat_dist = as.matrix(mat_dist)
  diag(mat_dist) = NA 
  moy_occ_ngb[i] = mean(apply(mat_dist,1,min,na.rm=TRUE))
  # moy_occ[i] <- mean(mat_dist)
  # med_occ[i] <- median(mat_dist)
  
}

moy_occ_ngb[moy_occ_ngb == 'Inf'] = NA
moy_ngb = mean(moy_occ_ngb, na.rm = T) # 4814.326 =  4.8 km

moy_ngb_thin = mean(moy_occ_ngb, na.rm = T) # 3836.088 = 3.8 km

moy_sp = mean(na.omit(moy_occ)) # 82598.48 = 82,6 km
med_sp = median(na.omit(med_occ)) # 65819.22 = 65,8 km

moy_sp_sup5 = mean(na.omit(moy_occ)) # 83 km
med_occ_sup5 = median(na.omit(med_occ)) # 66 km

moy_occ_UTM2 = mean(na.omit(moy_occ)) # 82,6 km
med_occc_UTM2 = median(na.omit(med_occ)) # 65,8 km

moy_occ_UTM3 = mean(na.omit(moy_occ)) # 82.6 
med_occc_UTM3 = median(na.omit(med_occ)) # 65,8

moy_testUTM = mean(na.omit(moy_occ)) # 82.6
med_testUTM = median(na.omit(med_occ)) # 65.8

# --> choix du range à partir de Pouteau 2019 et dires d'experts = 20km (echelle d'une vallée)

## Boucle range sur ESDM 
# transform occ to UTM (CRS)

source("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp/load_model_modif.R")
environment(load_esdm_modif) <- environment(load_esdm)
assignInNamespace("load_esdm", load_esdm_modif, ns = "SSDM")
path_load = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/ESDM_copie"
all_esdm_names <- list.files(path_load)
# list_all_esdm <- sapply(all_esdm_names, function(x) load_esdm_modif(x, path_load))
# list_all_esdm_range = list_all_esdm
# names(list_all_esdm) <- NULL
# names(list_all_esdm_range) <- NULL
list_all_esdm_706 = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/list_all_esdm_706.rds")
list_all_esdm_706_range = list_all_esdm_706

buf = 20000 # 20km

# tableau occ avec que nos sp

# occ =  read.csv2( "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final2.csv")
# occ = occ[,-1]
# occ$SPECIES <- gsub("\\s+", " ", stringr::str_trim(occ$SPECIES ))
# occ$SPECIES <- unlist(lapply(strsplit(occ$SPECIES, " "), function(x) paste(x[1:2],collapse="_")))
# occ = subset(occ,occ$SPECIES %in% all_esdm_names)
occ = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final2_thinning.rds")
names(occ) = c("SPECIES","LONGITUDE","LATITUDE")
occ = subset(occ, occ$SPECIES %in% all_esdm_names)


for(i in 1:length(list_all_esdm_706_range)){
  print(i)
  tab = subset(occ, occ$SPECIES == all_esdm_names[i] ) # subset tab pour sp i
  xy = data.frame(tab$LONGITUDE,tab$LATITUDE) # xy = 2 col long lat du fichier occ
  # names(list_all_esdm_range)==spi 
  esdm = list_all_esdm_706_range[[i]] #  subset esdm i 
  spdf <- SpatialPointsDataFrame(coords = xy, data = tab,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  spdf <- spTransform(spdf, CRS = "+proj=utm +zone=58 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  spdf <- st_as_sf(spdf) # modif spdf en sf
  buffer_tmp <- st_buffer(spdf, dist = buf, bOnlyEdges =FALSE ) # buffer autour des occ
  buffer_tmp <- st_transform(buffer_tmp, crs = crs(esdm@projection))
  # r2 <- crop(esdm@projection, extent(buffer_tmp))
  r3 <- mask(esdm@projection, buffer_tmp)
  list_all_esdm_706_range[[i]]@projection[is.na(r3[])] <- 0
  # list_all_esdm_range[[names(list_all_esdm_range)==sp i]]@raster_prob <- r3 
}


## Stacking prob et bin 
list_methods_stacking <- list(name = NULL, method = "pSSDM", rep.B = 1000,
                              Env = NULL,endemism = c("WEI", "Binary"),
                              eval = TRUE, verbose = TRUE, GUI = FALSE)

stack_prob_range <- do.call(stacking, c(list_all_esdm_706, list_methods_stacking))

save.stack(stack_prob_range, name = "SSDM_prob_range", path = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/")

saveRDS(stack_prob_range, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/stack_prob_range.rds")


stack_bin_range <- do.call(stacking, c(list_all_esdm_range, list_methods_stacking))

save.stack(stack_bin_range, name = "SSDM_bin_prob", path = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/")


saveRDS(stack_bin_range, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/stack_bin_range.rds")
saveRDS(stack_prob_range, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/stack_prob_range.rds")

saveRDS(stack_bin_range@diversity.map, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/stack_bin_range_div.rds")
saveRDS(stack_bin_range@endemism.map, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/stack_bin_range_end.rds")
saveRDS(stack_bin_range@uncertainty, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/stack_bin_range_unc.rds")

saveRDS(stack_prob_range@diversity.map, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/stack_prob_range_div.rds")
saveRDS(stack_prob_range@endemism.map, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/stack_prob_range_end.rds")
saveRDS(stack_prob_range@uncertainty, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/stack_prob_range_unc.rds")



#### Analyses sur SDM ####
path = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_final/manon/ESDM/" 
list_sdm = list.files(path)
data_final_family_thin = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final_family_thin.rds")
liste=unique(data_final_family_thin$SpeciesID)
nb_occ_thin = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/nb_occ_thin.rds")

sum(!list_sdm %in% liste)
manque = list_sdm[!list_sdm %in% liste]
liste[c(84,106,155,156,229,448,470,487,488,489,545,613,614,641,797,798,799,800,801,836,837,854,985,1100)] = manque 

a_changer = liste[c(84,106,155,156,229,448,470,487,488,489,545,613,614,641,797,798,799,800,801,836,837,854,985,1100)]


# for (i in 1:24){
#   data_final2_thinning[data_final2_thinning$SpeciesID==a_changer[i],1] = manque[i]
# }
# 
# saveRDS(data_final2_thinning, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final2_thinning_names.rds")
data_final2_thinning_names = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/data_final2_thinning_names.rds")


tab_sdm = cbind(liste,unique(data_final_family_thin[,c(1,4)])[2],nb_occ_thin)
tab_sdm = subset(tab_sdm, liste %in% list_sdm)
tab_sdm = cbind(tab_sdm,surfaces_sdm)
# saveRDS(tab_sdm,file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/tad_sdm.rds")
tab_sdm = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/tad_sdm.rds")


## surface des sdm + hist 
path = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_final/manon/ESDM/" 
list_sdm = list.files(path)
surfaces = rep(0,length(list_sdm))
for (i in 1:length(list_sdm)){
  sdm = raster(paste0(path,list_sdm[i],"/Rasters/Binary.tif"))
  surfaces[i] = sum(sdm[sdm==1])
  cat(i, "\n\n")
}
saveRDS(surfaces, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/surfaces_sdm.rds")
surfaces_sdm = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/surfaces_sdm.rds")

ggplot(tab_sdm) +
  geom_histogram(aes(x=surfaces_sdm)) +
  labs(  x = "Surface des aires de distribution potentielles (en ha)")

sum (surfaces_sdm < 500000) # 1003
1003/1112
median(surfaces_sdm) # 225215.5
mean(surfaces_sdm) # 256917.5
sum(surfaces_sdm > 1000000) # 5

## corrélation nb occ par sp et surface 
surfaces_sdm = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/surfaces_sdm.rds")
tab = data.frame(unique(data_final2_thinning_names$SpeciesID), nb_occ_thin)
names(tab) = c("SpeciesID","nb_occ")
tab = subset(tab,SpeciesID %in% list_sdm)

cor.test(surfaces_sdm,tab$nb_occ, method = 'pearson') 
# -0.1412783
# p-value = 2.25e-06

## corrélation aire occurrence et surface sdm
surfaces_sdm = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/surfaces_sdm.rds")
tab = data.frame(unique(data_final2_thinning_names$SpeciesID), surface_occ)
names(tab) = c("SpeciesID","surface_occ")
tab = subset(tab,SpeciesID %in% list_sdm)

cor.test(surfaces_sdm,tab$surface_occ, method = 'pearson') 
# -0.05920315
# p-value = 0.04841


## compter nb patate dans SDM + hist
# library(landscapemetrics)
# lsm_c_np(sdm2,directions = 8)
path = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_final/manon/ESDM/" 
list_sdm = list.files(path)
nb_patate = rep(0,length(list_sdm))
for (i in 1:length(list_sdm)) {
  sdm = raster(paste0(path,list_sdm[i],"/Rasters/Binary.tif"))
  sdm[sdm==0] = NA
  sdm = rasterToPolygons(sdm,dissolve = T)
  nb_patate[i] = length(sdm@polygons[[1]]@Polygons)
  cat(i)
}
saveRDS(nb_patate, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/nb_patate.rds" )
##Version serveur
library(raster)
library(terra)
library(sf)
library(SSDM)
library(rgdal)
library(sfhotspot)
library(rgeos)
library(ggplot2)
library(stats)
library(tidyverse)
library(dplyr)

path = "~/stage_ssdm/final/results/ESDM/" 
list_sdm = list.files(path)
nb_patate = rep(0,length(list_sdm))
for (i in 1:length(list_sdm)) {
  sdm = raster(paste0(path,list_sdm[i],"/Rasters/Binary.tif"))
  sdm[sdm==0] = NA
  sdm = rasterToPolygons(sdm,dissolve = T)
  nb_patate[i] = length(sdm@polygons[[1]]@Polygons)
  cat(i, '\n\n')
}
saveRDS(nb_patate, file = "~/stage_ssdm/final/results/nb_patate.rds" )


#### Analyses sur fragmentation, sur SDM ####

## Chargement des raster foret, reprojection et crop, création raster utilisables avec juste 1 et NA

base = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/Raster_base2/altitude_ok.tif")
plot(base)
base@crs
base@extent
# base@extent 
# class      : Extent 
# xmin       : 163.5696 
# xmax       : 168.1344 
# ymin       : -22.88187 
# ymax       : -19.52487
mask <- readOGR("D:/Vanessa/Mes Documents/Stage Manon C/r/mask_rasters/contour_grande_terre_clean.shp")
mask <- spTransform(mask, crs(base))

# past_deforestation = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/fcc123_NCL.tif")
# past_deforestation@srs
# past_deforestation = projectRaster(from = past_deforestation, crs = base, method = "ngb")
# plot(past_deforestation)
# writeRaster(past_deforestation, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/past_deforestation.tif", overwrite = T)
past_deforestation = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/past_deforestation.tif")
plot(past_deforestation)
table(values(prob_2100))

# prob_2020 = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_NCL.tif")
# prob_2020 = projectRaster(from = prob_2020, crs = base, method = "ngb")
# writeRaster(prob_2020, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2020.tif", overwrite = T)
prob_2020 = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2020.tif")
plot(prob_2020)

# prob_2050 = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/fcc_2050_NCL.tif" )
# prob_2050 = projectRaster(from = prob_2050, crs = base, method = "ngb")
# writeRaster(prob_2050, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2050.tif", overwrite = T)
prob_2050 = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2050.tif")
plot(prob_2050)

# prob_2100 = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/fcc_2100_NCL.tif" )
# prob_2100 = projectRaster(from = prob_2100, crs = base, method = "ngb")
# writeRaster(prob_2100, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2100.tif", overwrite = T)
prob_2100 = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2100.tif")
plot(prob_2100)

# past_deforestation <- crop(past_deforestation, mask)
# past_deforestation <- mask(past_deforestation, mask)
# past_deforestation_res = resample(past_deforestation,base, method = "ngb")
# writeRaster(past_deforestation_res, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/past_deforestation_ok.tif", overwrite =T)
past_deforestation_ok = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/past_deforestation_ok.tif")
plot(past_deforestation_ok)
table(values(past_deforestation_ok))

# prob_2020 <- crop(prob_2020, mask)
# prob_2020 <- mask(prob_2020, mask)
# prob_2020_res <- resample(prob_2020,base, method = "ngb")
# writeRaster(prob_2020_res, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2020_ok.tif", overwrite = T)
prob_2020_ok = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2020_ok.tif")
plot(prob_2020_ok)

# prob_2050 <- crop(prob_2050, mask)
# prob_2050 <- mask(prob_2050, mask)
# prob_2050_res <- resample(prob_2050,base, method = "ngb")
# writeRaster(prob_2050_res, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2050_ok.tif", overwrite = T)
prob_2050_ok = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2050_ok.tif")
plot(prob_2050_ok)

# prob_2100 <- crop(prob_2100, mask)
# prob_2100 <- mask(prob_2100, mask)
# prob_2100_res <- resample(prob_2100,base, method = "ngb")
# writeRaster(prob_2100_res, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2100_ok.tif", overwrite = T)
prob_2100_ok = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2100_ok.tif")
plot(prob_2100_ok)
table(values(prob_2100_ok))

actual_forest = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/actual_forest.tif")
table(values(actual_forest))
# actual_forest[!actual_forest==1] <- 0
# writeRaster(actual_forest, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/actual_forest.tif", overwrite=T )
actual_forest@crs
plot(actual_forest, col=c("beige","green3"), breaks = c(-0.5,0.5,1.5))

# actual_forest[actual_forest==0]<-NA
# writeRaster(actual_forest, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/actual_forest_crop.tif")
actual_forest_crop = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/actual_forest_crop.tif")
plot(actual_forest_crop)

# past_deforestation_ok[past_deforestation_ok==2] <- 1
# past_deforestation_ok[past_deforestation_ok==3] <- NA
# writeRaster(past_deforestation_ok,file ="D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/past_deforestation_ok_tot.tif")
past_deforestation_ok_tot=raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/past_deforestation_ok_tot.tif")
plot(past_deforestation_ok_tot, col = "grey")
plot(past_deforestation_ok, col = "grey")

# past_deforestation_ok[!past_deforestation_ok==3] <- NA
# past_deforestation_ok[past_deforestation_ok==3] <- 1
# writeRaster(past_deforestation_ok, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/past_deforestation_foret.tif", overwrite=T )
plot(past_deforestation_ok_tot, col="blue")
plot(past_deforestation_foret, col='blue')

past_deforestation_ok_1 = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/past_deforestation_ok_1.tif")
plot(past_deforestation_ok_1,col = 'grey')

past_deforestation_ok_2 = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/past_deforestation_ok_2.tif")
plot(past_deforestation_ok_2,col ='grey')

past_deforestation_ok_3 = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/past_deforestation_ok_3.tif")
plot(past_deforestation_ok_3, col = "grey")

# prob_2100_ok[prob_2100_ok==1] <- NA
# prob_2100_ok[prob_2100_ok==0] <- 1
# writeRaster(prob_2100_ok, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2100_perte.tif")
prob_2100_perte = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2100_perte.tif")
plot(prob_2100_perte)


## boucle : pour chaque espèce regarder part de surface perdue entre : potentielle et actuelle, 2000 et 2020, proj 2020 et 2100 

actual_forest = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/actual_forest.tif")
actual_forest_crop = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/actual_forest_crop.tif")
past_deforestation_ok = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/past_deforestation_ok.tif")
past_deforestation_ok_tot=raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/past_deforestation_ok_tot.tif")
past_deforestation_foret = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/past_deforestation_foret.tif")
prob_2050_ok = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2050_ok.tif")
prob_2100_ok = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2100_ok.tif")
prob_2100_perte = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2100_perte.tif")

# SSDM_45sp = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp/stack_bin_range.rds") # choix SSDM bin et range 
path = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_final/manon/ESDM"

list_sp = list.files( path)

a = rep(0,length(list_sp))
results = list(a,a,a)
names(results) = c("perte","perte_2000_2020","perte_2100")


base = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/Raster env/Raster_base2/altitude_ok.tif")
mask <- readOGR("D:/Vanessa/Mes Documents/Stage Manon C/r/mask_rasters/contour_grande_terre_clean.shp")
mask <- spTransform(mask, crs(base))

for (i in 1:length (list_sp)){
  sp = raster(paste0(path,"/",list_sp[i],"/Rasters/Binary.tif"))
  esdm = crop(sp,mask) # peut etre pas besoin si les sdm ont dejà la meme emprise que les var env 
  # esdm = mask(sp,mask) # same
  esdm_actuel = crop(esdm, actual_forest_crop)
  esdm_actuel = mask(esdm, actual_forest_crop)
  nb_sp = sum(esdm[esdm==1])
  nb_sp_actuel = sum(esdm_actuel[esdm_actuel==1])
  perdu = (nb_sp - nb_sp_actuel)/nb_sp
  results[[1]][i] <- perdu
  
  esdm_2020 = crop(esdm, past_deforestation_foret)
  esdm_2020 = mask(esdm, past_deforestation_foret)
  foret_2020 = sum(esdm_2020[esdm_2020==1])
  
  esdm_2000_2020 = crop(esdm,past_deforestation_ok_tot)
  esdm_2000_2020 = mask(esdm, past_deforestation_ok_tot)
  perdu_2000_2020 = sum(esdm_2000_2020[esdm_2000_2020==1])
  results[[2]][i] <- perdu_2000_2020/foret_2020
  
  esdm_2100 = crop(esdm,prob_2100_perte)
  esdm_2100 = mask(esdm, prob_2100_perte)
  perdu_2100 = sum(esdm_2100[esdm_2100==1])
  results[[3]][i] <- perdu_2100/foret_2020
  
  print(i)
}


results

# par(mfrow=c(1,3))
hist(results[[1]])
ggplot(data.frame(results)) +
  geom_histogram(aes(x=perte))
ggplot(data.frame(results)) +
  geom_histogram(aes(x=perte_2000_2020))
ggplot(data.frame(results)) +
  geom_histogram(aes(x=perte_2100))

## Liste espèces qui perdent +80% de leur surface 

sp_perte_75 =  list_45sp[which(results$perte>0.75)] 
sp_perte_2020_75 =  list_45sp[which(results$perte_2000_2020>0.15)] 
sp_perte_2100_75 =  list_45sp[which(results$perte_2100>0.75)] 





#### Erreurs SSDM serveur #### 
# erreurs sdm
# Error in read.table(file = file, header = header, sep = sep, quote = quote,  :
#                       first five rows are empty: giving up
#                     Algorithm correlation table empty !
# 
#                       Ensemble model Austrobuxus_carunculatus.Ensemble.SDM : Error in compareRaster(x) : different extent
#                     Error in compareRaster(x) : different extent
# 
#                     
# erreurs quand stack 1112 sp 
# > stack_final <- do.call(stacking, c(list_all_esdm, list_methods_stacking))
# Stack creation...
# naming... done.
# range restriction... done.
# diversity mapping...
# Local species richness computed by thresholding and then summing.
# Killed
#                     
                    
                    
#### analyse SSDM ####

## load
par(mfrow = c(2,2))

list_sp = list.files()

## hist richesses 
# bin
ssdm_bin = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/SSDM_final/ssdm_bin.tif")
plot(ssdm_bin, main = "Distribution pontentielle de la richesse (avec méthode binaire)" )
richesse1 = na.omit(values(ssdm_bin))
richesse1

# prob
ssdm_prob = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/SSDM_final/ssdm_prob.tif")
plot(ssdm_prob, main = "Distribution pontentielle de la richesse (avec méthode probabilité)" )
richesse2 = na.omit(values(ssdm_prob))

# bin_range
ssdm_bin_range = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/SSDM_final/ssdm_bin_range.tif")
plot(ssdm_bin_range, main = "Distribution pontentielle de la richesse avec filtre de dispersion de 20km (méthode binaire)" )
richesse3 = na.omit(values(ssdm_bin_range))

# prob_range 
ssdm_prob_range = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/SSDM_final/ssdm_prob_range.tif")
plot(ssdm_prob_range, main = "Distribution pontentielle de la richesse avec filtre de dispersion de 20km (méthode probabilité)" )
richesse4 = na.omit(values(ssdm_prob_range))

tab_richesse = data.frame(richesse1,richesse2,richesse3,richesse4)



ggplot(tab_richesse) +
  geom_histogram(aes(x=richesse1)) +
  labs(  x = "Richesse par ha (SSDM binaire)", y = "Fréquence")

ggplot(tab_richesse) +
  geom_histogram(aes(x=richesse2)) +
  labs(  x = "Richesse par ha (SSDM probabilité)", y = "Fréquence")

ggplot(tab_richesse) +
  geom_histogram(aes(x=richesse3)) +
  labs(  x = "Richesse par ha (SSDM binaire et filtre de dispersion)", y = "Fréquence")

ggplot(tab_richesse) +
  geom_histogram(aes(x=richesse4)) +
  labs(  x = "Richesse par ha (SSDM probabilité et filtre de dispersion)", y = "Fréquence")

## corrélaion nb occ richesse 


#### Test hypothèse nulle : pas d'effet des var env sur la distribution des espèces ####
SSDM@diversity.map
# regarder importance des var env dans la construction des modèles 



#### Analyse richesse spécifique, sur SSDM ####

## avec méthode quantile (1/5 le plus élevé)
par(mfrow=c(1,2))
SSDM = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp/SSDM_final2/Stack/Rasters/Diversity.tif")
plot(SSDM, main = "SSDM method pSSDM")
lim_80 = quantile(SSDM, probs = c(0.8), names=F)
SSDM[SSDM < lim_80] <- NA
plot(SSDM, main = "20% plus riches")


## Zones plus riches // 3 différents milieux 
holdridge_nc = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/Holdridge_CHELSA.tif")
plot(holdridge_nc)
table(values(holdridge_nc))
# 1 : milieu sec dry
# 2 : milieu humide moist
# 3 : milieu très humide wet

# holdridge_nc[! holdridge_nc == 1 ] = NA
# plot(holdridge_nc)
# writeRaster(holdridge_nc, file ="D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/holdridge1.tif" )
# 
# holdridge_nc[! holdridge_nc == 2 ] = NA
# plot(holdridge_nc)
# writeRaster(holdridge_nc, file ="D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/holdridge2.tif" )
# 
# holdridge_nc[! holdridge_nc == 3 ] = NA
# plot(holdridge_nc)
# writeRaster(holdridge_nc, file ="D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/holdridge3.tif" )

holdridge1 = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/holdridge1.tif" )
holdridge2 = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/holdridge2.tif" )
holdridge3 = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/holdridge3.tif" )
mask = readOGR("D:/Vanessa/Mes Documents/Stage Manon C/r/mask_rasters/mask_bon_crs.shp")


par(mfrow = c(1,3))
plot(holdridge1)
plot(holdridge2)
plot(holdridge3)

par(mfrow = c(1,1))

# milieu 1 sec 
ssdm = raster ("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_713sp/SSDM_706_prob/Stack/Rasters/Diversity.tif")
plot(ssdm)
plot(holdridge_nc)

ssdm[! holdridge_test==1]  = NA

ssdm[!holdridge1==1]=NA
ssdm = crop(ssdm,holdridge_test)
holdridge_test = crop(holdridge_test,ssdm)
ssdm = mask(ssdm, holdridge_test)

plot(test1)

ssdm[holdridge_test == 1] = NA
test1 = holdridge_test

extend(holdridge1) <- extend(ssdm)

holdridge_test = crop(holdridge_nc, mask)
holdridge_test = mask(holdridge_test, mask)
plot(holdridge_test)
table(values(holdridge_test))

extend(mask)
plot(mask)



## id hotspot de richesse 

SSDM = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp/SSDM_final2/Stack/Rasters/Diversity.tif")
SSDM_utm = as(SSDM, "SpatRaster") # transform RasterLayer en SpatRaster
SSDM_utm = terra::project( SSDM_utm,  "EPSG:32758", method = "near") # re project raster de long lat WGS84 en UTM EPSG 32758
SSDM_utm = as(SSDM_utm, "Raster") # transform SpatRaster en Raster
plot(SSDM_utm)
pt = rasterToPoints(SSDM_utm, spatial =T) # tranform raster to vecteur : points (grille)
pt = st_as_sf(pt) # transform to sf (objet spatial + colone valeurs associées)
hotspot = hotspot_gistar(pt,weights =  Diversity) # création hotspot 

plot(hotspot)
plot(hotspot$geometry)
###### analyser 


## Crop SSDM avec foret actuelle ??  
SSDM_div = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/SSDM_45sp/SSDM_final2/Stack/Rasters/Diversity.tif")
SSDM_actuel = crop(SSDM_div,actual_forest_crop)
SSDM_actuel = mask(SSDM_div, actual_forest_crop)
plot(SSDM_actuel)
plot(SSDM_div)


#### Mise évidence zone prioritaire de conservation (hotspot richesse / hotspot déforestation) ####
# biblio dossier conservation
# Utiliser méthode clustering : délimiter zones les plus riches 

# autre méthode art Ledru et al. 2016
# package Ash

## hotspot déforestation 2100 
# raster prob_2100_perte : codé en 1 sur zones qui seront déforestées
rast = raster("D:/Vanessa/Mes Documents/Stage Manon C/r/Raster_foret/prob_2100_perte.tif")
rast_utm = as(rast, "SpatRaster") # transform RasterLayer en SpatRaster
rast_utm = terra::project( rast_utm,  "EPSG:32758", method = "near") # re project raster de long lat WGS84 en UTM EPSG 32758
rast_utm = as(rast_utm, "Raster") # transform SpatRaster en Raster
plot(rast_utm)
pt = rasterToPoints(rast_utm, spatial =T) # tranform raster to vecteur : points (grille)
pt = st_as_sf(pt) # transform to sf (objet spatial + colone valeurs associées)
hotspot = hotspot_gistar(pt) # création hotspot 

# saveRDS(hotspot, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/Results/hotspot_def_2100.rds")
hotspot = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Final_1/Results/hotspot_def_2100.rds")

plot(hotspot)
hotspot %>% 
  filter(gistar > 0, pvalue < 0.05) %>% 
  ggplot(aes(colour = kde, fill = kde)) +
  geom_sf() +
  scale_colour_distiller(aesthetics = c("colour", "fill"), direction = 1) +
  labs(title = "Hotspot of estimated deforestation in 2100") +
  theme_void()


