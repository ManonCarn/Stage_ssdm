################################################################################
#### ESDM ON SERVEUR TEST GREG ####
################################################################################

#### Clean cash 
# Dans R:     
# clear temp files
tmp_dir <- tempdir()
unlink(tmp_dir, recursive = TRUE)
q()

# Dans le terminal (connecté sous greg@niamoto) :
# sudo sh -c 'echo 1 >/proc/sys/vm/drop_caches'

# Compter nb fichiers sell 
ls -1 | wc -l



library(methods)
library(raster)
library(SSDM)
library(parallel)
library(doSNOW)
# library(rgdal)

# Load environmental dataset and occurences dataset ----
# environmental dataset 
env_path = "~/stage_ssdm/final/data/Raster_base2"
Env = load_var(env_path, categorical = 'substrat_ok')
# occurences dataset 
occ_path = "~/stage_ssdm/final/data/"
# file_occ = "data_final2.csv"
# get occurences table and apply spatial thinning (Env resolution) on species occurrence to reduce spatial sampling bias
# Occ = load_occ(path = occ_path, Env, file = file_occ,
#                Xcol = "LONGITUDE", Ycol = "LATITUDE", Spcol = "SPECIES",
#                GeoRes = TRUE)

# modify species names (avoid blank spaces)
# Occ <- Occ[,-1]
# names(Occ) = c('SpeciesID', 'Longitude', 'Latitude')
# Occ$SpeciesID <- gsub("\\s+", " ", stringr::str_trim(Occ$SpeciesID ))
# Occ$SpeciesID <- unlist(lapply(trsplit(Occ$SpeciesID, " "), function(x) paste(x[1:2],collapse="_")))

# save 
# saveRDS(Occ, file = paste0(occ_path, "/data_final2_thinning"))
# load 
Occ <- readRDS(file = paste0(occ_path, "/data_final2_thinning.rds"))
# Occ <- readRDS(file = paste0(occ_path, "/data_final2_thinning_arranged.rds"))



# Modelling loop ----
# Saving directory
path = "~/stage_ssdm/final/results/ESDM" 

sp_done <- list.files(path)
Occ <- Occ[!Occ$SpeciesID %in% sp_done,] 

#### try with parrallel on species with forked parallel  ####
# Clustering parameters 
# cores_use = detectCores()-1 # Number of cores
cores_use = 9
# Loop accross all species in the occurrences dataset
species = levels(as.factor(Occ$SpeciesID))
startTime = Sys.time()
enms = mclapply(species,
                 function(species){
                   enm.name = species
                   SpOcc = subset(Occ, Occ$SpeciesID == species)
                   cat('Ensemble modelling :', enm.name,'\n\n\n\n')
                   enm = try(SSDM::ensemble_modelling("all" , SpOcc, Env, rep = 10,
                                                      tmp = T, n.cores = 1,  name = enm.name, verbose = T))
                   print(enm@name)
                   if (inherits(enm, "try-error")) {
                     cat(enm)
                   } else {
                     SSDM::save.esdm(enm, path = path)
                   }
                   cat(enm.name, 'Ensemble modelling done for',species, '\n\n\n\n')
                   rm(enm.name, SpOcc, enm)
                   gc()
                 }
                , mc.cores = cores_use)
cat('Closing clusters \n')

time_ESDM = (Sys.time() - startTime)
print(time_ESDM)

closeAllConnections()


# Loop for loading all the ESDM in a directory ----
library(methods)
library(raster)
library(SSDM)
library(parallel)
library(doSNOW)
startTime = Sys.time()
## modify load.esdm() function in SSDM package ##
source("~/stage_ssdm/R_scripts/load_model_modif.R")
# the call to environment() assures that the function will be able to call other hidden functions from the package.
environment(load_esdm_modif) <- environment(load_esdm)
# The call to assignInNamespace() assures that other functions from the package will call your updated version of the function.
assignInNamespace("load_esdm", load_esdm_modif, ns = "SSDM")
# load directory
path_load = '~/stage_ssdm/final/results/ESDM'
all_esdm_names <- list.files(path_load)
# apply function (modified) to load all esdms
list_all_esdm <- sapply(all_esdm_names, function(x) load_esdm_modif(x, path_load))
names(list_all_esdm) <- NULL
# load one esdm (if needed)
# esdm_Acropogon_schistophilus = load_esdm_modif(name = 'Acropogon_moratianus', path = '~/stage_ssdm/test2/results/ESDM/')
# esdm_Acropogon_schistophilus = load_esdm(name = 'Acropogon_moratianus', path = '~/stage_ssdm/test2/results/ESDM/')

# ESDM Stacking ----
# define stacking arguments for methods
list_methods_stacking <- list(name = NULL, method = "bSSDM",
                              Env = NULL, range = NULL, endemism = NULL,
                              eval = TRUE, verbose = TRUE, GUI = FALSE)
# stacking based on esdms list
# stack_test_3sp <- do.call(stacking, c(list_esdm_stacking, list_methods_stacking))

stack_final <- do.call(stacking, c(list_all_esdm, list_methods_stacking))

# print(time_ESDM)


save.stack(stack_final, name = "SSDM_final_tot", path = "~/stage_ssdm/final/results")

saveRDS(stack_final, file = "~/stage_ssdm/final/results/stack_final_tot.rds")

print(Sys.time() - startTime)


# scp -r  manon@niamoto.ird.nc:~/stage_ssdm/final/results/ESDM "D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/SSDM_final"   
# scp -r  manon@niamoto.ird.nc:~/stage_ssdm/final/results/ssdm_prob_range2.tif "D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/SSDM_final"   
# scp -r  manon@niamoto.ird.nc:~/stage_ssdm/final/results/var_imp_sd.rds "D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/SSDM_final"   

## Stack les 3 stack 
# path_load = '~/stage_ssdm/final/results'
# ssdm1 = load_stack(name = "SSDM_final1",path = path_load )
# ssdm1 = readRDS(paste0(path_load,"/stack_final1.rds"))
# ssdm2 = load_stack(name = "SSDM_final2",path = path_load )
# ssdm2 = readRDS(paste0(path_load,"/stack_final2.rds"))
# ssdm3 = load_stack(name = "SSDM_final3",path = path_load )
# ssdm3 = readRDS(paste0(path_load,"/stack_final3.rds"))
# 
# 
# stack_finally = stacking(ssdm1,ssdm2,ssdm3,name = NULL, method = "bSSDM", rep.B = 1000,
#                          Env = NULL, range = NULL, endemism = c("WEI", "Binary"),
#                          eval = TRUE, verbose = TRUE, GUI = FALSE)
# 
# 
# save.stack(stack_finally, name = "stack_final_tot", path = path_load)
# saveRDS(stack_finally, file = paste0(path_load,"/stack_final_tot.rds"))

# -> marche pas, fonction stack que pour objet sdm 



## esdm avec range 
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

# source("~/stage_ssdm/R_scripts/load_model_modif.R")
# environment(load_esdm_modif) <- environment(load_esdm)
# assignInNamespace("load_esdm", load_esdm_modif, ns = "SSDM")
path_load = '~/stage_ssdm/final/results/ESDM_range'
all_esdm_names <- list.files(path_load)
# list_all_esdm <- sapply(all_esdm_names, function(x) load_esdm_modif(x, path_load))
# names(list_all_esdm) <- NULL
# saveRDS(list_all_esdm, file = "~/stage_ssdm/final/data/list_all_esdm.rds")
# list_all_esdm = readRDS("~/stage_ssdm/final/data/list_all_esdm.rds")


buf = 20000 # 20km

occ = readRDS("~/stage_ssdm/final/data/data_final2_thinning_names.rds")
names(occ) = c("SPECIES","LONGITUDE","LATITUDE")
occ = subset(occ, occ$SPECIES %in% all_esdm_names)

for(i in 391:length(all_esdm_names)){
  print(i)
  sp_name = all_esdm_names[i]
  raster_name = paste0(path_load,"/",sp_name,"/Rasters/Binary.tif")
  esdm = raster::raster(raster_name)
  tab = subset(occ, occ$SPECIES == sp_name ) # subset tab pour sp i
  xy = data.frame(tab$LONGITUDE,tab$LATITUDE) # xy = 2 col long lat du fichier occ
  # esdm = list_all_esdm[[i]] #  subset esdm i 
  spdf <- SpatialPointsDataFrame(coords = xy, data = tab,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  spdf <- spTransform(spdf, CRS = "+proj=utm +zone=58 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  spdf <- st_as_sf(spdf) # modif spdf en sf
  buffer_tmp <- st_buffer(spdf, dist = buf, bOnlyEdges =FALSE ) # buffer autour des occ
  buffer_tmp <- st_transform(buffer_tmp, crs = crs(esdm))
  r1 <- esdm
  r3 <- terra::mask(r1, buffer_tmp, updatevalue = 0)
  terra::writeRaster(r3, filename = raster_name, overwrite=T)
  rm(list = c("r1","r3","spdf","buffer_tmp"))
  # list_all_esdm[[i]]@projection[is.na(r3[])] <- 0
}

# saveRDS(list_all_esdm, file = "~/stage_ssdm/final/data/list_all_esdm_range2.rds" )



## esdm prob avec range 
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

# source("~/stage_ssdm/R_scripts/load_model_modif.R")
# environment(load_esdm_modif) <- environment(load_esdm)
# assignInNamespace("load_esdm", load_esdm_modif, ns = "SSDM")
path_load = '~/stage_ssdm/final/results/ESDM_range'
all_esdm_names <- list.files(path_load)
# list_all_esdm <- sapply(all_esdm_names, function(x) load_esdm_modif(x, path_load))
# names(list_all_esdm) <- NULL
# saveRDS(list_all_esdm, file = "~/stage_ssdm/final/data/list_all_esdm.rds")
# list_all_esdm = readRDS("~/stage_ssdm/final/data/list_all_esdm.rds")


buf = 20000 # 20km

occ = readRDS("~/stage_ssdm/final/data/data_final2_thinning_names.rds")
names(occ) = c("SPECIES","LONGITUDE","LATITUDE")
occ = subset(occ, occ$SPECIES %in% all_esdm_names)

for(i in 1:length(all_esdm_names)){
  print(i)
  sp_name = all_esdm_names[i]
  raster_name = paste0(path_load,"/",sp_name,"/Rasters/Probability.tif")
  esdm = raster::raster(raster_name)
  tab = subset(occ, occ$SPECIES == sp_name ) # subset tab pour sp i
  xy = data.frame(tab$LONGITUDE,tab$LATITUDE) # xy = 2 col long lat du fichier occ
  # esdm = list_all_esdm[[i]] #  subset esdm i 
  spdf <- SpatialPointsDataFrame(coords = xy, data = tab,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  spdf <- spTransform(spdf, CRS = "+proj=utm +zone=58 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  spdf <- st_as_sf(spdf) # modif spdf en sf
  buffer_tmp <- st_buffer(spdf, dist = buf, bOnlyEdges =FALSE ) # buffer autour des occ
  buffer_tmp <- st_transform(buffer_tmp, crs = crs(esdm))
  r1 <- esdm
  r3 <- terra::mask(r1, buffer_tmp, updatevalue = 0)
  terra::writeRaster(r3, filename = raster_name, overwrite=T)
  rm(list = c("r1","r3","spdf","buffer_tmp"))
  # list_all_esdm[[i]]@projection[is.na(r3[])] <- 0
}

# saveRDS(list_all_esdm, file = "~/stage_ssdm/final/data/list_all_esdm_range2.rds" )




library(raster)
##  stack manuel binaire  
path_load = '~/stage_ssdm/final/results/ESDM/'
list_sdm <- list.files(path_load)
for (i in 1:length(list_sdm)){
  esdm = raster(paste0(path_load,list_sdm[i],"/Rasters/Binary.tif"))
  if (i == 1){ 
    ssdm = esdm
  }else{
    ssdm = ssdm + esdm
  }
  print(i)
}

writeRaster(ssdm, file = "~/stage_ssdm/final/results/ssdm_bin.tif", overwrite=T )

#   stack manuel proba
path_load = '~/stage_ssdm/final/results/ESDM/'
list_sdm <- list.files(path_load)
for (i in 1:length(list_sdm)){
  esdm = raster(paste0(path_load,list_sdm[i],"/Rasters/Probability.tif"))
  if (i == 1){ 
    ssdm = esdm
  }else{
    ssdm = ssdm + esdm
  }
  print(i)
}

writeRaster(ssdm, file = "~/stage_ssdm/final/results/ssdm_prob.tif" , overwrite = T)


#  stack manuel binaire range 
path_load = '~/stage_ssdm/final/results/ESDM_range/'
list_sdm <- list.files(path_load)
for (i in 1:length(list_sdm)){
  esdm = raster(paste0(path_load,list_sdm[i],"/Rasters/Binary.tif"))
  if (i == 1){ 
    ssdm = esdm
  }else{
    ssdm = ssdm + esdm
  }
  print(i)
}

writeRaster(ssdm, file = "~/stage_ssdm/final/results/ssdm_bin_range.tif" )

#  stack manuel prob range 
path_load = '~/stage_ssdm/final/results/ESDM_range/'
list_sdm <- list.files(path_load)
for (i in 1:length(list_sdm)){
  esdm = raster(paste0(path_load,list_sdm[i],"/Rasters/Probability.tif"))
  if (i == 1){ 
    ssdm = esdm
  }else{
    ssdm = ssdm + esdm
  }
  print(i)
}

writeRaster(ssdm, file = "~/stage_ssdm/final/results/ssdm_prob_range2.tif" , overwrite = T)



## Variable importance 
path_load = '~/stage_ssdm/final/results/ESDM/'
list_sdm <- list.files(path_load)
for (i in 1:length(list_sdm)){
  print(i)
  var_imp = read.csv(paste0(path_load,list_sdm[i],"/Tables/VarImp.csv"))
  if (i == 1){ 
    tab = var_imp  
    }else{
      colnames(var_imp) <- colnames(tab)
      tab = rbind(tab,var_imp)
  }
 
}


tab <- tab[-1]
summary_var_imp = summary(tab)
var_imp_mean = apply(tab,2,mean)
var_imp_sd = apply(tab,2,sd)

saveRDS(var_imp_mean, file = "~/stage_ssdm/final/results/var_imp_mean.rds")
saveRDS(var_imp_sd, file = "~/stage_ssdm/final/results/var_imp_sd.rds")
saveRDS(summary_var_imp, file = "~/stage_ssdm/final/results/summary_var_imp.rds")


## modèle évaluation 
path_load = '~/stage_ssdm/final/results/ESDM/'
list_sdm <- list.files(path_load)
for (i in 1:length(list_sdm)){
  print(i)
  mod_eval = read.csv(paste0(path_load,list_sdm[i],"/Tables/esdmeval.csv"))
  if (i == 1){ 
    tab = mod_eval  
  }else{
    colnames(mod_eval) <- colnames(tab)
    tab = rbind(tab,mod_eval)
  }
  
}

tab = tab[-1]
summary_mod_eval = summary(tab)
mod_eval_mean = apply(tab,2,mean, na.rm=TRUE)
mod_eval_sd = apply(tab,2,sd)

saveRDS(mod_eval_mean, file = "~/stage_ssdm/final/results/mod_eval_mean.rds")
saveRDS(mod_eval_sd, file = "~/stage_ssdm/final/results/mod_eval_sd.rds")
saveRDS(summary_mod_eval, file = "~/stage_ssdm/final/results/summary_mod_eval.rds")


