################################################################################
#### ESDM ON SERVEUR TEST GREG ####
################################################################################
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


# Modelling loop ----
# Saving directory
path = "~/stage_ssdm/final/results/ESDM" 

sp_done <- list.files(path)
Occ <- Occ[!Occ$SpeciesID %in% sp_done,] 

#### try with parrallel on species with forked parallel  ####
# Clustering parameters 
# cores_use = detectCores()-1 # Number of cores
cores_use = 5
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
list_methods_stacking <- list(name = NULL, method = "pSSDM", rep.B = 1000,
                              Env = NULL, range = NULL, endemism = c("WEI", "Binary"),
                              eval = TRUE, verbose = TRUE, GUI = FALSE)
# stacking based on esdms list
# stack_test_3sp <- do.call(stacking, c(list_esdm_stacking, list_methods_stacking))

stack_final <- do.call(stacking, c(list_all_esdm, list_methods_stacking))

print(time_ESDM)
print(Sys.time() - startTime)



save.stack(stack_final, name = "SSDM_final", path = "~/stage_ssdm/final/results")

# scp -r  manon@niamoto.ird.nc:~/stage_ssdm/final/results/SSDM_final "D:/Vanessa/Mes Documents/Stage Manon C/r/final_1"   


