################################################################################
#### ESDM ON SERVEUR TEST GREG ####
################################################################################
library(methods)
library(raster)
library(SSDM)
library(parallel)

# Load environmental dataset and occurences dataset ----
# environmental dataset 
env_path = "~/stage_ssdm/test1/data/Raster_base"
Env = load_var(env_path, categorical = 'substrat')
# occurences dataset 
occ_path = "~/stage_ssdm/test2/data/"
file_name = "data_final2.csv"
# get occurences table and apply spatial thinning (Env resolution) on species occurrence to reduce spatial sampling bias
Occ = load_occ(path = occ_path, Env, file = file_name,
               Xcol = "LONGITUDE", Ycol = "LATITUDE", Spcol = "SPECIES",
               GeoRes = TRUE)
# modify species names (avoid blanck spaces)
Occ <- Occ[,-1]
names(Occ) = c('SpeciesID', 'Longitude', 'Latitude')
Occ$SpeciesID <- gsub("\\s+", " ", stringr::str_trim(Occ$SpeciesID ))
Occ$SpeciesID <- unlist(lapply(strsplit(Occ$SpeciesID, " "), function(x) paste(x[1:2],collapse="_")))


# Modelling loop ----
#### test with foreach ####

# Saving directory
path = "~/stage_ssdm/test2/results/ESDM_final" 
# Clustering parameters 

sp_done <- list.files(path)
Occ <- Occ[!Occ$SpeciesID %in% sp_done,] 

library(doSNOW)
# Loop accross all species in the occurrences dataset
species <- levels(as.factor(Occ$SpeciesID))
n_species <- length(species)

cores = detectCores()-1 # Number of cores
cat('Opening clusters,', cores, 'cores \n')
cl <- makeCluster(cores,outfile = "")
registerDoSNOW(cl)
clusterEvalQ(cl, library(SSDM))
clusterEvalQ(cl, library(rgeos))
clusterEvalQ(cl, library(raster))

pb <- txtProgressBar(max = n_species, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
startTime = Sys.time()
system.time(
  enms <- foreach(i = 1:n_species, .options.snow = opts) %dopar%
    {
      sp_tmp <- species[i]
      SpOcc = subset(Occ, Occ$SpeciesID == sp_tmp)
      cat('Ensemble modelling :', sp_tmp, '\n\n')
      enm = try(SSDM::ensemble_modelling("all" , SpOcc, Env, rep = 10,
                                         tmp = FALSE, n.cores = 1,  name = sp_tmp))
      print(enm@name)
      if (inherits(enm, "try-error")) {
        cat(enm)
      } else {
        SSDM::save.esdm(enm, path = path)
      }
      cat(sp_tmp, 'Ensemble modelling done for',sp_tmp, '\n\n\n\n')
      rm(sp_tmp, SpOcc, enm)
      gc()
    }
)
close(pb)
stopCluster(cl)

time_ESDM = (Sys.time() - startTime)
print(time_ESDM)
                                                                                                                             


# Loop for loading all the ESDM in a directory ----
## modify load.esdm() function in SSDM package ##
source("~/stage_ssdm/R_scripts/load_model_modif.R")
# the call to environment() assures that the function will be able to call other hidden functions from the package.
environment(load_esdm_modif) <- environment(load_esdm)
# The call to assignInNamespace() assures that other functions from the package will call your updated version of the function.
assignInNamespace("load_esdm", load_esdm_modif, ns = "SSDM")
# load directory
path_load = '~/stage_ssdm/test2/results/ESDM_final'
all_esdm_names <- list.files(path_load)
# apply function (modified) to load all esdms
list_all_esdm <- sapply(all_esdm_names, function(x) load_esdm_modif(x, path_load))
names(list_all_esdm) <- NULL
# load one esdm (if needed)
# esdm_Acropogon_schistophilus = load_esdm_modif(name = 'Acropogon_schistophilus', path = '~/stage_ssdm/test1/results/ESDM/greg_test')

# ESDM Stacking ----
# define stacking arguments for methods
list_methods_stacking <- list(name = NULL, method = "pSSDM", rep.B = 1000,
                           Env = NULL, range = NULL, endemism = c("WEI", "Binary"),
                           eval = TRUE, verbose = TRUE, GUI = FALSE)
# stacking based on esdms list


stack_tot <- do.call(stacking, c(list_all_esdm, list_methods_stacking))

print(time_ESDM)
print(Sys.time() - startTime)

