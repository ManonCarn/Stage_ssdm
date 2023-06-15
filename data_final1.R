
data_tot = read.csv ("D:/Vanessa/Mes Documents/Stage Manon C/r/Data/data_final_2/occ_total_nc_202306091601.csv", sep = ';')
data_unique = read.csv ("D:/Vanessa/Mes Documents/Stage Manon C/r/Data/data_final_2/occ_total_nc_unique_202306091600.csv", sep = ';')


list_sp_tot = unique(data_tot$taxonref)
sources = unique (data_tot$source)
sources

list_sp_gbif = unique(subset(data_tot, source == 'occ_gbif')$taxaname)
list_sp_gbif
list_sp_sans_gbif = unique(subset(data_tot, source != 'occ_gbif')$taxaname)

sp_que_gbif = list_sp_gbif[!list_sp_gbif %in% list_sp_sans_gbif]

# Une seule espèce apporté en plus par données GBIF -> enlever les données gbif 

data_unique_sans_gbif = unique (subset(data_tot,source != 'occ_gbif')[,3:8]) 
table(data_unique_sans_gbif$province)
data_unique_sans_gbif = subset(data_unique_sans_gbif,data_unique_sans_gbif$province != "PIL")
# 1309 sp pour 151 407 occ

data_final = data_unique_sans_gbif[,3:5]
names(data_final) = c("SPECIES","LONGITUDE","LATITUDE")
data_final = data_final[complete.cases(data_final),]
data_final = data_final[order(data_final$SPECIES),]
hist(table(data_final$SPECIES))
sum(table(data_final$SPECIES)<5) 
list_sp_final = unique(data_final$SPECIES) # = list_data_sans_gbif 

centsp = sample(list_sp_sans_gbif,100)
data_test_100sp = data_final[data_final$SPECIES %in% centsp,]
data_test_100sp = data_test_100sp[order(data_test_100sp$SPECIES),]

# write.csv2(data_final, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2.csv")
# write.csv2(data_test_100sp, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_test_100sp.csv")
# scp -r  "D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2.csv"   manon@niamoto.ird.nc:~/stage_ssdm/test2/data
# scp -r  "D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_test_100sp.csv"   manon@niamoto.ird.nc:~/stage_ssdm/test2/data

data_final2 = read.csv2("D:/Vanessa/Mes Documents/Stage Manon C/r/final_1/data_final2.csv")
liste = unique( data_final2$SPECIES)

data_test_3sp = readRDS("D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/data_test_3sp.rds")
table(data_test_3sp$SPECIES)
data_test_3sp_moins = data_test_3sp[-c(74:95),]
table(data_test_3sp_moins$SPECIES)
  

# write.csv2(data_test_3sp_moins, file = "D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/data_test_3sp_moins.csv")
# scp -r  "D:/Vanessa/Mes Documents/Stage Manon C/r/Entrainement/data_test_3sp_moins.csv"   manon@niamoto.ird.nc:~/stage_ssdm/test2/data





