################################## 
library(dplyr) ; library(tidyr)

root <- rprojroot::has_file(".git/index")
datadir = root$find_file("data")
funsdir = root$find_file("functions")
savingdir = root$find_file("saved_files")

files_vec <- list.files(funsdir)

for( i in 1:length(files_vec)){
  source(root$find_file(paste0(funsdir,'/',files_vec[i])))
}

###############################
## Loading
###############################
df_geo_abiotics = readRDS(file = paste0(savingdir,'/','df_geo_abiotics'))
dfGrump_longer_filtered = readRDS(paste0(savingdir,'/','dfGrump_longer_filtered'))
list_AitDist = readRDS(paste0(savingdir,'/','list_AitDist'))
list_normalized_geo_abiotics_dists = readRDS(file = paste0(savingdir,'/','list_normalized_geo_abiotics_dists'))
list_geo_abiotics_dists = readRDS(file = paste0(savingdir,'/','list_geo_abiotics_dists'))
grid_base = readRDS(file = paste0(savingdir,'/','grid_base'))


#############################
dim_all <- lapply(list_AitDist,dim) %>% unlist()

#### there are two positions for which the dimension is not 165, so I'm removing those
## this might have happened because when we subset the columns (ASVs)
## some samples may have being composed by 
list_AitDist_clean = Filter(function(x) dim(x)[1] == 165, list_AitDist)


#list_cluster_membership_and_bounderies = lapply(list_AitDist_clean[1:100],coloring_map)
#saveRDS(list_cluster_membership_and_bounderies,file = paste0(savingdir,'/','list_cluster_membership_and_bounderies'))

list_cluster_membership_and_bounderies_mirroredLat = lapply(
  list_AitDist_clean[1:100],
  function(x){coloring_map(D = x,latMirrored = T,nclusters = 10)})
saveRDS(list_cluster_membership_and_bounderies_mirroredLat,
        file = paste0(savingdir,'/','list_cluster_membership_and_bounderies_mirroredLat'))

#
#saveRDS(df_clustRegion,paste0(savingdir,'/',"df_clustRegion"))
#saveRDS(df_Limits,paste0(savingdir,'/',"df_Limits"))


