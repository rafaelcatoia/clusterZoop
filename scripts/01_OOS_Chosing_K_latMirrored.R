
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

dim_all <- lapply(list_AitDist,dim) %>% unlist()
dim_all %>% unique()
## there are two positions for which the dimension is not 165
list_AitDist_clean = Filter(function(x) dim(x)[1] == 165, list_AitDist)

df_evaluation <- data.table::rbindlist(lapply(list_AitDist_clean[1:200],function(x){eval_clustering(D = x,latMirrored = T)}))
###### THE WARNINGS ARE SUPOSED TO HAPPEN!


saveRDS(df_evaluation,paste0(savingdir,'/','df_evaluation_lat_mirrored'))
#df_evaluation = readRDS(paste0(savingdir,'/','df_evaluation'))