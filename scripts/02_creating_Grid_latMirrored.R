####################################################### ---
## Have not been developed 
root <- rprojroot::has_file(".git/index")
datadir = root$find_file("data")
funsdir = root$find_file("functions")
savingdir = root$find_file("saved_files")

##Depends on 2 things:
df_cluster <- readRDS(paste0(savingdir,'/','df_cluster'))
df_geo_abiotics <- readRDS(paste0(savingdir,'/','df_geo_abiotics'))

## Filter only samples that were used in the clustering process
df_geo_abiotics = df_geo_abiotics %>%
  filter(SampleID %in% df_cluster$SampleID)

## Store mean and sd of the df_geo_abiotics
lat_scale_obj   <- c(mean(df_geo_abiotics$Latitude),sd(df_geo_abiotics$Latitude))
depth_scale_obj <- c(mean(df_geo_abiotics$Depth),sd(df_geo_abiotics$Depth))

df_geo_abiotics = df_geo_abiotics %>% 
  mutate(lat_scaled = (Latitude -mean(Latitude)) /sd(Latitude),
         depht_scaled = (Depth -mean(Depth)) /sd(Depth))

## Now setting the values that the grid will run on
min_lat = round(min(df_geo_abiotics$lat_scaled),1)-0.1
max_lat = round(max(df_geo_abiotics$lat_scaled),1)+0.1
min_depth = round(min(df_geo_abiotics$depht_scaled),1)-0.1
max_depth = round(max(df_geo_abiotics$depht_scaled),1)+0.1

lat_grid = seq(min_lat,max_lat,0.01)
depth_grid = seq(min_depth,max_depth,0.01)

## expanding the grid 
grid_base = expand_grid(lat_grid,depth_grid)

## Here we set the number of nearest neigh 
nneigh = 5

#matrix that will retain the index of the neighbors
nneigh_aux <- matrix(NA,nrow = nrow(grid_base),ncol=nneigh)

for( i in 1:nrow(grid_base)){
  nneigh_aux[i,] <- dist_grid_sample(grid_base[i,],n_neigh = nneigh)
}


## only giving names to the columns 
aux <- 1:ncol(nneigh_aux)
colnames(nneigh_aux) = ifelse(aux<10,
                              paste0('n_neighs0',aux),
                              paste0('n_neighs',aux))


grid_base = bind_cols(grid_base,nneigh_aux) 

## now we can basically start from here now.
#saveRDS(grid_base,paste0(savingdir,'/','grid_base'))
grid_base <- readRDS(paste0(savingdir,'/','grid_base'))
