########################################################
## Script to get a list containing k "Bootstrap" samples
########################################################

root <- rprojroot::has_file(".git/index")
datadir = root$find_file("data")
funsdir = root$find_file("functions")
savingdir = root$find_file("saved_files")

files_vec <- list.files(funsdir)

for( i in 1:length(files_vec)){
  source(root$find_file(paste0(funsdir,'/',files_vec[i])))
}

dat_tax = data.table::fread('https://raw.githubusercontent.com/rafaelcatoia/zoop_16N/main/treated_taxonomy_dat.csv') %>%
  as_tibble()

### Loading the new data 
grump_version = 'new'

if(grump_version=='new'){
  ## Use this if you are using the new GRUMP Data Set
  datapath = root$find_file(paste0(datadir,'/','grump_asv_long20240501.csv'))
  dframe = data.table::fread(input = datapath) %>%
    filter(Cruise %in% c('P16N','P16S')) %>% 
    #filter(Raw.Sequence.Counts>0) %>% 
    filter(Domain!='Unassigned') %>% 
    mutate(Raw.Sequence.Counts = Corrected_sequence_counts)
  
}else{
  ## Or this one if you are using the OLD GRUMP Data Set
  datapath = root$find_file(paste0(datadir,'/','grump_asv_long_20240110.csv'))
  dframe = data.table::fread(input = datapath) %>%
    filter(Cruise %in% c('P16N','P16S')) %>% 
    #filter(Raw.Sequence.Counts>0) %>% 
    filter(Domain!='Unassigned')
}

#### -- now lets subset by only using the ASV that we had before
dframe <- dframe %>% filter(ASV_hash %in% dat_tax$ASV_ID) %>% 
  left_join(dframe %>% filter(ASV_hash %in% dat_tax$ASV_ID) %>% 
              select(ASV_hash) %>% distinct() %>% 
              mutate(ID_ASV_Num = 1:n()) %>% 
              mutate(ID_ASV = ifelse(ID_ASV_Num<10,paste('000',ID_ASV_Num,sep=''),
                                     ifelse(ID_ASV_Num<100,paste('00',ID_ASV_Num,sep=''),
                                            ifelse(ID_ASV_Num<1000,paste('0',ID_ASV_Num,sep=''),
                                                   paste(ID_ASV_Num))))) %>% 
              mutate(ID_ASV = paste0('ASV_',ID_ASV)) %>% 
              select(ASV_hash,ID_ASV)) %>% 
  filter(!is.na(ID_ASV)) 

### -----------------------------------------------------------
## Creating abiotics dataframe  -------------------------------
### -----------------------------------------------------------
vet_abiotic = c(
  "Temperature",
  "Salinity",
  "Oxygen",
  "Silicate",
  "NO2",
  #"NO3",#this causes duplicates
  #"NH3",this is empty
  "PO4"
)

## Filtering the abiotics per sample
df_geo_abiotics <- dframe %>%
  select(SampleID,one_of(vet_abiotic),Latitude,Longitude,Depth,Longhurst_Short) %>% distinct() %>% 
  distinct() %>% arrange(SampleID)

## Saving the abiotics df
saveRDS(df_geo_abiotics,file = paste0(savingdir,'/','df_geo_abiotics'))
### -----------------------------------------------------------
## Lets now keep only the things that are interesting to us ---
### -----------------------------------------------------------

#here we have how many asvs were observed in each sample
ASVsPerSamples = dframe %>% select(SampleID,ID_ASV) %>% 
  distinct() %>% 
  group_by(SampleID) %>% 
  summarise(Sample_nof_ASVs=n()) %>% 
  arrange(Sample_nof_ASVs)

#here we have in how many samples each asv was observed
SamplesPerASVs = dframe %>% select(SampleID,ID_ASV) %>% 
  distinct() %>% 
  group_by(ID_ASV) %>% 
  summarise(ASVs_in_Samples=n()) %>% 
  arrange(ASVs_in_Samples) %>% arrange(ID_ASV)


dframe = dframe %>% 
  select(SampleID,ID_ASV,Raw.Sequence.Counts) %>% 
  left_join(ASVsPerSamples) %>% 
  left_join(SamplesPerASVs)


## Saving the abiotics df
saveRDS(dframe,file = paste0(savingdir,'/','dfGrump_longer_filtered'))

### --------------------------------------------------------------
### Creating the B replicates of aitchison distances 
### --------------------------------------------------------------
B=250
list_AitDist = list()
idASVs = dframe %>% select(ID_ASV) %>% distinct() %>% pull
pct_colSubSample = 0.75 

min_raw_count = dframe %>% select(Raw.Sequence.Counts) %>% min()
min_raw_count = min_raw_count/1000

seedI = 1234
set.seed(seedI)
for(ii in 1:B){
  
  set.seed(seedI+ii)
  asvSubset=sample(idASVs,size = pct_colSubSample*length(idASVs))
  
  list_AitDist[[ii]] <-dframe %>%
    filter(ID_ASV %in% asvSubset) %>% 
    mutate(Raw.Sequence.Counts=Raw.Sequence.Counts + min_raw_count) %>% 
    select(SampleID,ID_ASV,Raw.Sequence.Counts) %>% distinct() %>% 
    group_by(SampleID,ID_ASV) %>% 
    summarise(Sum_RawCounts = sum(Raw.Sequence.Counts)) %>% ungroup %>% 
    pivot_wider(id_cols = SampleID,names_from = ID_ASV ,
                values_from = Sum_RawCounts,
                values_fill = min_raw_count) %>%
    data.frame() %>% 
    mutate(across(where(is.numeric))/rowSums(across(where(is.numeric)))) %>% 
    relocate(SampleID,sort(names(.))) %>% 
    arrange(SampleID) %>% 
    select(-SampleID) %>% 
    vegan::vegdist(method = 'aitchison') %>% as.matrix() %>% 
    normalizeMatrix()
  
}

####### here we have a list of bootstraped normalized aitDist
####### now lets save this and move to the next step
saveRDS(list_AitDist,file = paste0(savingdir,'/','list_AitDist'))



###### creating the distance matrices that we will use in the convex mixture in the future
df_geo_abiotics = readRDS(file = paste0(savingdir,'/','df_geo_abiotics'))
df_geo_abiotics = df_geo_abiotics %>% arrange(SampleID)

## First the normalized ones 
geo_Dist = df_geo_abiotics %>% 
  transmute(lat_scaled = (Latitude -mean(Latitude)) /sd(Latitude),
            depht_scaled = (Depth -mean(Depth)) /sd(Depth)) %>%
  as.matrix() %>% dist() %>% as.matrix() %>% normalizeMatrix()

geo_Dist_mirrored = df_geo_abiotics %>% 
  mutate(Latitude = abs(Latitude)) %>% 
  transmute(lat_scaled = (Latitude - mean(Latitude)) /sd(Latitude),
            depht_scaled = (Depth -mean(Depth)) /sd(Depth)) %>%
  as.matrix() %>% dist() %>% as.matrix() %>% normalizeMatrix()

abioticDist = df_geo_abiotics %>% 
  transmute(Temperature = scale(Temperature),
            Salinity = scale(Salinity),
            Oxygen = scale(Oxygen),
            Silicate = scale(Silicate),
            NO2 = scale(NO2),
            PO4 = scale(PO4)) %>% 
  as.matrix() %>% dist() %>% as.matrix() %>%  normalizeMatrix()


list_normalized_geo_abiotics_dists <- list(
  geo_Dist = geo_Dist,
  geo_Dist_mirrored = geo_Dist_mirrored,
  abioticDist = abioticDist
)


## Now the unnormalized
geo_Dist = df_geo_abiotics %>% 
  transmute(lat_scaled = (Latitude -mean(Latitude)) /sd(Latitude),
            depht_scaled = (Depth -mean(Depth)) /sd(Depth)) %>%
  as.matrix() %>% dist() %>% as.matrix()

geo_Dist_mirrored = df_geo_abiotics %>% 
  mutate(Latitude = abs(Latitude)) %>% 
  transmute(lat_scaled = (Latitude -mean(Latitude)) /sd(Latitude),
            depht_scaled = (Depth -mean(Depth)) /sd(Depth)) %>%
  as.matrix() %>% dist() %>% as.matrix()

abioticDist = df_geo_abiotics %>% 
  transmute(Temperature = scale(Temperature),
            Salinity = scale(Salinity),
            Oxygen = scale(Oxygen),
            Silicate = scale(Silicate),
            NO2 = scale(NO2),
            PO4 = scale(PO4)) %>% 
  as.matrix() %>% dist() %>% as.matrix()

min_raw_count = dframe %>% select(Raw.Sequence.Counts) %>% min()
min_raw_count = min_raw_count/1000

list_geo_abiotics_dists <- list(
  geo_Dist = geo_Dist,
  geo_Dist_mirrored = geo_Dist_mirrored,
  abioticDist = abioticDist,
  aitDist = dframe %>%
    mutate(Raw.Sequence.Counts=Raw.Sequence.Counts + min_raw_count) %>% 
    select(SampleID,ID_ASV,Raw.Sequence.Counts) %>% distinct() %>% 
    group_by(SampleID,ID_ASV) %>% 
    summarise(Sum_RawCounts = sum(Raw.Sequence.Counts)) %>% ungroup %>% 
    pivot_wider(id_cols = SampleID,names_from = ID_ASV ,
                values_from = Sum_RawCounts,
                values_fill = min_raw_count) %>%
    data.frame() %>% 
    mutate(across(where(is.numeric))/rowSums(across(where(is.numeric)))) %>% 
    relocate(SampleID,sort(names(.))) %>% 
    arrange(SampleID) %>% 
    select(-SampleID) %>% 
    vegan::vegdist(method = 'aitchison') %>% as.matrix()
)


## So here we have two different list of distance matrices. 
saveRDS(list_geo_abiotics_dists,paste0(savingdir,'/','list_geo_abiotics_dists'))
saveRDS(list_normalized_geo_abiotics_dists,paste0(savingdir,'/','list_normalized_geo_abiotics_dists'))



