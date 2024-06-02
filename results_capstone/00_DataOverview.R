########################################################
## Script to get a list containing k "Bootstrap" samples
########################################################
library(dplyr)

root <- rprojroot::has_file(".git/index")
datadir = root$find_file("data")
funsdir = root$find_file("functions")
savingdir = root$find_file("saved_files")

files_vec <- list.files(funsdir)

for( i in 1:length(files_vec)){
  source(root$find_file(paste0(funsdir,'/',files_vec[i])))
}

####### --------------- Loading Grump 
## Use this if you are using the new GRUMP Data Set
datapath = root$find_file(paste0(datadir,'/','grump_asv_long.csv'))
dframe = data.table::fread(input = datapath)


dframe %>% select(ASV_hash) %>% 
  distinct() %>% nrow()


dframe %>% select(SampleID) %>% 
  distinct() %>% nrow()


grump_wide = dframe %>% pivot_wider(id_cols = SampleID,
                       names_from = ASV_hash,
                       values_from = Corrected_sequence_counts)

sum(is.na(grump_wide[,-1])) #155161785

## proportion of Zeros
# 155161785 / (1183*132265)



dim(grump_wide[,-1])

dframe$Cruise %>% unique()

dframe = dframe %>% filter(Genus=='Phaeocystis',Cruise!='MOSAIc')

dframe %>% select(SampleID) %>% distinct() %>% nrow()

