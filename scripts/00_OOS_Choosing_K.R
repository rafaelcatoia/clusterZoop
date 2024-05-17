
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

df_evaluation <- data.table::rbindlist(lapply(list_AitDist_clean[1:200], eval_clustering))

saveRDS(df_evaluation,paste0(savingdir,'/','df_evaluation'))

## Everything underneath this was used to create functions.. 
# df_evaluation = readRDS(paste0(savingdir,'/','df_evaluation'))
# 
# df_summary = bind_rows(
#   df_evaluation %>% 
#   group_by(ncluster,method,alpha,DistMetric) %>% 
#   summarise_all(.funs = mean) %>% mutate(summary_metric='Mean') %>% ungroup() %>% 
#   pivot_longer(cols = -c(ncluster,method,alpha,DistMetric,summary_metric),names_to = 'Metric'),
#   
#   df_evaluation %>% 
#   group_by(ncluster,method,alpha,DistMetric) %>% 
#   summarise_all(.funs = sd) %>% mutate(summary_metric='SD') %>% ungroup() %>% 
#   pivot_longer(cols = -c(ncluster,method,alpha,DistMetric,summary_metric),names_to = 'Metric')
# )
# 
# df_summary %>% filter(Metric=='within_sum') %>% select(-Metric) %>% 
#   pivot_wider(id_cols = ncluster:DistMetric,names_from = summary_metric ) %>% 
#   ggplot(aes(x=ncluster,y=Mean,color=method,fill=method,linetype=as.factor(alpha)))+
#   geom_line()+
#   geom_ribbon(aes(ymin=Mean-SD,ymax=Mean+SD),alpha=0.25)+
#   facet_grid(DistMetric~method,scales = 'free')+
#   theme_minimal()+
#   theme(legend.position = 'bottom')
#   
# 
# df_summary %>% filter(Metric=='avg_within_between_dist2medoid') %>% select(-Metric) %>% 
#   pivot_wider(id_cols = ncluster:DistMetric,names_from = summary_metric ) %>% 
#   ggplot(aes(x=ncluster,y=Mean,color=method,fill=method,linetype=as.factor(alpha)))+
#   geom_line()+
#   geom_ribbon(aes(ymin=Mean-SD,ymax=Mean+SD),alpha=0.1)+
#   facet_wrap(~DistMetric,scales = 'free')+
#   theme_minimal()+
#   theme(legend.position = 'bottom')
# 
# 
# df_summary %>% filter(Metric=='avg_dist_between_medoids') %>% select(-Metric) %>% 
#   pivot_wider(id_cols = ncluster:DistMetric,names_from = summary_metric ) %>% 
#   ggplot(aes(x=ncluster,y=Mean,color=method,fill=method,linetype=as.factor(alpha)))+
#   geom_line()+
#   geom_ribbon(aes(ymin=Mean-SD,ymax=Mean+SD),alpha=0.25)+
#   facet_wrap(~DistMetric,scales = 'free')+
#   theme_minimal()+
#   theme(legend.position = 'bottom')
# 
# 
# df_summary %>% filter(Metric=='avg_dist_within_dist2medoid') %>% select(-Metric) %>% 
#   pivot_wider(id_cols = ncluster:DistMetric,names_from = summary_metric ) %>% 
#   ggplot(aes(x=ncluster,y=Mean,color=method,fill=method,linetype=as.factor(alpha)))+
#   geom_line()+
#   geom_ribbon(aes(ymin=Mean-SD,ymax=Mean+SD),alpha=0.25)+
#   facet_wrap(~DistMetric,scales = 'free')+
#   theme_minimal()+
#   theme(legend.position = 'bottom')
