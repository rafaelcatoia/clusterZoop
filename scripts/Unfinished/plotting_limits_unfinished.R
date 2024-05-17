# "ward_0"    "ward_0.05" "ward_0.1"  "ward_0.2"  "pam_0"     "pam_0.05"  "pam_0.1"   "pam_0.2"


grid_base1 = grid_base %>% 
  mutate(Lat=lat_grid*lat_scale_obj[2] + lat_scale_obj[1]   ,
         Depth = depth_grid*depth_scale_obj[2] + depth_scale_obj[1],
         limits= rowMeans(df_Limits %>% filter(clust_control=="ward_0") %>% select(-clust_control))
  )


grid_base2 = grid_base %>% 
  mutate(Lat=lat_grid*lat_scale_obj[2] + lat_scale_obj[1]   ,
         Depth = depth_grid*depth_scale_obj[2] + depth_scale_obj[1],
         limits= rowMeans(df_Limits %>% filter(clust_control=="ward_0.05") %>% select(-clust_control))
  )

grid_base3 = grid_base %>% 
  mutate(Lat=lat_grid*lat_scale_obj[2] + lat_scale_obj[1]   ,
         Depth = depth_grid*depth_scale_obj[2] + depth_scale_obj[1],
         limits= rowMeans(df_Limits %>% filter(clust_control=="ward_0.1") %>% select(-clust_control))
  )


grid_base4 = grid_base %>% 
  mutate(Lat=lat_grid*lat_scale_obj[2] + lat_scale_obj[1]   ,
         Depth = depth_grid*depth_scale_obj[2] + depth_scale_obj[1],
         limits= rowMeans(df_Limits %>% filter(clust_control=="ward_0.2") %>% select(-clust_control))
  )





plt1 = ggplot()+
  #geom_tile(aes(x=Lat,y=Depth,fill = factor(ClustRegion)),
  #          data = grid_base %>% filter(Depth>=0),alpha=0.5)+
  geom_jitter(data = grid_base1 %>% filter(Depth>=0) %>% filter(limits>0),
            aes(x=Lat,y=Depth,fill=limits,alpha=limits),size=0.1)+
  geom_label(aes(x=Latitude,y=Depth,label=Cluster,color=Cluster),
             data = df_cluster %>% mutate(Cluster=as.factor(Cluster)) %>% 
               left_join(df_geo_abiotics %>% transmute(SampleID,Latitude,Depth)))+
  theme_minimal()+
  scale_y_reverse()+
  theme(legend.position = 'bottom')


plt2=ggplot()+
  #geom_tile(aes(x=Lat,y=Depth,fill = factor(ClustRegion)),
  #          data = grid_base %>% filter(Depth>=0),alpha=0.5)+
  geom_tile(data = grid_base2 %>% filter(Depth>=0) %>% filter(limits>0),
            aes(x=Lat,y=Depth,fill=limits,alpha=limits),size=0.1)+
  geom_label(aes(x=Latitude,y=Depth,label=Cluster,color=Cluster),
             data = df_cluster %>% mutate(Cluster=as.factor(Cluster)) %>% 
               left_join(df_geo_abiotics %>% transmute(SampleID,Latitude,Depth)))+
  theme_minimal()+
  scale_y_reverse()+
  theme(legend.position = 'bottom')

plt3 = ggplot()+
  #geom_tile(aes(x=Lat,y=Depth,fill = factor(ClustRegion)),
  #          data = grid_base %>% filter(Depth>=0),alpha=0.5)+
  geom_tile(data = grid_base3 %>% filter(Depth>=0) %>% filter(limits>0),
            aes(x=Lat,y=Depth,fill=limits,alpha=limits),size=0.1)+
  geom_label(aes(x=Latitude,y=Depth,label=Cluster,color=Cluster),
             data = df_cluster %>% mutate(Cluster=as.factor(Cluster)) %>% 
               left_join(df_geo_abiotics %>% transmute(SampleID,Latitude,Depth)))+
  theme_minimal()+
  scale_y_reverse()+
  theme(legend.position = 'bottom')


plt4 = ggplot()+
  #geom_tile(aes(x=Lat,y=Depth,fill = factor(ClustRegion)),
  #          data = grid_base %>% filter(Depth>=0),alpha=0.5)+
  geom_tile(data = grid_base4 %>% filter(Depth>=0) %>% filter(limits>0),
            aes(x=Lat,y=Depth,fill=limits,alpha=limits),size=0.1)+
  geom_label(aes(x=Latitude,y=Depth,label=Cluster,color=Cluster),
             data = df_cluster %>% mutate(Cluster=as.factor(Cluster)) %>% 
               left_join(df_geo_abiotics %>% transmute(SampleID,Latitude,Depth)))+
  theme_minimal()+
  scale_y_reverse()+
  theme(legend.position = 'bottom')


gridExtra::grid.arrange(plt1,plt2,plt3,plt4,ncol=2)
