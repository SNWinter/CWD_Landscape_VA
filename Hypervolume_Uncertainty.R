
# Hypervolume Uncertainty -------------------------------------------------
# Script 5 of 5 authored by Steven N. Winter

# Install the following packages if not previously installed
library(sp)
library(raster)
library(rgdal)
library(tidyverse)
library(tidyr)
library(dplyr)
library(magrittr)
library(Hmisc)
library(alphahull)
library(hypervolume)
library(doParallel)

# Import CWD Points and Organize for Hx -----------------------------------
setwd("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data")
# dat<- read.csv("Master_CWD_Data.csv", header=T) #read csv from wd
dat <- read.csv("CWD_Pseudo_Data_Random_Points.csv") # Psuedo points

dir.create("Presence_Only_Variability")
setwd("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Presence_Only_Variability")
# positives<- subset(dat, dat$status>0)
# positives_reduced<- positives%>%
#   dplyr::select(long, lat)

positives<- subset(dat, dat$status>0)
positives_reduced<- positives%>% dplyr::select(x, y)

# Enter EVI PCA environmental data ----------------------------------------
r1<-raster("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/NicheA_PCA/PC1.tif")
r2<-raster("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/NicheA_PCA/PC2.tif")
# r3<-raster("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/NicheA_PCA/PC3.tif")
# r4<-raster("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/NicheA_PCA/PC4.tif")
# env.layers <- stack(r1,r2,r3,r4)
env.layers <- stack(r1,r2)
#plot(env.layers)

# Create "Master"  Hypervolume at Both Scales -----------------------------
colnames(positives_reduced)<- c('x','y')
coordinates(positives_reduced)<- ~x + y
proj4string(positives_reduced)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Harvest Locations
positive_data <- as.data.frame(raster::extract(env.layers, positives_reduced))
positive_data<- na.omit(positive_data)
# Home Ranges
positive.buffer <- as.data.frame(raster::extract(env.layers, positives_reduced, 
                                                 buffer=600, fun=mean)) # Buffer size is the radius required for home range size (i.e., 600 m = 1.2 kmsq)
positive.buffer <- na.omit(positive.buffer) 

# Construct Master hypervolumes
hv_pos_points<- hypervolume_gaussian(positive_data, 
                                     name='Pos_Full_HarvLoc_Gaus', 
                                     kde.bandwidth = estimate_bandwidth(positive_data, 
                                                                        method = "cross-validation"), 
                                     samples.per.point=ceiling((10^(3 + sqrt(ncol(positive_data))))/nrow(positive_data)))

hv_pos_buffer<- hypervolume_gaussian(positive.buffer, 
                                     name='Pos_Full_HomRang_Gaus', 
                                     kde.bandwidth = estimate_bandwidth(positive.buffer, 
                                                                        method = "cross-validation"), 
                                     samples.per.point=ceiling((10^(3 + sqrt(ncol(positive.buffer))))/nrow(positive.buffer)))



# Create and compare iteratively removed CWD case in parallel -------------
# Create storage data frames and lists
hv_list <- list()
buf_list <- list()
volumes_df<- data.frame()
volumes_buf_df <- data.frame()

doParallel::registerDoParallel(cores = 5) # Set the number of available cores specific to computer used; mine allows 5

comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

# Revert back to DF for iterative removal
positives_reduced<- positives%>%
  dplyr::select(long, lat)

#Begin foreach loop that creates hypervolumes with n-1 cases, then calculate their volumes
volumes_results <- foreach(i = 1:length(positives$x), 
                           .combine = 'comb', 
                           .multicombine =TRUE, 
                           .init = list(data.frame(), data.frame(), list(), list(), list(), list()),
                           .packages = c("hypervolume", "raster")) %dopar% {
                             
                             df_minus <- positives_reduced[-i,] # The 1 case is removed here
                             colnames(df_minus)<- c('x','y') # Converted to spatial point
                             coordinates(df_minus)<- ~x + y
                             proj4string(df_minus)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
                             
                             #  Build hypervolumes like before in both scales
                             #  Harvest Location Data
                             data_subpositives<-raster::extract(env.layers, df_minus)
                             data_subpositives<- na.omit(data_subpositives)
                             #  Home Range Data
                             buffer_data_sub <- as.data.frame(raster::extract(env.layers, df_minus,
                                                                              buffer=600, fun=mean))
                             buffer_data_sub <- na.omit(buffer_data_sub) 
                             
                             
                             hv_obj<- hypervolume_gaussian(data_subpositives, 
                                                           name='Pos_Minus_HarvLoc_Gaus', 
                                                           kde.bandwidth = estimate_bandwidth(data_subpositives,
                                                                                              method = "cross-validation"), #Used to reduce predicitive error
                                                           samples.per.point=ceiling((10^(3 + sqrt(ncol(data_subpositives))))/nrow(data_subpositives)))
                             
                             hv_buffer <- hypervolume_gaussian(buffer_data_sub, 
                                                               name='Pos_Minus_HomRang_Gaus', 
                                                               kde.bandwidth = estimate_bandwidth(buffer_data_sub, 
                                                                                                  method = "cross-validation"), 
                                                               samples.per.point=ceiling((10^(3 + sqrt(ncol(buffer_data_sub))))/nrow(buffer_data_sub)))
                            
                              # Collect volume from n-1 hypervolume
                              #Harvest location volumes
                              temp_pt_volume <- data.frame(
                               minus= paste('Point',i, sep = '_'),
                               volume= get_volume(hv_obj))
                              
                              # Home range volumes
                              temp_volume <- data.frame(
                               minus= paste('Buffer',i, sep = '_'),
                               volume= get_volume(hv_buffer))
                             
                              
                              # Write Rasters
                              hv_obj_map<- hypervolume_project(hv_obj, rasters = env.layers, type = "probability")
                              hv_buffer_map <- hypervolume_project(hv_buffer, rasters = env.layers, type = "probability")
                              
                              
                            list(temp_pt_volume, temp_volume, hv_obj, hv_buffer, hv_obj_map, hv_buffer_map)
                             
                           }

# Unlist data and convert to data frame
volumes_df <- data.frame(matrix(unlist(volumes_results[[1]]), nrow=length(volumes_results[[1]]), byrow = TRUE))
volumes_buf_df <- data.frame(matrix(unlist(volumes_results[[2]]), nrow=length(volumes_results[[2]]), byrow = TRUE))

# Isolate the list of generated hypervolumes for later
hv_list <- volumes_results[[3]]
buf_list <- volumes_results[[4]]

# Write rasters and calculate average 
map_point_list <- volumes_results[[5]]
map_buffer_list <- volumes_results[[6]]
dir.create("Hypervolume_KDE_Projections_Point")

for (i in 1:length(map_point_list)) {
  hv_obj_map <- map_point_list[[i]]
  hv_buffer_map <- map_buffer_list[[i]]
  output_filename<- paste0('./Hypervolume_KDE_Projections_Point/','Without_',i,'_map')
  binary_output_filename<- paste0('./Hypervolume_KDE_Projections_Buffer_2020_cases/','Without_',i,'_map')
  writeRaster(hv_obj_map, filename = output_filename, format='GTiff', overwrite=T)
  writeRaster(hv_buffer_map, filename = binary_output_filename, format='GTiff', overwrite=T)
}

point_stack <- raster::stack(map_point_list)
buffer_stack <- raster::stack(map_buffer_list)
average_raster_point <- raster::calc(point_stack, fun = mean, na.rm = TRUE)
average_raster_buffer <- raster::calc(buffer_stack, fun = mean, na.rm = TRUE)
writeRaster(average_raster_point, filename = "Continuous_KDE_2020_Point_cases_Mean_Output.tif")
writeRaster(average_raster_buffer, filename = "Continuous_KDE_2020_buffer_cases_Mean_Output.tif")

# Write Volume CSVs -------------------------------------------------------
write.csv(volumes_df, "Point_Variation_Volumes.csv")
write.csv(volumes_buf_df, "Buffer_Variation_Volumes.csv")


# Examine Overlaps --------------------------------------------------------

# Create storage DFs
overlap_pt_df <- data.frame()
overlap_buf_df <- data.frame()
i=1 #Reset counter

dir.create('./Compare_Presence_N_vs_N-1')
setwd("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Presence_Only_Variability/Compare_Presence_N_vs_N-1")

#Overlap statistics at Harvest Location scale
overlap_pt_df <- foreach(i = 1:length(hv_list), .combine = 'rbind', .packages = "hypervolume") %dopar% {
  hv_full <- hv_pos_points # "Master"
  hv_nminus <- hv_list[[i]] # "n-1"
  hv_join = hypervolume_set(hv_full, hv_nminus, check.memory=FALSE)
  overlap_name<- data.frame(hv_pair= paste('Positive_','&','_N_minus_Point_',i, sep = ""))
  overlap_stats<- as.data.frame(t(hypervolume_overlap_statistics(hv_join)))
  overlap_temp<- cbind(overlap_name, overlap_stats)
  overlap_temp
}

#Overlap statistics at Home Range scale
overlap_buf_df <- foreach(i = 1:length(buf_list), .combine = 'rbind', .packages = "hypervolume") %dopar% {
  hv_full<- hv_pos_buffer
  hv_nminus<- buf_list[[i]] 
  hv_join_buf = hypervolume_set(hv_full, hv_nminus, check.memory=FALSE)
  overlap_name<- data.frame(hv_pair= paste('Positive_','&','_N_minus_Buffer_',i, sep = ""))
  overlap_stats<- as.data.frame(t(hypervolume_overlap_statistics(hv_join_buf)))
  overlap_buf_temp<- cbind(overlap_name, overlap_stats)
  overlap_buf_temp
}

# Save Overlap CSVs ------------------------------------------------------
write.csv(overlap_pt_df, file = "Overlap_Point_Pres_Variation.csv")
write.csv(overlap_buf_df, file = "Overlap_Buffer_Pres_Variation.csv")
