# Hypervolume Generation --------------------------------------------------
# Script 2 of 5 authored by Steven N. Winter

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
library(ENMeval)

# Import CWD Points and Organize for Hx -----------------------------------

# setwd("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data")
# dat<- read.csv("Master_CWD_Data.csv", header=T) #read csv from wd

#Pseudo data
dat <- read.csv("CWD_Pseudo_Data_Random_Points.csv")
positives<- subset(dat, dat$status>0)
positives_reduced<- positives%>% dplyr::select(x, y)

negatives<- subset(dat, dat$status<1)
negatives_reduced <- negatives%>% dplyr::select(x,y)

# dat_geo<- subset(dat, dat$lat!="NA")
# 
# dat_geo$merged <- paste(dat_geo$lat, dat_geo$long, sep=",")
# unique_coord <- unique(dat_geo$merged)
# 
# # Reduce to unique coordinates for quadrant organization in ENMeval -------
# changed <- 0
# for(i in 1:length(unique_coord)){
#   # Create subsets of the data using the merged.coord column
#   row <- which(dat_geo$merged == unique_coord[i])
#   coord_subset <- dat_geo[row, ]
#   
#   # Check length, if more than one there is a repeat
#   if(nrow(coord_subset) > 1 && sum(coord_subset$status) >= 1) {
#     for(j in 1:nrow(coord_subset)){
#       if(coord_subset$status[j] == 0)  coord_subset[j, c("lat", "long")] <- NA
#     }
#     changed <- changed + nrow(coord_subset)-sum(coord_subset$status)
#   }
#   # Change value in data frame
#   dat_geo[which(dat_geo$merged == unique_coord[i]), ][ ,c("lat", "long")] <-  coord_subset[ ,c("lat", "long")]
# }
# dat_geo1<- subset(dat_geo, dat_geo$lat!="NA")
# 
# positives<- subset(dat_geo1, dat_geo1$status>0)
# 
# positives_reduced<- positives%>%
#   dplyr::select(long, lat)
# 
# negatives <- subset(dat_geo1, dat_geo1$status<1)
# negatives$merged <- paste(dat_geo1$lat, dat_geo1$long, sep=",")
# negatives <- negatives %>% dplyr::distinct(merged, .keep_all = T)
# negatives_reduced <- negatives%>% dplyr::select(long,lat)
# rownames(negatives_reduced) <- 1:length(negatives_reduced$long)
# neg <- negatives_reduced
# neg$species <- "absence"
# neg <- neg[c(3,1,2)]

# Enter PCA Landscape Data ------------------------------------------------
r1<-raster("./EVI_Data/PC1.tif")
r2<-raster("./EVI_Data/PC2.tif")
# r3<-raster("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/NicheA_PCA/PC3.tif")
# r4<-raster("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/NicheA_PCA/PC4.tif")
# env.layers <- stack(r1,r2,r3,r4)
env.layers <- stack(r1,r2) #Only two being used for faster dummy data analyses
# plot(env.layers) # Double check


# Create results directory
dir.create("./Presence_only_GeoPartitioning_Eval")
setwd("./Presence_only_GeoPartitioning_Eval")


# Quadrant Construction ---------------------------------------------------
# Arrange positives by geographically splitting (negatives are only used to demarkate the quadrants in ENMeval)
blocks <- get.block(occ = positives_reduced, bg = negatives_reduced)
positives_reduced$block <- as.factor(blocks$occs.grp)
# head(positives_reduced) # Confirm df
# ggplot(positives_reduced, aes(x=x,y=y,col=block))+ # Confirm arrangement spatially
#   geom_point()

# Create 4 equal blocks
pos_NW <- positives_reduced[positives_reduced$block==3,] # Ordering of indexing is based on ENMeval arrangement
pos_NE <- positives_reduced[positives_reduced$block==4,]
pos_SW <- positives_reduced[positives_reduced$block==1,]
pos_SE <- positives_reduced[positives_reduced$block==2,]

# Drop block column
pos_NW <- pos_NW[,-3]
pos_NE <- pos_NE[,-3]
pos_SW <- pos_SW[,-3]
pos_SE <- pos_SE[,-3]

# Collect in list for retrieval
quad_list <- list(pos_NE, pos_NW, pos_SW, pos_SE)

#Reduce to 1:1 calibration and evaluation datasets across all combinations of quadrants
x <- seq(1, 4, 1)
combinations <- as.data.frame(combn(x,2))
evaluation_df <- data.frame()
group1 <- combinations[, 1:(length(combinations)/2)]
group2 <- combinations[, (1 + length(combinations)/2):6]

#Develop naming structure
name_list <- c("NE", "NW", "SW", "SE")
y <- seq(1, 4, 1)
name_combinations <- as.data.frame(combn(y,2))
name_group1 <- name_combinations[, 1:(length(name_combinations)/2)]
name_group2 <- name_combinations[, (1 + length(name_combinations)/2):6]


# KDE and SVM Multiscale Hypervolume Creation -----------------------------

#Create map directory for later evaluation
dir.create("KDE_Maps")
dir.create("SVM_Maps")

i=1
k=1
for(i in 1:3){
  # Create pairs of calibration and evaluation quadrants
  ###Calibration
  p1 <- quad_list[group1[1, i]]
  p1 <- as.data.frame(p1)
  p2 <- quad_list[group1[2, i]]
  p2 <- as.data.frame(p2)
  
  # Names for calibration
  n1 <- name_list[name_group1[1, k]]
  n2 <- name_list[name_group1[2, k]]
  
  j = length(group2) - (i - 1)
  l = length(name_group2) - (k - 1)
  
  ###Evaluation
  p3 <- quad_list[group2[1, j]]
  p3 <- as.data.frame(p3)
  p4 <- quad_list[group2[2, j]]
  p4 <- as.data.frame(p4)
  
  # Names for testing
  n3 <- name_list[name_group2[1, l]]
  n4 <- name_list[name_group2[2, l]]
  
  # Convert to calibratipm data tp spatial points for each set
  training.points <- rbind(p1, p2)
  colnames(training.points)<- c('x','y')
  coordinates(training.points)<- ~x + y
  proj4string(training.points)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  test.points <- rbind(p3, p4)
  
  # Write CSV for Evaluation steps
  csv_name <- paste0("./KDE_Maps/Test_Data_",n3,"_&_", n4, "_for_", n1,'_&_',n2, "_Calibration.csv")
  write.csv(test.points, file = csv_name)
  csv_name <- paste0("./SVM_Maps/Test_Data_",n3,"_&_", n4, "_for_", n1,'_&_',n2, "_Calibration.csv")
  write.csv(test.points, file = csv_name)
  
  
  # Collect raster extraction data 
  ###Harvest Location scale
  training.data <- as.data.frame(raster::extract(env.layers, training.points))
  training.data<- na.omit(training.data)
  
  ###Home Range Scale Buffers
  training.buffer <- as.data.frame(raster::extract(env.layers, training.points, buffer=600, fun=mean)) # Buffer size is the radius required for home range size (i.e., 600 m = 1.2 kmsq)
  training.buffer <- na.omit(training.buffer)
  
  # Construct KDE hypervolumes at both scales (very time intensive)
  #Harvest Location scale
  hv_train<- hypervolume::hypervolume_gaussian(training.data,
                                  name='training',
                                  kde.bandwidth = estimate_bandwidth(training.data, method = "cross-validation"),
                                  samples.per.point=ceiling((10^(3 + sqrt(ncol(training.data))))/nrow(training.data)))
  #Home Range scale
  hv_buffer<- hypervolume::hypervolume_gaussian(training.buffer,
                                   name='training.buffer',
                                   kde.bandwidth = estimate_bandwidth(training.buffer, method = "cross-validation"),
                                   samples.per.point=ceiling((10^(3 + sqrt(ncol(training.buffer))))/nrow(training.buffer)))
  
  # Construct SVM hypervolumes at both scales (less time intensive)
  #Harvest Location scale
  svm_hv_train<- hypervolume::hypervolume_svm(training.data, name='training') # Defaults with gamma and nu values of 0.5 and 0.1, respectively.
  
  #Home Range scale
  svm_hv_buffer<- hypervolume::hypervolume_svm(training.buffer, name='training.buffer',)
  
  
  #KDE
  
  # Project binary KDE maps with 0.95 threshold
  hyper_map <- hypervolume::hypervolume_project(hv_train, rasters = env.layers, type = "inclusion", fast.or.accurate="accurate", verbose = T)
  hyper_buffer_map <- hypervolume::hypervolume_project(hv_buffer, rasters = env.layers, type = "inclusion", fast.or.accurate="accurate", verbose = T)
  # Create filenames and write KDE rasters for evaluations
  output_filename<- paste0('./KDE_Maps/Point_Binary_',n1,'_and_',n2, "_Calibration") #Harvest Location
  output_buffer_filename<- paste0('./KDE_Maps/Buffer_Binary_',n1,'_and_',n2, "_Calibration") #Home Range
  writeRaster(hyper_map, filename = output_filename, format='ascii', overwrite=T)
  writeRaster(hyper_buffer_map, filename = output_buffer_filename , format='ascii', overwrite=T)
  
  # Create KDE continuous maps for pROC evaluations
  hyper_cont_map <- hypervolume::hypervolume_project(hv_train, rasters = env.layers, type = "probability", verbose = T)
  hyper_buffer_cont_map <- hypervolume::hypervolume_project(hv_buffer, rasters = env.layers, type = "probability", verbose = T)
  
  #Standardize outputs
  hyper_cont_map <- spatialEco::raster.transformation(hyper_cont_map, trans = "norm", 0, 1) # Transform to values from zero to one
  hyper_buffer_cont_map <- spatialEco::raster.transformation(hyper_buffer_cont_map, trans = "norm", 0, 1)
  
  # Create filenames and write rasters for evaluations
  output_filename<- paste0('./KDE_Maps/Standardized_Point_Continuous_',n1,'_and_',n2, "_Calibration")
  output_buffer_filename<- paste0('./KDE_Maps/Standardized_Buffer_Continuous_',n1,'_and_',n2, "_Calibration")
  writeRaster(hyper_cont_map, filename = output_filename, format='ascii', overwrite=T)
  writeRaster(hyper_buffer_cont_map, filename = output_buffer_filename , format='ascii', overwrite=T)
  
  
  #SVM
  
  # Project binary SVM maps with 0.95 threshold
  svm_hyper_map <- hypervolume::hypervolume_project(svm_hv_train, rasters = env.layers, type = "inclusion", fast.or.accurate="accurate", verbose = T)
  svm_hyper_buffer_map <- hypervolume::hypervolume_project(svm_hv_buffer, rasters = env.layers, type = "inclusion", fast.or.accurate="accurate", verbose = T)
  
  # Create filenames and write SVM rasters for evaluations
  output_filename<- paste0('./SVM_Maps/Point_SVM_',n1,'_and_',n2, "_Calibration")
  output_buffer_filename<- paste0('./SVM_Maps/Buffer_SVM_',n1,'_and_',n2, "_Calibration")
  writeRaster(svm_hyper_map, filename = output_filename, format='ascii', overwrite=T)
  writeRaster(svm_hyper_buffer_map, filename = output_buffer_filename , format='ascii', overwrite=T)
  
  i=i+1
  k=k+1
}

# Re-order groupings for the second half of paired arrangements
group1_old <- group1
group1 <- group2
group2 <- group1_old
name_group1_old <- name_group1
name_group1 <- name_group2
name_group2 <- name_group1_old

# Same as above
i = 1
k=1
for(i in 1:3){
  # Create pairs of calibration and evaluation quadrants
  ###Calibration
  p1 <- quad_list[group1[1, i]]
  p1 <- as.data.frame(p1)
  p2 <- quad_list[group1[2, i]]
  p2 <- as.data.frame(p2)
  
  # Names for calibration
  n1 <- name_list[name_group1[1, k]]
  n2 <- name_list[name_group1[2, k]]
  
  j = length(group2) - (i - 1)
  l = length(name_group2) - (k - 1)
  
  ###Evaluation
  p3 <- quad_list[group2[1, j]]
  p3 <- as.data.frame(p3)
  p4 <- quad_list[group2[2, j]]
  p4 <- as.data.frame(p4)
  
  # Names for testing
  n3 <- name_list[name_group2[1, l]]
  n4 <- name_list[name_group2[2, l]]
  
  # Convert to calibratipm data tp spatial points for each set
  training.points <- rbind(p1, p2)
  colnames(training.points)<- c('x','y')
  coordinates(training.points)<- ~x + y
  proj4string(training.points)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  test.points <- rbind(p3, p4)
  
  # Write CSV for Evaluation steps
  csv_name <- paste0("./KDE_Maps/Test_Data_",n3,"_&_", n4, "_for_", n1,'_&_',n2, "_Calibration.csv")
  write.csv(test.points, file = csv_name)
  csv_name <- paste0("./SVM_Maps/Test_Data_",n3,"_&_", n4, "_for_", n1,'_&_',n2, "_Calibration.csv")
  write.csv(test.points, file = csv_name)
  
  
  # Collect raster extraction data 
  ###Harvest Location scale
  training.data <- as.data.frame(raster::extract(env.layers, training.points))
  training.data<- na.omit(training.data)
  
  ###Home Range Scale Buffers
  training.buffer <- as.data.frame(raster::extract(env.layers, training.points, buffer=600, fun=mean)) # Buffer size is the radius required for home range size (i.e., 600 m = 1.2 kmsq)
  training.buffer <- na.omit(training.buffer)
  
  # Construct KDE hypervolumes at both scales (very time intensive)
  #Harvest Location scale
  hv_train<- hypervolume::hypervolume_gaussian(training.data,
                                  name='training',
                                  kde.bandwidth = estimate_bandwidth(training.data, method = "cross-validation"),
                                  samples.per.point=ceiling((10^(3 + sqrt(ncol(training.data))))/nrow(training.data)))
  #Home Range scale
  hv_buffer<- hypervolume::hypervolume_gaussian(training.buffer,
                                   name='training.buffer',
                                   kde.bandwidth = estimate_bandwidth(training.buffer, method = "cross-validation"),
                                   samples.per.point=ceiling((10^(3 + sqrt(ncol(training.buffer))))/nrow(training.buffer)))
  
  # Construct SVM hypervolumes at both scales (less time intensive)
  #Harvest Location scale
  svm_hv_train<- hypervolume::hypervolume_svm(training.data, name='training') # Defaults with gamma and nu values of 0.5 and 0.1, respectively.
  
  #Home Range scale
  svm_hv_buffer<- hypervolume::hypervolume_svm(training.buffer, name='training.buffer',)
  
  
  
  #KDE
  
  # Project binary KDE maps with 0.95 threshold
  hyper_map <- hypervolume::hypervolume_project(hv_train, rasters = env.layers, type = "inclusion", 
                                   fast.or.accurate="accurate", verbose = T)
  hyper_buffer_map <- hypervolume::hypervolume_project(hv_buffer, rasters = env.layers, type = "inclusion", 
                                          fast.or.accurate="accurate", verbose = T)
  # Create filenames and write KDE rasters for evaluations
  output_filename<- paste0('./KDE_Maps/Point_Binary_',n1,'_and_',n2, "_Calibration") #Harvest Location
  output_buffer_filename<- paste0('./KDE_Maps/Buffer_Binary_',n1,'_and_',n2, "_Calibration") #Home Range
  writeRaster(hyper_map, filename = output_filename, format='ascii', overwrite=T)
  writeRaster(hyper_buffer_map, filename = output_buffer_filename , format='ascii', overwrite=T)
  
  # Create KDE continuous maps for pROC evaluations
  hyper_cont_map <- hypervolume::hypervolume_project(hv_train, rasters = env.layers, type = "probability", verbose = T)
  hyper_buffer_cont_map <- hypervolume::hypervolume_project(hv_buffer, rasters = env.layers, type = "probability", verbose = T)
  
  #Standardize outputs
  hyper_cont_map <- spatialEco::raster.transformation(hyper_cont_map, trans = "norm", 0, 1) # Transform to values from zero to one
  hyper_buffer_cont_map <- spatialEco::raster.transformation(hyper_buffer_cont_map, trans = "norm", 0, 1)
  
  # Create filenames and write rasters for evaluations
  output_filename<- paste0('./KDE_Maps/Standardized_Point_Continuous_',n1,'_and_',n2, "_Calibration")
  output_buffer_filename<- paste0('./KDE_Maps/Standardized_Buffer_Continuous_',n1,'_and_',n2, "_Calibration")
  writeRaster(hyper_cont_map, filename = output_filename, format='ascii', overwrite=T)
  writeRaster(hyper_buffer_cont_map, filename = output_buffer_filename , format='ascii', overwrite=T)
  
  
  #SVM
  
  # Project binary SVM maps with 0.95 threshold
  svm_hyper_map <- hypervolume::hypervolume_project(svm_hv_train, rasters = env.layers, type = "inclusion", 
                                       fast.or.accurate="accurate", verbose = T)
  svm_hyper_buffer_map <- hypervolume::hypervolume_project(svm_hv_buffer, rasters = env.layers, type = "inclusion", 
                                              fast.or.accurate="accurate", verbose = T)
  
  # Create filenames and write SVM rasters for evaluations
  output_filename<- paste0('./SVM_Maps/Point_SVM_',n1,'_and_',n2, "_Calibration")
  output_buffer_filename<- paste0('./SVM_Maps/Buffer_SVM_',n1,'_and_',n2, "_Calibration")
  writeRaster(svm_hyper_map, filename = output_filename, format='ascii', overwrite=T)
  writeRaster(svm_hyper_buffer_map, filename = output_buffer_filename , format='ascii', overwrite=T)
  
  i=i+1
  k=k+1
}
