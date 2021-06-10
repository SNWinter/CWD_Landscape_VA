# Hypervolume Binary Evaluations ------------------------------------------
# Script 3 of 5 authored by Steven N. Winter

#Note: Quadrant masking depends on creation of shapefiles covering the extent of the quadrant.
      #Due to the dummy nature of the data in this sample code, quadrant shapefiles listed were created from real CWD data and thus not provided.
      #Code is provided for reference purposes.

# Install the following packages if not previously installed
library(raster)
library(ggplot2)
library(rgdal)

# Load in quadrants shapefiles for later masking
# Load shapefiles of quadrants
NE <- readOGR("../Block_Shapefile", layer = "Northeast_Quadrant")
NW <- readOGR("../Block_Shapefile", layer = "Northwest_Quadrant")
SW <- readOGR("../Block_Shapefile", layer = "Southwest_Quadrant")
SE <- readOGR("../Block_Shapefile", layer = "Southeast_Quadrant")

# NE <- readOGR("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Presence_only_GeoPartitioning_Eval/Block_Shapefiles", layer = "Northeast_Quadrant")
# NW <- readOGR("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Presence_only_GeoPartitioning_Eval/Block_Shapefiles", layer = "Northwest_Quadrant")
# SW <- readOGR("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Presence_only_GeoPartitioning_Eval/Block_Shapefiles", layer = "Southwest_Quadrant")
# SE <- readOGR("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Presence_only_GeoPartitioning_Eval/Block_Shapefiles", layer = "Southeast_Quadrant")

# Pair quadrants
Calibrated.with.NE_NW <- bind(SW, SE)
Calibrated.with.SE_SW <- bind(NE, NW)
Calibrated.with.NE_SE <- bind(SW, NW)
Calibrated.with.NW_SW <- bind(NE, SE)
Calibrated.with.NE_SW <- bind(SE, NW)
Calibrated.with.NW_SE <- bind(SW, NE)

shapefiles <- list(Calibrated.with.NE_NW, Calibrated.with.NE_SE, Calibrated.with.NE_SW, 
                   Calibrated.with.NW_SE, Calibrated.with.NW_SW, Calibrated.with.SE_SW)

names(shapefiles) <- c("Calibrated_with_NE_NW", "Calibrated_with_NE_SE", "Calibrated_with_NE_SW", 
                       "Calibrated_with_NW_SE", "Calibrated_with_NW_SW", "Calibrated_with_SE_SW")

# KDE Evaluation ----------------------------------------------------------

setwd("../Presence_only_GeoPartitioning_Eval/KDE_Maps")

#Collect rasters
p_rast_list <- list.files(".", pattern = "Point_Binary", recursive = T)
b_rast_list <- list.files(".", pattern = "Buffer_Binary", recursive = T)

#Prepare testing data and results storage from evalution
test_list <- list.files(".", pattern = "^Test_Data", recursive = T)

dir.create("Results")
kde_evaluation_df <- data.frame()

# Begin evaluation of masked quadrant rasters with testing data
for (i in 1:6) {
  point_raster <- raster(p_rast_list[i]) # Select raster
  buffer_raster <- raster(b_rast_list[i])
  
  crs(point_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"  # Set CRS
  crs(buffer_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
  masked_point <- raster::mask(point_raster, shapefiles[[i]]) # Mask to only testing quadrants
  masked_buffer <- raster::mask(buffer_raster, shapefiles[[i]])
  # plot(masked_buffer)
  
  test_points <- read.csv(test_list[i]) # Read in testing data
  test_points <- test_points[,2:3] # Select coordinates only
  colnames(test_points)<- c('x','y')
  coordinates(test_points)<- ~x + y
  proj4string(test_points)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # Convert to spatial points
  
  # Evaluate using cumulative binomial probability 
  
  # Harvest locations 
  validation_data <- raster::extract(masked_point, test_points) 
  successes <- sum(validation_data)
  number.trials <- length(validation_data)
  prop.area   <- summary(as.numeric(masked_point@data@values))
  prop.area <- prop.area[[4]]
  p.value <- 1- pbinom(successes, number.trials, prop.area)
  # print(p.value)
  
  # Home ranges
  validation_data_buffer <- raster::extract(masked_buffer, test_points)
  successes.buf <- sum(validation_data_buffer)
  number.trials.buf <- length(validation_data_buffer)
  prop.area.buf   <- summary(as.numeric(masked_buffer@data@values))
  prop.area.buf <- prop.area.buf[[4]]
  p.value.buff <- 1- pbinom(q=successes.buf, size = number.trials.buf, prob= prop.area.buf)
  # print(p.value.buff)
  
  #Bind and populate DF
  df.point <- data.frame(successes, number.trials, prop.area, p.value)
  df.buffer <- data.frame(successes.buf, number.trials.buf, prop.area.buf, p.value.buff)
  colnames(df.point) <- c("successes", "trials", "proportion.area", "p.value")
  colnames(df.buffer) <- c("successes", "trials", "proportion.area", "p.value")
  df.pvalues <- rbind(df.point, df.buffer)
  rownames(df.pvalues) <- c("point", "buffer")
  
  kde_evaluation_df <- rbind(kde_evaluation_df, df.pvalues)

}
# Save results
write.csv(kde_evaluation_df, file = "./Results/Cumulative_Binom_Stats_Pres_Only.csv")



# SVM Evaluation ----------------------------------------------------------

setwd("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Presence_only_GeoPartitioning_Eval/SVM_Maps")
svm_evaluation_df <- data.frame()

#Collect rasters
p_rast_list <- list.files(".", pattern = "Point_Binary", recursive = T)
b_rast_list <- list.files(".", pattern = "Buffer_Binary", recursive = T)


#Prepare testing data and results storage from evalution
test_list <- list.files(".", pattern = "^Test_Data", recursive = T)
test_list <- rev(test_list)
dir.create("Results")


# Begin evaluation of masked quadrant rasters with testing data
for (i in 1:6) {
  point_raster <- raster(p_rast_list[i]) # Select raster
  buffer_raster <- raster(b_rast_list[i])
  
  crs(point_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"  # Set CRS
  crs(buffer_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
  masked_point <- raster::mask(point_raster, shapefiles[[i]]) # Mask to only testing quadrants
  masked_buffer <- raster::mask(buffer_raster, shapefiles[[i]])
  # plot(masked_buffer)
  
  test_points <- read.csv(test_list[i]) # Read in testing data
  test_points <- test_points[,2:3] # Select coordinates only
  colnames(test_points)<- c('x','y')
  coordinates(test_points)<- ~x + y
  proj4string(test_points)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # Convert to spatial points
  
  # Evaluate using cumulative binomial probability
  
  # Harvest locations
  validation_data <- raster::extract(masked_point, test_points)
  successes <- sum(validation_data)
  number.trials <- length(validation_data)
  prop.area   <- summary(as.numeric(masked_point@data@values))
  prop.area <- prop.area[[4]]
  p.value <- 1- pbinom(successes, number.trials, prop.area)
  # print(p.value)
  
  # Home ranges
  validation_data_buffer <- raster::extract(masked_buffer, test_points)
  successes.buf <- sum(validation_data_buffer)
  number.trials.buf <- length(validation_data_buffer)
  prop.area.buf   <- summary(as.numeric(masked_buffer@data@values))
  prop.area.buf <- prop.area.buf[[4]]
  p.value.buff <- 1- pbinom(q=successes.buf, size = number.trials.buf, prob= prop.area.buf)
  # print(p.value.buff)
  
  #Bind and populate DF
  df.point <- data.frame(successes, number.trials, prop.area, p.value)
  df.buffer <- data.frame(successes.buf, number.trials.buf, prop.area.buf, p.value.buff)
  colnames(df.point) <- c("successes", "trials", "proportion.area", "p.value")
  colnames(df.buffer) <- c("successes", "trials", "proportion.area", "p.value")
  df.pvalues <- rbind(df.point, df.buffer)
  rownames(df.pvalues) <- c("point", "buffer")
  
  svm_evaluation_df <- rbind(svm_evaluation_df, df.pvalues)
  
}
# Save results
write.csv(SVM_evaluation_df, file = "./Results/Cumulative_Binom_Stats_Pres_Only.csv")

# Combine and save all cumulative binomial stats for all scales and algorithms
total_bin_stats <- rbind(KDE_evaluation_df, SVM_evaluation_df)
write.csv(total_bin_stats, "//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Presence_only_GeoPartitioning_Eval/Cumulative_Binomial_Stats_Total.csv")