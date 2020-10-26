
# Circuitscape ------------------------------------------------------------
# Script 6 of 7 authored by Steven N. Winter

# We are grateful to Dr. Kathy Zeller for providing assistance to interface with Circuitscape


# Install the following packages if not previously installed

library(raster)
library(spatialEco)
library(rgdal)
library(tidyverse)

# Circuitscape 4.0 (McRae et al. 2009) must be installed on computer
CS_Program_exe <- 'C:/"Program Files"/Circuitscape/cs_run.exe' # Exact path needed

#Create output directory and sub-directory
dir.create("CS_SDM_Out")
output.directory <- "//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Circuitscape/CS_SDM_Out/"
dir.create("CS_input_files")

#Change working directory for easier retrieval
setwd("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Circuitscape")
dir.create("CS_Files")
setwd("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Circuitscape/CS_Files")


# Load Hypervolume Outputs ------------------------------------------------

#Collect SDM data generated from script entitled "Hypervolume_Uncertainty.R"
#Harvest Locations
point_study_area_map <-  raster("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Presence_Only_Variability/Continuous_KDE_2020_point_cases_Mean_Output.tif")
#Home Ranges
buffer_study_area_map <-  raster("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Presence_Only_Variability/Continuous_KDE_2020_buffer_cases_Mean_Output.tif")

#Transform data to more standardized SDM outputs
point_study_area_map <- spatialEco::raster.transformation(point_study_area_map, trans = "norm",0,1)
buffer_study_area_map <- spatialEco::raster.transformation(buffer_study_area_map, trans = "norm",0,1)



# Resistance Data Preparation ---------------------------------------------


#Create directory for transformed SDM rasters
dir.create("CS_SDM_Files")

# # Negative Linear transformation
neg_lin_buf <- 1 - buffer_study_area_map
neg_lin_pnt <- 1 - point_study_area_map
writeRaster(neg_lin_buf, filename = "./CS_SDM_Files/Negative_Linear_Buffer_SDM.asc")
writeRaster(neg_lin_pnt, filename = "./CS_SDM_Files/Negative_Linear_Point_SDM.asc")

neg_lin_pnt <- raster("../CS_SDM_Files/Negative_Linear_Point_SDM.asc")
neg_lin_buf <- raster("../CS_SDM_Files/Negative_Linear_Buffer_SDM.asc")
neg_lin_pnt <- (neg_lin_pnt*100) +1 # +1 added because resistance rasters cannot posses zeros
neg_lin_buf <- (neg_lin_buf*100) +1
writeRaster(neg_lin_buf, filename = "../CS_SDM_Files/Negative_Linear_Buffer_SDM.asc", overwrite = TRUE)
writeRaster(neg_lin_pnt, filename = "../CS_SDM_Files/Negative_Linear_Point_SDM.asc", overwrite = TRUE)

# # c4 Transformation [following Keeley et al. (2016) and Zeller et al. (2018)]
c <- 4
c4_buffer <- 100-99*((1-exp(-c*buffer_study_area_map))/(1-exp(-c)))
c4_point <- 100-99*((1-exp(-c*point_study_area_map))/(1-exp(-c)))
writeRaster(c4_buffer, filename = "./CS_SDM_Files/c4_Buffer_SDM.asc")
writeRaster(c4_point, filename = "./CS_SDM_Files/c4_Point_SDM.asc")

# # c8 Transformation
c <- 8
c8_buffer <- 100-99*((1-exp(-c*buffer_study_area_map))/(1-exp(-c)))
c8_point <- 100-99*((1-exp(-c*point_study_area_map))/(1-exp(-c)))
writeRaster(c8_buffer, filename = "./CS_SDM_Files/c8_Buffer_SDM.asc")
writeRaster(c8_point, filename = "./CS_SDM_Files/c8_Point_SDM.asc")

# File list holding resistance surfaces
resistance.directory <- "//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Circuitscape/CS_SDM_Files/"
resistance.files <- list.files(resistance.directory, pattern = ".asc")


# Nodal Preparation -------------------------------------------------------

#Convert psuedo data to shapefiles for rasterization
dat <- read.csv("../CWD_Pseudo_Data_Random_Points.csv") # Psuedo points
positives<- subset(dat, dat$status>0)
positives_reduced<- positives%>% dplyr::select(x, y)
colnames(positives_reduced)<- c('x','y') # Convert to points
coordinates(positives_reduced)<- ~x + y
proj4string(positives_reduced)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

value <- 1:length(positives_reduced)
spdf <- SpatialPointsDataFrame(coords = positives_reduced, data = as.data.frame(positives$status), #Convert to spatial points data frame
                                proj4string = CRS(positives_reduced))
spdf@data <- cbind(spdf@data, value)
#Save shapefile
writeOGR(obj = spdf, dsn = "../Data", layer = "Positive_Points_shapefile", driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# Collect cases to be used as nodes
points <- rgdal::readOGR(dsn = "../Data", layer = "Positive_Points_shapefile")
# plot(points)
points_rast <- rasterize(points, point_study_area_map) # call raster as reference
# plot(points_rast$value)
writeRaster(points_rast$counter, "./Data/Rasterized_points.asc")
# writeRaster(points_rast$value, "./Data/Rasterized_points.asc")

#Collect files for nodes
node.directory <- "//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Circuitscape/Data/"
node.files <- list.files(node.directory, pattern = ".asc")




# Run Circuitscape --------------------------------------------------------

# Run loop to call CS for each pairwise node combination (time intensive)
for (i in 1:length(resistance.files)){
  raster <- paste("habitat_file = ", resistance.directory,resistance.files[i], sep="")
  for (j in 1:length(node.files)){
    nodes<-paste("point_file = ",node.directory,node.files[j], sep="")
    output<-paste("output_file = ", output.directory,node.files[j],"_",resistance.files[i],".out", sep="")
    # Make needed ini files
    CS_ini <- c("[circuitscape options]",            
                "data_type = raster",
                "scenario = pairwise",
                "write_cur_maps = 1",
                nodes,
                raster,
                output)
    
    # Write ini lines into working directory
    writeLines(CS_ini,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Circuitscape/CS_input_files/myini.ini")
    
    # Create the Circuitscape run command
    CS_run <- paste(CS_Program_exe, paste("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Circuitscape/CS_input_files","myini.ini",sep="/")) # Make the cmd
    
    # Run the command
    system(CS_run)
    
  }
}
