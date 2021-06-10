# Raster Preparation for KDE Partial ROC Evaluation  ----------------------

# Script 4 of 5 authored by Steven N. Winter

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



setwd("../Presence_only_GeoPartitioning_Eval/KDE_Maps")


#Collect continuous rasters
p_rast_list <- list.files(".", pattern = "Standardized_Point_Continuous", recursive = T)
b_rast_list <- list.files(".", pattern = "Standardized_Buffer_Continuous", recursive = T)

# Prepare data frames and directory
dir.create("Results")

# Begin evaluation of masked quadrant rasters with testing data
for (i in 1:6) {
  point_raster <- raster(p_rast_list[i]) # Select raster
  buffer_raster <- raster(b_rast_list[i])
  
  crs(point_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"  # Set CRS
  crs(buffer_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
  masked_point <- raster::mask(point_raster, shapefiles[[i]]) # Mask to only testing quadrants
  masked_buffer <- raster::mask(buffer_raster, shapefiles[[i]])
  
  file_point <- paste("./Results/Masked_",names(shapefiles[i]),"Point.asc", sep = "")
  file_buffer <- paste("./Results/Masked_",names(shapefiles[i]),"Buffer.asc", sep = "")
  
  writeRaster(masked_point, filename = file_point)
  writeRaster(masked_buffer, filename = file_buffer)
}

## Note: Evaluation was performed using the shiny app version of ntbox found at: http://shiny.conabio.gob.mx:3838/nichetoolb2/  (Osorio-Olivera et al. 2018)
## Each masked raster written was evaluated using the evaluation data generated in "Hypervolume_Creation.R"