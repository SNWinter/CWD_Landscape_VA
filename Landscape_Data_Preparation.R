# Landscape Data Preparation ----------------------------------------------
# Script 1 of 7 authored by Steven N. Winter

# Install the following packages if not previously installed
library("raster")
library("grid")
library("ggplot2")
library("rgdal")
library(tidyverse)
library(MODIStsp)

setwd("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data")
# dat<- read.csv("Master_CWD_Data.csv", header=T) # Commented out for data not publicly available
# data<- subset(dat, dat$status>0)
# positives <- data
# positives_reduced<- positives%>%
#   dplyr::select(long, lat)
# 
# data<- data%>%
#   dplyr::select(long, lat)

# Study area extent delineation from dispersal distance -------------------
# colnames(data) <- c('x','y')
# coordinates(data) <- ~x+y
# proj4string(data)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#Construct buffers of 45 kilometers around positive cases
# cwd_buffers<- raster::buffer(data, width= 45000, dissolve=T)


#For sharing dummy data, we constructed random points to preserve anonymity of CWD harvest locations
pos <- spsample(cwd_buffers, n = 20, "random")
neg <- spsample(cwd_buffers, n = 100, "random")

pos <- as.data.frame(pos)
pos$status <- 1
neg <- as.data.frame(neg)
neg$status <- 0

pseudo_data <- rbind(pos, neg)

#CSV of random points
write.csv(pseudo_data, file = "CWD_Pseudo_Data_Random_Points.csv")



# Collect EVI Data from MODIStsp (Busetto et al. 2016) --------------------
MODIStsp::MODIStsp()

#Enter EVI data
setwd("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI")
filesevi<- list.files(path = getwd(), pattern = ".tif$", full.names = F)

#Create new directory to avoid re-writing rasters at different extent
dir.create("Extent_2020")


# Crop EVI rasters to study area extent -----------------------------------

for (i in 1:length(filesevi)) {
  ras<- raster(filesevi[[i]])
  croppedevi<- crop(ras, cwd_buffers)
  filename <- gsub("\\..*","", basename(filesevi[i])) #Collect basename of raster
  output_filename<- paste('./Extent_2020/', filename, '_Cropped', sep ='')
  writeRaster(croppedevi, filename = output_filename, format = 'GTiff', overwrite= T)
}


# Create 32-day Mean rasters for cloud removal ----------------------------
setwd("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020")
dir.create("Mean_Rasters_32_Day")
evi_crop_files<- list.files(path = getwd(), pattern = ".tif", full.names = F)

#Pairing structure
a<- seq(from=1, to=344, by=2) # There are even numbered rasters 
b<- seq(from=2, to=344, by=2)
evi_crop_files_2<- evi_crop_files[-345] #Remove odd number 
evi_file_odd<- evi_crop_files_2[a]
evi_file_even<- evi_crop_files_2[b]


#Create mean raster
for (i in 1:172) {
  a<- evi_file_odd[i]
  a<- raster(a)
  b<- evi_file_even[i]
  b<- raster(b)
  c<- stack(a,b)
  mean_c<- calc(c, fun=mean, na.rm=T)
  filename <- gsub("\\..*","",basename(evi_file_even)) # Collect just the basename
  output_filename<- paste('./Mean_Rasters_32_Day/', filename, '_32_mean', sep ='')
  writeRaster(mean_c, filename = output_filename, format = 'GTiff', overwrite= T)
}

# PCA script adapted from NicheA (Qiao et al. 2015, Ecography) -----------------------

target<-"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/NicheA_PCA"
write.table(c(1), file=paste(target, "process.txt", sep="/"), row.names=F, col.names=F)
is_transfer<-F

group_origin<-c("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2005_001_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2005_033_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2005_065_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2005_097_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2005_129_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2005_161_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2005_193_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2005_225_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2005_257_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2005_289_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2005_321_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2005_353_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2006_017_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2006_049_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2006_081_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2006_113_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2006_145_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2006_177_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2006_209_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2006_241_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2006_273_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2006_305_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2006_337_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2007_001_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2007_033_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2007_065_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2007_097_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2007_129_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2007_161_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2007_193_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2007_225_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2007_257_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2007_289_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2007_321_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2007_353_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2008_017_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2008_049_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2008_081_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2008_113_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2008_145_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2008_177_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2008_209_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2008_241_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2008_273_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2008_305_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2008_337_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2009_001_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2009_033_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2009_065_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2009_097_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2009_129_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2009_161_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2009_193_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2009_225_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2009_257_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2009_289_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2009_321_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2009_353_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2010_017_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2010_049_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2010_081_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2010_113_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2010_145_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2010_177_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2010_209_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2010_241_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2010_273_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2010_305_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2010_337_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2011_001_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2011_033_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2011_065_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2011_097_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2011_129_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2011_161_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2011_193_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2011_225_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2011_257_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2011_289_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2011_321_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2011_353_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2012_017_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2012_049_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2012_081_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2012_113_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2012_145_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2012_177_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2012_209_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2012_241_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2012_273_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2012_305_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2012_337_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2013_001_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2013_033_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2013_065_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2013_097_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2013_129_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2013_161_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2013_193_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2013_225_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2013_257_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2013_289_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2013_321_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2013_353_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2014_017_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2014_049_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2014_081_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2014_113_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2014_145_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2014_177_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2014_209_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2014_241_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2014_273_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2014_305_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2014_337_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2015_001_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2015_033_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2015_065_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2015_097_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2015_129_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2015_161_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2015_193_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2015_225_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2015_257_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2015_289_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2015_321_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2015_353_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2016_017_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2016_049_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2016_081_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2016_113_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2016_145_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2016_177_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2016_209_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2016_241_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2016_273_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2016_305_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2016_337_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2017_001_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2017_033_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2017_065_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2017_097_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2017_129_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2017_161_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2017_193_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2017_225_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2017_257_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2017_289_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2017_321_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2017_353_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2018_017_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2018_049_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2018_081_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2018_113_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2018_145_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2018_177_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2018_209_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2018_241_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2018_273_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2018_305_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2018_337_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2019_001_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2019_033_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2019_065_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2019_097_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2019_129_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2019_161_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2019_193_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2019_225_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2019_257_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2019_289_32_mean.tif"
                ,"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/Extent_2020/Mean_Rasters_32_Day/MOD13Q1_EVI_2019_321_32_mean.tif"
)
group_trans<-c()

#before you set up the variables below, you must create the folders on your computer ahead.
pca_origin_folder<-"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/NicheA_PCA"
pca_trans_folder<-"//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/Data/EVI/Final_250m/VI_16Days_250m_v6/EVI/NicheA_PCA"

if (!file.exists(pca_origin_folder)){
  dir.create(pca_origin_folder)
}

if (is_transfer){
  if (!file.exists(pca_trans_folder)){
    dir.create(pca_trans_folder)
  }
}
if (is_transfer){
  if (length(group_origin)!=length(group_trans)){
    print("Error! The lengths of original dataset and target dataset are different!")
  }
}
d_o<-data.frame()
d_t<-data.frame()

i=1
for (i in c(1:length(group_origin))){
  write.table(c(1 + i), file=paste(target, "process.txt", sep="/"), row.names=F, col.names=F)
  print(paste("Reading raster file", group_origin[i]))
  r <- values(raster(group_origin[i]))
  if (i==1){
    r_standard <- r
  }
  r <- r[which(!is.na(r_standard))]
  
  if (dim(d_o)[1]==0){
    d_o<-data.frame(ID=c(1:length(r)))
  }
  d_o[,paste("V", i, sep="")] <- r  
  
  if (is_transfer){
    print(paste("Reading raster file", group_trans[i]))
    
    r <- values(raster(group_trans[i]))
    if (i==1){
      r_standard_trans<-r
    }
    r <- r[which(!is.na(r_standard_trans))]
    
    if (dim(d_t)[1]==0){
      d_t<-data.frame(ID=c(1:length(r)))
    }
    d_t[,paste("V", i, sep="")] <- r
  }
} 
write.table(c(30), file=paste(target, "process.txt", sep="/"), row.names=F, col.names=F)

d_t<-d_t[complete.cases(d_t),]
d_o<-d_o[complete.cases(d_o),]

print("calculating PCA")
pca <- prcomp(d_o[, c(2: dim(d_o)[2])],
              center = T,
              scale. = T) 
write.table(c(45), file=paste(target, "process.txt", sep="/"), row.names=F, col.names=F)
PCbiplot <- function(PC, x="PC1", y="PC2") {
  # PC being a prcomp object
  data <- data.frame(PC$x)
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  
  write.table(PC$rotation, 
              file=paste(target,"rotation.csv", sep="/"), 
              row.names=F, sep=",")
  
  
  plot <- ggplot(datapc)
  plot <- plot + coord_equal() + geom_text(data=datapc, 
                                           aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color="red")
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), 
                              alpha=0.75, color="red")
  ggsave(plot, file=paste(target, "biplot.png", sep="/"), unit="mm", width=200, height=200, dpi=100)
  
  vars <- apply(PC$x, 2, var)  
  props_value <- vars / sum(vars)
  props<-data.frame(PC=attributes(props_value)$names, Proportion=props_value)
  
  write.table(props, 
              file=paste(target,"proportion_of_variances.csv", sep="/"), 
              row.names=F, sep=",")
  
  props$PC<-factor(props$PC, levels = paste("PC", seq(from=1,to=length(vars)), sep=""))
  p <- ggplot(props, aes(x=factor(PC), y=Proportion, group=1))
  p <- p + geom_bar(fill="#DD8888", stat="identity")
  p <- p + geom_line(colour="red", size=1.5)
  p <- p + geom_point(colour="red", size=4, shape=21, fill="white")
  p <- p + theme_classic()
  p <- p + theme(axis.text.x = element_text(angle=90))
  p <- p + xlab("Principal Components")
  ggsave(plot = p, filename = paste(target, "proportion.png", sep="/"), 
         unit="mm", width=300, height=200, dpi=300)
}
PCbiplot(pca)

print("Calculate original PCA")
p_d<-predict(pca, 
             newdata=d_o[, c(2: dim(d_o)[2])])

sample_raster<-raster(group_origin[1])
values(sample_raster)<-NA

write.table(c(50), file=paste(target, "process.txt", sep="/"), row.names=F, col.names=F)
for (i in c(1:dim(p_d)[2])){
  write.table(c(50 + i), file=paste(target, "process.txt", sep="/"), row.names=F, col.names=F)
  print(paste("Saving results", i, "/", dim(p_d)[2]))
  values(sample_raster)<-NA
  values(sample_raster)[which(!is.na(r_standard))]<-p_d[,i]
  writeRaster(sample_raster, filename=paste(pca_origin_folder, "/", "PC", i , ".tif", sep=""), 
              format="GTiff", overwrite=T)
}

write.table(c(60), file=paste(target, "process.txt", sep="/"), row.names=F, col.names=F)
if (is_transfer){
  
  print("Transferring to target raster")
  p_d<-predict(pca, 
               newdata=d_t[, c(2: dim(d_t)[2])])
  
  sample_raster<-raster(group_trans[1])
  values(sample_raster)<-NA
  
  for (i in c(1:dim(p_d)[2])){
    write.table(c(60 + i), file=paste(target, "process.txt", sep="/"), row.names=F, col.names=F)
    print(paste("Saving results", i, "/", dim(p_d)[2]))
    values(sample_raster)<-NA
    values(sample_raster)[which(!is.na(r_standard_trans))]<-p_d[,i]
    writeRaster(sample_raster, filename=paste(pca_trans_folder, "/", "PC", i , ".tif", sep=""), 
                format="GTiff", overwrite=T)
  }
}
write.table(c(80), file=paste(target, "process.txt", sep="/"), row.names=F, col.names=F)