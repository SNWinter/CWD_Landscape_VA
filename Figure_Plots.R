# Data visualization and figure generation --------------------------------
# Script 7 of 7 authored by Steven N. Winter


#Note: Study area figures will be unavailable for replication without DWR data availability, code is provided for reference only.
#Disclaimer: Due to the high number of packages and dependencies, replications in visualizations may be difficult.

# Install the following packages if not previously installed
library(ggplot2)
library(tidyverse)
library(patchwork)
library(rgdal)
library(sp)
library(raster)
library(ggmap)
library(sf)
library(RColorBrewer)
library(grid)
library(magick)
library(cowplot)
library(ggthemes)
library(devtools)
library(ggsn)
library(ENMeval)
library(rnaturalearth)
library(rnaturalearthdata)
library(colorspace)
library(gridExtra)


devtools::source_gist("2a1bb0133ff568cbe28d", filename = "geom_flat_violin.R")

# Overlap and volume figure -----------------------------------------------

setwd("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Presence_Only_Variability/Compare_Presence_N_vs_N-1")
overlap_point_data<- read.csv(file = "Overlap_Point_Pres_Variation.csv")
overlap_buffer_data <- read.csv(file="Overlap_Buffer_Pres_Variation.csv")

buffer_vol <- read.csv(file = "../Buffer_Variation_Volumes.csv")
point_vol <- read.csv(file = "../Point_Variation_Volumes.csv")
buffer_vol <- buffer_vol[3]
point_vol <- point_vol[3]
colnames(buffer_vol) <- "volume"
colnames(point_vol) <- "volume"

# Volume Organization

buffer_vol$Model <- c("Presence-Only")
point_vol$Model <- c("Presence-Only")
buffer_vol$Scale <- c("Home Ranges")
point_vol$Scale <- c("Harvest Locations")
master_vol <- rbind(buffer_vol, point_vol)
master_vol$Scale <- factor(master_vol$Scale, levels=c("Harvest Locations", "Home Ranges"))

# Overlap Organization

overlap_point_data$Model <- c("Presence-Only")
overlap_buffer_data$Model <- c("Presence-Only")
overlap_point_data$Scale <- c("Harvest Locations")
overlap_buffer_data$Scale <- c("Home Ranges")
master_overlap <- rbind(overlap_buffer_data, overlap_point_data)
master_overlap$Scale <- factor(master_overlap$Scale, levels=c("Harvest Locations", "Home Ranges"))

# write.csv(master_overlap, "Master_Overlap.csv")
p1 <- ggplot(data = master_overlap, aes(x=Scale , y=jaccard, fill=Scale))+
  geom_flat_violin(scale = "width", trim = F)+
  stat_summary(fun.data = mean_sdl, 
               fun.args = list(mult=1), geom = "pointrange",
               position=position_nudge(0.01))+
  geom_dotplot(binaxis = "y", dotsize = 1, stackdir = "down",
               binwidth = 0.003, position = position_nudge(-0.020))+
  scale_fill_manual(values = c("gray", "deepskyblue3" ))+
  geom_hline(yintercept = 1, linetype="dashed", color="black")+
  # facet_wrap(~Model1)+
  # scale_fill_manual(values=c("gray","white"))+
  ylim(.85,1)+
  theme_linedraw()+
  xlab(NULL)+
  ggtitle("A")+
  ylab("Jaccard")+
  theme(legend.position = "none", axis.text.y = element_text(size = rel(1.4)), axis.title.y = element_text(size = rel(1.5)),
        axis.text.x = element_blank(), strip.text = element_text(size = rel(1.5)),
        axis.ticks.x = element_blank(), panel.grid = element_blank(), 
        plot.title = element_text(size = rel(1.9), face = 'bold' ,vjust=-6, hjust = 0.015))

p3 <- ggplot(data=master_vol, aes(x=Scale, y= volume, fill=Scale))+
  geom_flat_violin(scale = "width", trim = F)+
  stat_summary(fun.data = mean_sdl, 
               fun.args = list(mult=1), geom = "pointrange",
               position=position_nudge(0.01))+
  # geom_jitter(width = 0.2, aes(color=Scale))+
  geom_dotplot(binaxis = "y", dotsize = 4, stackdir = "down",
               binwidth = 90, position = position_nudge(-0.020))+
  scale_fill_manual(values = c("gray", "deepskyblue3" ))+
  # stat_summary(fun.data = mean_sdl, 
  #              fun.args = list(mult=1), geom = "pointrange",
  #              position=position_nudge(0))+
  # facet_wrap(~Model1)+
  ggtitle("B")+
  theme_linedraw()+
  xlab(NULL)+
  ylab("Volume")+
  theme(legend.position = "bottom", #axis.text.y = element_blank(),
        axis.text.x = element_blank(),axis.text.y = element_text(size = rel(1.4)), axis.title.y = element_text(size = rel(1.5)),
        axis.ticks.x = element_blank(), strip.background = element_blank(), strip.text = element_blank(),
        plot.title = element_text(size = rel(1.9), face = 'bold' ,vjust=-6, hjust = 0.015),
        legend.text = element_text(size = rel(1.4), face = 'bold'), legend.title = element_text(size = rel(1.5), face = 'bold'), panel.grid = element_blank())
  
p1/p3


# Basic calculations 
t.test(buffer_vol$volume, point_vol$volume, paired = T)
mean(overlap_buffer_data$jaccard)
t.test(overlap_buffer_data$jaccard , overlap_point_data$jaccard, paired = T)


# Uncertainty Hypervolume Map Figure --------------------------------------
dat <- read.csv("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Master_CWD_Data.csv")
dat_geo<- subset(dat, dat$lat!="NA")

dat_geo$merged <- paste(dat_geo$lat, dat_geo$long, sep=",")
unique_coord <- unique(dat_geo$merged)
changed <- 0
for(i in 1:length(unique_coord)){
  # Create subsets of the data using the merged.coord column
  row <- which(dat_geo$merged == unique_coord[i])
  coord_subset <- dat_geo[row, ]
  
  # Check length, if more than one there is a repeat
  if(nrow(coord_subset) > 1 && sum(coord_subset$status) >= 1) {
    for(j in 1:nrow(coord_subset)){
      if(coord_subset$status[j] == 0)  coord_subset[j, c("lat", "long")] <- NA
    }
    changed <- changed + nrow(coord_subset)-sum(coord_subset$status)
  }
  # Change value in data frame
  dat_geo[which(dat_geo$merged == unique_coord[i]), ][ ,c("lat", "long")] <-  coord_subset[ ,c("lat", "long")]
}
dat_geo1<- subset(dat_geo, dat_geo$lat!="NA")

positives<- subset(dat_geo1, dat_geo1$status>0)
positives_reduced<- positives%>%
  dplyr::select(long, lat)
data2 <- positives_reduced
colnames(data2)[1]<- 'x'
colnames(data2)[2]<- 'y'
coordinates(data2)<- ~x + y
proj4string(data2)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# Load average uncertainty maps
buffer_map <- raster("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Presence_Only_Variability/Continuous_KDE_2020_buffer_cases_Mean_Output.tif")
point_map <- raster("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Presence_Only_Variability/Continuous_KDE_2020_Point_cases_Mean_Output.tif")
buffer_map_new <- spatialEco::raster.transformation(buffer_map, trans = "norm", smin = 0, smax = 1)
point_map_new <- spatialEco::raster.transformation(point_map, trans = "norm", smin = 0, smax = 1)

#US shapefiles
usa_count <- rgdal::readOGR("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/USA_shp", layer = "USA_adm1")
counties <- rgdal::readOGR("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data//USA_shp", layer = "USA_adm2")
count_crop <- crop(counties, buffer_map)
usa_sub <- crop(usa_count, buffer_map)

ramp <- viridis::inferno(n = 100)

tiff(filename = "//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/CWD_Niche_Paper_Figures/Uncertainty_Maps_Revised.tiff",
     res = 300, width = 14, height = 10, units = "cm" )
par(mfrow = c(1,2))
par(mgp=c(2,0.5,0))

#Plot1
par(mar = c(2,1,3,0.8))
plot(point_map_new, col = ramp,# main = "Harvest Locations",
     legend = F, cex.axis = 0.5, cex.main = 0.75)
plot(data2, add = TRUE, pch = 16, col = "white", cex = 0.4)
plot(data2, add = TRUE, pch = 1, col = "black", cex = 0.4, lwd = 0.75)
plot(count_crop, add = TRUE, border = "white", lwd = 0.7)
plot(usa_sub, add = TRUE, border = "white", lwd = 1)
text(x = -79, y = 39.8, label = "A", col = "White", cex = 1)
plot(point_map_new, col = ramp, legend.only = TRUE, legend.width = 1, legend.shrink = 0.6, axis.args= list(cex.axis = 0.5))

#Plot2
par(mar = c(2,1.2,3,0))
plot(buffer_map_new, col = ramp, #main = "Home Ranges", 
     legend = FALSE, cex.axis = 0.5, cex.main = 0.75) # Plot to confirm
plot(data2, add = TRUE, pch = 16, col = "white", cex = 0.4)
plot(data2, add = TRUE, pch = 1, col = "black", cex = 0.4, lwd = 0.75)
plot(count_crop, add = TRUE, border = "white", lwd = 0.7)
plot(usa_sub, add = TRUE, border = "white", lwd = 1)
text(x = -79, y = 39.8, label = "B", col = "White", cex = 1)
dev.off()


# AUC Ratio Figure --------------------------------------------------------
setwd("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data/Presence_only_GeoPartitioning_Eval/Continuous_Rasters")

#Collect AUC Ratio CSVs from shiny ntbox (i.e., not automated in R scripts)
point_auc_files <- list.files(path=".", pattern="pROC_Point.csv", recursive = T)
buffer_auc_files <- list.files(path=".", pattern="pROC_Buffer.csv", recursive = T)

master_aucs <- data.frame()
point_auc <- data.frame()
buffer_auc <- data.frame()

quadrant_arrangement <- c("Calibrated with NE & NW", "Calibrated with NE & SE", "Calibrated with NE & SW", 
                           "Calibrated with NW & SE", "Calibrated with NW & SW", "Calibrated with SE & SW")

#Collect data from csvs
for(i in 1:length(point_auc_files)){

point_data <- read.csv(point_auc_files[i])
buffer_data <- read.csv(buffer_auc_files[i])

point_data$Quadrant <- quadrant_arrangement[i]
buffer_data$Quadrant <- quadrant_arrangement[i]

point_data$Scale <- "Harvest Locations"
buffer_data$Scale <- "Home Ranges"

point_auc <- rbind(point_auc, point_data)
buffer_auc <- rbind(buffer_auc, buffer_data)
}

# Basic calculations
mean(point_auc$AUC_ratio)
mean(buffer_auc$AUC_ratio)
t.test(x=buffer_auc$AUC_ratio, y=point_auc$AUC_ratio)
master_aucs <- rbind(point_auc, buffer_auc)

#Set scale as factor for visualization
master_aucs$Scale <- factor(master_aucs$Scale, levels=c("Harvest Locations", "Home Ranges"))

sub_auc <- subset(master_aucs, master_aucs$Quadrant=="Calibrated with NE & NW")
sub_auc <- subset(sub_auc, sub_auc$Scale=="Harvest Locations")
sub_auc <- subset(sub_auc, sub_auc$AUC_ratio<=1) #How many values are 1 or below?
length(sub_auc$AUC_ratio)/500 # > 5% are at 1 or below

#ggplot figure
ggplot(data = master_aucs, aes(x=Scale,y=AUC_ratio, fill=Scale))+
  geom_flat_violin(scale = "count", trim = F)+
  stat_summary(fun.data = mean_sdl, 
               fun.args = list(mult=1), geom = "pointrange",
               position=position_nudge(0.02))+
  geom_dotplot(binaxis = "y", dotsize = .5, stackdir = "down",
               binwidth = 0.01, position = position_nudge(-0.020))+
  scale_fill_manual(values = c("gray" ,"deepskyblue3"))+
  facet_wrap(~Quadrant)+
  xlab(NULL)+
  ylim(0.75,1.9)+
  geom_hline(yintercept= 1, color="red")+
  ylab("AUC Ratio")+
  # ylim(0,2)+
  theme_linedraw()+
  theme(legend.position = "bottom", legend.text = element_text(size= rel(1.4), face="bold"), legend.title = element_text(size = rel(1.4), face="bold"),
        axis.text.y = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5), face="bold"), axis.ticks.x = element_blank(),
      axis.text.x = element_blank(), axis.title.x = element_text(size = rel(1.4)), strip.text = element_text(size = rel(1.5)), panel.grid = element_blank(),
      strip.background = element_rect(fill="white"), strip.text.x = element_text(color="black"))




# Study area maps and figures ---------------------------------------------

# setwd("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data")
# 
# dat<- read.csv("Master_CWD_Data.csv", header=T) #read csv from wd
# dat_geo<- subset(dat, dat$lat!="NA")
# 
# dat_geo$merged <- paste(dat_geo$lat, dat_geo$long, sep=",")
# unique_coord <- unique(dat_geo$merged)
# 
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
# positives<- subset(dat_geo1, dat_geo1$status>0)
# positives_reduced<- positives%>%
#   dplyr::select(long, lat)
# data2 <- positives_reduced
# colnames(data2)[1]<- 'x'
# colnames(data2)[2]<- 'y'
# coordinates(data2)<- ~x + y
# proj4string(data2)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# buffer <- buffer(data2, width= 45000, dissolve=T)
# # buffer@bbox
# negatives <- subset(dat_geo1, dat_geo1$status<1)
# negatives_reduced <- negatives%>%
#   dplyr::select(long,lat)
# 
# #Arrange positives by geographically splitting
# blocks <- ENMeval::get.block(occ = positives_reduced, bg.coords = negatives_reduced)
# positives_reduced$block <- blocks$occ.grp
# positives_reduced$block <- replace(positives_reduced$block, positives_reduced$block=="1", "SW")
# positives_reduced$block <- replace(positives_reduced$block, positives_reduced$block=="2", "SE")
# positives_reduced$block <- replace(positives_reduced$block, positives_reduced$block=="3", "NW")
# positives_reduced$block <- replace(positives_reduced$block, positives_reduced$block=="4", "NE")
# positives_reduced$block <- as.factor(positives_reduced$block)
# 
# #Sampling effort
# vdgif_totals <- read.csv(file = "VDGIF_Total_samples_2007-2020.csv")
# vdgif_totals$annual <- vdgif_totals$TotalNumb / length(2007:2019)
# vdgif_reduced <- vdgif_totals[c(4,20,21)]
# colnames(vdgif_reduced) <- c("subregion", "total", "Sampling Effort")
# vdgif_reduced$subregion <- as.character(vdgif_reduced$subregion)
# 
# state <- map_data("state")
# county <- map_data("county")
# quad_state <- subset(state, region %in% c("kentucky","ohio","virginia", "maryland", 
#                                           "north carolina","west virginia", "pennsylvania", 
#                                           "tennessee","delaware", "district of columbia"))
# 
# quad_state_count <- subset(county,region %in% c("kentucky","ohio", "maryland", # NOT VIRGINIA
#                                                 "north carolina","west virginia", "pennsylvania", 
#                                                 "tennessee","delaware", "district of columbia"))
# 
# va <- subset(quad_state, region %in% c("virginia"))
# va_count <- subset(county, region %in% c("virginia"))
# va_count2 <- right_join(va_count, vdgif_reduced, by="subregion")
# va_count2 <- subset(va_count2, va_count2$total!=0)
# 
# dma1 <- subset(va_count, subregion %in% c("frederick", "shenandoah", "clarke", "warren"))
# dma2 <- subset(va_count, subregion %in% c("culpeper", "orange", "madison"))
# 
# 
# #Sampling effort figure
# scale_anchor <- c(x= -82.2,y= 36.49)
# studyarea_anchor <- data.frame(x= -82,y= 36.545, x2=-83.8, y2=36.545)
# studyarea <- ggplot(va)+
#   geom_polygon(aes(x= long, y= lat, group= group), fill="white")+
#   geom_polygon(data=va_count2, aes(x = long, y = lat, group = group, fill= `Sampling Effort`), na.rm = F, color="black")+
#   geom_polygon(data=quad_state_count ,aes(x = long, y = lat, group = group), fill="white", color = "lightgray")+
#   geom_polygon(data=quad_state ,aes(x = long, y = lat, group = group), fill=NA, lwd=1.3, color = "black")+
#   geom_polygon(data=dma1,aes(x = long, y = lat, group = group), linetype = "dotted",fill = NA, lwd = 1.2, color = "black")+
#   geom_polygon(data=dma2,aes(x = long, y = lat, group = group),linetype = "dotted",fill = NA, lwd = 1.2, color = "black")+
#   scale_fill_continuous_sequential(palette="YlOrRd")+
#   coord_sf(crs= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")+
#   coord_sf(xlim = c(-83.65, -75.7), ylim = c(36.3,40), expand = F)+
#   geom_rect(aes(xmin = -79.06703, xmax= -77.36469, ymin = 38.01524, ymax = 39.86566),col="dimgray", lwd=1.2, fill=NA)+
#   # geom_segment(aes(x = -79.06703, xend= -81, y = 38.01524, yend = 37.6),col="dimgray", lwd=1)+
#   # geom_segment(aes(x = -79.06703, xend= -80.7, y = 39.86566, yend = 40.41),col="dimgray", lwd=1)+
#   labs(x = "Longitude", y = "Latitude")+
#   ggtitle("A")+
#   theme_bw()+
#   ggsn::scalebar(data=va_count, anchor = scale_anchor, dist = 50,
#                  box.fil=c("white","white"), dist_unit = "km", transform = T, model = "WGS84",
#                  st.bottom = T, st.size = 5, border.size = 0, height = .02)+
#   geom_segment(aes(x=x,y=y,xend=x2,yend=y2), color="white", lwd=1, data=studyarea_anchor)+
#   annotate(geom = "text", x=-78, y=40.3, label="Pennsylvania", color="black", size=5.5)+
#   annotate(geom = "text", x=-76.8, y=39.5, label="Maryland", color="black", size=5.5)+
#   annotate(geom = "text", x=-79.9, y=39, label= "West Virginia", color="black", size=5.5)+
#   annotate(geom = "text", x=-83.3, y=37.3, label= "Kentucky", color="black", size=5.5)+
#   annotate(geom = "text", x=-76.8, y=38.675, label= "DC", color="black", size=5.5)+
#   geom_segment(aes(x=-76.9, y=38.75, xend= -77, yend = 38.9),col="black", lwd=.75)+
#   theme(axis.text.x = element_text(color = "black", size= rel(1.7)),axis.text.y = element_text(color = "black",size= rel(1.9)),
#         axis.title.x = element_text(size= rel(1.7)), axis.title.y = element_text(size= rel(1.9)),
#         panel.background = element_rect(fill = "#E2F7FF"), panel.grid = element_blank(),
#         plot.title = element_text(size = rel(2.2), face="bold", vjust=-6 ,hjust =0.05),
#         plot.margin = margin(t = -.05, r = .01, b = -0.02, l = 0, unit = "null"),
#         legend.background = element_rect(fill = "white", color = "black"), 
#         legend.position = c(0.25,0.6), legend.text = element_text(size = rel(1.6)),
#         legend.title = element_text(size = rel(1.4), face = "bold"), legend.key = element_rect(size = 4))
#   
# studyarea  
# 
# 
# # Demographic Plots 
# library(sp)
# library(raster)
# library(rgdal)
# library(tidyverse)
# library(tidyr)
# library(dplyr)
# library(magrittr)
# library(Hmisc)
# library(alphahull)
# library(hypervolume)
# library(ENMeval)
# 
# setwd("//idstorage.cnre.vt.edu/IDStorage1/Students/Steven_Winter/CWD/Raw_VA_CWD_Data")
# 
# dat<- read.csv("Master_CWD_Data.csv", header=T) #read csv from wd
# dat_geo<- subset(dat, dat$lat!="NA")
# 
# dat_geo$merged <- paste(dat_geo$lat, dat_geo$long, sep=",")
# unique_coord <- unique(dat_geo$merged)
# 
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
# 
# negatives_reduced <- negatives%>% dplyr::select(long,lat)
# rownames(negatives_reduced) <- 1:length(negatives_reduced$long)
# 
# neg <- negatives_reduced
# neg$species <- "absence"
# neg <- neg[c(3,1,2)]
# 
# positives_cleaned <- positives%>%
#   group_by(Year, .keep_all=T)%>%
#   summarise(total=length(Deer.ID))
# 
# positives_dem <- positives%>%
#   group_by(Sex, Year, .keep_all=T)%>%
#   summarise(total=length(Deer.ID))
# 
# 
# # Frederick
# 
# 
# fred <- subset(dat, dat$County =="Frederick")
# 
# prev_Fred <- fred%>%
#   group_by(Sex, Year, status, .keep_all=T)%>%
#   summarise(total=length(Deer.ID))
# 
# 
# prev_Fred <- subset(prev_Fred, prev_Fred$Sex!="Unknown")
# prev_Fred <- subset(prev_Fred, prev_Fred$Year!="NA")
# 
# fredprev2 <- fred%>%
#   group_by(Year)%>%
#   summarise(total=length(Deer.ID))
# 
# fredprev3 <- fred%>%
#   group_by(Year)%>%
#   summarise(numpos=sum(status))
# 
# fredprev2$pos <- fredprev3$numpos
# fredprev2$prevalence <- fredprev2$pos / fredprev2$total
# fredprev2 <- subset(fredprev2, fredprev2$Year!="NA")
# 
# dem_labels <- unique(as.numeric(positives_dem$Year))
# dem_labels <- sort(dem_labels)
# dem_labels <- as.character(dem_labels)
# 
# scaleFUN <- function(x) sprintf("%.0f", x)
# 
# positives_dem$Year <-  as.factor(positives_dem$Year)
# 
# demography <- ggplot(data=positives_dem, aes(Year, total, fill=Sex))+
#   geom_bar(stat = "identity")+
#   scale_fill_manual(values = c("firebrick", "dodgerblue2"))+
#   theme_linedraw()+
#   ggtitle(label = "B")+
#   scale_x_discrete(labels = dem_labels)+
#   xlab("Hunting Season")+
#   ylab("CWD-Positive (#)")+
#   theme(axis.text.x = element_text(angle= -45, hjust = 0, size= rel(1.7)),axis.text.y = element_text(size= rel(1.7)),
#         axis.title.x = element_text(size= rel(1.7)), axis.title.y = element_text(size= rel(1.8)),
#         legend.text = element_text(size = rel(1.2)), legend.title = element_text(size = rel(1.3)),
#         panel.grid = element_blank(), plot.title = element_text(size = rel(2.2), vjust= -5.5,hjust= 0.008,face = "bold"),
#         legend.position = c(0.18, 0.6), plot.margin = margin(t=-15, r=10))
# demography
# 
# #Sampling methods
# dat_org <- dat%>%
#   group_by(Sample)%>%
#   summarise(summary_method <- length(Deer.ID))
# colnames(dat_org) <- c("Sample", "count")
# dat_org <- data.frame(dat_org)
# dat_org[dat_org$Sample=="Active - Hunter Killed"] <- c("Hunter Harvest", "Active - Other",
#                                                        "Roadkill", "Clinical Suspect", "Other")
# 
# dat_org$Sample <-  factor(dat_org$Sample, levels=c("Other", "Clinical Suspect","Active - Other",
#                                                    "Roadkill","Hunter Harvest"))
# 
# sampling_method <- ggplot(dat_org, aes(x=Sample, y=count/1000))+
#   geom_bar(stat='identity', fill="gray", color="black")+
#   geom_text(aes(label=count), size= rel(5), vjust=1.2 , hjust=-0.1)+
#   xlab("Sampling Method")+
#   ggtitle(label = "D")+
#   ylim(0,11)+
#   ylab("Samples (#)")+
#   theme_linedraw()+
#   theme(axis.text.y = element_text(size=rel(1.7),angle = 0, hjust = 1) ,axis.text.x = element_blank(), axis.ticks.x = element_blank(), #element_text(size= rel(1.7)),
#         axis.title.x = element_text(size= rel(1.7)), axis.title.y = element_text(size= rel(1.8)),#,angle=0,hjust=-1, vjust=1, margin = margin(r=-90)),
#         panel.grid = element_blank(), plot.title = element_text(size = rel(2.2), vjust=-5.4, hjust = 0.99, face = "bold"),
#         plot.margin = margin(t=-15, r=7))
# sampling_method <- sampling_method + coord_flip()
# 
# fredprev2 <- subset(fredprev2, fredprev2$Year>2008)
# fredprev2$Year <-  as.factor(fredprev2$Year)
# 
# #Prevalence
# prevalence <- ggplot(data = fredprev2, aes(x=Year, y=prevalence*100))+
#   geom_bar(stat='identity')+
#   theme_linedraw()+
#   ggtitle("C")+
#   xlab("Hunting Season")+
#   ylab("Prevalence (%)")+
#   theme(axis.text.y = element_text(size=rel(1.7),angle = 0, hjust = 1) ,axis.text.x =element_text(hjust = 0 ,angle=-45,size= rel(1.7)),
#         axis.title.x = element_text(size= rel(1.7)), axis.title.y = element_text(size= rel(1.8)),#,angle=0,hjust=-1, vjust=1, margin = margin(r=-90)),
#         panel.grid = element_blank(), #plot.title = element_text(size = rel(2.2), vjust=-5.2, hjust = 0.99, face = "bold"),
#         plot.title = element_text(size = rel(2.2), vjust= -5.5,hjust= 0.008,face = "bold"), plot.margin = margin(t=-15, r=10))
#   
# # Final Combination Plot --------------------------------------------------
# lay <- rbind(c(1,1,1,1,1,1),
#              c(1,1,1,1,1,1),
#              c(1,1,1,1,1,1),#layout for plot
#              c(1,1,1,1,1,1),
#              c(2,2,4,4,3,3),
#              c(2,2,4,4,3,3))
# 
# gridExtra::grid.arrange(studyarea, demography, sampling_method,prevalence, layout_matrix=lay)#, nrow=2)



# Extent map/study area delineation ---------------------------------------


point_scale_anchor <- c(x= -77.45,y= 39.82)
line_anchor <- data.frame(x= -77.4,y= 39.845, x2=-78, y2=39.845)
palette_point <- RColorBrewer::brewer.pal(n=4, name = "Set1")
palette_point<- c("#E41A1C", "#377EB8", "#FFFF00", "#984EA3")
points <- ggplot(data=va_count2)+
  geom_polygon(aes(x = long, y = lat, group = group), fill="white", color = "black")+
  #geom_polygon(data=va_count2,aes(x = long, y = lat, group = group), fill="white", color = "black")+
  geom_polygon(data=dma1,aes(x = long, y = lat, group = group), fill="gray", color = "black")+
  geom_polygon(data=dma2,aes(x = long, y = lat, group = group), fill="gray", color = "black")+
  geom_polygon(data=quad_state_count ,aes(x = long, y = lat, group = group), fill="white", color = "black")+
  geom_polygon(data=quad_state ,aes(x = long, y = lat, group = group), fill=NA, lwd=1.3, color = "black")+
  
  geom_polygon(data=buffer,aes(x = long, y = lat), lwd= 1.2, fill=NA, color = "red")+
  guides(fill=F)+
  coord_sf(crs= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")+
  geom_rect(aes(xmin = -79.06703, xmax= -77.36469, ymin = 38.01524, ymax = 39.86566),col="dimgray", lwd=1.2, fill=NA)+
  coord_sf(xlim = c(-79.06703, -77.36469), ylim = c(38.01524, 39.86566), expand = T)+
  geom_point(data= positives_reduced, aes(x=long, y=lat, col=block),  shape=16, lwd=2.8, show.legend = T)+
  geom_point(data= positives_reduced, aes(x=long, y=lat),col="black",  shape=1, lwd=2.8, show.legend = T)+
  scale_color_manual(values=palette_point, name= "Quadrant")+
  theme_bw()+
  annotate(geom = "text", x=-78.55, y=39.3, label="DMA 1", color="black", size=5.5)+
  annotate(geom = "text", x=-77.76, y=38.67, label= "DMA 2", color="black", size=5.5)+
  theme(axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), axis.text = element_blank(),
        plot.title = element_text(size=rel(2.2), face = "bold", hjust=0.025, vjust=-5), plot.margin = margin(t=-29),
        legend.position = c(0.13,0.15), legend.background = element_rect(fill="white",color="black"),
        legend.title = element_text(size = rel(1.4)), legend.text = element_text(size = rel(1.3)), 
        legend.margin = margin(t=1, b=1, r=1.4, l=1))+
  xlab(NULL)+
  ylab(NULL)+
  # ggtitle("B")+
  ggsn::scalebar(data=va_count, location= "topright", anchor = point_scale_anchor, dist = 20, 
                 box.fil=c("white","white"), dist_unit = "km", transform = T, model = "WGS84",
                 st.bottom = T, st.size = 4, border.size = 0, height = .01)+
  geom_segment(aes(x=x,y=y,xend=x2,yend=y2), color="white", lwd=2, data=line_anchor)
points
