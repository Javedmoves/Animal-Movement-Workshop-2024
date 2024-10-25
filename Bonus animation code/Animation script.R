# Author: Javed Riaz
############################################################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################################################

# R script created for:
# Tracking animations


############################################################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################################################


## First read in a range of relevant packages

library(tidyverse)
library(hms)
library(viridis)
library(sp)
require(sf)
library(gganimate)
library(gifski)
library(ggOceanMaps)
library(scico)

setwd("//saeri-file/users$/jriaz/Documents/Github/Bonus animation code/")


############################################################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################################################

## Read in data and basemap

SAFSData_SEA <- read_rds("SAFSData_dt.rds")

Basemap <- basemap(limits = c(-80, -37, -60, -32), bathy.style = "rcb" , rotate = TRUE, grid.col = NA) + 
   theme(legend.position = "none") +
   labs(y = "Longitude", x = "Latitude") +
   scale_fill_scico(palette= "devon", direction = -1, begin = 0.3, alpha = 0.5)
Basemap


############################################################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################################################
###########################################################################################################################################################################################################################################################################################

## Configure so all animals leave their tagging location at the same time. Just for visual purposes


SAFSData_SEA$UniqueID <- paste(SAFSData_SEA$id, SAFSData_SEA$rep)

AniamtionData <- SAFSData_SEA %>%
 group_by(UniqueID) %>%
 arrange(UniqueID, date) %>% 
 mutate(sequential_column = row_number()) 


# Check <- AniamtionData %>%
#   group_by(UniqueID) %>%
#   summarise(max(sequential_column))


## Create the plot

Animation_Plot <- Basemap +
    ggspatial::geom_spatial_point(data = AniamtionData, aes(x = lon, y= lat, colour = UniqueID, group = UniqueID), size = 1) +
   labs(y = "Longitude", x = "Latitude", fill = "Jurisdiction") + 
   theme_bw(10) + 
  scale_colour_viridis_d(option = "H", begin = 0.7, end = 1) +
   theme(legend.position = "none") +
   transition_reveal(sequential_column, keep_last = FALSE)  +
   shadow_wake(wake_length = 0.05, alpha = 0.5)



# Save the animated the plot! :) 

gganimate::animate(Animation_Plot, duration = 20, fps = 10, height = 4.5, width = 7, units = "in", res = 300, renderer = av_renderer("Animation_plot.mp4"))
 

# End of script
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################


## Bonus code to animate multi-year tracks by month according to one annual cycle (refer to "Fur seal tracking animation.mp4" 

#AniamtionData <- SAFSData_SEA %>%
# group_by(id) %>%
#  #arrange(id, date) %>%
#  mutate(sequential_column = row_number()) %>%
#  mutate(date1 = update(date, year = 2023)) %>%
#  mutate(date2 = ShortDate) %>%
#  mutate(date2 = update(date2, year = 2023))
  
# Y1 <- AniamtionData %>%
# filter(date2 > "2023-07-11") %>%
#  mutate(date2 = update(date2, year = 2022))
# Y2 <- AniamtionData %>%
#  filter(date2 <= "2023-07-11")

# AniamtionData <- Y1 %>%
 # full_join(Y2)

# Animation_Age_CLASS_DATE <- ggplot() + 
#  geom_sf(data = cropped_sf, aes(fill = geoname)) +
#  scale_fill_manual(values = c("#6489C6", "#9E6054", "#FEFEB2")) +
#  geom_sf(data = Mappping, aes(fill = geoname), fill = "grey6", colour = "black") +
#  coord_sf(xlim = c(-70,-31), ylim = c(-38,-55.5), expand = FALSE) +
#  geom_point(data = AniamtionData, aes(x = lon, y= lat, colour = Group, group = id), size = 1) +
#  scale_colour_viridis_d(option = "H", guide = guide_legend(direction = "horizontal")) + 
#  labs(colour = "Fur seal tracking data", y = "Longitude", x = "Latitude", fill = "Jurisdiction",   
#         title = "{format(frame_time, '%d %B')}") + 
#  theme_bw(10) + 
#  theme(strip.background = element_rect(fill="gray85"),
#        panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(),  
#        panel.border = element_rect(colour = "black")) +
#  theme(legend.position = "right") +
#  theme(legend.justification = c(0.95,0.1),
#        legend.position = c(0.9,0.55),
#        legend.background = element_rect(fill = "lightgrey", colour = "black"),
#        legend.key = element_rect(fill = "lightgrey"))  +
#  theme(plot.title=element_text(colour = "red", margin=margin(t=10,b=-15))) +
#  guides(colour = guide_legend(title.position = "top", 
#                               direction = "horizontal",
#                               override.aes = list(size = 5))) +
#  gganimate::transition_time(date2) + 
#   shadow_wake(wake_length = 0.01,
#               alpha = 0.5)

# gganimate::animate(Animation_Age_CLASS_DATE, duration = 90, fps = 10, height = 4.3, width = 6.3, units = "in", res = 300, renderer = av_renderer("Fur seal tracking animation.mp4"))










