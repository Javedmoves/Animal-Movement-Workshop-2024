############################################################################################################################################################################################################################################################################################

#### Script for processing raw SAFS PTT tracking data (2023) using animotum. This is the complete dataset, expanding upon the data processed in Riaz et al 2024

############################################################################################################################################################################################################################################################################################

library(ggplot2)
library(tidyverse)
library(aniMotum)
library(hms)
library(viridis)
library(scales)
library(readr)
library(viridis)
library(rgdal)
library(grid)
library(raster)
library(stringr)
library(lubridate)
library(maptools)
library(sp)
library(diveMove)
library(sf)
library(rnaturalearthhires)
library(data.table)
library(parallel)
library(future)
library(future.apply)
library(adehabitatLT)
library(ggOceanMaps)

setwd("//saeri-file/users$/jriaz/Documents/Github/Raw Data Processing/GPS processing/")

data_path <- ("//saeri-file/users$/jriaz/Documents/Github/Raw Data Processing/GPS processing/")


## Read in PTT

Males2023_BI_locs <- read_csv("237540-Locations.csv")

Males2023_BI_locs$DeployID <- as.factor(Males2023_BI_locs$DeployID)

Males2023_BI_locs$date<-as.POSIXct(Males2023_BI_locs$Date, format = "%H:%M:%S %d-%b-%Y", "GMT")#format times


mAP <- read_rds("Basemap.rds")

# 
# Basemap1 <- basemap(limits = c(-72, -48.8, -57, -34), bathymetry = TRUE, rotate = TRUE, grid.col = NA,) + 
#   theme(legend.position = "none") +
#   annotation_scale(location = "br")  
# # annotation_north_arrow(location = "tr", which_north = "true") 
# Basemap1

IDPlot <- mAP + 
  ggspatial::geom_spatial_point(data = Males2023_BI_locs, aes(x = Longitude, y = Latitude, colour = DeployID)) +
  scale_colour_viridis_d() + theme(legend.position = "none")
IDPlot


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## Read in DF and some basic time and date formatting

Males2023_BI_locs <- Males2023_BI_locs %>%
  dplyr::select(DeployID, date, Quality, Longitude, Latitude, `Error Semi-major axis`, `Error Semi-minor axis`, `Error radius`) ## Selects only the relevant columns

names(Males2023_BI_locs) <- c("id","date","lc","lon","lat", "smaj", "smin", "eor") ## Rename cols

unique(Males2023_BI_locs$id) ## 19


##############################################################################################################################################
## Arrange DF by date and change any Z class locations to B class? Kept as Z class for now


newd <- Males2023_BI_locs %>%
  group_by(id) %>%
  arrange(id, date)

##############################################################################################################################################
## Remove any duplicates or near duplicates that occured within 120 s


d1new <- newd %>%
  group_by(id) %>%
  do(distinct(., date, .keep_all = TRUE)) %>%
  do(mutate(
    .,
    dup = difftime(date, lag(date), units = "secs") < 120)) %>%
  do(arrange(., order(date))) %>%
  dplyr:: filter(.,!dup) %>%
  dplyr:: select(-dup)

d1.rep.new <- nrow(newd) - nrow(d1new)
cat(sprintf("%d duplicate time &/or near duplicate location/time records removed\n", d1.rep.new)) 


##############################################################################################################################################
## Remove extreme travel rate locations to be ignored at ssm filter stage

vmax <- 4

d5new <- d1new %>%
  do(mutate(., keep = grpSpeedFilter(cbind(date, lon, lat), speed.thr = vmax)))
d5.rep.new <- nrow(d1new) - sum(d5new$keep)
cat(sprintf(
  paste(
    "\n%d records with travel rates >",
    vmax,
    "m/s will be ignored by SSM filter\n"
  ),
  d5.rep.new
)) 

##############################################################################################################################################

NewTracks <- d5new %>%
  filter(keep == "TRUE") %>%
  ungroup() %>%
  dplyr::select(-keep)

NewTracks$id <- as.character(NewTracks$id)

##############################################################################################################################################
##############################################################################################################################################
# Run tracks through SSM. These are are GPS tracks so I've run with a 2 hour time step
# Run the Predicted first and then run for the Fitted

d3 <- data.frame(NewTracks) 


fits <- aniMotum::fit_ssm(d3, model="rw", 
                          vmax = 4,
                          spdf = FALSE,
                          # control = ssm_control(optim=c("nlminb")), 
                          time.step = 1)



my.aes <- aes_lst(obs=TRUE, line=TRUE,mp=FALSE, conf = FALSE)


## Create plot and save the plot to your folder. 
## Track coloured by date and orange triangles represent raw fixes

  for(i in unique(fits$id)){
   subID <- subset(fits, id == i)
   gi <- aniMotum::map(subID, what = "predicted", aes = my.aes, by.date = TRUE)
   ggsave(filename = sprintf('%s.png', i), plot = gi, width = 9, height = 9)
 }


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## END OF SCRIPT
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################





# 
# fits_mp <- fit_mpm(fits, what = "predicted", model = "mpm")
# 
# Males_DF_SSM <- grab(fits, what = "predicted", as_sf = FALSE, normalise = FALSE, group = FALSE)
# 
# Males_SSM <- grab(fits_mp, what = "fitted", as_sf = FALSE, normalise = FALSE, group = FALSE)
# 
# Males_SSM_Norm <- grab(fits_mp, what = "fitted", as_sf = FALSE, normalise = TRUE, group = FALSE)
# 
# Males_SSM_Norm <- Males_SSM_Norm %>%
#   dplyr::select(id, date, g) %>%
#   rename(g_norm = g)
# 
# 
# Males_SSM_Clean <- Males_DF_SSM %>%
#   left_join(Males_SSM) %>%
#   left_join(Males_SSM_Norm)
# 
# Males_SSM_Clean$ID <- Males_SSM_Clean$id
# Males_SSM_Clean$trip <- Males_SSM$id
# Males_SSM_Clean$col <- "BIRD"
# Males_SSM_Clean$time <- Males_SSM_Clean$date
# Males_SSM_Clean$tag <- "PTT"
# Males_SSM_Clean$stage <- "WINTER"
# 
# write_rds(Males_SSM_Clean, "All_Males23_SSM_Clean.rds")
# 




