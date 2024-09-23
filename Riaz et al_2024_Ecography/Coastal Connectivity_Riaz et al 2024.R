############################################################################################################################################################################################################################################################################################
# R script created adapted from:
## Riaz et al 2024 Ecography  
### Coastal connectivity of marine predators over the Patagonian Shelf during the highly pathogenic avian influenza (HPAI) outbreak
############################################################################################################################################################################################################################################################################################


## First read in a range of relevant packages

library(tidyverse)
library(aniMotum)
library(hms)
library(viridis)
library(scales)
library(readr)
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
library(ggpubr)
library(sp)
require(sf)
library(rasterVis)
library(RColorBrewer)
library(gganimate) 
require(transformr) 
library(rasterVis)
library(gganimate)
library(gifski)
library(devtools)
library(cividis)
library(wesanderson)
library(geosphere)
library(mapview)
library(ggOceanMaps)
require(devtools)
library(rgeos)
library(units)

setwd("//saeri-file/users$/jriaz/Documents/Github/Connectivity/")


# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################


##############################################################################################################################################
## Fur seals
##############################################################################################################################################

SAFSData <- read_rds("SimulatedTracks.rds")

SAFSData$SPP <- "SAFS"

IDS <- SAFSData %>%
  dplyr::select(id) %>%
  distinct()
  
SAFSData <- as.data.frame(SAFSData)

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
# Calculate distance from South American coast for each animal location
# TAKES A LONG TIME 

## Function to calculatue distance from land
 SAFSData <- dist2land(data = SAFSData, lon = "lon", lat = "lat", binary = FALSE)


# # Define the size of each subset (e.g., 10,000 rows per chunk)
# chunk_size <- 10000
# # Create an empty list to store results
# results_list <- list()
# # Calculate the number of iterations required
# n_chunks <- ceiling(nrow(SAFSData) / chunk_size)
# # Loop through each chunk
# for (i in 1:n_chunks) {
#   # Define the row range for the current chunk
#   start_row <- (i - 1) * chunk_size + 1
#   end_row <- min(i * chunk_size, nrow(SAFSData)) # Ensure we don't go beyond the last row
#     # Subset the SAFSData
#   SAFSData_subset <- SAFSData[start_row:end_row, ]
#     # Apply the dist2land function to the subset
#   result <- dist2land(data = SAFSData_subset, lon = "lon", lat = "lat", binary = FALSE)
#     # Store the result in the results_list
#   results_list[[i]] <- result
#     # Optional: Print progress
#   print(paste("Processed chunk", i, "of", n_chunks))
# }
# # Combine all the chunks back into a single dataframe
# SAFSData_combined <- do.call(rbind, results_list)

# write_rds(SAFSData_combined, "SAFSData_dt.rds")



# Read in save distance dataframe to save time
SAFSData_dt <- read_rds("SAFSData_dt.rds")


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
# Now calculate distance from the  coastline for each animal location

UK_Shape<-read_sf("Mappping.shp")
st_crs(UK_Shape)  

UK_Shape$COUNTRY

plot(UK_Shape)

UK_Shape <- UK_Shape %>%
  filter(COUNTRY == "U.K.")

UK_Shape1 <- as(UK_Shape, 'Spatial')
plot(UK_Shape1)

UK_sf_polygons <- st_as_sf(UK_Shape1)

## Crop to only include Falklands
box = c(xmin = -65, ymin = -50, xmax = -55, ymax = -54) ## Set bounding box for cropping
cropped_UK <- st_crop(UK_sf_polygons, box) ## Crop the shapefile accordingly
plot(cropped_UK)



lsf <- SAFSData_dt %>% ## =
  dplyr::select(lon, lat) %>%
  st_as_sf(coords= c("lon","lat")) %>% # make spatial
  st_set_crs(st_crs(4326)) 

head(lsf)
st_crs(lsf)  


# Calculate distances using st_distance
ts <- Sys.time()
distances <- st_distance(lsf, cropped_UK, by_element = FALSE)
difftime(Sys.time(),ts)


distances_km <- set_units(distances, km)
distances_km <- set_units(distances_km, NULL) 
distances_km


SAFSData_dt <-  cbind(SAFSData_dt, distances_km)


SAFSData_dt <- SAFSData_dt %>%
  rename(distance_FI_km = 13)


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## Summary statistics and manuscript results 


SAFSData_dt <- SAFSData_dt %>%
  mutate(Connectivity = case_when(
    ldist <= 1  ~ "Nearshore",  
    ldist > 1 & ldist <= 22.5 ~ "Coastal",  # Classify as "Coastal"
    TRUE ~ "At-sea"  # All other rows classified as "Other"
  )) %>%
  mutate(Behaviour = case_when(
    ldist <= 1 & gnorm <= 0.5 ~ "Nearshore residency",   # Classify as "Resident"
    ldist > 1 & ldist <= 22.5 & gnorm > 0.5 ~ "Coastal residency",  # Classify as "Coastal"
    TRUE ~ "Other"  # All other rows classified as "Other"
  ))


Basemap2 <- basemap(limits = c(-80, -37, -68, -32), bathymetry = FALSE, rotate = TRUE, grid.col = NA,) + 
  theme(legend.position = "none") 
Basemap2


LandPoints_Data <- SAFSData_dt %>%
  filter(!Connectivity == "At-sea")


## Quick visual check
LandPoints <- Basemap2 + 
  ggspatial::geom_spatial_point(data = SAFSData_dt, aes(x = lon, y= lat, colour = Connectivity), alpha = 0.8, size = 0.4) +
  scale_colour_manual(values = c("grey2", "orange", "red")) +
  labs(colour = "Distance", y = "Longitude", x = "Latitude") +
  scale_linetype_manual(values=c("dashed", "solid")) +
  theme(legend.position = c(0.8, 0.8), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = "transparent", color = "black", size = 1.5),
        legend.margin = margin(10, 10, 10, 10),
        legend.box.margin = margin(5, 5, 5, 5)) +
  theme(legend.title = element_text(face = "bold"))
LandPoints


BehaviourPoints <- SAFSData_dt %>%
  filter(!Behaviour == "Other")


CR <- BehaviourPoints %>%
  filter(Behaviour == "Coastal residency")
NR <- BehaviourPoints %>%
  filter(!Behaviour == "Coastal residency")

## Quick visual check
ConnectivityPoints <- Basemap2 + 
  # ggspatial::geom_spatial_point(data = BehaviourPoints, aes(x = lon, y= lat, colour = Behaviour), alpha = 0.7, size = 1) +
   ggspatial::geom_spatial_point(data = CR, aes(x = lon, y= lat, colour = Behaviour), alpha = 0.8, size = 1) +
   ggspatial::geom_spatial_point(data = NR, aes(x = lon, y= lat, colour = Behaviour), alpha = 0.8, size = 1) +
    scale_colour_manual(values = c("orange", "red")) +
  labs(colour = "Behaviour", y = "Longitude", x = "Latitude") +
  scale_linetype_manual(values=c("dashed", "solid")) +
  theme(legend.position = c(0.8, 0.8), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = "transparent", color = "black", size = 1.5),
        legend.margin = margin(10, 10, 10, 10),
        legend.box.margin = margin(5, 5, 5, 5)) +
  theme(legend.title = element_text(face = "bold"))
ConnectivityPoints




LandPoints + ConnectivityPoints



##############################################################################################################################################
## Calcuate means time in area
##############################################################################################################################################

# Step 1: Calculate the proportion of rows for each behavior state per individual
proportions_per_id <- SAFSData_dt %>%
  group_by(id, Behaviour) %>%
  summarise(count = n()) %>%  # Count rows for each behavior per individual
  mutate(total_count = sum(count),  # Total rows per individual
         proportion = count / total_count) %>%  # Proportion of each behavior per individual
  ungroup()

# Step 2: Calculate the mean proportion and standard deviation for each behavior across all individuals
summary_proportions <- proportions_per_id %>%
  group_by(Behaviour) %>%
  summarise(mean_proportion = mean(proportion, na.rm = TRUE),
            sd_proportion = sd(proportion, na.rm = TRUE),
            n = n()) %>%
  mutate(se_proportion = sd_proportion / sqrt(n),  # Standard Error
         ci_low = mean_proportion - 1.96 * se_proportion,  # 95% CI lower bound
         ci_high = mean_proportion + 1.96 * se_proportion)  # 95% CI upper bound

# Step 3: Plotting both proportions per individual and the overall mean with error bars
ggplot() +
  geom_jitter(data = proportions_per_id, aes(x = Behaviour, y = proportion, color = id), width = 0.2, alpha = 0.6, size = 2) +
  geom_point(data = summary_proportions, aes(x = Behaviour, y = mean_proportion), size = 4, color = "black") +
  geom_errorbar(data = summary_proportions, aes(x = Behaviour, ymin = ci_low, ymax = ci_high), width = 0.2, color = "black") +
  # scale_colour_viridis_d() +
  labs(title = "Proportion of Time Spent in Each Behavioural State",
       x = "Behaviour",
       y = "Proportion of Time",
       color = "ID") +
  theme_minimal()





##############################################################################################################################################
## Time in differnt jurisdicitons
##############################################################################################################################################

Mappping1 <- read_sf("Mappping.shp")

Mapping_map <- ggplot() +
  geom_sf(data = Mappping1, aes(fill = COUNTRY) ) +
  theme(legend.position = "none") 
Mapping_map


###############################################################################################################################################
## Then the maritime boundaries
###############################################################################################################################################

AllShapefiles1 <- read_sf("AllShapefiles.shp")

plot(AllShapefiles1)

AllShapefiles1$geoname <- ifelse(AllShapefiles1$geoname == "Argentinean Exclusive Economic Zone",
                                 "Argentina EEZ", AllShapefiles1$geoname)
AllShapefiles1$geoname <- ifelse(AllShapefiles1$geoname == "Disputed area South Georgia and South Sandwich Exclusive Economic Zone: UK / Argentina",
                                 "South Georgia and South Sandwich EEZ", AllShapefiles1$geoname)
AllShapefiles1$geoname <- ifelse(AllShapefiles1$geoname == "FOCZ",
                                 "Falkland Islands EEZ", AllShapefiles1$geoname)
AllShapefiles1$geoname <- ifelse(AllShapefiles1$geoname == "Chilean Exclusive Economic Zone",
                                 "Chile EEZ", AllShapefiles1$geoname)
AllShapefiles1$geoname <- ifelse(AllShapefiles1$geoname == "Uruguayan Exclusive Economic Zone",
                                 "Uruguay EEZ", AllShapefiles1$geoname)

plot(AllShapefiles1)




###############################################################################################################################################
###############################################################################################################################################
# HOW LONG DO SEALS SPEND IN EACH JURISDICTION?
## Based on distance from coast
###############################################################################################################################################
###############################################################################################################################################

## First let's visualise

Fig1 <- ggplot() + 
  geom_sf(data = AllShapefiles1, aes(fill = geoname)) +
  scale_fill_manual(values = c("#6489C6", "#9E6054", "darkgreen", "white","purple1", "#FEFEB2")) +
  # scico::scale_fill_scico_d(palette = "nuuk") +
  geom_sf(data = Mappping1, aes(fill = geoname), fill = "grey6", colour = "black") +
  # coord_sf(xlim = c(-70,-31), ylim = c(-38,-55.5), expand = FALSE) +
  # ggspatial::geom_spatial_point(data = SAFSData_dt, aes(x = lon, y= lat), size = 0.1, alpha = 0.2) +
  ggspatial::geom_spatial_point(data = LandPoints_Data, aes(x = lon, y= lat, colour = Connectivity), alpha = 0.8, size = 0.4) +
  scale_colour_manual(values = c("orange", "red")) +
  # scale_colour_viridis_d(option = "B") + 
  labs(colour = "ID", y = "Longitude", x = "Latitude", fill = "Jurisdiction") + 
  theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) +
  theme(legend.position = "right") + guides(colour = "none") +
  theme(legend.justification = c(0.95,0.1),
        legend.position = c(0.9,0.7),
        legend.background = element_rect(fill = "lightgrey", colour = "black"),
        legend.key = element_rect(fill = "lightgrey"))  
Fig1




### Lets do some basic summary calculation

# First make into a spatial object
Spatialseal <- st_as_sf(x = SAFSData_dt,                         
                        coords = c("lon", "lat"),
                        crs = st_crs(4326))


# Perform a spatial join to find which EEZ each seal location is in
seals_in_eez <- st_join(Spatialseal, AllShapefiles1, join = st_intersects)


# Count unique seals in each EEZ
seal_count_per_eez <- seals_in_eez %>%
  group_by(geoname) %>%
  summarise(num_seals = n_distinct(id))


# Add a time_spent column 
if (!"time_spent" %in% colnames(Spatialseal)) {
  Spatialseal <- Spatialseal %>% 
    mutate(time_spent = 1)  # Remembering there are equal time interavals (15 mins) between locations
}


# Join seals with EEZs to find which EEZ each seal location is in
seals_in_eez <- st_join(Spatialseal, AllShapefiles1, join = st_intersects)



# Calculate the proportion of time each seal spent in each EEZ
time_spent_per_eez <- seals_in_eez %>%
  group_by(id, geoname) %>%  
  summarise(total_time_spent = sum(time_spent)) %>%
  ungroup()


# Calculate the total time spent by each seal across all EEZs
total_time_per_seal <- time_spent_per_eez %>%
  group_by(id) %>%
  summarise(total_time_all_eez = sum(total_time_spent))


# Convert to data frames
time_spent_per_eez_df <- as.data.frame(time_spent_per_eez)
total_time_per_seal_df <- as.data.frame(total_time_per_seal)


# Join the total time spent back to the main data to calculate proportions
time_spent_per_eez_with_proportions <- time_spent_per_eez_df %>%
  left_join(total_time_per_seal_df, by = "id") %>%
  mutate(proportion_time = total_time_spent / total_time_all_eez)


## Function to calculate some summary metrics
calc_conf_int <- function(x, conf.level = 0.95) {
  n <- length(x)
  mean_x <- mean(x)
  stderr <- sd(x) / sqrt(n)
  alpha <- 1 - conf.level
  error_margin <- qt(1 - alpha / 2, df = n - 1) * stderr
  lower_bound <- mean_x - error_margin
  upper_bound <- mean_x + error_margin
  return(c(lower_bound, upper_bound))
}


## Table summarising metrics
summary_stats <- time_spent_per_eez_with_proportions %>%
  group_by(geoname) %>%
  summarise(
    Mean = mean(proportion_time, na.rm = TRUE),
    Lower_CI = calc_conf_int(proportion_time)[1],
    Upper_CI = calc_conf_int(proportion_time)[2],
    min_proportion = min(proportion_time, na.rm = TRUE),
    max_proportion = max(proportion_time, na.rm = TRUE)) %>%
   rename(EEZ = geoname) %>%        
   mutate(EEZ = c("Argentina EEZ",
                  "Chile EEZ",
                  "Falkland Islands EEZ", 
                  "Joint Arg/Urg",
                  "SGSSI EEZ",
                  "Uruguay EEZ",
                  "High Seas"))



## My plot function
JZR_style <- function() {
  font <- "Helvetica"
  ggplot2::theme(
    #Text format:
    plot.title = ggplot2::element_text(family=font,
                                       size=28,
                                       face="bold",
                                       color="#222222"),
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=22,
                                          margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_blank(),
    #Legend format
    legend.position = "right",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=18,
                                        color="#222222"),
    
    #Axis format
    # axis.title = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(family=font,
                                       size=18,
                                       color="#222222"),
    axis.text = ggplot2::element_text(family=font,
                                      size=18,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    panel.background = ggplot2::element_blank(),
    
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}



## Summary plot
JurisdictionPlot <- ggplot(summary_stats, aes(x = EEZ, y = Mean, fill = EEZ)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2,) +
  geom_text(aes(label = round(Mean, 3)), vjust = -0.5, hjust = 1, fontface = 2) +
  # geom_text(aes(label = round(Mean, 3)), vjust = -0.5, hjust = 1.1, fontface = 2) +
  scale_fill_manual(values = c("#6489C6", "#9E6054", "darkgreen", "white","purple1", "#FEFEB2", "orange" )) +
  labs(
    # title = "Time in Area results",
    fill = "Jurisdiction",
    x = NULL,
    y = "Proportion of time (%)"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0,1)) +
  ggtitle(("Where do South American fur seals go?")) +
  JZR_style() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 
JurisdictionPlot


##############################################################################################################################################
##############################################################################################################################################
## END OF SCRIPT
## END OF SCRIPT
## END OF SCRIPT
##############################################################################################################################################
##############################################################################################################################################

