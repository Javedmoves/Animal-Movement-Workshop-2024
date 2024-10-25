##############################################################################################################################################
## Horizontal-vertical movement integration and spatial overlap with fisheries
## Code and data adapted from Riaz et al 2023_Global Ecology and Conservation
##############################################################################################################################################

## Read in relevant packages and set data path


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
library(sp)
library(diveMove)
library(sf)
library(ggpubr)
library(ggOceanMaps)
library(ggspatial)
library(data.table)


setwd("//saeri-file/users$/jriaz/Documents/Github/GPS-Dive integration/")

## Read in data

FS_Predicted_locs <- read_rds("FurSeal_Predicted_locs.rds")

FS_Predicted_locs <- FS_Predicted_locs %>%
  group_by(id) %>%
  arrange(date)

unique(FS_Predicted_locs$id) ##18 individuals

DiveData <- read_rds("Seal_DiveData.rds")

DiveData <- DiveData %>%
  dplyr::select(-date) %>%
  group_by(id) %>%
  arrange(begdesc)

unique(DiveData$id) ##18 individuals


##############################################################################################################################################
##############################################################################################################################################
## Initial visualisation of data
##############################################################################################################################################
##############################################################################################################################################

## GPS data

# Basemap <- basemap(limits = c(-70, -58, -56, -47), bathy.style = "rcb" , rotate = TRUE, grid.col = NA) + 
#   theme(legend.position = "right")  +
#   theme(legend.background = element_rect(fill = "white", colour = "white"),
#         legend.key = element_rect(fill = "white")) +
#   guides(fill ="none") 
#   ## Select 1 here to download basemap
# Basemap

# write_rds(Basemap, "Basemap.rds")
Basemap <- read_rds("Basemap.rds")

Figure1 <- Basemap + ggspatial::geom_spatial_point(data=FS_Predicted_locs, aes(x=lon,y=lat,color=id)) +
  scale_colour_viridis_d()
Figure1


Figure1_Panels <- Basemap + ggspatial::geom_spatial_point(data=FS_Predicted_locs, aes(x=lon,y=lat,color=id), size = 0.8) +
  scale_colour_viridis_d() + facet_wrap(~id, nrow = 3) + theme(legend.position = "none")
Figure1_Panels



## Dive data

Figure2 <- DiveData %>%
  ggplot() + geom_point(aes(x = begdesc, y = maxdep)) +
  labs(x = "Date", y = "Depth (m)") +
  facet_wrap(~ id, scales = "free", nrow = 3) + scale_y_reverse() +
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black"))
Figure2



##############################################################################################################################################
##############################################################################################################################################
## Integrate data
##############################################################################################################################################
##############################################################################################################################################

FS_Predicted_locs$endGPStime <- FS_Predicted_locs$date + minutes(15) ## Add column to create a 15-minute window to movement data

DiveData$start_dive <- DiveData$begdesc

#### DPLYR way to do this if you're a tidyverse fanatic where we loop through each ID. 
#### Works but is slower than SetDT way used below

# merged_data <- data.frame()

# # Get unique ids
# unique_ids <- unique(FS_Predicted_locs$id)

# for (id in unique_ids) {
#   # Subset data for the current id
#   gps_subset <- FS_Predicted_locs %>% filter(id == !!id)
#   dive_subset <- DiveData %>% filter(id == !!id)
#   
#   # Perform the inner join and filter for the current id
#   merged_subset <- gps_subset %>%
#     inner_join(dive_subset, by = "id") %>%
#     filter(begdesc >= date & begdesc <= endGPStime)
#   
#   # Append the results to the merged_data dataframe
#   merged_data <- rbind(merged_data, merged_subset)
# }


Move <- setDT(FS_Predicted_locs) 
Dive <- setDT(DiveData)


Integration <- Dive[Move, on = .(id = id,
                                  begdesc >= date , begdesc <= endGPStime), 
                    .(id, lon, lat, date, start_dive, endGPStime, g, maxdep, divetim, botttim, desctim, asctim, bottdist, postdive.dur, Species, Divenum, dive.tot, Month, Year)]  # indicate columns in the output

Integration <- Integration %>%
  drop_na(Divenum) %>%
  arrange(id, date)

Move_AND_Dive <- as.data.frame(Integration)
  
Move_AND_Dive["botttim"][is.na(Move_AND_Dive["botttim"])] <- 0
Move_AND_Dive["bottdist"][is.na(Move_AND_Dive["bottdist"])] <- 0


# write_rds(Move_AND_Dive, "Move_AND_Dive.rds") ### 82771 dives



##############################################################################################################################################
##############################################################################################################################################
## Visualise and check integrated data
##############################################################################################################################################
##############################################################################################################################################


Seal_dive_stats <- Move_AND_Dive %>%
  filter(Species == "SAFS") %>%
  ungroup() %>%
  group_by(id, date, lon, lat, g) %>%
  mutate(count = 1) %>%
     dplyr::summarise(NoDive = sum(count),
                      depth_sum = sum(maxdep),
                      depth_mean = mean(maxdep),
                      duration_sum = sum(divetim), 
                      duration_mean = mean(divetim), 
                      botdur_sum = sum(botttim), 
                      botdur_mean = mean(botttim),  
                      wiggle_sum = sum(bottdist),
                      wiggle_mean = mean(bottdist)) %>%
  ungroup()


Figure3 <- Basemap + ggspatial::geom_spatial_point(data=Seal_dive_stats, aes(x=lon,y=lat,group=id, colour = depth_sum)) +
  scale_colour_viridis_c(option = "inferno")
Figure3


MeanDepthPlot <- ggplot() + geom_histogram(data = Seal_dive_stats, aes(x = depth_mean), colour = "black", fill = "darkgrey") +
  labs(x = "Depth (m)", y = "Frequency") +
  ggtitle("Mean depth (m) in 15-minute windows")+
  scale_x_continuous(breaks = c(0,50,100,150,200,250,300)) 

SumDepthPlot <- ggplot() + geom_histogram(data = Seal_dive_stats, aes(x = depth_sum), colour = "black", fill = "darkgrey") +
  labs(x = "Depth (m)", y = "Frequency") +
  ggtitle("Total vertical distance (m) travelled in 15-minute windows") +
  scale_x_continuous(breaks = c(0,100, 200, 300, 400, 500, 600)) 


ggarrange(MeanDepthPlot, SumDepthPlot, nrow = 2)


Seal_dive_stats <- Seal_dive_stats %>%
  group_by(id) %>%
  dplyr::mutate(depth_norm= (depth_sum - min(depth_sum))/(max(depth_sum) - min(depth_sum)))  # This is proportion relative to the range. So the lowest value is 0 and the highest value is 1 
  

Figure3 <- Basemap + ggspatial::geom_spatial_point(data=Seal_dive_stats, aes(x=lon,y=lat,group=id, colour = depth_norm)) +
  scale_colour_viridis_c(option = "B") + labs(colour = "Depth\n(scaled)")
Figure3



##############################################################################################################################################
##############################################################################################################################################
## Grid the horizontal-vertical data
##############################################################################################################################################
##############################################################################################################################################


Locs_SF <- st_as_sf(Seal_dive_stats, coords=c("lon","lat"), crs=4326)

hgrid <- st_make_grid(Locs_SF, cellsize = 0.25, square = FALSE) %>% # 0.25 x 0.25 degree
  st_transform(crs=4326) %>%
  st_as_sf() %>%
  mutate(GridID = c(1:NROW(.)))


ggplot()+
  geom_sf(data=hgrid)+
  geom_sf(data=Locs_SF, aes(colour = depth_sum)) +
  scale_colour_viridis_c(option = "B")


Gridded <- st_join(hgrid, Locs_SF) %>%
  group_by(id, GridID) %>%
  mutate(count = 1) %>%
  dplyr::summarise(NoDive = sum(NoDive),
                   depth_sum = sum(depth_sum),
                   duration_sum = sum(duration_sum), 
                   botdur_sum = sum(botdur_sum), 
                   wiggle_sum = sum(wiggle_sum),
                   move_persistence = mean(g, na.rm = "TRUE")) %>%
  ungroup() %>%
  drop_na(depth_sum)


Facet_Grid <- Basemap + 
  ggnewscale::new_scale_fill() +
  geom_sf(data=Gridded, aes(fill = NoDive), alpha = 0.8) +
  scale_fill_viridis_c(option = "B") +
  facet_wrap(~ id) +
  labs(fill = "# dives", x = "Longitude", y = "Latitude")
Facet_Grid


PlottingGrids <- Gridded %>%
  group_by(GridID) %>%
  dplyr::summarise(NoDive = sum(NoDive),
                   depth_sum = sum(depth_sum),
                   duration_sum = sum(duration_sum), 
                   botdur_sum = sum(botdur_sum), 
                   wiggle_sum = sum(wiggle_sum),
                   move_persistence = mean(move_persistence, na.rm = "TRUE")) %>%
  ungroup() %>%
  drop_na(depth_sum)



GriddedPlotA <- Basemap + 
  ggnewscale::new_scale_fill() +
  geom_sf(data=PlottingGrids, aes(fill = depth_sum/1000), alpha = 0.8) +
  scale_fill_viridis_c(option = "B") +
  labs(fill = "Seal vertical\ndistance (km)", x = "Longitude", y = "Latitude")
GriddedPlotA


GriddedPlotB <- Basemap + 
  ggnewscale::new_scale_fill() +
  geom_sf(data=PlottingGrids, aes(fill = NoDive), alpha = 0.8) +
  scale_fill_viridis_c(option = "B") +
  labs(fill = "Seal dive\nfrequency (#)", x = "Longitude", y = "Latitude")
GriddedPlotB


GriddedPlotC <- Basemap + 
  ggnewscale::new_scale_fill() +
  geom_sf(data=Gridded, aes(fill = botdur_sum/60/60), alpha = 0.8) +
  scale_fill_viridis_c(option = "B") +
  labs(fill = "Seal bottom\nduration (hrs)", x = "Longitude", y = "Latitude")
GriddedPlotC


GriddedPlotD <- Basemap + 
  ggnewscale::new_scale_fill() +
  geom_sf(data=PlottingGrids, aes(fill = wiggle_sum/60), alpha = 0.8) +
  scale_fill_viridis_c(option = "B") +
  labs(fill = "Seal dive\nwiggles (m)", x = "Longitude", y = "Latitude")
GriddedPlotD


GriddedPlotE <- Basemap + 
  ggnewscale::new_scale_fill() +
  geom_sf(data=PlottingGrids, aes(fill = move_persistence), alpha = 0.8) +
  scale_fill_viridis_c(option = "B", limits = c(0.65, 1)) +
  labs(fill = "Seal move\npersistence", x = "Longitude", y = "Latitude")
GriddedPlotE



Figure4 <- GriddedPlotA + GriddedPlotB + GriddedPlotC + GriddedPlotD + GriddedPlotE
Figure4


##############################################################################################################################################
##############################################################################################################################################
## Fisheries Data
##############################################################################################################################################
##############################################################################################################################################


# Read in monthly fisheries data corresponding to when we have seal tracking data (2018 and 2019)

gfw_dat_2018 <- read_rds("gfw_dat_2018.rds")
gfw_dat_2019 <- read_rds("gfw_dat_2019.rds")

unique(gfw_dat_2018$geartype)

Full_gfw_dat <- gfw_dat_2018 %>%
  full_join(gfw_dat_2019) %>%
  arrange("Time Range") %>%
  filter(geartype == "squid_jigger"| ## Retain only the dominant fisheries - we don't need ALL fisheries data
           geartype == "trawlers" |
           # geartype == "fishing" |
           geartype == "set_longlines") 


Full_gfw_dat$date <- Full_gfw_dat$`Time Range`
Full_gfw_dat$FishingHours <- Full_gfw_dat$`Apparent Fishing Hours`
Full_gfw_dat$lon <- Full_gfw_dat$Lon
Full_gfw_dat$lat <- Full_gfw_dat$Lat

Full_gfw_dat <- Full_gfw_dat %>%
  dplyr::select(- `Apparent Fishing Hours`, -`Time Range`, - Lon, -Lat)

Full_gfw_dat$Month <- gsub("^.{0,5}", "", Full_gfw_dat$date)
Full_gfw_dat$Month <- as.numeric(Full_gfw_dat$Month)
Full_gfw_dat$Month <- lubridate::month(Full_gfw_dat$Month)


unique(Move_AND_Dive$Month)


Subset_gfw_dat <- Full_gfw_dat %>%
  filter(Month == "7" |
           Month == "8")
  

## First make into spatial data
MONTH_GFW_Locs_SF <- st_as_sf(Subset_gfw_dat, coords=c("lon","lat"), crs=4326)

# ## Make a spatial grid based on the seal tracking data
 MONTH_Fish_Gridded <- st_join(hgrid, MONTH_GFW_Locs_SF) %>%
   group_by(GridID, geartype, Month) %>%
   mutate(count = 1)

 ggplot()+
   geom_sf(data=MONTH_Fish_Gridded)+
   geom_sf(data=MONTH_GFW_Locs_SF) +
   scale_colour_viridis_c(option = "B")
 
 ## Summarise data spatially. Takes the sum of all fishing hours recorded in a grid cell
 Sub_GFW <- MONTH_Fish_Gridded %>%
   group_by(GridID, geartype) %>%
   summarise(FishingHours = sum(FishingHours)) %>%
   arrange(FishingHours) 

 Sub_GFW_Crop <- Sub_GFW %>%
   drop_na(FishingHours)
 
 GriddedPlot_Fisheries <- Basemap + 
   ggnewscale::new_scale_fill() +
   geom_sf(data=Sub_GFW_Crop, aes(fill = FishingHours), alpha = 0.8) +
   scale_fill_viridis_c(option = "A") +
   facet_wrap(~ geartype) +
   labs(fill = "Fishing effort", x = "Longitude", y = "Latitude")
 GriddedPlot_Fisheries
 
 
 Sub_GFW_Crop <- Sub_GFW_Crop %>%
  filter( geartype == "trawlers")
 
 
 GriddedPlot_Fisheries <- Basemap + 
   ggnewscale::new_scale_fill() +
   geom_sf(data=Sub_GFW_Crop, aes(fill = FishingHours), alpha = 0.8) +
   scale_fill_viridis_c(option = "A") +
   labs(fill = "Fishing effort\n(hours)", x = "Longitude", y = "Latitude")
 GriddedPlot_Fisheries
 
 
 
 ##############################################################################################################################################
 ##############################################################################################################################################
 ## Seal, Prey-field and Fisheries data integration
 ##############################################################################################################################################
 ##############################################################################################################################################
 
 
 AnalysisData <- as.data.frame(Gridded)
 
 Sub_GFW_Crop <- as.data.frame(Sub_GFW_Crop)
 
 Sub_GFW_Crop <- Sub_GFW_Crop %>%
   dplyr::select( - x)
 
 AnalysisData <- AnalysisData %>%
   left_join(Sub_GFW_Crop) %>%
   drop_na(NoDive)


  AnalysisData <- AnalysisData %>% 
    mutate(FishingHours = replace_na(FishingHours, 0))
 

##############################################################################################################################################
##############################################################################################################################################
## Statistical analysis
##############################################################################################################################################
##############################################################################################################################################

# write_rds(AnalysisData, "AnalysisData.rds")

# AnalysisData <-  read_rds("AnalysisData.rds") 
 
AnalysisData <- as.data.frame(AnalysisData)


library(performance)    
library(effects)
library(AICcmodavg)
library(ggeffects) 
library(lmerTest)
library(glmmTMB)
library(DHARMa)
library(ggcorrplot)
library(emmeans)
library(ggeffects)

## Response variables
ggplot() + geom_histogram(data = AnalysisData, aes(x = NoDive), colour = "grey", fill = "black")
ggplot() + geom_histogram(data = AnalysisData, aes(x = depth_sum), colour = "grey", fill = "black")
ggplot() + geom_histogram(data = AnalysisData, aes(x = duration_sum), colour = "grey", fill = "black")
ggplot() + geom_histogram(data = AnalysisData, aes(x = botdur_sum), colour = "grey", fill = "black")
ggplot() + geom_histogram(data = AnalysisData, aes(x = wiggle_sum), colour = "grey", fill = "black")


## Predictor variables
ggplot() + geom_histogram(data = AnalysisData, aes(x = FishingHours), colour = "grey", fill = "black")


##############################################################################################################################################
##############################################################################################################################################
## Potential data transformation approaches
##############################################################################################################################################
##############################################################################################################################################

## Standardise data

AnalysisData$depth_sum_stand <- datawizard::standardise(AnalysisData$depth_sum)

AnalysisData$FishingHours_stand <- datawizard::standardise(AnalysisData$FishingHours)



## Capping quantiles

cap_values <- function(x) {
  lower_bound <- quantile(x, 0.01)  # 5th percentile
  upper_bound <- quantile(x, 0.99)  # 95th percentile
  x[x < lower_bound] <- lower_bound  # Cap values below 5th percentile
  x[x > upper_bound] <- upper_bound  # Cap values above 95th percentile
  return(x)
}

AnalysisData$NoDive_Cap <- cap_values(AnalysisData$NoDive)
AnalysisData$depth_sum_Cap <- cap_values(AnalysisData$depth_sum)
AnalysisData$duration_sum_Cap <- cap_values(AnalysisData$duration_sum)
AnalysisData$botdur_sum_Cap <- cap_values(AnalysisData$botdur_sum)
AnalysisData$wiggle_sum_Cap <- cap_values(AnalysisData$wiggle_sum)
AnalysisData$FishingHours_Cap <- cap_values(AnalysisData$FishingHours)


## Removing outliers

remove_outliers <- function(x) {
  lower_bound <- quantile(x, 0.01)  # 5th percentile
  upper_bound <- quantile(x, 0.99)  # 99th percentile
  x[x >= lower_bound & x <= upper_bound]  # Keep only values within the 5th and 95th percentiles
}

AnalysisData_filtered <- AnalysisData[
  AnalysisData$NoDive >= quantile(AnalysisData$NoDive, 0.01) & AnalysisData$NoDive <= quantile(AnalysisData$NoDive, 0.99) &
    AnalysisData$depth_sum >= quantile(AnalysisData$depth_sum, 0.01) & AnalysisData$depth_sum <= quantile(AnalysisData$depth_sum, 0.99) &
    AnalysisData$wiggle_sum >= quantile(AnalysisData$wiggle_sum, 0.01) & AnalysisData$wiggle_sum <= quantile(AnalysisData$wiggle_sum, 0.99) &
    AnalysisData$botdur_sum >= quantile(AnalysisData$botdur_sum, 0.01) & AnalysisData$botdur_sum <= quantile(AnalysisData$botdur_sum, 0.99) &
    AnalysisData$FishingHours >= quantile(AnalysisData$FishingHours, 0.01) & AnalysisData$FishingHours <= quantile(AnalysisData$FishingHours, 0.99),
]


## Log transform and fit models with normal distribution

AnalysisData <- AnalysisData %>%
  mutate(depth_sum_log = log(depth_sum_Cap),
         NoDive_log = log(NoDive),
         botdur_sum_log = log(botdur_sum),
         wiggle_sum_log = log(wiggle_sum))



##############################################################################################################################################
## Depth Model
##############################################################################################################################################

FS_Depth <-  glmmTMB(depth_sum ~ FishingHours_stand + (1 | id), data = AnalysisData, family =  Gamma(link = "log"),
                     ziformula = ~ FishingHours_stand)


check_model(FS_Depth) ## Check Assumptions

# Simulate residuals using the DHARMa package
sim_residuals <- simulateResiduals(FS_Depth)
plot(sim_residuals)

# Test for overdispersion
testDispersion(sim_residuals)

# Test for zero inflation
testZeroInflation(sim_residuals)

# Plot random effects
ranef(FS_Depth)

# Plot residuals vs fitted values
plotResiduals(sim_residuals, form = fitted(FS_Depth))

# QQ plot for residuals - Not as important for models that aren't guassian
qqnorm(residuals(FS_Depth))
qqline(residuals(FS_Depth))


summary(FS_Depth)

drop1(FS_Depth, test = "Chisq") # perform likelihood ratio tests

plot(ggpredict(FS_Depth, terms = c("FishingHours_stand")))



##############################################################################################################################################
## Number dives
##############################################################################################################################################


FS_DiveFreq <-  glmmTMB(NoDive ~ FishingHours_stand + (1 | id), data = AnalysisData, family =  nbinom2(), 
                        ziformula =  ~ FishingHours_stand)


check_model(FS_DiveFreq) ## Check Assumptions

# Simulate residuals using the DHARMa package
sim_residuals <- simulateResiduals(FS_DiveFreq)
plot(sim_residuals)

# Test for overdispersion
testDispersion(sim_residuals)

# Test for zero inflation
testZeroInflation(sim_residuals)

# Plot random effects
ranef(FS_DiveFreq)

# Plot residuals vs fitted values
plotResiduals(sim_residuals, form = fitted(FS_DiveFreq))

# QQ plot for residuals - Not as important for models that aren't guassian
qqnorm(residuals(FS_DiveFreq))
qqline(residuals(FS_DiveFreq))

 
summary(FS_DiveFreq)

drop1(FS_DiveFreq, test = "Chisq") # perform likelihood ratio tests


##############################################################################################################################################
## Bottom duration
##############################################################################################################################################

AnalysisData <- AnalysisData %>%
  mutate(botdur_sum = ifelse(botdur_sum < 0.1, 0.1  , botdur_sum))


FS_BotDur <-  glmmTMB(botdur_sum ~ FishingHours_stand + (1 | id), data = AnalysisData, family =  Gamma(link = log),
                      ziformula =  ~ FishingHours_stand)

check_model(FS_BotDur) ## Check Assumptions

# Simulate residuals using the DHARMa package
sim_residuals <- simulateResiduals(FS_BotDur)
plot(sim_residuals)

# Test for overdispersion
testDispersion(sim_residuals)

# Test for zero inflation
testZeroInflation(sim_residuals)

# Plot random effects
ranef(FS_BotDur)

# Plot residuals vs fitted values
plotResiduals(sim_residuals, form = fitted(FS_BotDur))

# QQ plot for residuals - Not as important for models that aren't guassian
qqnorm(residuals(FS_BotDur))
qqline(residuals(FS_BotDur))


summary(FS_BotDur)

drop1(FS_BotDur, test = "Chisq") # perform likelihood ratio tests


##############################################################################################################################################
## Wiggles
##############################################################################################################################################

AnalysisData <- AnalysisData %>%
  mutate(wiggle_sum = ifelse(wiggle_sum < 0.1, 0.1  , wiggle_sum))

FS_Wiggles <-  glmmTMB(wiggle_sum ~ FishingHours_stand + (1 | id), data = AnalysisData, family =  Gamma(link = log),
                       ziformula =  ~ FishingHours_stand)

check_model(FS_Wiggles) ## Check Assumptions

# Simulate residuals using the DHARMa package
sim_residuals <- simulateResiduals(FS_Wiggles)
plot(sim_residuals)

# Test for overdispersion
testDispersion(sim_residuals)

# Test for zero inflation
testZeroInflation(sim_residuals)

# Plot random effects
ranef(FS_Wiggles)

# Plot residuals vs fitted values
plotResiduals(sim_residuals, form = fitted(FS_Wiggles))

# QQ plot for residuals - Not as important for models that aren't guassian
qqnorm(residuals(FS_Wiggles))
qqline(residuals(FS_Wiggles))

summary(FS_Wiggles)

drop1(FS_Wiggles, test = "Chisq") # perform likelihood ratio tests



