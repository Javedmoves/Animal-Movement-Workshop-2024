############################################################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################################################
# R script adatped from Riaz et al 2024 Ecography 
# Script for processing raw SAFS GPS tracking data using animotum
############################################################################################################################################################################################################################################################################################
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
library(ggOceanMaps)
library(ggspatial)


Basemap1 <- basemap(limits = c(-72, -48.8, -57, -34), bathymetry = TRUE, rotate = TRUE, grid.col = NA,) + 
  theme(legend.position = "none") 
Basemap1

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## Read in DF and some basic time and date formatting

setwd("//saeri-file/users$/jriaz/Documents/Github/Connectivity/")

data_path <- ("//saeri-file/users$/jriaz/Documents/Github/Connectivity/")

SAFS_GPS <- read_rds("SAFS_GPS.rds")


Basemap1 + ggspatial::geom_spatial_point(data = SAFS_GPS, aes(x = lon, y = lat, colour = id)) +
  theme(legend.position = "none")


unique(SAFS_GPS$id)

newd <- SAFS_GPS %>%
  group_by(id) %>%
  arrange(date)


##############################################################################################################################################
## Remove any duplicates or near duplicates that occured within 10 s

d1new <- newd %>%
  group_by(id) %>%
  do(distinct(., date, .keep_all = TRUE)) %>%
  do(mutate(
    .,
    dup = difftime(date, lag(date), units = "secs") < 10)) %>%
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
## Remove all locations flagged to discard during the above prefiltering stage before running the SSM


NewTracks <- d5new %>%
  filter(keep == "TRUE") %>%
  mutate(lc = "G") %>%
  dplyr :: select(id, date, lc, lon, lat) %>%
  ungroup()



##############################################################################################################################################
##############################################################################################################################################
# Run tracks through SSM. 
# Run the Predicted first and then run for the Fitted

# NewTracks <- NewTracks

d3 <- data.frame(NewTracks) 

split_d <- split(d3, d3$id)[unique(d3$id)] ## creates ID list

# Fit the SSM
fits <- aniMotum::fit_ssm(d3, 
                          model="rw", 
                          vmax = 4,
                          spdf = FALSE,
                          # control = ssm_control(optim=c("nlminb")), 
                          time.step = 0.25)

## Plotting to check
my.aes <- aes_lst(obs=TRUE, line=TRUE,mp=FALSE, conf = FALSE)
for(i in unique(fits$id)){
  subID <- subset(fits, id == i)
  gi <- aniMotum::map(subID, what = "predicted", aes = my.aes, by.date = TRUE)
  ggsave(filename = sprintf('%s.pdf', i), plot = gi, width = 9, height = 9)
}

# Grab the predicted locations from the SSM and make model output into dataframe
GPS_Seal_DF_SSM <- grab(fits, what = "predicted", as_sf = FALSE, normalise = FALSE, group = FALSE)


# Fit the move persistence model to the predicted
fits_mp <- fit_mpm(fits, what = "predicted", model = "mpm") 


# Grab the fitted mpm model
GPS_SSM <- grab(fits_mp, what = "fitted", as_sf = FALSE, normalise = FALSE, group = FALSE)


# Grab the fitted mpm model, but this time normalise the g values. 
GPS_SSM_Norm <- grab(fits_mp, what = "fitted", as_sf = FALSE, normalise = TRUE, group = FALSE)


## Join the SSM locs with the MPM
GPS_SSM_Norm <- GPS_SSM_Norm %>%
  dplyr::select(id, date, g) %>%
  rename(g_norm = g)


Seal_GPS_SSM_Clean <- GPS_Seal_DF_SSM %>%
  left_join(GPS_SSM) %>%
  left_join(GPS_SSM_Norm)

Seal_GPS_SSM_Clean$ID <- Seal_GPS_SSM_Clean$id
Seal_GPS_SSM_Clean$trip <- Seal_GPS_SSM_Clean$id
Seal_GPS_SSM_Clean$col <- "BIRD"
Seal_GPS_SSM_Clean$time <- Seal_GPS_SSM_Clean$date
Seal_GPS_SSM_Clean$tag <- "GPS"
Seal_GPS_SSM_Clean$stage <- "WINTER"


Basemap1 + ggspatial::geom_spatial_point(data = Seal_GPS_SSM_Clean, aes(x = lon, y = lat, colour = g_norm)) +
  theme(legend.position = "none") + scale_colour_viridis_c()


write_rds(Seal_GPS_SSM_Clean, "Seal_GPS_SSM_Clean.rds")


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## Simulate some tracks!


fits1 <- aniMotum::fit_ssm(d3, 
                           model="crw", 
                           control=ssm_control(),
                           time.step = 0.25)

st.cpf <- sim_fit(fits1, what = "predicted", reps=10, cpf = FALSE)

write_rds(st.cpf, "st.cpf.rds")

st.cpf_frr <- route_path(st.cpf, map_scale = 10, centroids = TRUE)


sf_sim_lines <- do.call(rbind,st.cpf_frr$sims)

Sims1 <- unnest(st.cpf_frr)

# write_rds(Sims1, "SimulatedTracks.rds")



SimulatedTracks <- read_rds("SimulatedTracks.rds")


Basemap2 <- basemap(limits = c(-80, -37, -68, -32), bathymetry = FALSE, rotate = TRUE, grid.col = NA,) + 
  theme(legend.position = "none") 
Basemap2


Basemap2 + ggspatial::geom_spatial_point(data = SimulatedTracks, aes(x = lon, y = lat, colour = id), size = 0.1) +
  theme(legend.position = "none") + scale_colour_viridis_d(option = "A")


### Simulate move persistence values
generate_ar1_left_skewed <- function(n, rho, shape1 = 5, shape2 = 2) {
  ar_values <- numeric(n)
  ar_values[1] <- rbeta(1, shape1 = shape1, shape2 = shape2)  # Initial random value
  for (i in 2:n) {
    ar_values[i] <- rho * ar_values[i-1] + (1 - rho) * rbeta(1, shape1 = shape1, shape2 = shape2)
  }
  return(ar_values)
}

# Set parameters for autocorrelation
rho <- 0.8  # Autocorrelation parameter (adjust as needed)


SimulatedTracks <- SimulatedTracks %>%
  group_by(id) %>%
  mutate(g = generate_ar1_left_skewed(n(), rho)) %>%
  mutate(gnorm = (g - min(g))/(max(g) - min(g))) %>% ## Standardise move peristence data. Scales 0 - 1
  ungroup()


Basemap2 + ggspatial::geom_spatial_point(data = SimulatedTracks, aes(x = lon, y = lat, colour = gnorm), size = 0.1) +
  theme(legend.position = "right") + scale_colour_viridis_c()


write_rds(SimulatedTracks, "SimulatedTracks.rds")




##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
# End of script
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################















