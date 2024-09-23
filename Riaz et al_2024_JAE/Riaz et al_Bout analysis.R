# Manuscript - Bout time for krill - contrasting Adélie penguin foraging behaviour during years of high and low krill availability
# Author - Javed Riaz

##############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

# Read in some relevant packages
library(ggplot2)
library(tidyverse)
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
library(ggpubr)
library(rasterVis)
library(RColorBrewer)
library(devtools)
library(cividis)
library(wesanderson)
library(geosphere)
library(glmmTMB)
library(performance)
library(marmap)

setwd("//saeri-file/users$/jriaz/Documents/Github/Bouts/")


###############################################################################################################################################
###############################################################################################################################################
# LOAD IN KRILL ACOUSTIC DATA

## Massage and basic spatial summaries

###############################################################################################################################################
###############################################################################################################################################

All_Acoustic <- read_rds("Krill_Data.rds")

ggplot() + geom_point(data = All_Acoustic, aes(x = Lon_M, y = Lat_M, colour = PRC_NASC)) + 
  facet_wrap(~Season)


All_Acoustic$Lon <- All_Acoustic$Lon_M
All_Acoustic$Lat <- All_Acoustic$Lat_M

# Convert to spatial object
Acoustic_SF <- st_as_sf(All_Acoustic, coords=c("Lon","Lat"), crs=4326)

## Transform coordinates to metres (changing projection)
transect_data_UTM <- st_transform(Acoustic_SF, crs = 32633)  


## Calculate the total length (m) of transect
line_by_season <- transect_data_UTM %>%
  arrange(Season, Date_M, Time_M) %>%  
  group_by(Season) %>%
  summarise(geometry = st_cast(st_combine(geometry), "LINESTRING"))

line_by_season <- line_by_season %>%
  mutate(total_length_m = as.numeric(st_length(geometry))/1000)


# Spatially grid data into a 0.05 degree resolution
hexgrid <- st_make_grid(Acoustic_SF, cellsize = 0.05, square = FALSE) %>% 
  st_transform(crs=4326) %>%
  st_as_sf() %>%
  mutate(GridID = c(1:NROW(.)))


ggplot()+
  geom_sf(data=hexgrid)+
  geom_sf(data=Acoustic_SF, colour = "red")


joined <- st_join(hexgrid, Acoustic_SF) %>%
  group_by(Season, GridID) %>%
  summarise(NASC_sum = sum(PRC_NASC)) %>%
  drop_na(NASC_sum)

joined$Year <- joined$Season

# ## First season
# Joined_Y1 <- joined %>%
#   filter(Year == "2019/20")
# 
# Joined_Y1$area <- st_area(Joined_Y1) 
# 
# ggplot() + geom_sf(data=joined, aes(fill = NASC_sum)) +
#   geom_point(data = All_Acoustic, aes(x = Lon_M, y = Lat_M), size = 0.5) + 
#   facet_wrap(~Season) +
#   scale_fill_viridis()


### Spatial summaries of the krill data

SumAcoustic <- All_Acoustic %>%
  group_by(Season, Region_ID, Lon, Lat) %>%
  summarise(NASC_Sum = sum(PRC_NASC)) %>%
  distinct()


## Calculate distance of transects from penguin colony

distHaversine(cbind(-58.933333, -62.216667),cbind(-58.5826,-62.3207))/1000 ## Penguin colony location

SumAcoustic$nestlon <- -58.933333
SumAcoustic$nestlat <- -62.216667

SumAcoustic <- SumAcoustic %>%
  mutate(Distance_Col=distHaversine(cbind(nestlon, nestlat),cbind(Lon,Lat))/1000)


## Compute some overall krill summaries

KrillSummaries <- SumAcoustic %>%
  ungroup() %>%
  group_by(Season) %>%
  summarise(max = max(NASC_Sum),
            meanNASC = mean(NASC_Sum),
            sumNASC = sum(NASC_Sum),
            sdNASC = sd(NASC_Sum),
            n = n()) %>%
  mutate(se.mpg = sdNASC / sqrt(n),
         lower.ci.mpg = meanNASC - qt(1 - (0.05 / 2), n - 1) * se.mpg,
         upper.ci.mpg = meanNASC + qt(1 - (0.05 / 2), n - 1) * se.mpg)


## Distance-stratified krill summaries

KrillSummaries_Distance <- SumAcoustic %>%
  ungroup() %>%
  mutate(Dist_cat = cut(Distance_Col, breaks = c(0,10,20,30, 40, 60))) %>%
  group_by(Season, Dist_cat) %>%
  summarise(max = max(NASC_Sum),
            meanNASC = mean(NASC_Sum),
            sumNASC = sum(NASC_Sum),
            sdNASC = sd(NASC_Sum),
            n = n()) %>%
  mutate(se.mpg = sdNASC / sqrt(n),
         lower.ci.mpg = meanNASC - qt(1 - (0.05 / 2), n - 1) * se.mpg,
         upper.ci.mpg = meanNASC + qt(1 - (0.05 / 2), n - 1) * se.mpg)



###############################################################################################################################################
###############################################################################################################################################
# LOAD IN PENGUIN MOVEMENT AND DIVE DATA
###############################################################################################################################################
###############################################################################################################################################

load("DiveALL_A_Hunt.RData") 

Dives <- Dive_hunt ##

unique(Dives$ID) ##57 individuals
unique(Dives$Trip) ##205 trips
unique(Dives$Year) ##3 years


Year19_20 <- Dives %>%
  filter(Year == "2019/20") 
length(unique(Year19_20$ID)) ## 19 individuals


Year20_21 <- Dives %>%
  filter(Year == "2020/21") 
length(unique(Year20_21$ID)) ## 16 individuals

Year21_22 <- Dives %>%
  filter(Year == "2021/22") 
length(unique(Year21_22$ID)) ## 22 individuals


Dives <- Dives %>%
  mutate(randcount = 1) %>%
  group_by(ID, Year) %>%
  mutate(NoALLDives = sum(randcount)) 


## Criterion to define foraging dives
numhunts <- 4 ### consistent with Machado-Gaye et al. (2024) 

HuntOnly <-  Dives %>% 
  filter(hunt > numhunts) %>% ## 
  group_by(ID) %>%
  mutate(AllHUNTCount = sum(hunt)) %>%
  ungroup()


Year19_20 <- Year19_20 %>%
  mutate(HuntPresAbs = case_when(hunt > numhunts ~ 'Hunt',
                                 hunt <= numhunts ~ 'No hunt'))

Year21_22 <- Year21_22 %>% #####
mutate(HuntPresAbs = case_when(hunt > numhunts ~ 'Hunt',
                               hunt <= numhunts ~ 'No hunt'))

Hunting <- HuntOnly %>%
  group_by(ID) %>%
  mutate(AllHuntingDives = sum(randcount)) %>%
  ungroup() %>%
  dplyr::select(ID, AllHUNTCount, AllHuntingDives) %>%
  distinct()


Dives1 <- Dives %>%
  arrange(ID, Trip, start, NoALLDives) %>%
  left_join(Hunting)


IDS <- Dives1 %>%
  group_by(ID, Trip) %>%
  distinct(ID, Trip) %>%
  ungroup()


load("sum_water.RData") ## Cleaned trip durations

sum_water$Trip <- sum_water$ID_Trip

sum_water <- sum_water %>%
  left_join(IDS) %>%
  drop_na(ID) %>%
  group_by(ID, Trip) %>% 
  summarise(TripDuration = sum(trip_water)/60/60) %>%
  distinct() %>%
  ungroup()


###############################################################################################################################################
###############################################################################################################################################
# REMOVE NON-FEEDING DIVES 

## Also checks post dive diff outliers
###############################################################################################################################################
###############################################################################################################################################

AllDiveData <- Year19_20 %>% ## Merge the years of interest
  full_join(Year21_22)

Bouts <- AllDiveData %>%   ##
  ungroup() %>%            ##
  arrange(ID, start) %>%
  group_by(ID) %>%
  filter (hunt > numhunts) %>% 
  mutate(DiveEnd = start + seconds(dur)) %>%
  group_by(ID, Trip) %>%
  mutate(diff = start - lag(DiveEnd),
         diff_secs = as.numeric(diff, units = 'secs')) %>%
  drop_na(diff_secs)

interbouts <- Bouts [!(Bouts$diff_secs == "0"),]
plot(interbouts$diff_secs)


## Cut-off for postdive.diff

outlier_2 <- as.numeric(quantile (interbouts$diff_secs, prob = 0.99))
outlier_2 ## 2397 seconds


###############################################################################################################################################
###############################################################################################################################################
# APPROACH 1 and 2 - CALCULATE BECs FOR EACH INDIVIDUAL AND THEN TAKE THE MEAN
# USING DIVEMOVE NLS METHOD

# Removes postdive periods > 2397 seconds
# This produces BEC values ranging from 21 - 406

###############################################################################################################################################
###############################################################################################################################################


df_total = data.frame() ## Set up empty dataframe for the below loop
df_total

for (a in unique(Bouts$ID)){
  subID <- subset(Bouts, ID == a)
  idDF <- as.data.frame(subID$ID)
  postdives.diff <- abs(diff(subID$diff_secs)) ## 
  postdives.diff <- postdives.diff[postdives.diff < 2397] # 
  lnfreq <- boutfreqs(postdives.diff, bw=1, plot=FALSE) ###
  startval <- boutinit(lnfreq, 300, plot = TRUE) ### 
  # ## NLS 2-process
  bouts2.fit <- fitNLSbouts(lnfreq, start=startval, maxiter=5000)
  ### MLE
  # bouts2.fit <- fitMLEbouts(lnfreq, start=startval, optim_opts0=NULL, optim_opts1=NULL)
  bec<- bec(bouts2.fit)
  setBEC <- bec
  
  bec <- as.data.frame(print(bec))
  BoutsData <- subID %>%
    mutate(group = cumsum(diff_secs > setBEC) + 1)
  bind <- cbind(BoutsData, bec)
  df_total <- rbind(df_total,bind)
}


newdatatest <- df_total


FistSeason <- newdatatest %>%
  filter(Year == "2019/20") %>%
  distinct(`print(bec)`, .keep_all = TRUE) 
mean(FistSeason$`print(bec)`) 


SecondSeason <- newdatatest %>%
  filter(Year == "2021/22") %>%
  distinct(`print(bec)`, .keep_all = TRUE) 
mean(SecondSeason$`print(bec)`) 


PopBEC <- FistSeason %>%
  full_join(SecondSeason)

PopBEC$Approach1_ID <- PopBEC$`print(bec)`

unique(PopBEC$Approach1_ID) 
range(PopBEC$Approach1_ID) 
mean(PopBEC$Approach1_ID) 


PopBEC$Approach1_Mean <- mean(PopBEC$Approach1_ID)



###############################################################################################################################################
###############################################################################################################################################
# APPROACH 3 - CALCULATE ONE BEC FOR ALL DATA AGGREGATED -

# USES THE DIVEMOVE MLE APPROACH
# Produces a single BEC of 68 seconds

###############################################################################################################################################
###############################################################################################################################################

Bouts <- Bouts %>%
  group_by(ID, Trip) %>%
  arrange(start)

postdives.diff <- abs(diff(Bouts$diff_secs)) ## 
postdives.diff <- postdives.diff[postdives.diff < 2397] # 
lnfreq <- boutfreqs(postdives.diff, bw=0.1, plot=FALSE) ###
startval <- boutinit(lnfreq, 200, plot = TRUE) ### 
# ## NLS 2-process
#  bouts2.fit <- fitNLSbouts(lnfreq, start=startval, maxiter=5000)
#  bec<- bec(bouts2.fit)
### MLE
bouts2.fit <- fitMLEbouts(lnfreq, start=startval, optim_opts0=NULL, optim_opts1=NULL)
bec<- bec(bouts2.fit)
setBEC <- bec

bec <- as.data.frame(print(bec))

setBEC

PopBEC <- PopBEC %>%
  mutate(Approach2 = 68)


###############################################################################################################################################
###############################################################################################################################################
# APPROACH USE IN THE MANUSCRIPT - USE THE SEGMENTED PACKAGE INSTEAD OF DIVEMOVE

## This provide a BEC of 90 seconds
###############################################################################################################################################
###############################################################################################################################################

library(segmented)

BreakPoint <- Bouts 

cumulative_frequency <- cbind(as.data.frame(table(BreakPoint$diff_secs)), cumsum(table(BreakPoint$diff_secs))) %>%
  mutate(CumFreq = `cumsum(table(BreakPoint$diff_secs))`) %>%
  mutate_if(is.factor, as.numeric) %>%
  dplyr::select(-3)

ggplot(cumulative_frequency, aes(x = Var1, y = CumFreq)) +
  geom_line(group = 1) +
  scale_y_continuous(trans = "log2")

model.lm <- lm(log(CumFreq) ~ Var1, data = cumulative_frequency) ## GLM with poisson distribution also tested for comparison. Doesn't change results
summary(model.lm)


# Mode coeffs
model.coeffs <- coef(model.lm)
model.coeffs


ggplot(cumulative_frequency, aes(y = log(CumFreq), x = Var1)) +
  geom_line(group = 1) +
  geom_abline(intercept = model.coeffs[1],
              slope = model.coeffs[2], col = "orange")+
  geom_vline(xintercept = 40, linetype = "dashed") +
  geom_vline(xintercept = 90, linetype = "dashed")


# Estimate breakpoints
segments <- segmented(model.lm, 
                    seg.Z = ~ Var1, 
                    psi = list(Var1 = c(40,90))) ##Estimate where the breakpoints are


summary(segments)

# Get model breakpoints
segments$psi ## breakpoint at 90 
slope(segments)

# get the fitted data
fitted_segs <- fitted(segments)
model_segs <- data.frame(Times = cumulative_frequency$Var1, Freq = fitted_segs)

bec <- 90


PopBEC <- PopBEC %>%
  mutate(Approach3 = bec)


###############################################################################################################################################
###############################################################################################################################################
# PLOT THE DATA TO MAKE COMPARISONS 
###############################################################################################################################################
###############################################################################################################################################


PopBEC_Summary <- PopBEC %>%
  ungroup() %>%
  dplyr::select(ID, Year, 25:28) %>%
  distinct()


FigS1 <- ggplot() + geom_histogram(data = PopBEC_Summary, aes(x = Approach1_ID, fill = Year), colour = "black", position = "dodge") +
  labs (x = "Bout-ending criteria (s)", y = "Number of individuals (#)") +
  theme_bw() + 
  scale_y_continuous(expand = c(0,0), breaks = c(2, 4,6,8,10,12), limits = c(0,11)) +
  geom_vline(xintercept = 77, colour = "red", linetype = "dashed", size = 1) +
ggtitle("Alternative BEC calculations - approach 1 and approach 2") +
  # scale_x_continuous(breaks = c(50, 100, 150, 200, 250)) +
  scico::scale_fill_scico_d(palette = "hawaii")+
  theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) 
FigS1   

 # pdf("FigS1.pdf", width = 8, height = 8, onefile = FALSE)
 # FigS1
 # dev.off()
 # 


###############################################################################################################################################
###############################################################################################################################################
# ASSIGN BOUTS
###############################################################################################################################################
###############################################################################################################################################

# Bouts <- read_rds("Bouts.rds")

Ass_Bouts <- Bouts %>%
  left_join(PopBEC_Summary)

Ass_Bouts <- Bouts %>%
  group_by(ID, Trip) %>%
  mutate(Bout = ifelse(diff_secs < 90 | lag(diff_secs < 90),"TRUE", NA)) %>%
  drop_na() %>%
  ungroup() %>%
  group_by(ID) %>%
  mutate(group = cumsum(diff_secs >= 90) + 1) %>%
  mutate(randcount = 1)


Ass_Bouts$nestlon <- -58.933333
Ass_Bouts$nestlat <- -62.216667



###############################################################################################################################################
###############################################################################################################################################
# Bout summary calculations 
###############################################################################################################################################
###############################################################################################################################################

## This removes all non-bout dives and then calculates some bout metrics
BoutID <- Ass_Bouts %>% 
  ungroup() %>%
  group_by(ID, Trip) %>%
  left_join(Dives1) %>%
  mutate(totaldives = NoALLDives) %>% ## Total bout-structured dives for an individual
  dplyr::select(-NoALLDives) %>%
  ungroup() %>%
  group_by(ID, Trip, group) %>%
  mutate(count=n()) %>%
  mutate(divesperbout = sum(randcount)) %>%
  mutate(Distance=distHaversine(cbind(nestlon, nestlat),cbind(lon,lat))/1000) %>%
  filter(divesperbout >= 3) %>%   # Retains only clusters of 3 or more dives
  mutate(Sumdur = sum(dur)) %>% # total bout duration time in seconds
  mutate(boutduration_mins = sum(dur)/60, ## total bout duration time in minutes
         depthMean = mean(dep), ## Mean dive depth in a bout
         meanhunt = mean(hunt), # Mean hunts per dive in a bout
         meanduration = mean(dur), # Mean dive duration of a bout
         sumhunt = sum(hunt), ## Total hunts attempts per bout
         boutsuccess = sumhunt/boutduration_mins) %>% ### Hunts per minute of bouting
  ungroup() %>%
  group_by(ID, Trip) %>%
  arrange(ID, Trip, group) %>%
  mutate(TemporalBout = seq_along(group)) %>% 
  ungroup()


TripID <- BoutID %>%
  # dplyr::select(Distance) %>%
  group_by(ID, Trip, group) %>%
  arrange(ID, Trip, group, start) %>%
  filter(start == min(start)) %>%
  #   distinct(ID, Trip, group, .keep_all = TRUE) %>%
  left_join(sum_water) %>%
  group_by(ID, Trip) %>%
  mutate(allboutdives = sum(divesperbout)) %>% ## Total bout-structured dives for an individual
  mutate(boutprop = allboutdives/totaldives) %>% ### Proportion of bouts-structured dives in the foraging trip
  mutate(boutForageprop = allboutdives/AllHuntingDives) %>% ### Proportion of bouts-structured dives across foraging dives
  
  mutate(bouthuntprop = allboutdives/sum(sumhunt)) %>% ### Proportion of bouts-structured dives in the foraging trip
  mutate(birdbouts = length(unique(group))) %>% ## Total number of bouts by a single individual 
  arrange(ID, Trip, start) %>%
  mutate(Boutdiff = start - lag(start),
         Bout_secs = as.numeric(Boutdiff, units = 'secs')) %>%
  mutate(Boutinterval_mins = Bout_secs - lag(Sumdur)) %>%
  mutate(Boutinterva_mins = Boutinterval_mins/60) 
  # filter(Boutinterva_mins > 0)


ggplot() + geom_point(data = TripID, aes(x = lon, y = lat, colour = Distance)) +
  facet_wrap(~ Year)



FigS2 <-  ggplot() + geom_histogram(data = TripID, aes(x = Distance, fill = Year), colour = "black", position = "dodge") +
  # facet_wrap(~ Year) +
  scale_x_continuous(breaks = c(0,10,20,30,40,50)) +
  scale_y_continuous(expand = c(0,0), breaks = c(25,50,75,100,125,150,175), limits = c(0,175)) +
  scico::scale_fill_scico_d(palette = "hawaii")+
  labs (y = "Frequency of bouts (#)", x = "Distance from colony (km)") +
  theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) 
FigS2   

# pdf("FigS2.pdf", width = 8, height = 8, onefile = FALSE)
# FigS2
# dev.off()


Bout_Distance_Summary <- TripID %>%
  ungroup() %>%  
  mutate(Dist_cat = cut(Distance, breaks = c(0,10,20,30, 40, 60))) %>%
  group_by(Year, Dist_cat) %>%
  summarise(meanInter = max(Boutinterva_mins),
            meanHunt = mean(meanhunt),
            n = n()) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(Prop = n/sum(n))


 ###############################################################################################################################################
 ###############################################################################################################################################
 # Figure 1

 ###############################################################################################################################################
 ###############################################################################################################################################
 
 library(wesanderson)
 library(scico)
 library(ggpubr)
 scico_palette_names(categorical = FALSE)
 
 
 AllForagingDives <- Bouts %>%
   group_by(ID, Trip) %>%
   mutate(Bout = ifelse(diff_secs < 90 | lag(diff_secs < 90),"TRUE", NA)) %>%
   # drop_na() %>%
   ungroup() %>%
   group_by(ID) %>%
   mutate(group = cumsum(diff_secs >= 90) + 1) %>%
   mutate(randcount = 1)
 
 AllForagingDives$Bout[is.na(AllForagingDives$Bout)] <- FALSE
 
 AllForagingDives <- AllForagingDives %>%
   group_by(ID, Trip, group, Bout) %>%
   mutate(divesperbout = sum(randcount)) 
 
  Fig1A <- AllForagingDives %>%
   ungroup() %>%
   arrange(ID, start) %>%
    filter(Trip == "A012_1920_1") %>%
    mutate(Bout = ifelse(divesperbout <= 3, FALSE, Bout)) %>%
    mutate(group = ifelse(Bout == FALSE, round(runif(n(), min = 0, max = 1), 3), group)) %>%
    group_by(group) %>%
    mutate(depthmean = mean(dep),
           sumhunt = sum(hunt)) %>%
    ungroup()
  
  
SurfaceTime <-   Fig1A %>%
  mutate(start = start + 5) %>%
  mutate(dep = 0)

Fig1A <- Fig1A %>% ## 
  full_join(SurfaceTime) %>%
  arrange(start) 

# Fig1A <- mutate(Fig1A, UniqueVals = rownames(Fig1A))


  Fig1A_Plot <- ggplot() +
    geom_line(data = Fig1A,aes(x = start, y = dep, group = group, colour = Bout), linewidth = 0.1) +
    geom_hline(yintercept= 0) +
    # facet_wrap(~ ID, scales = "free") +
    scale_y_reverse(limits=c(80,0), expand = c(0,0)) +
    scale_colour_manual(values = c("black", "orange"), labels = c("Bout", "Non-bout")) +
    scale_x_datetime(
      date_breaks = "1 hour",
      date_labels = "%H:%M") +
    labs(x = NULL, y = "Dive depth (m)", colour = "Dive\nstructure") +
    theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()) +
    guides(colour = guide_legend(ncol = 1, override.aes = list(size = 5, fill=NA, alpha = 1, stroke = 6))) +
    annotate("text",x=min(Fig1A$start + 1000),y=max(Fig1A$dep + 2),hjust=.2,label="BEC = 90 seconds", colour = "red", fontface = 2)
  
  Fig1A_Plot
  
  
  
  Fig1B <- Fig1A %>%
    ungroup() %>%
    filter(Bout == "TRUE") %>%
    group_by(group) %>%
    # filter(start == min(start))
   filter(dep > 0.001) %>%
  filter(row_number()==ceiling(n()/2))
  
  

  Fig1B_Plot <- ggplot() +
   # geom_point(data = NonID,aes(x = start, y = dep), alpha = 0.8) +
    geom_line(data = Fig1A,aes(x = start, y = dep, group = group), linewidth = 0.1, colour = "white") +
       geom_point(data = Fig1B,aes(x = start, y = depthmean, size = divesperbout, fill  = sumhunt ), colour   ="black", shape = 21, alpha = 0.8, stroke = 1) +
   # facet_wrap(~ ID, scales = "free") +
   scale_y_reverse(limits=c(80,0), expand = c(0,0)) +
   # ylim(40, 0) +
    # scale_fill_cividis() +
   scico::scale_fill_scico(palette = "lajolla", direction = 1) +
   scale_x_datetime(
     date_breaks = "1 hour",
     date_labels = "%H:%M") +
   labs(x = "Time", y = "Mean bout depth (m)", fill = "PCA\nper bout", size = "No. of\ndives per\nbout") +
   theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) 
 # theme(
 #   strip.background = element_blank(),
 #   strip.text.x = element_blank()) 
 
  Fig1B_Plot
 
 
 
 Figure_1 <- ggarrange(Fig1A_Plot, Fig1B_Plot, nrow = 2, common.legend = FALSE, align = "hv", labels = "AUTO")
 Figure_1 <- annotate_figure(Figure_1, top = text_grob("Example of penguin dive and bout activity", 
                                                       color = "black", face = "bold", size = 12))
 Figure_1
 
 
 penguin_image <- cowplot::ggdraw() + 
   cowplot::draw_image("penguin.png", 
                       x = 1, y = 1, hjust = 1, vjust = 1, width = 0.5, height = 0.5)
 
 
 # Arrange the figure with the plot and the image
 Figure_1_with_image <- cowplot::ggdraw() +
   cowplot::draw_plot(Figure_1, 0, 0, 1, 1) +
   cowplot::draw_plot(penguin_image, x = 0.755, y = 0.74, width = 0.25, height = 0.25)
 Figure_1_with_image
 
 
 # pdf("Fig1.pdf", width = 9, height = 7, onefile = FALSE)
 # Figure_1_with_image
 # dev.off()
 #  
 #  
 #  tiff("Fig_1.tiff", width = 9, height = 7, units = 'in', res = 300)
 #  Figure_1_with_image
 #  dev.off()
 # 

###############################################################################################################################################
###############################################################################################################################################
# Figure 2 map
###############################################################################################################################################
###############################################################################################################################################


BoutDist <- BoutID %>%
  # filter(BoutOrder == "End") %>%
  ungroup() %>%
  group_by(ID) %>%
  mutate(DiveNoQuant = case_when(divesperbout < quantile(divesperbout, 0.33) ~ "Low",
                                 divesperbout >= quantile(divesperbout, 0.33) & divesperbout <= quantile(divesperbout, 0.66) ~ "Medium",
                                 divesperbout > quantile(divesperbout, 0.66) ~ "High")) %>%
  mutate(SuccessQuant = case_when(sumhunt < quantile(sumhunt, 0.33) ~ "Low",
                                  sumhunt >= quantile(sumhunt, 0.33) & sumhunt <= quantile(sumhunt, 0.66) ~ "Medium",
                                  sumhunt > quantile(sumhunt, 0.66) ~ "High"))

BoutDist$DiveNoQuant <- ordered(BoutDist$DiveNoQuant, levels = c("Low", "Medium", "High"))
BoutDist$SuccessQuant <- ordered(BoutDist$DiveNoQuant, levels = c("Low", "Medium", "High"))

ggplot() +geom_histogram(data = BoutDist, aes(x = DiveNoQuant), stat = "count")

# 
# coast_wgs<-read_sf("add_coastline_high_res_polygon_v7_5.shp")
# st_crs(coast_wgs)  
# st_geometry(coast_wgs)
# AntarcticMap <- coast_wgs %>%
#   st_transform(4326)
# box = c(xmin = -59.35, ymin = -63, xmax = -57.6, ymax = -61.9)
# cropped_sf <- st_crop(AntarcticMap, box)
# cropped_sf <- cropped_sf %>%
#   dplyr::select(-surface)
# Antarcticamap <- ggplot() +
#   geom_sf(data = cropped_sf, fill = "darkgrey")
# Antarcticamap
# write_rds(Antarcticamap, "Antarcticamap.rds")



Antarcticamap <- read_rds("Antarcticamap.rds")

joined$NASC_Standard <- datawizard::normalize(joined$NASC_sum)

All_Acoustic$Year <- All_Acoustic$Season

Map1<- Antarcticamap + 
  geom_sf(data=joined, aes(fill = NASC_Standard)) +
  facet_wrap(~ Year) +
  scale_fill_viridis() +
  geom_point(data = All_Acoustic, aes(x = Lon_M, y = Lat_M, colour = "black"), size = 0.3) +
  facet_wrap(~ Year, ncol = 2) + 
  scale_colour_manual(values = c("#191900", "#F1C659", "#C34C4A"), labels = c("")) +
  # scico::scale_fill_scico_d(palette = "lajolla", direction = 1) +
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) +
  theme(legend.direction = "vertical") + labs(fill = "NASC\n(scaled)", colour = "Acoustic\ntransect", size = "Hunt activity", y = "Latitude", x = "Longitude") +
  scale_y_continuous(expand = c(0,0)) +   scale_x_continuous(expand = c(0,0)) +
  guides(colour = guide_legend(row = 1, override.aes = list(size = 6, fill=NA, alpha = 1, stoke = 3))) +
  guides(size = guide_legend(row = 1, override.aes = list(fill=NA, alpha = 1))) +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.ontop = FALSE) +
  theme(legend.justification = c(0.95,0.05),
        legend.position = c(0.95,0.1),
        legend.background = element_rect(fill = "lightgrey", colour = "black"),
        legend.key = element_rect(fill = "lightgrey")) + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.x=element_blank()) +
  coord_sf(xlim = c(-59.4, -57.6), ylim = c(-62.75, -61.9), expand = FALSE) +
  theme(plot.margin = unit(c(0,0,-2,0), 'lines'))
# theme(plot.margin = unit(c(0, 0, 0, 0), 
#                          "cm"))

Map1


Map2<- Antarcticamap + 
  ggspatial:: geom_spatial_path(data = BoutDist, aes(x = lon, y = lat, group = ID), alpha = 0.5, colour = "black") +
  ggspatial:: geom_spatial_point(data = BoutDist, aes(x = lon, y = lat, colour = SuccessQuant), alpha = 0.5, size = 0.4, shape = 1, stroke = 1) +
  facet_wrap(~ Year, ncol = 2) + 
  scale_colour_manual(values = c("#191900", "#F1C659", "#C34C4A")) +
  # scico::scale_fill_scico_d(palette = "lajolla", direction = 1) +
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) +
  theme(legend.direction = "vertical") + labs(fill = "NASC\n(scaled)", colour = "PCA per bout", size = "PCA per bout", y = "Latitude", x = "Longitude") +
  scale_y_continuous(expand = c(0,0)) +   scale_x_continuous(expand = c(0,0)) +
  guides(colour = guide_legend(row = 1, override.aes = list(shape = 16, size = 6, fill=NA, alpha = 1, stoke = 5))) +
  guides(size = guide_legend(row = 1, override.aes = list(fill=NA, alpha = 1))) +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.ontop = FALSE) +
  theme(legend.justification = c(0.95,0.1),
        legend.position = c(0.2,0.05),
        legend.background = element_rect(fill = "lightgrey", colour = "black"),
        legend.key = element_rect(fill = "lightgrey")) +
  # theme(
  #   strip.background = element_blank(),
  #   strip.text.x = element_blank()
  # ) +
  coord_sf(xlim = c(-59.4, -57.6), ylim = c(-62.75, -61.9), expand = FALSE) 
# theme(plot.margin = unit(c(0, 0, 0, 0), 
#                          "inches"))

Map2



library(patchwork)
Fig2 <- Map1 / Map2 + plot_layout(ncol = 1, heights = c(1, 1)) +
  plot_annotation(tag_levels = 'A', tag_prefix = "", tag_suffix = "") & 
  theme(
    plot.tag.position = c(0, 1),  # Top left corner
    plot.tag = element_text(size = 14, face = "bold", hjust = -0.5, vjust = 1)
  )
Fig2

# pdf("Fig2.pdf", width = 7, height = 7, onefile = FALSE)
# Fig2
# dev.off()


###############################################################################################################################################
###############################################################################################################################################
# Modelling work
###############################################################################################################################################
###############################################################################################################################################

library(DHARMa)
library(datawizard)
library(effects)
library(emmeans)

DiveID <- BoutID
BoutID <- TripID



BoutID$BEC1 <- 90

TripLevel <- BoutID %>% ## Aggregate to get trip-level summaries
  ungroup() %>%
  group_by(ID, Trip) %>%
  summarise(sumhunt = sum(sumhunt), ## All bout hunts in the trip
            TripHours = max(TripDuration), ## ID's trip duration
            DivesPerBout = sum(divesperbout), ## Total bouting dives
            birdbouts = max(birdbouts), ## Total bird bouts
            BEC = max(BEC1),
            Year = unique(Year)) %>%
  mutate(HUNTACPUE = sumhunt/TripHours)

DiveID$Year <- as.character(DiveID$Year)
BoutID$Year <- as.character(BoutID$Year)
TripLevel$Year <- as.character(TripLevel$Year)

# TripLevel$TripHours <- TripLevel$TripDuration
# TripLevel$HUNTACPUE <- TripLevel$sumhunt/TripLevel$TripHours
# TripLevel$HUNTPerDive <- TripLevel$sumhunt/TripLevel$DivesPerBout
# TripLevel$BoutIntensity <- TripLevel$birdbouts/TripLevel$TripHours


BoutID <- BoutID %>% 
  ungroup() %>%
  dplyr::arrange(ID, Trip, start) %>%
  mutate(DiveFreq = divesperbout/boutduration_mins) %>%
  mutate(timeLub = lubridate::decimal_date(start)) %>%
  mutate(times = glmmTMB::numFactor(timeLub)) ## For temporal autocorrelation



###############################################################################################################################################
###############################################################################################################################################
# TRIP-LEVEL MODELS 
###############################################################################################################################################
###############################################################################################################################################


###############################################################################################################################################
### Model 1 - Total Number of Hunts per foraging trip
###############################################################################################################################################

ggplot() + geom_histogram(data = TripLevel, aes(x = sumhunt)) +
  facet_wrap(~ Year)

SumHunt_Mod <-  glmmTMB::glmmTMB(sumhunt ~  Year + ( 1 | ID), data = TripLevel, family = gaussian()) #Year/ID for ID nested within Year
summary(SumHunt_Mod)
drop1(SumHunt_Mod, test = "Chisq") # 
check_model(SumHunt_Mod)
# check_autocorrelation(Nonbout_Mod) ### Configure with autocorrelation
simulationOutput <- DHARMa:: simulateResiduals(fittedModel = SumHunt_Mod)
plot(simulationOutput)


ef <- effect("Year", SumHunt_Mod)
x <- as.data.frame(ef)
x

ModelPlot <- ggplot(data = x, aes(x = Year, y = fit)) +
  geom_point(size = 3, position = position_dodge(0.4)) +
  labs(y= "PCA per foraging trip (s)", x= "Year")  + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) + theme(legend.text = element_text(face = "italic")) +  theme(legend.title = element_blank(), legend.position = "right") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.4, size = 1, position = position_dodge(0.4))
ModelPlot


###############################################################################################################################################
### Model 2 - Hunts per hour of foraging trip (PCA Frequency)
###############################################################################################################################################

ggplot() + geom_histogram(data = TripLevel, aes(x = HUNTACPUE)) +
  facet_wrap(~ Year)

PCAFreq_Mod <-  glmmTMB::glmmTMB(HUNTACPUE ~  Year + ( 1 | ID), data = TripLevel, family = gaussian()) 
summary(PCAFreq_Mod)
drop1(PCAFreq_Mod, test = "Chisq") # perform likelihood ratio tests
check_model(PCAFreq_Mod)
# check_autocorrelation(Nonbout_Mod) ### Configure with autocorrelation
simulationOutput <- DHARMa:: simulateResiduals(fittedModel = PCAFreq_Mod)
plot(simulationOutput)

ef <- effect("Year", PCAFreq_Mod)
x <- as.data.frame(ef)
x

ModelPlot1 <- ggplot(data = x, aes(x = Year, y = fit)) +
  geom_point(size = 3, position = position_dodge(0.4)) +
  labs(y= expression ("PCA frequency ("* hr^-1 *")"), x= "Year")  + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) + theme(legend.text = element_text(face = "italic")) +  theme(legend.title = element_blank(), legend.position = "right") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.4, size = 1, position = position_dodge(0.4))
ModelPlot1



###############################################################################################################################################
###############################################################################################################################################
# BOUT-LEVEL MODELS 
###############################################################################################################################################
###############################################################################################################################################


###############################################################################################################################################
### Model 3 - Interbout model
###############################################################################################################################################


ggplot() + geom_histogram(data = BoutID, aes(x = Boutinterva_mins)) +
  facet_wrap(~ Year)

InterboutModel_Mod <- BoutID %>%
  filter(Boutinterva_mins < 120) ## Remove post-dive surface intervals over 2 hours for models.


ggplot() + geom_histogram(data = InterboutModel_Mod, aes(x = , Boutinterva_mins, fill = Year), colour = "black", position = "dodge") +
  labs (x = "Interbout period (mins)", y = "Frequency #") +
  theme_bw() + scale_y_continuous(expand = c(0,0)) 
# scale_fill_manual(values=c("#69b3a8", "#404080")) +
# scale_x_continuous(breaks = c(0,50, 100, 150, 200, 250, 300, 350, 400))


# load("Interbout_Mod.RData")
Interbout_Mod <-  glmmTMB::glmmTMB(Boutinterva_mins ~  Year + (1|ID), data = InterboutModel_Mod, family = Gamma()) #Year/ID for ID nested within Year
# ou(times+0|ID)
# save(Interbout_Mod,file= "Interbout_Mod.RData")

summary(Interbout_Mod)
drop1(Interbout_Mod, test = "Chisq") # perform likelihood ratio tests
# check_model(Interbout_Mod) ## Check Assumptions
# check_autocorrelation(Interbout_Mod) #OK: Residuals appear to be independent and not autocorrelated.
simulationOutput <- DHARMa:: simulateResiduals(fittedModel = Interbout_Mod)
plot(simulationOutput)


ef <- effect("Year", Interbout_Mod)
x <- as.data.frame(ef)
x

ModelPlot2 <- ggplot(data = x, aes(x = Year, y = fit)) +
  geom_point(size = 3, position = position_dodge(0.4)) +
  labs(y= "Interbout period (mins)", x= "Year")  + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) + theme(legend.text = element_text(face = "italic")) +  theme(legend.title = element_blank(), legend.position = "right") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.4, size = 1, position = position_dodge(0.4))
ModelPlot2



###############################################################################################################################################
### Model 4 - Dives per bout model
###############################################################################################################################################

ggplot() + geom_histogram(data = BoutID, aes(x = divesperbout)) +
  facet_wrap(~ Year)

Diveperbout_Mod <-  glmmTMB::glmmTMB(divesperbout ~  Year + ou(times+0|ID), data = BoutID, family = poisson()) #Year/ID for ID nested within Year

# ou(times+0|ID)
# save(Diveperbout_Mod,file= "Diveperbout_Mod.RData")
load("Diveperbout_Mod.RData")

summary(Diveperbout_Mod)
drop1(Diveperbout_Mod, test = "Chisq") # perform likelihood ratio tests
# check_model(Diveperbout_Mod) ## Check Assumptions
# check_autocorrelation(Diveperbout_Mod)
# simulationOutput <- DHARMa:: simulateResiduals(fittedModel = Diveperbout_Mod)
# plot(simulationOutput)

ef <- effect("Year", Diveperbout_Mod)
x <- as.data.frame(ef)
x

ModelPlot3 <- ggplot(data = x, aes(x = Year, y = fit)) +
  geom_point(size = 3, position = position_dodge(0.4)) +
  labs(y= "Dives per bout (#)", x= "Year")  + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) + theme(legend.text = element_text(face = "italic")) +  theme(legend.title = element_blank(), legend.position = "right") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.4, size = 1, position = position_dodge(0.4))
ModelPlot3



###############################################################################################################################################
### Model 5 - Bout Depth Model
###############################################################################################################################################

ggplot() + geom_histogram(data = BoutID, aes(x = depthMean)) +
  facet_wrap(~ Year)


# load("BoutDur_Mod.RData")
Depth_Mod <-  glmmTMB::glmmTMB(depthMean ~  Year + ou(times+0|ID), data = BoutID, family = gaussian()) #Year/ID for ID nested within Year
# save(Depth_Mod, file = "Depth_Mod.RData")

summary(Depth_Mod)
drop1(Depth_Mod, test = "Chisq") # perform likelihood ratio tests
# check_model(Depth_Mod) ## Check Assumptions
# check_autocorrelation(Depth_Mod) ### 
# simulationOutput <- DHARMa:: simulateResiduals(fittedModel = Depth_Mod)
# plot(simulationOutput)

ef <- effect("Year", Depth_Mod)
x <- as.data.frame(ef)
x

ModelPlot5 <- ggplot(data = x, aes(x = Year, y = fit)) +
  geom_point(size = 3, position = position_dodge(0.4)) +
  labs(y= "Mean bout depth (m)", x= "Year")  + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) + theme(legend.text = element_text(face = "italic")) +  theme(legend.title = element_blank(), legend.position = "right") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.4, size = 1, position = position_dodge(0.4))
ModelPlot5



###############################################################################################################################################
### Model 6 - Bout Duration Model
###############################################################################################################################################


ggplot() + geom_histogram(data = BoutID, aes(x = boutduration_mins)) +
  facet_wrap(~ Year)

# load("BoutDur_Mod.RData")
Duration_Mod <-  glmmTMB::glmmTMB(boutduration_mins ~  Year + ou(times+0|ID), data = BoutID, family = nbinom1()) #Year/ID for ID nested within Year
# save(Duration_Mod, file = "Duration_Mod.RData")

summary(Duration_Mod)
# drop1(Duration_Mod, test = "Chisq") # perform likelihood ratio tests
# check_model(Duration_Mod) ## Check Assumptions
# check_autocorrelation(Duration_Mod) ### No autocorrelation
# simulationOutput <- DHARMa:: simulateResiduals(fittedModel = Duration_Mod)
# plot(simulationOutput)

ef <- effect("Year", Duration_Mod)
x <- as.data.frame(ef)
x

ModelPlot6 <- ggplot(data = x, aes(x = Year, y = fit)) +
  geom_point(size = 3, position = position_dodge(0.4)) +
  labs(y= "Mean bout duration (mins)", x= "Year")  + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) + theme(legend.text = element_text(face = "italic")) +  theme(legend.title = element_blank(), legend.position = "right") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.4, size = 1, position = position_dodge(0.4)) 
# scale_fill_brewer(palette = "Set1", labels=c('2019/20', '2021/22')) +   # <-- change fill scale
# scale_colour_brewer(palette = "Set1", labels=c('2019/20', '2021/22')) # <-- change colour scale
ModelPlot6


###############################################################################################################################################
### Model 7 - Hunt efficiency
###############################################################################################################################################


ggplot() + geom_histogram(data = BoutID, aes(x = meanhunt)) +
  facet_wrap(~ Year)


Efficiency_Mod <-  glmmTMB::glmmTMB(meanhunt ~  Year + ou(times+0|ID), data = BoutID, family = gaussian()) #Year/ID for ID nested within Year

summary(Efficiency_Mod)
drop1(Efficiency_Mod, test = "Chisq") # perform likelihood ratio tests
# check_model(Efficiency_Mod) ## Check Assumptions
# check_autocorrelation(Efficiency_Mod) ### No autocorrelation
# simulationOutput <- DHARMa:: simulateResiduals(fittedModel = Efficiency_Mod)
# plot(simulationOutput)

ef <- effect("Year", Efficiency_Mod)
x <- as.data.frame(ef)
x

ModelPlotEX<- ggplot(data = x, aes(x = Year, y = fit)) +
  geom_point(size = 3, position = position_dodge(0.4)) +
  labs(y= "Mean PCA per dive in a bout (#)", x= "Year")  + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) + theme(legend.text = element_text(face = "italic")) +  theme(legend.title = element_blank(), legend.position = "right") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.4, size = 1, position = position_dodge(0.4)) 

ModelPlotEX


###############################################################################################################################################
### Model 8 - Dive Frequency
###############################################################################################################################################

ggplot() + geom_histogram(data = BoutID, aes(x = DiveFreq)) +
  facet_wrap(~ Year)#


Freq_Mod <-  glmmTMB::glmmTMB(DiveFreq ~  Year + ou(times+0|ID), data = BoutID, family = poisson()) #Year/ID for ID nested within Year

summary(Freq_Mod)
drop1(Freq_Mod, test = "Chisq") # perform likelihood ratio tests
# check_model(Freq_Mod) ## Check Assumptions
# check_autocorrelation(Freq_Mod) ### No autocorrelation
# simulationOutput <- DHARMa:: simulateResiduals(fittedModel = Freq_Mod)
# plot(simulationOutput)

ef <- effect("Year", Freq_Mod)
x <- as.data.frame(ef)
x

ModelPlotEX1 <- ggplot(data = x, aes(x = Year, y = fit)) +
  geom_point(size = 3, position = position_dodge(0.4)) +
  labs(y= "Dive frequency\n(# dives per minute within bout)", x= "Year")  + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) + theme(legend.text = element_text(face = "italic")) +  theme(legend.title = element_blank(), legend.position = "right") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.4, size = 1, position = position_dodge(0.4)) 
# scale_fill_brewer(palette = "Set1", labels=c('2019/20', '2021/22')) +   # <-- change fill scale
# scale_colour_brewer(palette = "Set1", labels=c('2019/20', '2021/22')) # <-- change colour scale
ModelPlotEX1


###############################################################################################################################################
### Model 9 - Interbout duration as a function of distance from colony
###############################################################################################################################################

library(mgcv)
library(gratia)


GAM1920 <- InterboutModel_Mod %>%
  filter(Year == "2019/20")
GAM2122 <- InterboutModel_Mod %>%
  filter(Year == "2021/22")

# BoutID$Year <- as.factor(BoutID$Year)
ggplot(data = InterboutModel_Mod, aes(x = Distance, y = Boutinterva_mins, colour = Year)) +
  geom_point() +
  geom_smooth(method = "gam")


TemporalBout_Mod <-  mgcv::gam(Boutinterva_mins ~  s(Distance, k = 20, bs = "tp") + s(ID, bs = "re"), data = GAM1920, method = "REML", family = Gamma(), select = TRUE) #Year/ID for ID nested within Year
gam.check(TemporalBout_Mod)
draw(TemporalBout_Mod, residuals = TRUE)
summary.gam(TemporalBout_Mod)
concurvity(TemporalBout_Mod)
mgcv.helper::vif.gam(TemporalBout_Mod)

TemporalBout_Mod2 <-  mgcv::gam(Boutinterva_mins ~  s(Distance, k = 20, bs = "tp") + s(ID, bs = "re"), data = GAM2122, method = "REML", family = Gamma(), select = TRUE) #Year/ID for ID nested within Year
gam.check(TemporalBout_Mod2)
draw(TemporalBout_Mod2, residuals = TRUE)
summary.gam(TemporalBout_Mod2)


Comp <- compare_smooths(TemporalBout_Mod, TemporalBout_Mod2, smooths = "s(Distance)")

ucomp <- unnest(Comp, data)  %>%
  add_confint()
ucomp


p_edu <- ucomp |>
  filter(smooth == "s(Distance)") |> # <-- only one comparison at a time
  ggplot(aes(x = Distance, y = est)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = model),
              alpha = 0.2) +
  geom_line(aes(colour = model)) + 
  scale_fill_brewer(palette = "Set1", labels=c('2019/20', '2021/22')) +   # <-- change fill scale
  scale_colour_brewer(palette = "Set1", labels=c('2019/20', '2021/22')) +
  # geom_rug(data = BoutID,                  # <-- rug
  #          mapping = aes(x = Distance, y = NULL),
  #          sides = "b", alpha = 0.4) +  
  labs(title = NULL, y = "Interbout duration (model estimate)",
       colour = "Year", fill = "Year") + 
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black"))+ 
  theme(legend.direction = "horizontal",
        legend.justification = c(0.95,0.1),
        legend.position = c(0.8,0.1),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key = element_rect(fill = "white"))
p_edu

ModelPlotEX2 <- p_edu
ModelPlotEX2
 


##############################################################################################################################################
##############################################################################################################################################
# Figure 3

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
# Manuscript Figures

ModelPlot4 <- ModelPlotEX
BlankPlot <- ggplot() + theme_void()

Figure3A <- ModelPlot + ModelPlot1 + BlankPlot + BlankPlot + ModelPlot2 + ModelPlot3 + ModelPlot4 + BlankPlot + ModelPlotEX1 +  ModelPlot5 +  ModelPlot6 + ModelPlotEX2
Figure3A


# pdf("Fig3_BoutMS.pdf", width = 9, height = 9, onefile = FALSE)
# Figure3A
# dev.off()


#############################################################################################################################################
###############################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## End of Script

