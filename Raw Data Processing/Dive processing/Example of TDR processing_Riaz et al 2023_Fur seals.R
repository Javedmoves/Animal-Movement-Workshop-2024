##############################################################################################################################################
## Example of dive processing code used in Riaz et al 2023_Global Ecology and Conservation
##############################################################################################################################################

## Read in relevant packages and set data path

library(diveMove)
library(readr)
library(tidyverse)

rm(list=ls())

setwd("//saeri-file/users$/jriaz/Documents/Github/Dive processing/")

datapath <- setwd("//saeri-file/users$/jriaz/Documents/Github/Dive processing/")

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## Read in TDR data for 1 individual and do some formatting so it's good to go

TDR_ID <- "16A0600"

TDR1_data <- read_csv("Example_ID.csv", show_col_types = FALSE) 

TDR1_data <- TDR1_data %>%
  dplyr::select(Time, Depth)
  
TDR1_data <- TDR1_data[complete.cases(TDR1_data$Depth), ]

TDR1_data$Time <- as.POSIXct(strptime(TDR1_data$Time,"%H:%M:%S %d-%b-%Y", tz = "GMT"))

TDR1_data$Time <- as.POSIXct(TDR1_data$Time, format = "%d-%m-%Y %H:%M:%S", tz = "GMT")

ggplot() + geom_line(data = TDR1_data, aes(x= Time, y = Depth)) +
  scale_y_reverse()

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## Start the processing using divemove

tdrX <- createTDR(time=TDR1_data$Time,
                  depth=TDR1_data$Depth,
                  dtime=1, file=datapath)

plotTDR(tdrX) ## Assess ZOC required for each ID

dcalib <- calibrateDepth(tdrX,
                         zoc.method="offset",
                         offset= 0.5
                         ,dive.thr = 3)

# write_rds(dcalib, "dcalib.rds")

dcalib <- read_rds("dcalib.rds")

plotTDR(dcalib)

plotTDR(dcalib, diveNo=100:102) ##Subset to assess performance

Divemetrics <- diveStats(dcalib) ## Calculate dive statistics

Divemetrics$id <- TDR_ID
Divemetrics$Year <- "2019"
Divemetrics$Species <- "SAFS"

Divemetrics$Divenum <- 1:nrow(Divemetrics) ## Tally number of dives
Divemetrics$dive.tot <- max(Divemetrics$Divenum) ## Total number of dives in dataframe
Divemetrics$DiveEnd <- Divemetrics$begdesc + seconds(Divemetrics$divetim) # Dive end timestamp


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## Visualise processed data

ggplot() + geom_line(data = Divemetrics, aes(x= begdesc, y = maxdep)) +
  scale_y_reverse() +
  geom_hline(yintercept = 0, colour  = "black") +
  theme_classic() +
  labs(x = "Date", y = "Depth (m)")



##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## Animation 

library(gganimate)
library(grid)
library(png)
library(ggimage)


Subset <- as.data.frame(tdrX)

Subset$Day <- lubridate::date(Subset$time)
Subset$Hours <- lubridate::hour(Subset$time)

Subset <- Subset %>%
  arrange(time) %>%
  filter(Day == "2019-07-21") %>%
  filter(Hours == 19) %>%
   mutate(sequential_column = row_number()) 
  # filter(sequential_column > 10000 & sequential_column < 90000) 



image_path = "Seal1.png" 
image_path

Subset <- Subset %>%
  mutate(image = image_path)

DiveAnim <- ggplot() + 
  geom_image(data = Subset, aes(x = time, y = depth, image = image_path), size = 0.15, colour = "grey8") + # Adjust size as needed
    # geom_point(data = Subset, aes(x= time, y = depth), size = 1.5, colour = "red") +
    geom_line(data = Subset, aes(x= time, y = depth), size = 0.5, colour = "orange") +
  scale_y_reverse(breaks = seq(0, 20, by = 2)) +
  scale_x_datetime(expand = c(0,0)) +
  # geom_hline(yintercept = 0, colour  = "darkblue", size = 5) +
  theme_classic() +
  labs(x = "Time (minutes)", y = "Depth (m)") +
  theme(axis.text.x=element_blank(),
       axis.ticks.x=element_blank()) +
  # scale_x_datetime(date_breaks = "2 hours", date_labels = "%H") +
  transition_reveal(sequential_column, keep_last = FALSE) +
  shadow_wake(wake_length = 0)
DiveAnim

# gganimate::animate(DiveAnim, duration = 30, fps = 10, height = 4, width = 7, units = "in", res = 300, renderer = av_renderer("DiveAnim.mp4"))

gganimate::animate(DiveAnim, duration = 45, fps = 10, height = 4, width = 7, units = "in", res = 300, renderer = av_renderer("DiveAnim1.mp4"))


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## Save the data

write.csv(Divemetrics, (paste("Dive_Processed_2019_",TDR_ID,".csv", sep = "")), row.names=FALSE)


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## THE END
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
