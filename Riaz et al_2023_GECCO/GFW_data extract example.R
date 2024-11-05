##############################################################################################################################################
##############################################################################################################################################
# Script to extract Global Fishing Watch data
##############################################################################################################################################
##############################################################################################################################################


# usethis::edit_r_environ()
key <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImtpZEtleSJ9.eyJkYXRhIjp7Im5hbWUiOiJKYXZlZCBSaWF6IiwidXNlcklkIjozNTk3OCwiYXBwbGljYXRpb25OYW1lIjoiSmF2ZWQgUmlheiIsImlkIjoxNzI0LCJ0eXBlIjoidXNlci1hcHBsaWNhdGlvbiJ9LCJpYXQiOjE3MjIzNDk2OTYsImV4cCI6MjAzNzcwOTY5NiwiYXVkIjoiZ2Z3IiwiaXNzIjoiZ2Z3In0.QxBFeyoFQYuDN6cz5-RpbGmBcB8oidA8mTEAq-TRFHSDadbPz9VRu-gJS12RJIMyn5RaQV9PS8AYXKTOOSABMablj5vsM1LEC2xDfAdkyxg59GFdztGhqejRn_Usse8ZZAGS7eGsa4IfCxgxvuIx-QEpwNorpetFkmtiSCQ_EBMeVHmX3l42LxoGjzW20cxrXWneruefv7OeOIGwkSfHWMkkeLgzc7iiipQogmZ4Va4DEXHo7SjHtrYYcparA8ttT6VNgCnloPQrK8YvvqP1GKV-e5Ss0BS7EVyr1FF40xGgm2X1Yot7I-mC8JctVa1rHkYO_ILFnsfySQJo1rtgBmSL3KtdxgOmdv8uI2TI9IUzktosIgpPBnwWYsJSaXhXZk--cAMndlaePBTNfVXkIzua-A4Q4Nz7qh34VOenL2XWJVBEdVuHH5qJNc9H3KvfUi4Yg5cmDrZLM479QbbwD931scOEBIKB8XxG6MQ_vaXkGC8TqegcR9hT1jMOefv7"

key <- Sys.getenv("GFW_API_KEY")

library(gfwr)
library(fields)
library(tidyverse)
library(lubridate)
library(sf)

region_json <- sf::st_bbox(c(xmin = -70, xmax = -35, ymin = -55, ymax = -40), crs = 4326) |> 
  sf::st_as_sfc() |> 
  sf::st_as_sf() |> 
  dplyr::rename(geometry = x)


##############################################################################################################################################
## ## EXCTRACT MONTHLY DATE FOR THE FULL DATE RANGE FOR BROAD SCALE OVERLAP
##############################################################################################################################################


gfw_dat_2023_DAILY <- get_raster(spatial_resolution = 'LOW',
                                 temporal_resolution = 'DAILY',
                                 group_by = 'GEARTYPE',
                                 start_date = '2023-01-01',
                                 end_date = '2023-12-31',
                                 region = region_json,
                                 region_source = 'USER_SHAPEFILE',
                                 key = key)

write_rds(gfw_dat_2023_DAILY, "gfw_dat_2023_DAILY.rds")


############################################################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################################################
# SCRIPT END
############################################################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################################################





