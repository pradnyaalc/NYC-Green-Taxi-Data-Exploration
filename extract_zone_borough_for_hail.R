require("maptools")
require(rgdal)
require(sp)
library(plyr)
library(dplyr)
library (raster)
library(sf)
data_hail <- read.csv("/home/pradnya/monash/5147/viz-2/newd/hail_df.csv", sep = '\t')
shape <- readOGR("/home/pradnya/monash/5147/viz-2/taxi_zones/taxi_zones.shp", layer = 'taxi_zones')
head(shape)

df <- data_hail[,c(5,6)] 
coordinates(df) <- ~Pickup_longitude+Pickup_latitude 
proj4string(df) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+proj=lcc +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")
d.ch1903 <- spTransform(df, CRS.new)
fdf <- over(d.ch1903, shape)
data_hail$Pickup_borough <- fdf$borough
data_hail$Pickup_zone <- fdf$zone


df <- data_hail[,c(7,8)] 
coordinates(df) <- ~Dropoff_longitude+Dropoff_latitude 
proj4string(df) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+proj=lcc +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")
d.ch1903 <- spTransform(df, CRS.new)
fdf <- over(d.ch1903, shape)
data_hail$Dropoff_borough <- fdf$borough
data_hail$Dropoff_zone <- fdf$zone


write.csv(data_hail, file = "/home/pradnya/monash/5147/viz-2/newd/data_hail_borough.csv")
