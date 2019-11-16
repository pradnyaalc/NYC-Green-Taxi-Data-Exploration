require("maptools")
require(rgdal)
require(sp)
library(plyr)
library(dplyr)
library (raster)
library(sf)

data_hail <- read.csv("/home/pradnya/monash/5147/viz-2/poi_cleaned_data.csv", sep='\t')
shape <- readOGR("/home/pradnya/monash/5147/viz-2/taxi_zones/taxi_zones.shp", layer = 'taxi_zones')
head(shape)
data_hail <- na.omit(data_hail, c(3,4))
data_hail <- data_hail[data_hail$BOROUGH==1,]
df <- data_hail[,c(3,4)]
head(df)
coordinates(df) <- ~LONGITUDE+LATITUDE 
proj4string(df) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+proj=lcc +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")
d.ch1903 <- spTransform(df, CRS.new)
fdf <- over(d.ch1903,shape)
data_hail$zone=fdf$zone
head(data_hail)

write.csv(data_hail, file = "/home/pradnya/monash/5147/viz-2/newd/poi_data_manhattan_zone_cleaned.csv")
