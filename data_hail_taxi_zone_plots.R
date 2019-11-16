library(maptools)
shape <- readOGR("/home/pradnya/monash/5147/viz-2/taxi_zones/taxi_zones.shp", layer = 'taxi_zones')
summary(shape@data)
shp_df <- broom::tidy(shape, region = "borough")
lapply(shp_df, class)
cnames <- aggregate(cbind(long, lat) ~ id, data=shp_df, FUN=mean)


shp_df1 <- broom::tidy(shape, region = "zone")
head(shp_df1)
lapply(shp_df1, class)
znames <- aggregate(cbind(long, lat) ~ id, data=shp_df1, FUN=mean)


#Most pickups and dropOffs from which borough
hail_data <- read.csv("/home/pradnya/monash/5147/viz-2/newd/data_hail_match_poi.csv")
head(hail_data)

library(sqldf)
pu_count <-sqldf("SELECT Pickup_borough, COUNT(*) as PUCount FROM hail_data GROUP BY Pickup_borough;")
cname_pu_mgr <- merge(cnames, pu_count, by.x="id", by.y="Pickup_borough")
mgr <- merge(shp_df,pu_count,by.x="id",by.y="Pickup_borough")
map <- ggplot() + geom_polygon(data = mgr, aes(x = long, y = lat, group = group, fill = PUCount),color = "black")+theme_void()
map + geom_label(data = cname_pu_mgr, aes(x = long, y = lat, label =paste(id,"\n",PUCount), color="white"), size = 4.09, show.legend = FALSE, fontface = "bold") + labs(title = "Taxi PickUps in each New York Borough") + theme_void()

do_count <-sqldf("SELECT Dropoff_borough, COUNT(*) as DOCount FROM hail_data GROUP BY Dropoff_borough;")
cname_do_mgr <- merge(cnames, do_count, by.x="id", by.y="Dropoff_borough")
mgr1 <- merge(shp_df,do_count,by.x="id",by.y="Dropoff_borough")
map <- ggplot() + geom_polygon(data = mgr1, aes(x = long, y = lat, group = group, fill = DOCount),color = "black")+theme_void()
map + geom_label(data = cname_do_mgr, aes(x = long, y = lat, label =paste(id,"\n",DOCount), color="white"), size = 4.09, show.legend = FALSE, fontface = "bold") + labs(title = "Taxi DropOffs in each New York Borough") + theme_void()


#Which zones in Brooklyn were visited most
br_shape <- shape[shape$borough=="Brooklyn",]
br_shp_df <- broom::tidy(br_shape, region = "zone")
head(br_shp_df)
lapply(br_shp_df, class)
znames <- aggregate(cbind(long, lat) ~ id, data=br_shp_df, FUN=mean)
map <- ggplot() + geom_polygon(data = br_shp_df, aes(x = long, y = lat, group = group, fill=id), colour = "black", show.legend = FALSE)+theme_void()
map + geom_text(data = znames, aes(x = long, y = lat, label = id), size = 3) + labs(title = "Zones in Brooklyn") + theme_void()

br_pu_data <- hail_data[hail_data$Pickup_borough=="Brooklyn",]
br_pu_count <- sqldf("SELECT Pickup_borough,Pickup_zone, COUNT(*) as PUCount FROM br_pu_data GROUP BY Pickup_zone")

zname_pu_mgr <- merge(znames, br_pu_count, by.x="id", by.y="Pickup_zone", all.x=TRUE)
zname_pu_mgr <- na.omit(zname_pu_mgr, cols=(4))

zname_pu_mgr <- zname_pu_mgr[order(-zname_pu_mgr$PUCount),]
zmax <- zname_pu_mgr[1:2,]
z_wo_max <- zname_pu_mgr[3:nrow(zname_pu_mgr),]

br_mgr <- merge(br_shp_df,br_pu_count,by.x="id",by.y="Pickup_zone", all.x=TRUE)
map <- ggplot() + geom_polygon(data = br_mgr, aes(x = long, y = lat, group = group, fill = PUCount),color = "black")+theme_void()
map + geom_label(data = zmax, aes(x = long, y = lat, label =paste(id,"\n",PUCount), color="white"), size = 4.09, show.legend = FALSE, fontface = "bold",hjust=0,vjust=1) + geom_text(data = z_wo_max, aes(x = long, y = lat, label =paste(id,"\n",PUCount), color="red", hjust=0), size = 3, show.legend = FALSE)+ labs(title = "Taxi PickUps Per Zone in Brooklyn") + theme_void()

br_do_data <- hail_data[hail_data$Dropoff_borough=="Brooklyn",]
br_do_count <- sqldf("SELECT Dropoff_borough,Dropoff_zone, COUNT(*) as DOCount FROM br_do_data GROUP BY Dropoff_zone")

zname_do_mgr <- merge(znames, br_do_count, by.x="id", by.y="Dropoff_zone", all.x=TRUE)
zname_do_mgr <- na.omit(zname_do_mgr, cols=(4))

zname_do_mgr <- zname_do_mgr[order(-zname_do_mgr$DOCount),]
zmax <- zname_do_mgr[1:2,]
z_wo_max <- zname_do_mgr[3:nrow(zname_do_mgr),]

br_mgr1 <- merge(br_shp_df,br_do_count,by.x="id",by.y="Dropoff_zone", all.x=TRUE)
map <- ggplot() + geom_polygon(data = br_mgr1, aes(x = long, y = lat, group = group, fill = DOCount),color = "black")+theme_void()
map + geom_label(data = zmax, aes(x = long, y = lat, label =paste(id,"\n",DOCount), color="white"), size = 4.09, show.legend = FALSE, fontface = "bold", vjust=1) + geom_text(data = z_wo_max, aes(x = long, y = lat, label =paste(id,"\n",DOCount), color="red", hjust=0), size = 3, show.legend = FALSE)+ labs(title = "Taxi Dropoffs Per Zone in Brooklyn") + theme_void()

#which zones were most visited in Manhattan
br_shape <- shape[shape$borough=="Manhattan",]
br_shp_df <- broom::tidy(br_shape, region = "zone")
head(br_shp_df)
lapply(br_shp_df, class)
znames <- aggregate(cbind(long, lat) ~ id, data=br_shp_df, FUN=mean)
map <- ggplot() + geom_polygon(data = br_shp_df, aes(x = long, y = lat, group = group, fill=id), colour = "black", show.legend = FALSE)+theme_void()
map + geom_text(data = znames, aes(x = long, y = lat, label = id), size = 2) + labs(title = "Zones in Manhattan") + theme_void()

br_pu_data <- hail_data[hail_data$Pickup_borough=="Manhattan",]
br_pu_count <- sqldf("SELECT Pickup_borough,Pickup_zone, COUNT(*) as PUCount FROM br_pu_data GROUP BY Pickup_zone")

zname_pu_mgr <- merge(znames, br_pu_count, by.x="id", by.y="Pickup_zone", all.x=TRUE)
zname_pu_mgr <- na.omit(zname_pu_mgr, cols=(4))

zname_pu_mgr <- zname_pu_mgr[order(-zname_pu_mgr$PUCount),]
zmax <- zname_pu_mgr[1:2,]
z_wo_max <- zname_pu_mgr[3:nrow(zname_pu_mgr),]

br_mgr <- merge(br_shp_df,br_pu_count,by.x="id",by.y="Pickup_zone", all.x=TRUE)
map <- ggplot() + geom_polygon(data = br_mgr, aes(x = long, y = lat, group = group, fill = PUCount),color = "black")+theme_void()
map + geom_label(data = zmax, aes(x = long, y = lat, label =paste(id,"\n",PUCount), color="white"), size = 4.09, show.legend = FALSE, fontface = "bold",hjust=0,vjust=1) + geom_text(data = z_wo_max, aes(x = long, y = lat, label =paste(id,"\n",PUCount), color="red", hjust=0), size = 3, show.legend = FALSE)+ labs(title = "Taxi PickUps Per Zone in Manhattan") + theme_void()

br_do_data <- hail_data[hail_data$Dropoff_borough=="Manhattan",]
br_do_count <- sqldf("SELECT Dropoff_borough,Dropoff_zone, COUNT(*) as DOCount FROM br_do_data GROUP BY Dropoff_zone")

zname_do_mgr <- merge(znames, br_do_count, by.x="id", by.y="Dropoff_zone", all.x=TRUE)
zname_do_mgr <- na.omit(zname_do_mgr, cols=(4))

zname_do_mgr <- zname_do_mgr[order(-zname_do_mgr$DOCount),]
zmax <- zname_do_mgr[1:2,]
z_wo_max <- zname_do_mgr[3:nrow(zname_do_mgr),]

br_mgr1 <- merge(br_shp_df,br_do_count,by.x="id",by.y="Dropoff_zone", all.x=TRUE)
map <- ggplot() + geom_polygon(data = br_mgr1, aes(x = long, y = lat, group = group, fill = DOCount),color = "black")+theme_void()
map + geom_label(data = zmax, aes(x = long, y = lat, label =paste(id,"\n",DOCount), color="white"), size = 4.09, show.legend = FALSE, fontface = "bold", vjust=1) + geom_text(data = z_wo_max, aes(x = long, y = lat, label =paste(id,"\n",DOCount), color="red", hjust=0), size = 3, show.legend = FALSE)+ labs(title = "Taxi Dropoffs Per Zone in Manhattan") + theme_void()



# Most visited POI in 'Park Slope' Brooklyn

poi_data <- read.csv("/home/pradnya/monash/5147/viz-2/newd/poi_data_broklyn_zone_cleaned.csv")
high_pu_zone <- hail_data[hail_data$Pickup_zone=='Park Slope',]
poi_data_high_zone <- poi_data[poi_data$zone=='Park Slope',]
poi_data_high_zone <- na.omit(poi_data_high_zone, c(4,5))

poi_labels <- poi_data_high_zone[(poi_data_high_zone$NAME=='HRA JOB CENTER SOUTHERN BROOKLYN #70')|(poi_data_high_zone$NAME=='PACIFIC LIBRARY')|(poi_data_high_zone$NAME=='TIMES PLAZA AREA'),]

plt1 <- ggplot() + geom_point(data = high_pu_zone, aes(x = Pickup_longitude, y = Pickup_latitude, color="Pickup")) 
plt2 <- plt1 + geom_point(data=poi_data_high_zone, aes(x = LONGITUDE, y = LATITUDE, fill=NAME), shape=21, show.legend = FALSE)+geom_label(data=poi_labels,aes(x =LONGITUDE, y =LATITUDE, label=NAME, fontface="bold"),size=3,hjust=0,vjust=1)+scale_color_manual(values = c("Pickup"="red"))
plt2 + labs(x="Longitude", y="Latitude", title = "PointOfInterest and TaxiPickUpPoints Plot in Park Slope")

high_do_zone <- hail_data[hail_data$Dropoff_zone=='Park Slope',]
poi_data_high_zone <- poi_data[poi_data$zone=='Park Slope',]
poi_data_high_zone <- na.omit(poi_data_high_zone, c(4,5))

poi_labels <- poi_data_high_zone[(poi_data_high_zone$NAME=='HRA JOB CENTER SOUTHERN BROOKLYN #70')|(poi_data_high_zone$NAME=='PACIFIC LIBRARY')|(poi_data_high_zone$NAME=='TIMES PLAZA AREA')|(poi_data_high_zone$NAME=='JOHN JAY HS HISTORICAL'),]

plt1 <- ggplot() + geom_point(data = high_do_zone, aes(x = Dropoff_longitude, y = Dropoff_latitude, color="DropOff")) 
plt2 <- plt1 + geom_point(data=poi_data_high_zone, aes(x = LONGITUDE, y = LATITUDE, fill=NAME), shape=21, show.legend = FALSE)+geom_label(data=poi_labels,aes(x =LONGITUDE, y =LATITUDE, label=NAME, fontface="bold"),size=3,hjust=0,vjust=1)+scale_color_manual(values = c("DropOff"="red"))
plt2 + labs(x="Longitude", y="Latitude", title = "PointOfInterest and TaxiDropOffPoints Plot in Park Slope")



# Most visited POI in 'East Harlem North' Manhattan

poi_data <- read.csv("/home/pradnya/monash/5147/viz-2/newd/poi_data_manhattan_zone_cleaned.csv")
high_pu_zone <- hail_data[hail_data$Pickup_zone=='East Harlem North',]
poi_data_high_zone <- poi_data[poi_data$zone=='East Harlem North',]
poi_data_high_zone <- na.omit(poi_data_high_zone, c(4,5))

poi_labels <- poi_data_high_zone[(poi_data_high_zone$NAME=='PARK AVENUE HOTEL')|(poi_data_high_zone$NAME=='ABC ECHO PARK CHILDREN & FAMILY CENTER')|(poi_data_high_zone$NAME=='LATIN AMER COUNCIL PENTECOSTAL'),]

plt1 <- ggplot() + geom_point(data = high_pu_zone, aes(x = Pickup_longitude, y = Pickup_latitude, color="Pickup")) 
plt2 <- plt1 + geom_point(data=poi_data_high_zone, aes(x = LONGITUDE, y = LATITUDE, fill=NAME), shape=21, show.legend = FALSE)+geom_label(data=poi_labels,aes(x =LONGITUDE, y =LATITUDE, label=NAME, fontface="bold"),size=2.5,hjust=0,vjust=1)+scale_color_manual(values = c("Pickup"="red"))
plt2 + labs(x="Longitude", y="Latitude", title = "PointOfInterest and TaxiPickUpPoints Plot in East Harlem North")


high_do_zone <- hail_data[hail_data$Dropoff_zone=='East Harlem North',]
poi_data_high_zone <- poi_data[poi_data$zone=='East Harlem North',]
poi_data_high_zone <- na.omit(poi_data_high_zone, c(4,5))

poi_labels <- poi_data_high_zone[(poi_data_high_zone$NAME=='PARK AVENUE HOTEL')|(poi_data_high_zone$NAME=='ABC ECHO PARK CHILDREN & FAMILY CENTER')|(poi_data_high_zone$NAME=='EAST RIVER PLAZA SHOPPING MALL')|(poi_data_high_zone$NAME=='MT SINAI DIALYSIS CENTER'),]

plt1 <- ggplot() + geom_point(data = high_do_zone, aes(x = Dropoff_longitude, y = Dropoff_latitude, color="DropOff")) 
plt2 <- plt1 + geom_point(data=poi_data_high_zone, aes(x = LONGITUDE, y = LATITUDE, fill="POI"), shape=21)+geom_label(data=poi_labels,aes(x =LONGITUDE, y =LATITUDE, label=NAME),size=2.5,hjust=0,vjust=1)+scale_color_manual(values = c("DropOff"="red"))+scale_fill_manual(values = c("POI"="green"))
plt2 + labs(x="Longitude", y="Latitude", title = "PointOfInterest and TaxiDropOffPoints Plot in East Harlem North")


## calculating distance
library(geosphere)
high_do_zone$Distance_from_POI <- 0
get_distance <- function(lat2, lon2){
  dist1 <- c()
  for (row in 1:nrow(high_do_zone)){
    lat1 <- high_do_zone[row, "Dropoff_latitude"]
    lon1 <- high_do_zone[row, "Dropoff_longitude"]
    d <- distHaversine(c(lat1, lon1), c(lat2, lon2), r=3958.756)
    dist1[row] <- d
  }
  return(dist1)
}

dist1 <- get_distance(lat2=poi_labels[4, "LATITUDE"], lon2=poi_labels[4,"LONGITUDE"])
high_do_zone$Distance_from_POI <- dist1
o <- high_do_zone[(high_do_zone$Distance_from_POI<=0.4),]
test <- rbind(l,m,n,o)
test <- test[,-c(1,2,3,28)]
test1 <- test[-which(duplicated(test)), ]


#################################################


high_pu_zone$Distance_from_POI <- 0
get_distance <- function(lat2, lon2){
  dist1 <- c()
  for (row in 1:nrow(high_pu_zone)){
    lat1 <- high_pu_zone[row, "Pickup_latitude"]
    lon1 <- high_pu_zone[row, "Pickup_longitude"]
    d <- distHaversine(c(lat1, lon1), c(lat2, lon2), r=3958.756)
    dist1[row] <- d
  }
  return(dist1)
}

dist1 <- get_distance(lat2=poi_labels[3, "LATITUDE"], lon2=poi_labels[3,"LONGITUDE"])
high_pu_zone$Distance_from_POI <- dist1
n <- high_pu_zone[(high_pu_zone$Distance_from_POI<=0.4),]
test <- rbind(l,m,n)
test <- test[,-c(1,2,3,28)]
test2 <- test[-which(duplicated(test)), ]



####final

f <- rbind(test2,test1)
final <- f[-which(duplicated(f)), ]

write.csv(final, file = "/home/pradnya/monash/5147/viz-2/newd/near_to_poi_manhattan_east_harlem.csv")
