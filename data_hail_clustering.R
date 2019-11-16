require(ggplot2)
# Get the input values.
hail_data <- read.csv("/home/pradnya/monash/5147/viz-2/newd/data_hail_borough.csv")
input <- hail_data[,c('Pickup_longitude','Pickup_latitude')]
poi_data <- read.csv("/home/pradnya/monash/5147/viz-2/poi_cleaned_data.csv", sep = '\t')
input1 <- poi_data[,c('LONGITUDE','LATITUDE')]

# Plot the chart for pickup latitude and longitude.
kmean_func <- function(df, cluster_name){
  fit <- kmeans(df, 4, nstart = 25)
  print(cluster_name)
  print(fit)
  df[[cluster_name]] <- factor(fit$cluster)
  return(df)
}
input <- kmean_func(input, "pickupPointCluster")
input1 <- kmean_func(input1, "poiPointCluster")

theme_set(theme_bw())
plt1 <- ggplot() + geom_point(data = input1, aes(x = input1$LONGITUDE, y = input1$LATITUDE, color=poiPointCluster))
plt2 <- plt1 + geom_point(data = input, aes(x = input$Pickup_longitude, y = input$Pickup_latitude, fill=pickupPointCluster),shape=21)
plt3 <- plt2 + labs(x="Longitude", y="Latitude", title = "PointOfInterest and TaxiPickupPoints Plot")
plt4 <- plt3 + scale_color_manual(breaks=input1$poiPointCluster, values = c("red","blue","green","purple")) + scale_fill_manual(values = c("orange","cyan","black","yellow"))
plt4

pickup_df <- input[ ((input$pickupPointCluster==1)|(input$pickupPointCluster==2)|(input$pickupPointCluster==4)), ]
min_pick_lon <- min(pickup_df$Pickup_longitude) #-74.24826
max_pick_lon <- max(pickup_df$Pickup_longitude) #-73.70779
max_pick_lat <- max(pickup_df$Pickup_latitude) # 40.86767
min_pick_lat <- min(pickup_df$Pickup_latitude) # 40.51491

## Plot the chart for dropoff latitude and longitude.
input <- hail_data[,c('Dropoff_longitude','Dropoff_latitude')]
input <- kmean_func(input, "dropoffPointCluster")

plt1 <- ggplot() + geom_point(data=input1, aes(x = input1$LONGITUDE, y = input1$LATITUDE, color=poiPointCluster ))
plt2 <- plt1 + geom_point(data = input, aes(x = input$Dropoff_longitude, y = input$Dropoff_latitude, fill=dropoffPointCluster), shape=21)
plt3 <- plt2 + labs(x="Longitude", y="Latitude", title = "PointOfInterest and TaxiDropOffPoints Plot")
plt4 <- plt3 + scale_color_manual(breaks=input1$poiPointCluster, values = c("red","blue","green","purple")) + scale_fill_manual(values = c("orange","cyan","black","yellow"))
plt4


dropOff_df <- input[ ((input$dropoffPointCluster==1)|(input$dropoffPointCluster==2)), ]
min_drop_lon <- min(dropOff_df$Dropoff_longitude) #-74.24824
max_drop_lon <- max(dropOff_df$Dropoff_longitude) #-73.85519
max_drop_lat <- max(dropOff_df$Dropoff_latitude) # 40.80232
min_drop_lat <- min(dropOff_df$Dropoff_latitude) # 40.51491



# pu_data <- hail_data[ ((hail_data$Pickup_longitude >= min_pick_lon) & (hail_data$Pickup_longitude <= max_pick_lon) & (hail_data$Pickup_latitude >= min_pick_lat) & (hail_data$Pickup_latitude <= max_pick_lat)),]
# do_data <- hail_data[((hail_data$Dropoff_longitude>= min_drop_lon) & (hail_data$Dropoff_longitude<= max_drop_lon) & (hail_data$Dropoff_latitude >= min_drop_lat) & (hail_data$Dropoff_latitude <= max_drop_lat)),]
data <- hail_data[ (((hail_data$Pickup_longitude >= min_pick_lon) & (hail_data$Pickup_longitude <= max_pick_lon) & (hail_data$Pickup_latitude >= min_pick_lat) & (hail_data$Pickup_latitude <= max_pick_lat))
                        | ((hail_data$Dropoff_longitude>= min_drop_lon) & (hail_data$Dropoff_longitude<= max_drop_lon) & (hail_data$Dropoff_latitude >= min_drop_lat) & (hail_data$Dropoff_latitude <= max_drop_lat))),]

write.csv(data, file = "/home/pradnya/monash/5147/viz-2/newd/data_hail_match_poi.csv")
