
# Analysis for Broklyn Park Slope trips near POI
data_hail_ps <- read.csv("/home/pradnya/monash/5147/viz-2/newd/near_to_poi_broklyn_park_slope.csv")
head(data_hail_ps)

high_pu_zone_ps <- data_hail_ps[data_hail_ps$Pickup_zone=='Park Slope',]
high_do_zone_ps <- data_hail_ps[data_hail_ps$Dropoff_zone=='Park Slope',]

#Time of the week vs Number of Pickups/DropOffs Park Slope
df_pu_week <- sqldf("SELECT pickup_day_of_week as day_of_week, COUNT(*) as Count FROM high_pu_zone_ps GROUP BY pickup_day_of_week")
df_pu_week$TripType <- "PickUp"

for(row in 1:nrow(high_do_zone_ps)){high_do_zone_ps[row, "dropOff_day_of_week"] = weekdays(as.POSIXct(high_do_zone_ps[row, "Lpep_dropoff_datetime"]), abbreviate = F)}

df_do_week <- sqldf("SELECT dropOff_day_of_week as day_of_week, COUNT(*) as Count FROM high_do_zone_ps GROUP BY dropOff_day_of_week")
df_do_week$TripType <- "DropOff"
final_week_df <- bind_rows(df_pu_week,df_do_week)


ggplot(data=final_week_df, aes(x=day_of_week, y=Count, fill=TripType, label=Count))+geom_bar(stat = "identity")+geom_text(size = 3, position = position_stack(vjust = 0.5))+labs(y="Number of Trips",title = "Number of Trips per TripType Park Slope")

#Time of the week vs Number of Pickups/DropOffs East Harlem North
data_hail_en <- read.csv("/home/pradnya/monash/5147/viz-2/newd/near_to_poi_manhattan_east_harlem.csv")
high_pu_zone_en <- data_hail_en[data_hail_en$Pickup_zone=='East Harlem North',]
high_do_zone_en <- data_hail_en[data_hail_en$Dropoff_zone=='East Harlem North',]

df_pu_week <- sqldf("SELECT pickup_day_of_week as day_of_week, COUNT(*) as Count FROM high_pu_zone_en GROUP BY pickup_day_of_week")
df_pu_week$TripType <- "PickUp"

for(row in 1:nrow(high_do_zone_en)){high_do_zone_en[row, "dropOff_day_of_week"] = weekdays(as.POSIXct(high_do_zone_en[row, "Lpep_dropoff_datetime"]), abbreviate = F)}

df_do_week <- sqldf("SELECT dropOff_day_of_week as day_of_week, COUNT(*) as Count FROM high_do_zone_en GROUP BY dropOff_day_of_week")
df_do_week$TripType <- "DropOff"
final_week_df <- bind_rows(df_pu_week,df_do_week)


ggplot(data=final_week_df, aes(x=day_of_week, y=Count, fill=TripType, label=Count))+geom_bar(stat = "identity")+geom_text(size = 3, position = position_stack(vjust = 0.5))+labs(y="Number of Trips",title = "Number of Trips per TripType East Harlem North(Manhattan)")

#Peak Hours Of Travel in the zone Park Slope
df_pu <- sqldf("SELECT pickup_hour, COUNT(*) as PUCount FROM high_pu_zone_ps GROUP BY pickup_hour")

library(lubridate)
high_do_zone_ps$dropOffHour <- 0
for(row in 1:nrow(high_do_zone_ps)){high_do_zone_ps[row, "dropOffHour"] = hour(high_do_zone_ps[row, "Lpep_dropoff_datetime"])}

df_do <- sqldf("SELECT dropOffHour, COUNT(*) as DOCount FROM high_do_zone_ps GROUP BY dropOffHour")

p1 <- ggplot() + geom_line(data=df_pu, aes(x = pickup_hour, y = PUCount, colour = "PickUp"))+geom_line(data = df_do, aes(x = dropOffHour, y = DOCount, colour = "DropOff"))+scale_color_manual(values = c("PickUp"="red", "DropOff"="blue"))
p1+labs(x="Time in Hours",y="Number Of Trips",title = "Peak Hours Of Travel in Park Slope(Brooklyn)")

#Peak Hours Of Travel in the zone East Harlem North
df_pu <- sqldf("SELECT pickup_hour, COUNT(*) as PUCount FROM high_pu_zone_en GROUP BY pickup_hour")

library(lubridate)
high_do_zone_en$dropOffHour <- 0
for(row in 1:nrow(high_do_zone_en)){high_do_zone_en[row, "dropOffHour"] = hour(high_do_zone_en[row, "Lpep_dropoff_datetime"])}

df_do <- sqldf("SELECT dropOffHour, COUNT(*) as DOCount FROM high_do_zone_en GROUP BY dropOffHour")

p1 <- ggplot() + geom_line(data=df_pu, aes(x = pickup_hour, y = PUCount, colour = "PickUp"))+geom_line(data = df_do, aes(x = dropOffHour, y = DOCount, colour = "DropOff"))+scale_color_manual(values = c("PickUp"="red", "DropOff"="blue"))
p1+labs(x="Time in Hours",y="Number Of Trips",title = "Peak Hours Of Travel in East Harlem North(Manhattan)")



#fare amount with respect to time when pickup and drop off are in same zone Park Slope
library(sqldf)
zn_do_count <- sqldf("SELECT Pickup_borough, Pickup_zone, Dropoff_borough,Dropoff_zone, COUNT(*) as DOCount FROM high_pu_zone_ps GROUP BY Dropoff_zone")
zn_do_count <- zn_do_count[order(-zn_do_count$DOCount),]
zn_do_count <- zn_do_count[zn_do_count$DOCount>100,]
barplot(zn_do_count$DOCount, names.arg=zn_do_count$Dropoff_zone, cex.names=0.8, las=2, xlab="DropOff Zones", ylab="Frequency Of DropOffs", main="DropOff Zones when PickupZone is Park Slope")


zn_fare_data_ps <- data_hail_ps[(data_hail_ps$Pickup_zone=='Park Slope')&(data_hail_ps$Dropoff_zone=='Park Slope'),]


fareStatsByHour = zn_fare_data_ps %>% 
  group_by(pickup_hour) %>% 
  summarise(minFare = min(Fare_amount), maxFare = max(Fare_amount), meanFare = mean(Fare_amount), medianFare = median(Fare_amount))
fareStatsByHour %>% 
  dplyr::select(-maxFare, -minFare) %>% 
  gather(metric, value, -pickup_hour) %>% 
  ggplot(aes(x = pickup_hour, y = value, colour = metric)) +
  geom_line() + 
  labs(x = "Hour of the day", y = "Metric Value", title = "Mean and Median Fare by Hour of the Day PickUp=Park Slope and DropOff=Park Slope")



#fare amount with respect to time when pickup and drop off are in same zone East Harlem North
library(sqldf)
zn_do_count <- sqldf("SELECT Pickup_borough, Pickup_zone, Dropoff_borough,Dropoff_zone, COUNT(*) as DOCount FROM high_pu_zone_en GROUP BY Dropoff_zone")
zn_do_count <- zn_do_count[order(-zn_do_count$DOCount),]
zn_do_count <- zn_do_count[zn_do_count$DOCount>100,]
barplot(zn_do_count$DOCount, names.arg=zn_do_count$Dropoff_zone, cex.names=0.8, las=2, xlab="DropOff Zones", ylab="Frequency Of DropOffs", main="DropOff Zones when PickupZone is East Harlem North")


zn_fare_data_en <- data_hail_en[(data_hail_en$Pickup_zone=='East Harlem North')&(data_hail_en$Dropoff_zone=='East Harlem North'),]


fareStatsByHour = zn_fare_data_en %>% 
  group_by(pickup_hour) %>% 
  summarise(minFare = min(Fare_amount), maxFare = max(Fare_amount), meanFare = mean(Fare_amount), medianFare = median(Fare_amount))
fareStatsByHour %>% 
  dplyr::select(-maxFare, -minFare) %>% 
  gather(metric, value, -pickup_hour) %>% 
  ggplot(aes(x = pickup_hour, y = value, colour = metric)) +
  geom_line() + 
  labs(x = "Hour of the day", y = "Metric Value", title = "Mean and Median Fare by Hour of the Day PickUp=East Harlem North and DropOff=East Harlem North")


##Trip frequency when pickup/dropOff from Park Slope
zn_do_count <- sqldf("SELECT pickup_date, COUNT(*) as DOCount FROM high_pu_zone_ps GROUP BY pickup_date")
barplot(zn_do_count$DOCount, names.arg=zn_do_count$pickup_date, cex.names=0.8, las=2, xlab="Pickup Date", ylab="Number Of Trips", main="Number of Trips Per Day when PickUp=Park Slope", col="blue")

zn_fare_data_freq <- high_pu_zone_ps[(high_pu_zone_ps$pickup_date=='2016-06-04')&(high_pu_zone_ps$Trip_distance <=0.5),]
zn_fare_data_freq$FrequencyCnt <- 0
for(i in 1:nrow(zn_fare_data_freq)){
  zn_fare_data_freq[i, "FrequencyCnt"] <- i
}

plt1 <- ggplot(data=zn_fare_data_freq, aes(x = zn_fare_data_freq$FrequencyCnt, y = zn_fare_data_freq$Fare_amount)) + geom_line(color="red")
plt1+labs(x="Number Of Pickups", y="Fare Amount", title = "Fare Amount variations with respect to Number of Pickups on 2016-06-04")


library(lubridate)
for(row in 1:nrow(high_do_zone_ps)){
   high_do_zone_ps[row, "dropOff_date"] <- toString(as.POSIXct(high_do_zone_ps[row, "Lpep_dropoff_datetime"], format = "%Y-%m-%d"))
}

zn_do_count <- sqldf("SELECT dropOff_date, COUNT(*) as DOCount FROM high_do_zone_ps GROUP BY dropOff_date")
barplot(zn_do_count$DOCount, names.arg=zn_do_count$dropOff_date, cex.names=0.8, las=2, xlab="DropOff Date", ylab="Number Of DropOffs", main="Number of DropOffs Per Day when DropOff=Park Slope", col="blue")

zn_fare_data_freq <- high_do_zone_ps[(high_do_zone_ps$pickup_date=='2016-06-04')&(high_do_zone_ps$Trip_distance <=0.5),]

zn_fare_data_freq$FrequencyCnt <- 0
for(i in 1:nrow(zn_fare_data_freq)){
  zn_fare_data_freq[i, "FrequencyCnt"] <- i
}

plt1 <- ggplot(data=zn_fare_data_freq, aes(x = zn_fare_data_freq$FrequencyCnt, y = zn_fare_data_freq$Fare_amount)) + geom_line(color="red")
plt1+labs(x="Number Of DropOffs", y="Fare Amount", title = "Fare Amount variations with respect to Number of DropOffs on 2016-06-04")




##Trip frequency when pickup/dropOff from East Harlem
zn_do_count <- sqldf("SELECT pickup_date, COUNT(*) as DOCount FROM high_pu_zone_en GROUP BY pickup_date")
barplot(zn_do_count$DOCount, names.arg=zn_do_count$pickup_date, cex.names=0.8, las=2, xlab="Pickup Date", ylab="Number Of PickUps", main="Number of PickUps Per Day when PickUp=East Harlem North", col="blue")

zn_fare_data_freq <- high_pu_zone_en[(high_pu_zone_en$pickup_date=='2016-06-04')&(high_pu_zone_en$Trip_distance <=0.5),]
zn_fare_data_freq$FrequencyCnt <- 0
for(i in 1:nrow(zn_fare_data_freq)){
  zn_fare_data_freq[i, "FrequencyCnt"] <- i
}

plt1 <- ggplot(data=zn_fare_data_freq, aes(x = zn_fare_data_freq$FrequencyCnt, y = zn_fare_data_freq$Fare_amount)) + geom_line(color="red")
plt1+labs(x="Number Of Pickups", y="Fare Amount", title = "Fare Amount variations with respect to Number of Pickups on 2016-06-04")


library(lubridate)
for(row in 1:nrow(high_do_zone_en)){
  high_do_zone_en[row, "dropOff_date"] <- toString(as.POSIXct(high_do_zone_en[row, "Lpep_dropoff_datetime"], format = "%Y-%m-%d"))
}

zn_do_count <- sqldf("SELECT dropOff_date, COUNT(*) as DOCount FROM high_do_zone_en GROUP BY dropOff_date")
barplot(zn_do_count$DOCount, names.arg=zn_do_count$dropOff_date, cex.names=0.8, las=2, xlab="DropOff Date", ylab="Number Of DropOffs", main="Number of DropOffs Per Day when DropOff=East Harlem North", col="blue")

zn_fare_data_freq <- high_do_zone_en[(high_do_zone_en$pickup_date=='2016-06-03')&(high_do_zone_en$Trip_distance <=0.5),]

zn_fare_data_freq$FrequencyCnt <- 0
for(i in 1:nrow(zn_fare_data_freq)){
  zn_fare_data_freq[i, "FrequencyCnt"] <- i
}

plt1 <- ggplot(data=zn_fare_data_freq, aes(x = zn_fare_data_freq$FrequencyCnt, y = zn_fare_data_freq$Fare_amount)) + geom_line(color="red")
plt1+labs(x="Number Of DropOffs", y="Fare Amount", title = "Fare Amount variations with respect to Number of DropOffs on 2016-06-03")



## fare amount with respect to time when pickup and drop off are in different zone Brooklyn
zn_fare_data_diff_ps <- data_hail_ps[(data_hail_ps$Pickup_zone=='Park Slope')&(data_hail_ps$Dropoff_zone!='Park Slope'),]

fareStatsByHour = zn_fare_data_diff_ps %>% 
  group_by(pickup_hour) %>% 
  summarise(minFare = min(Fare_amount), maxFare = max(Fare_amount), meanFare = mean(Fare_amount), medianFare = median(Fare_amount))
fareStatsByHour %>% 
  dplyr::select(-maxFare, -minFare) %>% 
  gather(metric, value, -pickup_hour) %>% 
  ggplot(aes(x = pickup_hour, y = value, colour = metric)) +
  geom_line() + 
  labs(x = "Hour of the day", y = "Metric Value", title = "Mean and Median Fare by Hour of the Day PickUp=Park Slope and DropOff=Other zones in Brooklyn")

zn_fare_data_diff_ps <- data_hail_ps[(data_hail_ps$Dropoff_zone=='Park Slope')&(data_hail_ps$Pickup_zone!='Park Slope'),]

fareStatsByHour = zn_fare_data_diff_ps %>% 
  group_by(pickup_hour) %>% 
  summarise(minFare = min(Fare_amount), maxFare = max(Fare_amount), meanFare = mean(Fare_amount), medianFare = median(Fare_amount))
fareStatsByHour %>% 
  dplyr::select(-maxFare, -minFare) %>% 
  gather(metric, value, -pickup_hour) %>% 
  ggplot(aes(x = pickup_hour, y = value, colour = metric)) +
  geom_line() + 
  labs(x = "Hour of the day", y = "Metric Value", title = "Mean and Median Fare by Hour of the Day DropOff=Park Slope and PickUp=Other zones in Brooklyn")



## fare amount with respect to time when pickup and drop off are in different zone Manhattan
zn_fare_data_diff_en <- data_hail_en[(data_hail_en$Pickup_zone=='East Harlem North')&(data_hail_en$Dropoff_zone!='East Harlem North'),]

fareStatsByHour = zn_fare_data_diff_en %>% 
  group_by(pickup_hour) %>% 
  summarise(minFare = min(Fare_amount), maxFare = max(Fare_amount), meanFare = mean(Fare_amount), medianFare = median(Fare_amount))
fareStatsByHour %>% 
  dplyr::select(-maxFare, -minFare) %>% 
  gather(metric, value, -pickup_hour) %>% 
  ggplot(aes(x = pickup_hour, y = value, colour = metric)) +
  geom_line() + 
  labs(x = "Hour of the day", y = "Metric Value", title = "Mean and Median Fare by Hour of the Day PickUp=East Harlem North and DropOff=Other zones in Manhattan")

zn_fare_data_diff_en <- data_hail_en[(data_hail_en$Dropoff_zone=='East Harlem North')&(data_hail_en$Pickup_zone!='East Harlem North'),]

fareStatsByHour = zn_fare_data_diff_en %>% 
  group_by(pickup_hour) %>% 
  summarise(minFare = min(Fare_amount), maxFare = max(Fare_amount), meanFare = mean(Fare_amount), medianFare = median(Fare_amount))
fareStatsByHour %>% 
  dplyr::select(-maxFare, -minFare) %>% 
  gather(metric, value, -pickup_hour) %>% 
  ggplot(aes(x = pickup_hour, y = value, colour = metric)) +
  geom_line() + 
  labs(x = "Hour of the day", y = "Metric Value", title = "Mean and Median Fare by Hour of the Day DropOff=East Harlem North and PickUp=Other zones in Manhattan")



