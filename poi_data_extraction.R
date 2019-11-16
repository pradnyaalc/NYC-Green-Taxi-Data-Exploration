library(tidyr)
library(dplyr)

poi <- read.csv("/home/pradnya/monash/5147/viz-2/Point_Of_Interest.csv")
head(poi)

poi_1 <- poi[,c("the_geom","FACI_DOM","BOROUGH","NAME")]
poi_1 <- extract(poi_1,the_geom, into = c('LONGITUDE', 'LATITUDE'), '\\((.*) (.*)\\)', conv = T)

head(poi_1)

write.csv(poi_1, file = "/home/pradnya/monash/5147/viz-2/poi_cleaned_data.csv")