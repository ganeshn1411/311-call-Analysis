library(data.table)
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)
library(geosphere)
library(plyr)


weather <- read.csv("674038.csv")
nyca_copy<- nyca
weather_copy <- weather
# Get the date by removing time. 
t<-substr(nyca_copy$Created.Date,1,10)
i<-as.Date(t,"%m/%d/%Y")
nyca_copy$Created.Date<- i





min(nyca_copy$Created.Date)# "2014-06-24"
max(nyca_copy$Created.Date)# "2016-01-10"

nyca.sample <- nyca_copy[c(1:5000), ]
#nyca.sample <- subset(nyca_copy,Borough == "BRONX")
min(nyca.sample$Created.Date)# "2014-06-24"
max(nyca.sample$Created.Date)# "2016-01-10"

weather_copy$DATE <- as.Date(as.character(weather_copy$DATE), "%Y%m%d")

min(weather_copy$DATE)# "2010-01-01"
max(weather_copy$DATE)# "2016-01-16"

sample.index <- which((weather_copy$DATE >=min(nyca.sample$Created.Date)) & (weather_copy$DATE <= max(nyca.sample$Created.Date)))

weather_sample <- weather_copy[sample.index,]
min(weather_sample$DATE)# "2014-06-24"
max(weather_sample$DATE)# "2016-01-10"

unique(weather_sample$LATITUDE)
unique(weather_sample$LONGITUDE)

str(c(table(nyca.sample$Latitude)))
str(c(table(nyca.sample$Longitude)))

nyca.latlong <- delete.na(nyca.sample[,c(match("Latitude", colnames(nyca.sample)), match("Longitude", colnames(nyca.sample)))])
weather.unique.latlong <- delete.na(unique ( weather_sample[, c("STATION","LATITUDE","LONGITUDE")]))

nyca.latlong.out<-mergeLoc(nyca.latlong, weather.unique.latlong)
nyca.latlong.out[,4] <- paste("(",nyca.latlong.out[,1],", ", nyca.latlong.out[,2], ")", sep = "") 
nc <- ncol(nyca.sample)
lat_index <- which(colnames(nyca.sample)== "Latitude")
nyca.sample[,nc+1] <- paste("(",nyca.sample[,51],", ", nyca.sample[,52], ")", sep = "") 

merged_data <- merge(nyca.sample,nyca.latlong.out[,c(3,4), drop = FALSE],by.x = "V54",by.y = "V4")
names(merged_data)[names(merged_data) == 'V3'] <- 'Station' 
merged_data.final <- merge(merged_data,weather_sample,by.x =c("Station", "Created.Date"),by.y = c("STATION","DATE"))


mergeLoc <- function(nyca.func.latlong,weather.func)
{
  nf <- ncol(nyca.func.latlong)
  nyca.func.latlong[,nf+1]<- NA
  for(i in 1:nrow(nyca.func.latlong))
  {
    station.near<-  findDist(nyca.func.latlong[i,1], nyca.func.latlong[i,2],weather.func)
    nyca.func.latlong[i,nf+1] <- station.near
  }
  
  
  return(nyca.func.latlong)
}

map_station <- data.frame(weather$LATITUDE,weather$LONGITUDE)
map_station <- unique(map_station)
map_station$weather.LATITUDE = as.numeric(as.character(map_station$weather.LATITUDE))
map_station$weather.LONGITUDE = as.numeric(as.character(map_station$weather.LONGITUDE))

write.csv(map_station,file="map_station.csv")

map_actual <- data.frame(nyca_copy$Latitude,nyca_copy$Longitude)
map_actual <- unique(map_actual)
map_actual <- map_actual[sample(nrow(map_actual), 10000), ]
map_actual$nyca_copy.Latitude = as.numeric(as.character(map_actual$nyca_copy.Latitude))
map_actual$nyca_copy.Longitude = as.numeric(as.character(map_actual$nyca_copy.Longitude))

write.csv(map_actual,file="map_actual.csv")

merged_data.final1 <- merged_data.final
merged_data.final$Weeks <- sapply(1:nrow(merged_data.final), function(x){
  
  return(difftime(strptime("2010-06-24", format = "%Y-%m-%d"),
                  strptime(merged_data.final$Created.Date[x], format = "%Y-%m-%d"),units="weeks"))
})

merged_data.final$Weeks <- floor(merged_data.final$Weeks)
merged_data.final$Weeks <- abs(merged_data.final$Weeks)



merged_data.final$Weeks <- sapply(1:nrow(nyca.sample), function(x){
  
  return(difftime(strptime("2010-06-24", format = "%Y-%m-%d"),
                  strptime(nyca.sample$Created.Date[x], format = "%Y-%m-%d"),units="weeks"))
})

merged_data.final$Weeks <- floor(nyca.sample$Weeks)
merged_data.final$Weeks <- abs(nyca.sample$Weeks)




tmaxagg <- aggregate(TMAX ~ Weeks, data=merged_data.final, mean)
tminagg <- aggregate(TMIN ~ Weeks, data=merged_data.final, mean)
prcpagg <- aggregate(PRCP ~ Weeks, data=merged_data.final, mean)
snowagg <- aggregate(SNOW ~ Weeks, data=merged_data.final, mean)
snwdagg <- aggregate(SNWD ~ Weeks, data=merged_data.final, mean)
#latagg <- aggregate(LATITUDE ~ Weeks, data=merged_data.final, max)
#longagg <- aggregate(LONGITUDE ~ Weeks, data=merged_data.final, function(x){
  
#})
no_complaints <- count(merged_data.final,c("Weeks"))

write.csv(merged_data.final,"merged.data.final.csv")


merged_agg <- tmaxagg
merged_agg$tmin <- tminagg$TMIN
merged_agg$prcp <- prcpagg$PRCP
merged_agg$snow <- snowagg$SNOW
merged_agg$snowd <- snwdagg$SNWD
merged_agg$no_complaints <- no_complaints$freq
no_complaints_val <- merged_agg[ncol(merged_agg)]
formula = paste(no_complaints_val,"~.-",no_complaints_val)
mod <- lm(as.formula(formula),data = merged_agg)
summary(mod)

delete.na <- function(df, n=0) {
  log <- apply(df, 2, is.na)
  logindex <- apply(log, 1, function(x) sum(x) <= n)
  df[logindex, ]
}

findDist <- function(lat_nyc, long_nyc, weather.func)
{
  weather.func$LATITUDE <- as.numeric(as.character(weather.func$LATITUDE))
  weather.func$LONGITUDE <- as.numeric(as.character(weather.func$LONGITUDE))
  weather.func[,4]<- NA
  for(i in 1: nrow(weather.func))
  {
    dist <- distGeo(c(weather.func[i,2], weather.func[i,3]), c(lat_nyc,long_nyc))
    weather.func[i,4]<- dist 
  }
  rowind <- which(weather.func[,4]== min(weather.func[,4]))
  return(as.character(weather.func[rowind,1]))
}

df <- merged_agg

do_cv <- function(df){
  dfr <- df[sample(nrow(df)),]
  nf <- nrow(df)
  trainrow <- nf*9/10
  train <- data.frame(dfr[(1:(trainrow)),])
  test <- data.frame(dfr[(trainrow + 1):(nf),])
  train.y<-colnames(train)[ncol(train)]
  formula<-paste(train.y," ~. -", train.y)
  mod <- lm(as.formula(formula),data = dfr)
  pred <- predict(mod,test,type = "response")
  pred
}
