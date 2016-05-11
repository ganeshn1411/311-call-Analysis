library(data.table)
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)
library(geosphere)
library(plyr)
nyca <- read.csv("nyc311dataaa.csv", header=TRUE)
nycb <- read.csv("nyc311dataab.csv", header=TRUE)
nycc <- read.csv("nyc311dataac.csv", header=TRUE)
nycd <- read.csv("nyc311dataad.csv", header=TRUE)
colnames(nycb) <- colnames(nyca)
nyca.col <- colnames(nyca)
nyca.dt <- data.table(nyca, key = colnames(nyca))
nycb.dt <- data.table(nycb, key = colnames(nyca))


# Working Code

nyc.ab <- rbind(nyca.dt, nycb.dt)
colnames(nyc.ab)
nyc.complaintLevels <- levels(nyc.ab[["Complaint.Type"]])
nyc.agencyLevels <- levels(nyc.ab[["Agency"]])
str(nyc.ab[["Created.Date"]])

nyc.sample <- nyc.ab[sample(nrow(nyc.ab), 3), ]

daily_precip =read.csv("Daily precipitation Weather Data.csv")

dates <- as.Date(strDates, "%m/%d/%Y")
nyca$Created.Date=as.Date(as.character(nyca$Created.Date), "%Y%m%d")
n=strftime(nyca$Created.Date,"%Y-%m-%d %H:%M:%S")
str(nyca)
#getwd()
nyca_copy=nyca
t=substr(nyca_copy$Created.Date,1,10)
i=as.Date(t,"%m/%d/%Y")
new_precip_weather=read.csv("672556 _data.csv")
new_precip_weather$DATE=as.Date(as.character(new_precip_weather$DATE), "%Y%m%d")
nyca$Created.Date=i
nyca_merge=merge(nyca,new_precip_weather)
#merged_data=merge(nyca,daily_precip,by.x = "Created.Date",by.y = "DATE")


# updated code 

# Merge of Nyca and weather 


daily_precip =read.csv("Daily precipitation Weather Data.csv")

dates <- as.Date(strDates, "%m/%d/%Y")
nyca$Created.Date=as.Date(as.character(nyca$Created.Date), "%Y%m%d")
n=strftime(nyca$Created.Date,"%Y-%m-%d %H:%M:%S")
str(nyca)
#getwd()
nyca_copy=nyca
colnames(nycc)=colnames(nyca)
nycc_copy=nycc
t=substr(nycc_copy$Created.Date,1,10)
t_sub=substr(t,7,10)
i=as.Date(t,"%m/%d/%Y")
new_precip_weather=read.csv("672556 _data.csv")
new_precip_weather$DATE=as.Date(as.character(new_precip_weather$DATE), "%Y%m%d")
nyca$Created.Date=i
nyca_merge=merge(nyca,new_precip_weather)
#merged_data=merge(nyca,daily_precip,by.x = "Created.Date",by.y = "DATE")

nycc_copy=nycc_copy[,-(30:41)]
nycc_copy=subset(nycc_copy,Borough != "Unspecified")
#df_no.of.complaints <-  aggregate(.~nycc_copy$Borough+nycc_copy$year,data = nycc_copy, length)
nycc_copy$no.of.complaints =1
df2 <- aggregate(nycc_copy[c("no.of.complaints")], by=list(borough=nycc_copy$Borough, year=nycc_copy$year), FUN=sum, na.rm=TRUE)

form =paste(colnames(df2[,-ncol(df2)]),collapse  = '+')
formula=as.formula(paste(colnames(df2)[ncol(df2)],"~",form))
lm_out=lm(formula = formula,data=df2)


# naveen code 

weather <- read.csv("674038.csv")
nyca_copy<- nyca
weather_copy <- weather
# Get the date by removing time. 
t<-substr(nyca_copy$Created.Date,1,10)
i<-as.Date(t,"%m/%d/%Y")
nyca_copy$Created.Date<- i

min(nyca_copy$Created.Date)# "2014-06-24"
max(nyca_copy$Created.Date)# "2016-01-10"

nyca.sample <- nyca_copy[sample(nrow(nyca_copy), 100), ]
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
dist

nyca.sample.na <- delete.na(nyca.sample)
nyca.latlong.out<-mergeLoc(nyca.latlong, weather.unique.latlong)
nf.latlong <- ncol(nyca.latlong.out)
nf.sample <- ncol(nyca.sample.na)
nyca.sample.na[,nf.sample+1]<- NA
nyca.sample.na[,nf.sample+1]<- nyca.latlong.out[,nf.latlong]

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