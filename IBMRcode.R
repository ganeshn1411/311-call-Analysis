library(plyr)
library(reshape)
library(ggplot2)

import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

nyca <- import.csv("nyca.csv")

nyca_copy <- nyca
t=substr(nyca_copy$Created.Date,1,10)
i=as.Date(t,"%m/%d/%Y")
nyca_copy$Created.Date=i

nyca_bronx <- subset(nyca_copy,Borough == "BRONX")
nyca_brooklyn <- subset(nyca_copy,Borough == "BROOKLYN")
nyca_manhattan <- subset(nyca_copy,Borough == "MANHATTAN")
nyca_queens <- subset(nyca_copy,Borough == "QUEENS")
nyca_staten_island <- subset(nyca_copy,Borough == "STATEN ISLAND")

nyca_bronx[,54] <- format(nyca_bronx$Created.Date,"%m")
nyca_bronx$Year <- format(nyca_bronx$Created.Date,"%y")
nyca_bronx[,54] <- as.numeric(nyca_bronx[,54])
colnames(nyca_bronx)[54] = "Month"
nyca_bronx_a <- transform(nyca_bronx, Month=factor( mapvalues(Month, 1:12, c( "Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec" ) ) ))

nyca_bronx_agg <- count(nyca_bronx_a, c("Month","Agency.Name","Year"))
colnames(nyca_bronx_agg)[3] <- "Year"
colnames(nyca_bronx_agg)[4] <- "Total.Complaints"

formula<-paste("Total.Complaints"," ~ ", "Month","+","Agency.Name")
mod <- lm(as.formula(formula), data = nyca_bronx_agg)
summary(mod)
plot(mod)

# location based model
nyca_bronx_agg_loc <- count(nyca_bronx,c("Latitude","Longitude"))

mod <- lm(freq ~ Latitude + Longitude, data = nyca_bronx_agg_loc)
summary(mod)

nyca_bronx[ 298375,c(6,7,8)]
which(colnames(nyca_bronx)=="Descriptor")
plot(mod)


Pop <- import.csv("Pop.csv")
Pop <- subset(Pop,Year=2010)
Pop_agg <- aggregate(Population ~ Borough,data=Pop,sum)
borough_comp <- data.frame(Total.Complaints = c(nrow(nyca_bronx), nrow(nyca_brooklyn),nrow(nyca_queens),nrow(nyca_manhattan),nrow(nyca_staten_island)), Borough.Name = c("Bronx", "Brooklyn","Queens","Manhattan","Staten Island"))
Pop_agg$Total.Complaints <- borough_comp$Total.Complaints


Names <- c("Bronx","Brooklyn","Manhattan","Queens","Staten Island")
data.m <- melt(Pop_agg, id.vars='Borough')
ggplot(data.m, aes(Names, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")


transformed_data <- data.frame(Borough=c("Population","Total.Complaints"),Bronx=c(2717758,533843),Brooklyn=c(4970026,896317),Manhattan=c(3123068,670714),Queens=c(4460101,621944),Staten_Island=c(912458,139328))
dat.m <- melt(transformed_data, id.vars='Borough')
ggplot(dat.m, aes(variable, value)) +   
  geom_bar(aes(fill = Borough), position = "dodge", stat="identity")

new_precip_weather <- import.csv("672556.csv")
new_precip_weather$DATE=as.Date(as.character(new_precip_weather$DATE), "%Y%m%d")

nycc_sample <- nycc_copy[sample(nrow(nycc_copy),10000),]
str(new_precip_weather)

nycc_merge=merge(x=nycc_sample,y=new_precip_weather,by.x = "Created.Date",by.y = "DATE")

new_precip_weather_date <- unique(new_precip_weather$DATE)
# Removed measurement flag column
new_precip_weather<- new_precip_weather[,-ncol(new_precip_weather)]
new_precip_weather<- new_precip_weather[,-ncol(new_precip_weather)]
# Get row indexes of missing values
new_precip_miss_index <- which(new_precip_weather$HPCP == 99999)

# Remove the missing values rows. 
new_precip_weather.nomiss <- new_precip_weather[-new_precip_miss_index, ]

# Name it as final 

new_precip_weather.final<- new_precip_weather.nomiss

new_aggregate_df <- aggregate(HPCP~.,data = new_precip_weather.final, max)
colnames(new_aggregate_df)[ncol(new_aggregate_df)-1] <- "Max_HPCP"
new_aggregate_df.min <-  aggregate(HPCP~.,data = new_precip_weather.final, min)
new_aggregate_df.min<- new_aggregate_df.min[,ncol(new_aggregate_df.min), drop = FALSE]
colnames(new_aggregate_df.min)[ncol(new_aggregate_df.min)] <- "Min HPCP"

new_aggregate_df$minhpcp <- new_aggregate_df.min
colnames(new_aggregate_df)[ncol(new_aggregate_df)] <- "Min_HPCP"
new_aggregate_df$sum <- new_aggregate_df$LATITUDE + new_aggregate_df$LONGITUDE



