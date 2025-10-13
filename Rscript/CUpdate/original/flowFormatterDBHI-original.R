# flow loader for DBHydro Insight (DBHI); generates daily, monthly, and annual flow.

library(httr)
library(jsonlite)
library(reshape)
library(zoo)
#library(Hmisc)
#library(rgdal)
library(lubridate)
library(sf)
library(terra)

# data to load ----
flow_ld <- function(staName,sdate,edate,wy){
  # load dbkey table
  dbkeys <- read.csv('C:/donatto/lox/r_codes/dbkeysFlow.csv',header=T)
  
  dbkeys$Start.Date <- as.Date(as.character(dbkeys$Start.Date),'%m/%d/%Y')
  dbkeys$End.Date <- as.Date(as.character(dbkeys$End.Date),'%m/%d/%Y')
  
  dbkeys <- dbkeys[!is.na(dbkeys$Start.Date),]
  
  dbkeys$Long <- as.numeric(substr(dbkeys$Longitude,1,2))+(as.numeric(substr(dbkeys$Longitude,3,4))/60)+
    (as.numeric(substr(dbkeys$Longitude,5,nchar(dbkeys$Longitude)))/3600)
  
  dbkeys$Lat <- as.numeric(substr(dbkeys$Latitude,1,2))+(as.numeric(substr(dbkeys$Latitude,3,4))/60)+
    (as.numeric(substr(dbkeys$Latitude,5,nchar(dbkeys$Latitude)))/3600)
  names(dbkeys)[1] <- 'DBKEY'
  
  
  cord.dec <- st_as_sf(dbkeys,coords=c('Long','Lat'),crs=4326)
  
  cord.UTM <- st_transform(cord.dec,crs=26917)
  
  dbkeys$Xutm <- st_coordinates(cord.UTM$geometry)[,1] # cord.UTM@coords[1:nrow(dbkeys)]
  dbkeys$Yutm <- st_coordinates(cord.UTM$geometry)[,2]  #cord.UTM@coords[(nrow(dbkeys)+1):(nrow(dbkeys)+nrow(dbkeys))]
  
  # Select stations with 1) most recent record and 2) oldest record based on date range provided as input
  ss<-dbkeys[which(grepl(staName,dbkeys$Station)),]
  ss$dateDel <- ss$End.Date - ss$Start.Date
  
  dbkeySelect <- ss$DBKEY[which.max(ss$dateDel)]
  
  
  
  # Create the url
  sdate.f <- as.numeric(format(as.Date(sdate),'%Y%m%d'))
  edate.f <- as.numeric(format(as.Date(edate),'%Y%m%d'))  
  
  # Define the API endpoint
  api_url <- paste0('https://api.sfwmd.gov/v1/insights-data/cont/data?timeseriesIds=',dbkeySelect,'&reportType=timeseries&format=json&startDate=',sdate.f,'&endDate=',edate.f)  
  # Make the GET request
  response <- GET(api_url)
  
  print(api_url)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON content
    data <- fromJSON(content(response, as="text", encoding = "UTF-8"))
  }    
  
  if(is.list(data)){
    result <- data$timeseries
  }
  
  # daily headers
  # "WY"         "MY"         "Date"       "Data Value"
  d.flow <- result
  c.keep <- c('timestamp','value')
  d.flow <- d.flow[which(grepl(paste(c(c.keep),collapse='|'),names(d.flow)))]
  names(d.flow) <- c('Date','Data Value') 
  #d.flow$Date <- gsub(' .*','',d.flow$Date)
  d.flow$Date <- as.Date(d.flow$Date,'%m/%d/%Y')
  d.flow <- data.frame(WY=ifelse(month(d.flow$Date)>wy,year(d.flow$Date)+1,year(d.flow$Date)),MY=as.yearmon(d.flow$Date),d.flow)
  # my headers and data
  my.flow <- aggregate(d.flow[4],by=list(d.flow$MY),function(x) sum(x,na.rm=T))
  names(my.flow)[1] <- 'MY'
  # wy headers and data
  wy.flow <- aggregate(d.flow[4],by=list(d.flow$WY),function(x) sum(x,na.rm=T))
  names(wy.flow)[1] <- 'WY'
  
  return(list(d.flow,my.flow,wy.flow))
}
